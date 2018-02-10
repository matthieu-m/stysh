//! Syntactic pass, aka parsing.
//!
//! Expression parser.

use std;

use basic::mem;
use model::tt;
use model::syn::*;
use pass::syn::typ;

use super::com::RawParser;

pub fn parse_expression<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Expression<'g>
{
    let mut parser = ExprParser::new(*raw);
    let expr = parser.parse();
    *raw = parser.into_raw();
    expr
}

pub fn parse_pattern<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Pattern<'g>
{
    let mut parser = PatternParser::new(*raw);
    let stmt = parser.parse();
    *raw = parser.into_raw();
    stmt
}

pub fn parse_statement<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Statement<'g>
{
    let mut parser = StmtParser::new(*raw);
    let stmt = parser.parse();
    *raw = parser.into_raw();
    stmt
}

//
//  Implementation Details (Expression)
//
struct ExprParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct Precedence(u8);

struct ShuntingYard<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    op_stack: mem::Array<'local, (Operator, u32, Precedence)>,
    expr_stack: mem::Array<'local, Expression<'g>>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
enum Operator {
    Bin(BinaryOperator),
    Pre(PrefixOperator),
}

impl<'a, 'g, 'local> ExprParser<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> ExprParser<'a, 'g, 'local> {
        ExprParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse(&mut self) -> Expression<'g> {
        use model::tt::Kind as K;

        fn binop(kind: tt::Kind) -> Option<(Operator, Precedence)> {
            use model::syn::BinaryOperator as B;

            //  Ordered from higher precedence to lower; higher binding tigther.
            match kind {
                K::SignDoubleSlash => Some((B::FloorBy, 7)),
                K::SignStar => Some((B::Times, 7)),

                K::SignDash => Some((B::Minus, 6)),
                K::SignPlus => Some((B::Plus, 6)),

                K::SignLeft => Some((B::LessThan, 5)),
                K::SignLeftEqual => Some((B::LessThanOrEqual, 5)),
                K::SignRight => Some((B::GreaterThan, 5)),
                K::SignRightEqual => Some((B::GreaterThanOrEqual, 5)),

                K::SignBangEqual => Some((B::Different, 4)),
                K::SignDoubleEqual => Some((B::Equal, 4)),

                K::KeywordAnd => Some((B::And, 3)),

                K::KeywordXor => Some((B::Xor, 2)),

                K::KeywordOr => Some((B::Or, 1)),

                _ => None
            }.map(|(b, p)| (Operator::Bin(b), Precedence(p)))
        }

        //  An expression is an expression, optionally followed by a binary 
        //  operator and another expression.

        //  Note:   an expression immediatelly followed by a tuple expression is
        //          a constructor or function call expression.

        //  Use the Shunting Yard algorithm to parse this "expr [op expr]" into
        //  an expression tree.
        let mut yard = ShuntingYard::new(self.raw.global(), self.raw.local());

        while let Some(node) = self.raw.peek() {
            //  An expression.
            let expr = match node {
                tt::Node::Run(tokens) => {
                    let kind = tokens[0].kind();
                    let range = tokens[0].range();

                    match kind {
                        K::KeywordIf => self.parse_if_else(),
                        K::KeywordNot => {
                            self.raw.pop_tokens(1);
                            yard.push_operator(
                                Operator::Pre(PrefixOperator::Not),
                                tokens[0].offset() as u32,
                                Precedence(8)
                            );
                            continue;
                        },
                        K::NameField => {
                            self.raw.pop_tokens(1);
                            yard.push_field(FieldIdentifier(range));
                            continue;
                        },
                        K::SignBind => break,
                        _ => {
                            let ty = typ::try_parse_type(&mut self.raw);
                            if let Some(ty) = ty {
                                self.parse_constructor(ty)
                            } else {
                                self.raw.pop_tokens(1);
                                tokens[0]
                                    .into_expr()
                                    .expect(&format!("FIXME: {:?}", tokens[0]))
                            }
                        },
                    }
                },
                tt::Node::Braced(o, n, c) => {
                    self.raw.pop_node();

                    match o.kind() {
                        K::BraceOpen => {
                            let block = self.parse_braces(n, o, c);
                            Expression::Block(self.raw.intern(block))
                        },
                        K::ParenthesisOpen => self.parse_parens(n, o, c),
                        _ => unimplemented!(),
                    }
                },
                tt::Node::Bytes(_, f, _) => {
                    self.raw.pop_node();

                    let bytes = self.raw.global().insert_slice(f);
                    Expression::Lit(Literal::Bytes(bytes, node.range()))
                },
                tt::Node::String(_, f, _) => {
                    self.raw.pop_node();

                    let string = self.raw.global().insert_slice(f);
                    Expression::Lit(Literal::String(string, node.range()))
                },
                tt::Node::UnexpectedBrace(_) => unimplemented!(),
            };

            yard.push_expression(expr);

            //  Optionally followed by a binary operator and another expression.
            if let Some(tt::Node::Run(tokens)) = self.raw.peek() {
                if let Some((op, prec)) = binop(tokens[0].kind()) {
                    self.raw.pop_tokens(1);
                    yard.push_operator(op, tokens[0].offset() as u32, prec);
                    continue;   // go get right hand side!
                }
            }

            match self.raw.peek_kind() {
                //  It might be a field access
                Some(K::NameField) |
                //  It might be a constructor or function call.
                Some(K::ParenthesisOpen) => continue,
                _ => break,
            };
        }

        yard.pop_expression()
    }

    fn parse_block(&mut self) -> Block<'g> {
        if let Some(tt::Node::Braced(o, n, c)) = self.raw.peek() {
            self.raw.pop_node();
            return self.parse_braces(n, o, c);
        }

        unimplemented!("Expected block, got {:?}", self.raw.peek());
    }

    fn parse_braces(&self, ns: &[tt::Node], o: tt::Token, c: tt::Token)
        -> Block<'g>
    {
        let mut raw = self.raw.spawn(ns);
        let mut stmts = raw.local_array();

        while let Some(tok) = raw.peek().map(|n| n.front()) {
            if tok.kind() != tt::Kind::KeywordSet &&
                tok.kind() != tt::Kind::KeywordVar {
                break;
            }
            stmts.push(parse_statement(&mut raw));
        }

        let statements = self.raw.intern_slice(&stmts);
        let expression = ExprParser::new(raw).parse();

        Block {
            statements: statements,
            expression: expression,
            open: o.offset() as u32,
            close: c.offset() as u32,
        }
    }

    fn parse_parens(
        &mut self,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token
    )
        -> Expression<'g>
    {
        Expression::Tuple(self.parse_tuple(ns, o, c))
    }

    fn parse_tuple(
        &mut self,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token
    )
        -> Tuple<'g, Expression<'g>>
    {
        parse_tuple_impl(&mut self.raw, parse_expression, ns, o, c)
    }

    fn parse_constructor(&mut self, ty: Type<'g>) -> Expression<'g> {
        let c = parse_constructor_impl(&mut self.raw, parse_expression, ty);
        Expression::Constructor(c)
    }

    fn parse_if_else(&mut self) -> Expression<'g> {
        let if_ = self.raw.pop_kind(tt::Kind::KeywordIf).expect(":if");

        let condition = self.parse();
        //  FIXME(matthieum): possibly missing.
        let true_expr = self.parse_block();

        if let Some(else_) = self.raw.pop_kind(tt::Kind::KeywordElse) {
            //  FIXME(matthieum): only ":if" and "{ ... }" are legal.

            let false_expr = self.parse_block();

            return Expression::If(self.raw.intern(IfElse {
                condition: condition,
                true_expr: true_expr,
                false_expr: false_expr,
                if_: if_.offset() as u32,
                else_: else_.offset() as u32,
            }));
        }

        //  FIXME(matthieum): ";" is legal.
        unimplemented!()
    }
}

impl<'g, 'local> ShuntingYard<'g, 'local>
    where 'g: 'local
{
    fn new(global_arena: &'g mem::Arena, local_arena: &'local mem::Arena)
        -> ShuntingYard<'g, 'local>
    {
        ShuntingYard {
            global_arena: global_arena,
            op_stack: mem::Array::new(local_arena),
            expr_stack: mem::Array::new(local_arena),
        }
    }

    fn push_expression(&mut self, expr: Expression<'g>) {
        //  Function calls are distinguished from regular expression by having
        //  a normal expression immediately followed by a tuple expression
        //  without an intervening operator.
        if let Expression::Tuple(tuple) = expr {
            if let Some(callee) = self.pop_trailing_expression() {
                self.expr_stack.push(
                    Expression::FunctionCall(
                        FunctionCall {
                            function: self.global_arena.insert(callee),
                            arguments: tuple,
                        }
                    )
                );
                return;
            }
        }
        self.expr_stack.push(expr);
    }

    fn push_field(&mut self, field: FieldIdentifier) {
        if let Some(accessed) = self.pop_trailing_expression() {
            self.expr_stack.push(
                Expression::FieldAccess(FieldAccess {
                    accessed: self.global_arena.insert(accessed),
                    field: field,
                })
            );
            return;
        }

        unimplemented!("{:?} -> {:?}", field, self);
    }

    fn push_operator(
        &mut self,
        op: Operator,
        pos: u32,
        prec: Precedence)
    {
        self.pop_operators(prec);
        self.op_stack.push((op, pos, prec));
    }

    fn pop_expression(&mut self) -> Expression<'g> {
        self.pop_operators(Precedence(0));

        assert!(self.expr_stack.len() == 1, "{:?}", self);

        self.expr_stack.pop().expect("Asserted")
    }

    fn pop_operators(&mut self, threshold: Precedence) {
        while let Some((op, pos)) = self.pop_operator_impl(threshold) {
            match op {
                Operator::Bin(op) => {
                    let right_hand = self.expr_stack.pop().expect("Right");
                    let left_hand = self.expr_stack.pop().expect("Left");
                    self.expr_stack.push(Expression::BinOp(
                        op,
                        pos,
                        self.global_arena.insert(left_hand),
                        self.global_arena.insert(right_hand),
                    ));
                },
                Operator::Pre(op) => {
                    let expr = self.expr_stack.pop().expect("Expression");
                    self.expr_stack.push(Expression::PreOp(
                        op,
                        pos,
                        self.global_arena.insert(expr),
                    ));
                },
            };
        }
    }

    /// Pops the last expression if there was no operator afterwards
    fn pop_trailing_expression(&mut self) -> Option<Expression<'g>> {
        let last_op = self.op_stack.peek().map(|&(_, pos, _)| pos).unwrap_or(0);
        if let Some(expr) = self.expr_stack.peek().cloned() {
            if last_op as usize <= expr.range().offset() {
                return self.expr_stack.pop();
            }
        }
        None
    }

    fn pop_operator_impl(&mut self, threshold: Precedence)
        -> Option<(Operator, u32)>
    {
        if let Some((op, pos, prec)) = self.op_stack.peek().cloned() {
            if threshold <= prec {
                self.op_stack.pop();
                return Some((op, pos));
            }
        }
        None
    }
}

trait IntoExpr<'g> {
    fn into_expr(self) -> Option<Expression<'g>>;
}

impl<'g> IntoExpr<'g> for tt::Token {
    fn into_expr(self) -> Option<Expression<'g>> {
        use self::tt::Kind::*;
        use self::Expression::*;
        use self::Literal::*;

        match self.kind() {
            LitBoolFalse => Some(Lit(Bool(false, self.range()))),
            LitBoolTrue => Some(Lit(Bool(true, self.range()))),
            LitIntegral => Some(Lit(Integral(self.range()))),
            NameValue => Some(Var(VariableIdentifier(self.range()))),
            _ => None,
        }
    }
}

impl<'g, 'local> std::fmt::Debug for ShuntingYard<'g, 'local> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "expr_stack: {:?}, op_stack: {:?}",
            self.expr_stack,
            self.op_stack
        )
    }
}

//
//  Implementation Details (Pattern)
//
#[derive(Debug)]
struct PatternParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>,
}

impl<'a, 'g, 'local> PatternParser<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> Self {
        PatternParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse(&mut self) -> Pattern<'g> {
        use model::tt::Kind as K;

        match self.raw.peek_kind() {
            Some(K::NameType) => self.parse_type_name(),
            Some(K::NameValue) => self.parse_value_name(),
            Some(K::ParenthesisOpen) => self.parse_parens(),
            Some(K::SignUnderscore) => self.parse_underscore(),
            Some(k) => unimplemented!("Expected identifier or tuple, got {:?}", k),
            None => unimplemented!("Expected identifier or tuple, got nothing"),
        }
    }

    fn parse_parens(&mut self) -> Pattern<'g> {
        match self.raw.peek() {
            Some(tt::Node::Braced(o, n, c)) => {
                self.raw.pop_node();
                self.parse_tuple(n, o, c)
            },
            n => unimplemented!("Expected tuple, got {:?}", n),
        }
    }

    fn parse_type_name(&mut self) -> Pattern<'g> {
        if let Some(ty) = typ::try_parse_type(&mut self.raw) {
            let c = parse_constructor_impl(&mut self.raw, parse_pattern, ty);
            Pattern::Constructor(c)
        } else {
            unimplemented!("parse_type_name - {:?}", self)
        }
    }

    fn parse_underscore(&mut self) -> Pattern<'static> {
        let u =
            self.raw.pop_kind(tt::Kind::SignUnderscore).expect("underscore");
        Pattern::Ignored(VariableIdentifier(u.range()))
    }

    fn parse_value_name(&mut self) -> Pattern<'static> {
        let name = self.raw.pop_kind(tt::Kind::NameValue).expect("name");
        Pattern::Var(VariableIdentifier(name.range()))
    }

    fn parse_tuple(
        &mut self,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token
    )
        -> Pattern<'g>
    {
        Pattern::Tuple(
            parse_tuple_impl(&mut self.raw, parse_pattern, ns, o, c)
        )
    }
}

//
//  Implementation Details (Statement)
//
struct StmtParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>,
}

impl<'a, 'g, 'local> StmtParser<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> Self {
        StmtParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse(&mut self) -> Statement<'g> {
        use model::tt::Kind as K;

        match self.raw.peek_kind() {
            Some(K::KeywordSet) => self.parse_set(),
            Some(K::KeywordVar) => self.parse_var(),
            Some(k) => unimplemented!("Expected :set or :var, got {:?}", k),
            None => unimplemented!("Expected :set or :var, got nothing"),
        }
    }

    fn parse_set(&mut self) -> Statement<'g> {
        let set =
            self.raw
                .pop_kind(tt::Kind::KeywordSet)
                .map(|t| t.offset() as u32)
                .expect(":set");

        let left = parse_expression(&mut self.raw);

        let (expr, bind, semi) = self.parse_bind();

        Statement::Set(VariableReBinding {
            left: left,
            expr: expr,
            set: set,
            bind: bind,
            semi: semi,
        })
    }

    fn parse_var(&mut self) -> Statement<'g> {
        let var =
            self.raw
                .pop_kind(tt::Kind::KeywordVar)
                .map(|t| t.offset() as u32)
                .expect(":var");

        let pattern = parse_pattern(&mut self.raw);

        //  TODO(matthieum): parse type.

        let (expr, bind, semi) = self.parse_bind();

        Statement::Var(VariableBinding {
            pattern: pattern,
            type_: None,
            expr: expr,
            var: var,
            colon: 0,
            bind: bind,
            semi: semi,
        })
    }

    fn parse_bind(&mut self) -> (Expression<'g>, u32, u32) {
        let bind =
            self.raw
                .pop_kind(tt::Kind::SignBind)
                .map(|t| t.offset() as u32)
                .unwrap_or(0);

        let expr = parse_expression(&mut self.raw);

        let semi =
            self.raw
                .pop_kind(tt::Kind::SignSemiColon)
                .map(|t| t.offset())
                .unwrap_or(expr.range().end_offset() - 1) as u32;

        (expr, bind, semi)
    }
}

//
//  Implementation Details (Tuple)
//
fn parse_constructor_impl<'a, 'g, 'local, T: 'g + Copy + Range>(
    raw: &mut RawParser<'a, 'g, 'local>,
    inner_parser: fn(&mut RawParser<'a, 'g, 'local>) -> T,
    ty: Type<'g>
)
    -> Constructor<'g, T>
{
    let tuple = if let Some(tt::Node::Braced(o, ns, c)) = raw.peek() {
        assert_eq!(o.kind(), tt::Kind::ParenthesisOpen);

        raw.pop_node();
        parse_tuple_impl(raw, inner_parser, ns, o, c)
    } else {
        Default::default()
    };
    Constructor { type_: ty, arguments: tuple }
}


fn parse_tuple_impl<'a, 'g, 'local, T: 'g + Copy + Range>(
    raw: &mut RawParser<'a, 'g, 'local>,
    inner_parser: fn(&mut RawParser<'a, 'g, 'local>) -> T,
    ns: &'a [tt::Node<'a>],
    o: tt::Token,
    c: tt::Token,
)
    -> Tuple<'g, T>
{
    let mut fields = raw.local_array();
    let mut commas = raw.local_array();

    let mut inner = raw.spawn(ns);

    while let Some(_) = inner.peek() {
        let f = inner_parser(&mut inner);
        fields.push(f);

        if let Some(c) = inner.pop_kind(tt::Kind::SignComma) {
            commas.push(c.range().offset() as u32)
        } else {
            commas.push(f.range().end_offset() as u32 - 1)
        };
    }

    assert!(inner.peek().is_none());

    Tuple {
        fields: raw.intern_slice(fields.into_slice()),
        commas: raw.intern_slice(commas.into_slice()),
        open: o.offset() as u32,
        close: c.offset() as u32,
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::mem;
    use model::syn::*;
    use model::syn_builder::Factory;

    #[test]
    fn basic_add() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b"1 + 2"),
            e.bin_op(e.int(0, 1), e.int(4, 1)).build()
        );
    }

    #[test]
    fn basic_var() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);

        assert_eq!(
            stmtit(&global_arena, b" :var fool := 1234;"),
            syn.stmt().var(syn.pat().var(6, 4), syn.expr().int(14, 4)).build()
        );
    }

    #[test]
    fn basic_var_automatic_insertion() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);

        assert_eq!(
            stmtit(&global_arena, b" :var fool 1234"),
            syn.stmt()
                .var(syn.pat().var(6, 4), syn.expr().int(11, 4))
                .bind(0)
                .semi_colon(14)
                .build()
        );
    }

    #[test]
    fn basic_block() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        assert_eq!(
            exprit(&global_arena, b"{\n    :var fool := 1234;\n    fool\n}"),
            Expression::Block(
                &e.block(e.var(29, 4))
                    .range(0, 35)
                    .push_stmt(s.var(p.var(11, 4), e.int(19, 4)).build())
                    .build()
            )
        );
    }

    #[test]
    fn basic_bytes() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b"b'1 + 2'"),
            e.literal(0, 8).push_text(2, 5).bytes().build()
        );
    }

    #[test]
    fn basic_constructor() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);

        assert_eq!(
            exprit(&global_arena, b"True"),
            syn.expr().constructor(syn.type_().simple(0, 4)).build()
        );
    }

    #[test]
    fn basic_constructor_arguments() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, t) = (syn.expr(), syn.type_());

        assert_eq!(
            exprit(&global_arena, b"Some(1)"),
            e.constructor(t.simple(0, 4)).push(e.int(5, 1)).build()
        );

    }

    #[test]
    fn basic_nested_constructor() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);

        assert_eq!(
            exprit(&global_arena, b"Bool::True"),
            syn.expr().constructor(
                syn.type_().nested(6, 4).push(0, 4).build()
            ).build()
        );
    }

    #[test]
    fn basic_function_call() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        assert_eq!(
            exprit(&global_arena, b"basic(1, 2)"),
            e.function_call(e.var(0, 5), 5, 10)
                .push(e.int(6, 1))
                .push(e.int(9, 1))
                .build()
        );

        assert_eq!(
            stmtit(&global_arena, b":var a := basic(1, 2);"),
            s.var(
                p.var(5, 1),
                e.function_call(e.var(10, 5), 15, 20)
                    .push(e.int(16, 1))
                    .push(e.int(19, 1))
                    .build(),
            )
            .build()
        );
    }

    #[test]
    fn basic_if_else() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b":if true { 1 } :else { 0 }"),
            Expression::If(
                &e.if_else(
                    e.bool_(4, 4),
                    e.block(e.int(11, 1)).build(),
                    e.block(e.int(23, 1)).build(),
                ).build()
            )
        )
    }

    #[test]
    fn basic_string() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b"'1 + 2'"),
            e.literal(0, 7).push_text(1, 5).string().build()
        );
    }

    #[test]
    fn boolean_basic() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        assert_eq!(
            stmtit(&global_arena, b":var x := true;"),
            s.var(p.var(5, 1), e.bool_(10, 4)).build()
        );
    }

    #[test]
    fn field_access_basic() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b"tup.42"),
            e.field_access(e.var(0, 3)).length(3).build()
        );
    }

    #[test]
    fn field_access_recursive() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b"tup.42.53"),
            e.field_access(e.field_access(e.var(0, 3)).length(3).build())
                .length(3)
                .build()
        );
    }

    #[test]
    fn set_basic() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let e = syn.expr();

        assert_eq!(
            stmtit(&global_arena, b" :set fool := 1234;"),
            syn.stmt().set(e.var(6, 4), e.int(14, 4)).build()
        );
    }

    #[test]
    fn set_field() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let e = syn.expr();

        assert_eq!(
            stmtit(&global_arena, b" :set foo.0 := 1234;"),
            syn.stmt().set(
                e.field_access(e.var(6, 3)).build(),
                e.int(15, 4)
            ).build()
        );
    }

    #[test]
    fn var_constructor() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s, t) = (syn.expr(), syn.pat(), syn.stmt(), syn.type_());

        assert_eq!(
            stmtit(&global_arena, b":var Some(x) := Some(1);"),
            s.var(
                p.constructor(t.simple(5, 4)).push(p.var(10, 1)).build(),
                e.constructor(t.simple(16, 4)).push(e.int(21, 1)).build(),
            ).build()
        );
    }

    #[test]
    fn var_ignored() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        assert_eq!(
            stmtit(&global_arena, b":var _ := 1;"),
            s.var(p.ignored(5), e.int(10, 1)).build()
        );
    }

    #[test]
    fn var_ignored_nested() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        assert_eq!(
            stmtit(&global_arena, b":var (_, b) := (1, 2);"),
            s.var(
                p.tuple().push(p.ignored(6)).push(p.var(9, 1)).build(),
                e.tuple().push(e.int(16, 1)).push(e.int(19, 1)).build(),
            ).build()
        );
    }

    #[test]
    fn var_tuple() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        assert_eq!(
            stmtit(&global_arena, b":var (a, b) := (1, 2);"),
            s.var(
                p.tuple().push(p.var(6, 1)).push(p.var(9, 1)).build(),
                e.tuple().push(e.int(16, 1)).push(e.int(19, 1)).build(),
            ).build()
        );
    }

    #[test]
    fn shunting_yard_prefix() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b":not a :or b :and c"),
            e.bin_op(
                e.pre_op(e.var(5, 1)).build(),
                e.bin_op(e.var(11, 1), e.var(18, 1)).and().build()
            ).or().build()
        );
    }

    #[test]
    fn shunting_yard_simple() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        let left = e.bin_op(
            e.int(0, 1),
            e.bin_op(e.int(4, 1), e.int(8, 1)).times().build()
        ).build();

        let right =
            e.bin_op(e.int(12, 1), e.int(17, 1)).floor_by().build();

        assert_eq!(
            exprit(&global_arena, b"1 + 2 * 3 < 4 // 5"),
            e.bin_op(left, right).less_than().build()
        )
    }

    #[test]
    fn tuple_basic() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        assert_eq!(
            exprit(&global_arena, b"(1)"),
            e.tuple().push(e.int(1, 1)).build()
        );
    }

    #[test]
    fn tuple_nested() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        let inner = e.tuple().push(e.int(5, 1)).push(e.int(8, 1)).build();

        assert_eq!(
            exprit(&global_arena, b"(1, (2, 3), 4)"),
            e.tuple().push(e.int(1, 1)).push(inner).push(e.int(12, 1)).build()
        );
    }

    fn exprit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Expression<'g> {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let e = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_expression(&mut raw)
        };
        local_arena.recycle();

        e
    }

    fn stmtit<'g>(global_arena: &'g mem::Arena, raw: &[u8])
        -> Statement<'g>
    {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let stmt = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_statement(&mut raw)
        };
        local_arena.recycle();

        stmt
    }
}
