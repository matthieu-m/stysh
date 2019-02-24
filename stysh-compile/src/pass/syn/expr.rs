//! Syntactic pass, aka parsing.
//!
//! Expression parser.

use std;

use basic::mem;
use basic::com::Span;
use model::tt;
use model::ast::*;
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
            use model::ast::BinaryOperator as B;

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
                    let token = tokens[0];
                    let kind = token.kind();

                    match kind {
                        K::KeywordIf => self.parse_if_else(),
                        K::KeywordLoop => self.parse_loop(),
                        K::KeywordNot => {
                            self.raw.pop_tokens(1);
                            yard.push_operator(
                                Operator::Pre(PrefixOperator::Not),
                                token.offset() as u32,
                                Precedence(8)
                            );
                            continue;
                        },
                        K::LitBoolFalse | K::LitBoolTrue
                            => self.parse_bool(kind),
                        K::LitIntegral => self.parse_integral(),
                        K::NameField => {
                            let field = self.parse_field_identifier();
                            yard.push_field(field);
                            continue;
                        },
                        K::NameValue => {
                            self.raw.pop_tokens(1);
                            Expression::Var(self.raw.resolve_variable(token))
                        },
                        K::SignBind => break,
                        _ => {
                            let ty = typ::try_parse_type(&mut self.raw);
                            if let Some(ty) = ty {
                                self.parse_constructor(ty)
                            } else {
                                unimplemented!("Expected type, got {:?}", token);
                            }
                        },
                    }
                },
                tt::Node::Braced(..) => self.parse_braced(node),
                tt::Node::Bytes(..) => self.parse_bytes(node),
                tt::Node::String(..) => self.parse_string(node),
                tt::Node::UnexpectedBrace(..) => unimplemented!(),
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

    fn parse_bool(&mut self, kind: tt::Kind) -> Expression<'g> {
        use self::tt::Kind::*;

        let value = match kind {
            LitBoolFalse => false,
            LitBoolTrue => true,
            _ => panic!("Unexpected kind {:?}", kind),
        };

        let token = self.raw.pop_kind(kind).expect("true/false");
        Expression::Lit(Literal::Bool(value, token.span()))
    }

    fn parse_braced(&mut self, node: tt::Node<'a>) -> Expression<'g> {
        use model::tt::Kind as K;

        if let tt::Node::Braced(o, n, c) = node {
            self.raw.pop_node();

            match o.kind() {
                K::BraceOpen => {
                    let block = self.parse_braces(n, o, c);
                    Expression::Block(self.raw.intern(block))
                },
                K::ParenthesisOpen => self.parse_parens(n, o, c),
                _ => unimplemented!(),
            }
        } else {
            unreachable!("Not a Braced node: {:?}", node);
        }
    }

    fn parse_braces(&self, ns: &[tt::Node], o: tt::Token, c: tt::Token)
        -> Block<'g>
    {
        let mut raw = self.raw.spawn(ns);
        let statements = parse_statements_impl(&mut raw);
        let expression = if let Some(_) = raw.peek_kind() {
            Some(raw.intern(ExprParser::new(raw).parse()))
        } else {
            None
        };

        Block {
            statements: statements,
            expression: expression,
            open: o.offset() as u32,
            close: c.offset() as u32,
        }
    }

    fn parse_bytes(&mut self, node: tt::Node) -> Expression<'g> {
        if let tt::Node::Bytes(_, f, _) = node {
            self.raw.pop_node();

            let (fragments, result) = self.parse_string_impl(f);
            Expression::Lit(Literal::Bytes(fragments, result, node.span()))
        } else {
            unreachable!("Not a Bytes node: {:?}", node);
        }
    }

    fn parse_constructor(&mut self, ty: Type<'g>) -> Expression<'g> {
        let sep = tt::Kind::SignBind;
        let c =
            parse_constructor_impl(&mut self.raw, parse_expression, sep, ty);
        Expression::Constructor(c)
    }

    fn parse_field_identifier(&mut self) -> FieldIdentifier {
        let token = self.raw.pop_kind(tt::Kind::NameField).expect("Token");
        let id = self.raw.intern_id_of(token);
        let source = self.raw.source(token);
        debug_assert!(!source.is_empty());

        if source[0] < b'0' || source[0] > b'9' {
            return FieldIdentifier::Name(id, token.span());
        }

        if let Some(i) = parse_integral_impl(source, false) {
            assert!(i >= 0);
            FieldIdentifier::Index(i as _, token.span())
        } else {
            unimplemented!("Cannot parse {:?} from {:?}", source, token)
        }
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

    fn parse_integral(&mut self) -> Expression<'g> {
        let token = self.raw.pop_kind(tt::Kind::LitIntegral).expect("Token");
        let source = self.raw.source(token);

        if let Some(i) = parse_integral_impl(source, true) {
            Expression::Lit(Literal::Integral(i, token.span()))
        } else {
            unimplemented!("Cannot parse {:?} from {:?}", source, token)
        }
    }

    fn parse_loop(&mut self) -> Expression<'g> {
        let loop_ = self.raw.pop_kind(tt::Kind::KeywordLoop).expect(":loop");
        let loop_ = loop_.offset() as u32;

        if let Some(tt::Node::Braced(o, ns, c)) = self.raw.peek() {
            self.raw.pop_node();
            let mut raw = self.raw.spawn(ns);
            let statements = parse_statements_impl(&mut raw);
            assert!(raw.peek().is_none());

            let open = o.offset() as u32;
            let close = c.offset() as u32;

            return Expression::Loop(
                self.raw.intern(Loop { statements, loop_, open, close })
            );
        }

        unimplemented!("Expected braces after :loop");
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

    fn parse_string(&mut self, node: tt::Node) -> Expression<'g> {
        if let tt::Node::String(_, f, _) = node {
            self.raw.pop_node();

            let (fragments, result) = self.parse_string_impl(f);
            Expression::Lit(Literal::String(fragments, result, node.span()))
        } else {
            unreachable!("Not a String node: {:?}", node);
        }
    }

    fn parse_tuple(
        &mut self,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token
    )
        -> Tuple<'g, Expression<'g>>
    {
        self.raw.parse_tuple(parse_expression, tt::Kind::SignBind, ns, o, c)
    }

    fn parse_string_impl(&self, f: &[StringFragment])
        -> (&'g [StringFragment], mem::InternId)
    {
        use self::StringFragment::*;

        let mut buffer = self.raw.local_array();
        for &fragment in f {
            match fragment {
                Text(tok) => buffer.extend(self.raw.source(tok)),
                SpecialCharacter(tok) => match self.raw.source(tok) {
                    b"N" => buffer.push(b'\n'),
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        }

        (
            self.raw.global().insert_slice(f),
            self.raw.intern_bytes(buffer.into_slice()),
        )
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
            if last_op as usize <= expr.span().offset() {
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

fn parse_integral_impl(raw: &[u8], allow_underscores: bool) -> Option<i64> {
    let mut value = 0;
    for byte in raw {
        match *byte {
            b'0'...b'9' => {
                value *= 10;
                value += (byte - b'0') as i64;
            },
            b'_' if allow_underscores => (),
            _ => return None,
        }
    }

    Some(value)
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
            let sep = tt::Kind::SignColon;
            let c =
                parse_constructor_impl(&mut self.raw, parse_pattern, sep, ty);
            Pattern::Constructor(c)
        } else {
            unimplemented!("parse_type_name - {:?}", self)
        }
    }

    fn parse_underscore(&mut self) -> Pattern<'static> {
        let u =
            self.raw.pop_kind(tt::Kind::SignUnderscore).expect("underscore");
        Pattern::Ignored(u.span())
    }

    fn parse_value_name(&mut self) -> Pattern<'static> {
        let name = self.raw.pop_kind(tt::Kind::NameValue).expect("name");
        Pattern::Var(self.raw.resolve_variable(name))
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
            self.raw.parse_tuple(parse_pattern, tt::Kind::SignColon, ns, o, c)
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
            Some(K::KeywordReturn) => self.parse_return(),
            Some(K::KeywordSet) => self.parse_set(),
            Some(K::KeywordVar) => self.parse_var(),
            Some(k) => unimplemented!(
                "Expected :return, :set or :var, got {:?}",
                k
            ),
            None => unimplemented!(
                "Expected :return, :set or :var, got nothing"
            ),
        }
    }

    fn parse_return(&mut self) -> Statement<'g> {
        use model::tt::Kind as K;

        let ret = self.pop(K::KeywordReturn).expect(":return");

        let expr = if self.raw.peek_kind() != Some(K::SignSemiColon) {
            Some(parse_expression(&mut self.raw))
        } else {
            None
        };

        let semi = self.pop(K::SignSemiColon).unwrap_or(
            expr.map(|e| e.span().end_offset() as u32 - 1).unwrap_or(ret + 6)
        );

        Statement::Return(Return { expr, ret, semi })
    }

    fn parse_set(&mut self) -> Statement<'g> {
        let set = self.pop(tt::Kind::KeywordSet).expect(":set");

        let left = parse_expression(&mut self.raw);

        let (expr, bind, semi) = self.parse_bind();

        Statement::Set(VariableReBinding { left, expr, set, bind, semi })
    }

    fn parse_var(&mut self) -> Statement<'g> {
        let var = self.pop(tt::Kind::KeywordVar).expect(":var");

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
        let bind = self.pop(tt::Kind::SignBind).unwrap_or(0);

        let expr = parse_expression(&mut self.raw);

        let semi =
            self.pop(tt::Kind::SignSemiColon)
                .unwrap_or(expr.span().end_offset() as u32 - 1);

        (expr, bind, semi)
    }

    fn pop(&mut self, kind: tt::Kind) -> Option<u32> {
        self.raw
            .pop_kind(kind)
            .map(|t| t.offset() as u32)
    }
}

//
//  Implementation Details (Tuple)
//
fn parse_constructor_impl<'a, 'g, 'local, T: 'g + Copy + Span>(
    raw: &mut RawParser<'a, 'g, 'local>,
    inner_parser: fn(&mut RawParser<'a, 'g, 'local>) -> T,
    separator: tt::Kind,
    ty: Type<'g>,
)
    -> Constructor<'g, T>
{
    let tuple = if let Some(tt::Node::Braced(o, ns, c)) = raw.peek() {
        assert_eq!(o.kind(), tt::Kind::ParenthesisOpen);

        raw.pop_node();
        raw.parse_tuple(inner_parser, separator, ns, o, c)
    } else {
        Default::default()
    };
    Constructor { type_: ty, arguments: tuple }
}

fn parse_statements_impl<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> &'g [Statement<'g>]
{
    let mut stmts = raw.local_array();

    while let Some(tok) = raw.peek().map(|n| n.front()) {
        if tok.kind() != tt::Kind::KeywordReturn &&
            tok.kind() != tt::Kind::KeywordSet &&
            tok.kind() != tt::Kind::KeywordVar {
            break;
        }
        stmts.push(parse_statement(raw));
    }

    raw.intern_slice(&stmts)
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::super::com::tests::{Env, LocalEnv};
    use model::ast::*;

    #[test]
    fn basic_add() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"1 + 2"),
            e.bin_op(e.int(1, 0), e.int(2, 4)).build()
        );
    }

    #[test]
    fn basic_var() {
        let env = Env::new();
        let (e, _, p, s, _) = env.factories();

        assert_eq!(
            stmtit(&env, b" :var fool := 1234;"),
            s.var(p.var(6, 4), e.int(1234, 14)).build()
        );
    }

    #[test]
    fn basic_var_automatic_insertion() {
        let env = Env::new();
        let (e, _, p, s, _) = env.factories();

        assert_eq!(
            stmtit(&env, b" :var fool 1234"),
            s.var(p.var(6, 4), e.int(1234, 11))
                .bind(0)
                .semi_colon(14)
                .build()
        );
    }

    #[test]
    fn basic_bytes() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"b'1 + 2'"),
            e.literal(0, 8).push_text(2, 5).bytes().build()
        );
    }

    #[test]
    fn basic_constructor() {
        let env = Env::new();
        let f = env.factory();

        assert_eq!(
            exprit(&env, b"True"),
            f.expr().constructor(f.type_().simple(0, 4)).build()
        );
    }

    #[test]
    fn basic_constructor_arguments() {
        let env = Env::new();
        let (e, _, _, _, t) = env.factories();

        assert_eq!(
            exprit(&env, b"Some(1)"),
            e.constructor(t.simple(0, 4)).push(e.int(1, 5)).build()
        );

    }

    #[test]
    fn basic_nested_constructor() {
        let env = Env::new();
        let f = env.factory();

        assert_eq!(
            exprit(&env, b"Bool::True"),
            f.expr().constructor(
                f.type_().nested(6, 4).push(0, 4).build()
            ).build()
        );
    }

    #[test]
    fn basic_function_call() {
        let env = Env::new();
        let (e, _, p, s, _) = env.factories();

        assert_eq!(
            exprit(&env, b"basic(1, 2)"),
            e.function_call(e.var(0, 5), 5, 10)
                .push(e.int(1, 6))
                .push(e.int(2, 9))
                .build()
        );

        assert_eq!(
            stmtit(&env, b":var a := basic(1, 2);"),
            s.var(
                p.var(5, 1),
                e.function_call(e.var(10, 5), 15, 20)
                    .push(e.int(1, 16))
                    .push(e.int(2, 19))
                    .build(),
            )
            .build()
        );
    }

    #[test]
    fn basic_if_else() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b":if true { 1 } :else { 0 }"),
            Expression::If(
                &e.if_else(
                    e.bool_(true, 4),
                    e.block(e.int(1, 11)).build(),
                    e.block(e.int(0, 23)).build(),
                ).build()
            )
        )
    }

    #[test]
    fn basic_string() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"'1 + 2'"),
            e.literal(0, 7).push_text(1, 5).string().build()
        );
    }

    #[test]
    fn block_basic() {
        let env = Env::new();
        let (e, _, p, s, _) = env.factories();

        assert_eq!(
            exprit(&env, b"{\n    :var fool := 1234;\n    fool\n}"),
            Expression::Block(
                &e.block(e.var(29, 4))
                    .range(0, 35)
                    .push_stmt(s.var(p.var(11, 4), e.int(1234, 19)).build())
                    .build()
            )
        );
    }

    #[test]
    fn block_return() {
        let env = Env::new();
        let (e, _, _, s, _) = env.factories();

        assert_eq!(
            exprit(&env, b"{ :return 1; }"),
            Expression::Block(
                &e.block_div()
                    .push_stmt(s.ret().expr(e.int(1, 10)).build())
                    .build()
            )
        );
    }

    #[test]
    fn boolean_basic() {
        let env = Env::new();
        let (e, _, p, s, _) = env.factories();

        assert_eq!(
            stmtit(&env, b":var x := true;"),
            s.var(p.var(5, 1), e.bool_(true, 10)).build()
        );
    }

    #[test]
    fn constructor_keyed() {
        let env = Env::new();
        let local = env.local(b"Person(.name := jack_jack, .age := 1)");
        let (e, _, _, _, t) = env.factories();

        let person = local.resolve_type(0, 6);
        let jack_jack = local.resolve_variable(16, 9);
        let (name, age) =
            (local.resolve_field(7, 5), local.resolve_field(27, 4));

        assert_eq!(
            exprit_resolved(&local),
            e.constructor(t.simple_named(person))
                .full_name(name).separator(13).push(e.var_named(jack_jack))
                .full_name(age).separator(32).push(e.int(1, 35))
                .build()
        );
    }

    #[test]
    fn field_access_basic() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"tup.42"),
            e.field_access(e.var(0, 3)).index(42).range(3, 3).build()
        );
    }

    #[test]
    fn field_access_keyed() {
        let env = Env::new();
        let local = env.local(b"tup.x");
        let e = env.factory().expr();

        let tup = local.resolve_variable(0, 3);
        let x = local.resolve_field(3, 2);
        let x = FieldIdentifier::Name(x.0, x.1);

        assert_eq!(
            exprit_resolved(&local),
            e.field_access(e.var_named(tup)).named(x).build()
        );
    }

    #[test]
    fn field_access_recursive() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"tup.42.53"),
            e.field_access(
                e.field_access(e.var(0, 3)).index(42).range(3, 3).build()
            )
                .index(53)
                .range(6, 3)
                .build()
        );
    }

    #[test]
    fn loop_empty() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b":loop { }"),
            e.loop_(0).build()
        );
    }

    #[test]
    fn set_basic() {
        let env = Env::new();
        let f = env.factory();
        let e = f.expr();

        assert_eq!(
            stmtit(&env, b" :set fool := 1234;"),
            f.stmt().set(e.var(6, 4), e.int(1234, 14)).build()
        );
    }

    #[test]
    fn set_field() {
        let env = Env::new();
        let f = env.factory();
        let e = f.expr();

        assert_eq!(
            stmtit(&env, b" :set foo.0 := 1234;"),
            f.stmt().set(
                e.field_access(e.var(6, 3)).index(0).build(),
                e.int(1234, 15)
            ).build()
        );
    }

    #[test]
    fn var_constructor() {
        let env = Env::new();
        let f = env.factory();
        let (e, p, s, t) = (f.expr(), f.pat(), f.stmt(), f.type_());

        assert_eq!(
            stmtit(&env, b":var Some(x) := Some(1);"),
            s.var(
                p.constructor(t.simple(5, 4)).push(p.var(10, 1)).build(),
                e.constructor(t.simple(16, 4)).push(e.int(1, 21)).build(),
            ).build()
        );
    }

    #[test]
    fn var_constructor_keyed() {
        let env = Env::new();
        let f = env.factory();
        let (e, p, s, t) = (f.expr(), f.pat(), f.stmt(), f.type_());

        let code = b":var Person(.name: n, .age: a) := p;";

        assert_eq!(
            stmtit(&env, code),
            s.var(
                p.constructor(t.simple(5, 6))
                    .name(12, 5).push(p.var(19, 1))
                    .name(22, 4).push(p.var(28, 1))
                    .build(),
                e.var(34, 1),
            ).build()
        );
    }

    #[test]
    fn var_ignored() {
        let env = Env::new();
        let f = env.factory();
        let (e, p, s) = (f.expr(), f.pat(), f.stmt());

        assert_eq!(
            stmtit(&env, b":var _ := 1;"),
            s.var(p.ignored(5), e.int(1, 10)).build()
        );
    }

    #[test]
    fn var_ignored_nested() {
        let env = Env::new();
        let f = env.factory();
        let (e, p, s) = (f.expr(), f.pat(), f.stmt());

        assert_eq!(
            stmtit(&env, b":var (_, b) := (1, 2);"),
            s.var(
                p.tuple().push(p.ignored(6)).push(p.var(9, 1)).build(),
                e.tuple().push(e.int(1, 16)).push(e.int(2, 19)).build(),
            ).build()
        );
    }

    #[test]
    fn var_tuple() {
        let env = Env::new();
        let f = env.factory();
        let (e, p, s) = (f.expr(), f.pat(), f.stmt());

        assert_eq!(
            stmtit(&env, b":var (a, b) := (1, 2);"),
            s.var(
                p.tuple().push(p.var(6, 1)).push(p.var(9, 1)).build(),
                e.tuple().push(e.int(1, 16)).push(e.int(2, 19)).build(),
            ).build()
        );
    }

    #[test]
    fn var_tuple_keyed() {
        let env = Env::new();
        let f = env.factory();
        let (e, p, s) = (f.expr(), f.pat(), f.stmt());

        assert_eq!(
            stmtit(&env, b":var (.x: a, .y: b) := foo();"),
            s.var(
                p.tuple()
                    .name(6, 2).push(p.var(10, 1))
                    .name(13, 2).push(p.var(17, 1))
                    .build(),
                e.function_call(e.var(23, 3), 26, 27).build(),
            ).build()
        );
    }

    #[test]
    fn shunting_yard_prefix() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b":not a :or b :and c"),
            e.bin_op(
                e.pre_op(e.var(5, 1)).build(),
                e.bin_op(e.var(11, 1), e.var(18, 1)).and().build()
            ).or().build()
        );
    }

    #[test]
    fn shunting_yard_simple() {
        let env = Env::new();
        let e = env.factory().expr();

        let left = e.bin_op(
            e.int(1, 0),
            e.bin_op(e.int(2, 4), e.int(3, 8)).times().build()
        ).build();

        let right =
            e.bin_op(e.int(4, 12), e.int(5, 17)).floor_by().build();

        assert_eq!(
            exprit(&env, b"1 + 2 * 3 < 4 // 5"),
            e.bin_op(left, right).less_than().build()
        )
    }

    #[test]
    fn tuple_basic() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"(1)"),
            e.tuple().push(e.int(1, 1)).build()
        );
    }

    #[test]
    fn tuple_keyed() {
        let env = Env::new();
        let e = env.factory().expr();

        assert_eq!(
            exprit(&env, b"(.x := 1, .y := 2)"),
            e.tuple()
                .name(1, 2).separator(4).push(e.int(1, 7))
                .name(10, 2).separator(13).push(e.int(2, 16))
                .build()
        );
    }

    #[test]
    fn tuple_keyed_named() {
        let env = Env::new();
        let local = env.local(b"(.x := 1, .y := 2)");
        let e = env.factory().expr();

        let (x, y) = (local.resolve_field(1, 2), local.resolve_field(10, 2));

        assert_eq!(
            exprit_resolved(&local),
            e.tuple()
                .full_name(x).separator(4).push(e.int(1, 7))
                .full_name(y).separator(13).push(e.int(2, 16))
                .build()
        );
    }

    #[test]
    fn tuple_nested() {
        let env = Env::new();
        let e = env.factory().expr();

        let inner = e.tuple().push(e.int(2, 5)).push(e.int(3, 8)).build();

        assert_eq!(
            exprit(&env, b"(1, (2, 3), 4)"),
            e.tuple().push(e.int(1, 1)).push(inner).push(e.int(4, 12)).build()
        );
    }

    fn exprit<'g>(env: &'g Env, raw: &[u8]) -> Expression<'g> {
        let local = env.local(raw);
        env.scrubber().scrub_expr(exprit_resolved(&local))
    }

    fn exprit_resolved<'g>(local: &LocalEnv<'g>) -> Expression<'g> {
        let mut raw = local.raw();
        super::parse_expression(&mut raw)
    }

    fn stmtit<'g>(env: &'g Env, raw: &[u8]) -> Statement<'g> {
        let local = env.local(raw);
        env.scrubber().scrub_stmt(stmtit_resolved(&local))
    }

    fn stmtit_resolved<'g>(local: &LocalEnv<'g>) -> Statement<'g> {
        let mut raw = local.raw();
        super::parse_statement(&mut raw)
    }
}
