//! Syntactic pass, aka parsing.
//!
//! Expression parser.

use std::{cell, fmt};

use crate::basic::mem;
use crate::basic::com::{Range, Span, Store, MultiStore};
use crate::model::tt;
use crate::model::ast::*;

use super::typ;
use super::com::RawParser;

pub fn parse_expression<'a, 'tree>(raw: &mut RawParser<'a, 'tree>)
    -> ExpressionId
{
    let mut parser = ExprParser::new(*raw);
    let expr = parser.parse();
    *raw = parser.into_raw();
    expr
}

pub fn parse_pattern<'a, 'tree>(raw: &mut RawParser<'a, 'tree>)
    -> PatternId
{
    let mut parser = PatternParser::new(*raw);
    let stmt = parser.parse();
    *raw = parser.into_raw();
    stmt
}

pub fn parse_statement<'a, 'tree>(raw: &mut RawParser<'a, 'tree>)
    -> StatementId
{
    let mut parser = StmtParser::new(*raw);
    let stmt = parser.parse();
    *raw = parser.into_raw();
    stmt
}

//
//  Implementation Details (Expression)
//
struct ExprParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct Precedence(u8);

struct ShuntingYard<'a> {
    tree: &'a cell::RefCell<Tree>,
    op_stack: Vec<(Operator, u32, Precedence)>,
    expr_stack: Vec<(Expression, Range)>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
enum Operator {
    Bin(BinaryOperator),
    Pre(PrefixOperator),
}

impl<'a, 'tree> ExprParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> ExprParser<'a, 'tree> {
        ExprParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse(&mut self) -> ExpressionId {
        use crate::model::tt::Kind as K;

        fn binop(kind: tt::Kind) -> Option<(Operator, Precedence)> {
            use crate::model::ast::BinaryOperator as B;

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
        let mut yard = ShuntingYard::new(self.raw.tree());

        while let Some(node) = self.raw.peek() {
            //  An expression.
            let (expr, range) = match node {
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
                            (Expression::Var(self.raw.resolve_variable(token)), token.span())
                        },
                        K::SignBind => break,
                        _ => {
                            let ty = typ::try_parse_type(&mut self.raw);
                            if let Some(ty) = ty {
                                self.parse_constructor(ty)
                            } else {
                                unimplemented!("Expected type, got {:?}\n{:?}", token, self.raw);
                            }
                        },
                    }
                },
                tt::Node::Braced(..) => self.parse_braced(node),
                tt::Node::Bytes(..) => self.parse_bytes(node),
                tt::Node::String(..) => self.parse_string(node),
                tt::Node::UnexpectedBrace(..) => unimplemented!(),
            };

            yard.push_expression(expr, range);

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

        let (expr, range) = yard.pop_expression();
        self.raw.tree().borrow_mut().push_expression(expr, range)
    }

    fn parse_block(&mut self) -> Block {
        if let Some(tt::Node::Braced(o, n, c)) = self.raw.peek() {
            self.raw.pop_node();
            return self.parse_braces(n, o, c);
        }

        unimplemented!("Expected block, got {:?}", self.raw.peek());
    }

    fn parse_bool(&mut self, kind: tt::Kind) -> (Expression, Range) {
        use self::tt::Kind::*;

        let value = match kind {
            LitBoolFalse => false,
            LitBoolTrue => true,
            _ => panic!("Unexpected kind {:?}", kind),
        };

        let token = self.raw.pop_kind(kind).expect("true/false");
        (Expression::Lit(Literal::Bool(value)), token.span())
    }

    fn parse_braced(&mut self, node: tt::Node<'a>) -> (Expression, Range) {
        use crate::model::tt::Kind as K;

        if let tt::Node::Braced(o, n, c) = node {
            self.raw.pop_node();

            match o.kind() {
                K::BraceOpen => {
                    let block = self.parse_braces(n, o, c);
                    (Expression::Block(block), block.span())
                },
                K::ParenthesisOpen => self.parse_parens(n, o, c),
                _ => unimplemented!(),
            }
        } else {
            unreachable!("Not a Braced node: {:?}", node);
        }
    }

    fn parse_braces(&self, ns: &'a [tt::Node], o: tt::Token, c: tt::Token)
        -> Block
    {
        let mut raw = self.raw.spawn(ns);
        let statements = parse_statements_impl(&mut raw);
        let expression = if let Some(_) = raw.peek_kind() {
            Some(ExprParser::new(raw).parse())
        } else {
            None
        };

        Block {
            statements,
            expression,
            open: o.offset() as u32,
            close: c.offset() as u32,
        }
    }

    fn parse_bytes(&mut self, node: tt::Node) -> (Expression, Range) {
        if let tt::Node::Bytes(_, f, _) = node {
            self.raw.pop_node();

            let (fragments, result) = self.parse_string_impl(f);
            (Expression::Lit(Literal::Bytes(fragments, result)), node.span())
        } else {
            unreachable!("Not a Bytes node: {:?}", node);
        }
    }

    fn parse_constructor(&mut self, ty: TypeId) -> (Expression, Range) {
        let sep = tt::Kind::SignBind;
        let (c, range) =
            parse_constructor_impl(&mut self.raw, parse_expression, sep, ty);
        (Expression::Constructor(c), range)
    }

    fn parse_field_identifier(&mut self) -> FieldIdentifier {
        let token = self.raw.pop_kind(tt::Kind::NameField).expect("Token");
        let source = self.raw.source(token);
        debug_assert!(!source.is_empty());

        if source[0] < b'0' || source[0] > b'9' {
            let id = self.raw.resolve_identifier(token);
            return FieldIdentifier::Name(id);
        }

        if let Some(i) = parse_integral_impl(source, false) {
            assert!(i >= 0);
            FieldIdentifier::Index(i as _, token.span())
        } else {
            unimplemented!("Cannot parse {:?} from {:?}", source, token)
        }
    }

    fn parse_if_else(&mut self) -> (Expression, Range) {
        let if_ = self.raw.pop_kind(tt::Kind::KeywordIf).expect(":if");

        let condition = self.parse();
        //  FIXME(matthieum): possibly missing.
        let true_expr = self.parse_block();

        if let Some(else_) = self.raw.pop_kind(tt::Kind::KeywordElse) {
            //  FIXME(matthieum): only ":if" and "{ ... }" are legal.

            let false_expr = self.parse_block();

            return (
                Expression::If(IfElse {
                    condition,
                    true_expr: self.insert_block(true_expr),
                    false_expr: self.insert_block(false_expr),
                    if_: if_.offset() as u32,
                    else_: else_.offset() as u32,
                }),
                if_.span().extend(false_expr.span()),
            );
        }

        //  FIXME(matthieum): ";" is legal.
        unimplemented!()
    }

    fn parse_integral(&mut self) -> (Expression, Range) {
        let token = self.raw.pop_kind(tt::Kind::LitIntegral).expect("Token");
        let source = self.raw.source(token);

        if let Some(i) = parse_integral_impl(source, true) {
            (Expression::Lit(Literal::Integral(i)), token.span())
        } else {
            unimplemented!("Cannot parse {:?} from {:?}", source, token)
        }
    }

    fn parse_loop(&mut self) -> (Expression, Range) {
        let loop_ = self.raw.pop_kind(tt::Kind::KeywordLoop).expect(":loop");
        let loop_ = loop_.offset() as u32;

        if let Some(tt::Node::Braced(o, ns, c)) = self.raw.peek() {
            self.raw.pop_node();
            let mut raw = self.raw.spawn(ns);
            let statements = parse_statements_impl(&mut raw);
            assert!(raw.peek().is_none());

            let open = o.offset() as u32;
            let close = c.offset() as u32;

            let loop_ = Loop { statements, loop_, open, close };
            return (Expression::Loop(loop_), loop_.span());
        }

        unimplemented!("Expected braces after :loop");
    }

    fn parse_parens(
        &mut self,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token
    )
        -> (Expression, Range)
    {
        (Expression::Tuple(self.parse_tuple(ns, o, c)), o.span().extend(c.span()))
    }

    fn parse_string(&mut self, node: tt::Node) -> (Expression, Range) {
        if let tt::Node::String(_, f, _) = node {
            self.raw.pop_node();

            let (fragments, result) = self.parse_string_impl(f);
            (Expression::Lit(Literal::String(fragments, result)), node.span())
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
        -> Tuple<Expression>
    {
        let sep = tt::Kind::SignBind;
        let tree = self.raw.tree();
        self.raw.parse_tuple(tree, parse_expression, sep, ns, o, c)
    }

    fn parse_string_impl(&self, f: &[StringFragment])
        -> (Id<[StringFragment]>, mem::InternId)
    {
        use self::StringFragment::*;

        let mut buffer = vec!();
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
            self.raw.tree().borrow_mut().push_string_fragments(f),
            self.raw.intern_bytes(&buffer),
        )
    }

    fn insert(&self, expr: Expression, range: Range) -> ExpressionId {
        self.raw.tree().borrow_mut().push_expression(expr, range)
    }

    fn insert_block(&self, block: Block) -> ExpressionId {
        self.insert(Expression::Block(block), block.span())
    }
}

impl<'a> ShuntingYard<'a> {
    fn new(tree: &'a cell::RefCell<Tree>) -> ShuntingYard<'a> {
        ShuntingYard {
            tree,
            op_stack: vec!(),
            expr_stack: vec!(),
        }
    }

    fn push_expression(&mut self, expr: Expression, range: Range) {
        //  Function calls are distinguished from regular expression by having
        //  a normal expression immediately followed by a tuple expression
        //  without an intervening operator.
        if let Expression::Tuple(arguments) = expr {
            if let Some((callee, range)) = self.pop_trailing_expression() {
                let function = self.insert(callee, range);
                self.expr_stack.push((
                    FunctionCall { function, arguments, }.into(),
                    range.extend(arguments.span()),
                ));
                return;
            }
        }
        self.expr_stack.push((expr, range));
    }

    fn push_field(&mut self, field: FieldIdentifier) {
        if let Some((accessed, range)) = self.pop_trailing_expression() {
            let accessed = self.insert(accessed, range);
            self.expr_stack.push((
                FieldAccess { accessed, field, }.into(),
                range.extend(field.span()),
            ));
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

    fn pop_expression(&mut self) -> (Expression, Range) {
        self.pop_operators(Precedence(0));

        if let Some(r) = self.expr_stack.pop() {
            r
        } else {
            unreachable!("Could not pop expression - {:?}", self);
        }
    }

    fn pop_operators(&mut self, threshold: Precedence) {
        while let Some((op, pos)) = self.pop_operator_impl(threshold) {
            match op {
                Operator::Bin(op) => {
                    let (right_hand, right_range) = self.expr_stack.pop().expect("Right");
                    let (left_hand, left_range) = self.expr_stack.pop().expect("Left");
                    let left = self.insert(left_hand, left_range);
                    let right = self.insert(right_hand, right_range);
                    self.expr_stack.push((
                        Expression::BinOp(op, pos, left, right),
                        left_range.extend(right_range),
                    ));
                },
                Operator::Pre(op) => {
                    let (expr, range) = self.expr_stack.pop().expect("Expression");
                    let expr = self.insert(expr, range);
                    self.expr_stack.push((
                        Expression::PreOp(op, pos, expr),
                        Range::new(pos as usize, 1).extend(range),
                    ));
                },
            };
        }
    }

    /// Pops the last expression if there was no operator afterwards
    fn pop_trailing_expression(&mut self) -> Option<(Expression, Range)> {
        let last_op = self.op_stack.last().map(|&(_, pos, _)| pos).unwrap_or(0);
        if let Some((_, range)) = self.expr_stack.last().cloned() {
            if last_op as usize <= range.offset() {
                return self.expr_stack.pop();
            }
        }
        None
    }

    fn pop_operator_impl(&mut self, threshold: Precedence)
        -> Option<(Operator, u32)>
    {
        if let Some((op, pos, prec)) = self.op_stack.last().cloned() {
            if threshold <= prec {
                self.op_stack.pop();
                return Some((op, pos));
            }
        }
        None
    }

    fn insert(&self, expr: Expression, range: Range) -> ExpressionId {
        self.tree.borrow_mut().push_expression(expr, range)
    }
}

impl<'a> fmt::Debug for ShuntingYard<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
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
struct PatternParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

impl<'a, 'tree> PatternParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self {
        PatternParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse(&mut self) -> PatternId {
        use crate::model::tt::Kind as K;

        match self.raw.peek_kind() {
            Some(K::NameType) => self.parse_type_name(),
            Some(K::NameValue) => self.parse_value_name(),
            Some(K::ParenthesisOpen) => self.parse_parens(),
            Some(K::SignUnderscore) => self.parse_underscore(),
            Some(k) => unimplemented!("Expected identifier or tuple, got {:?}", k),
            None => unimplemented!("Expected identifier or tuple, got nothing"),
        }
    }

    fn parse_parens(&mut self) -> PatternId {
        match self.raw.peek() {
            Some(tt::Node::Braced(o, n, c)) => {
                self.raw.pop_node();
                self.parse_tuple(n, o, c)
            },
            n => unimplemented!("Expected tuple, got {:?}", n),
        }
    }

    fn parse_type_name(&mut self) -> PatternId {
        if let Some(ty) = typ::try_parse_type(&mut self.raw) {
            let sep = tt::Kind::SignColon;
            let (c, range) =
                parse_constructor_impl(&mut self.raw, parse_pattern, sep, ty);
            self.insert(Pattern::Constructor(c), range)
        } else {
            unimplemented!("parse_type_name - {:?}", self)
        }
    }

    fn parse_underscore(&mut self) -> PatternId {
        let u =
            self.raw.pop_kind(tt::Kind::SignUnderscore).expect("underscore");
        self.insert(Pattern::Ignored(u.span()), u.span())
    }

    fn parse_value_name(&mut self) -> PatternId {
        let name = self.raw.pop_kind(tt::Kind::NameValue).expect("name");
        self.insert(Pattern::Var(self.raw.resolve_variable(name)), name.span())
    }

    fn parse_tuple(
        &mut self,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token
    )
        -> PatternId
    {
        let sep = tt::Kind::SignColon;
        let tree = self.raw.tree();
        let tup = self.raw.parse_tuple(tree, parse_pattern, sep, ns, o, c);
        self.insert(Pattern::Tuple(tup), tup.span())
    }

    fn insert(&self, pat: Pattern, range: Range) -> PatternId {
        self.raw.tree().borrow_mut().push_pattern(pat, range)
    }
}

//
//  Implementation Details (Statement)
//
struct StmtParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

impl<'a, 'tree> StmtParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self {
        StmtParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse(&mut self) -> StatementId {
        use crate::model::tt::Kind as K;

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

    fn parse_return(&mut self) -> StatementId {
        use crate::model::tt::Kind as K;

        let ret = self.pop(K::KeywordReturn).expect(":return");

        let expr = if self.raw.peek_kind() != Some(K::SignSemiColon) {
            Some(parse_expression(&mut self.raw))
        } else {
            None
        };

        let expr_range = expr.map(|e| self.raw.tree().borrow().get_expression_range(e));

        let semi = self.pop(K::SignSemiColon).unwrap_or(
            expr_range.map(|e| e.end_offset() as u32 - 1).unwrap_or(ret + 6)
        );

        self.insert(Statement::Return(Return { expr, ret, semi }))
    }

    fn parse_set(&mut self) -> StatementId {
        let set = self.pop(tt::Kind::KeywordSet).expect(":set");

        let left = parse_expression(&mut self.raw);

        let (expr, bind, semi) = self.parse_bind();

        self.insert(Statement::Set(VariableReBinding { left, expr, set, bind, semi }))
    }

    fn parse_var(&mut self) -> StatementId {
        let var = self.pop(tt::Kind::KeywordVar).expect(":var");

        let pattern = parse_pattern(&mut self.raw);

        //  TODO(matthieum): parse type.

        let (expr, bind, semi) = self.parse_bind();

        self.insert(Statement::Var(VariableBinding {
            pattern,
            type_: None,
            expr,
            var,
            colon: 0,
            bind,
            semi,
        }))
    }

    fn parse_bind(&mut self) -> (ExpressionId, u32, u32) {
        let bind = self.pop(tt::Kind::SignBind).unwrap_or(0);

        let expr = parse_expression(&mut self.raw);
        let expr_range = self.raw.tree().borrow().get_expression_range(expr);

        let semi =
            self.pop(tt::Kind::SignSemiColon)
                .unwrap_or(expr_range.end_offset() as u32 - 1);

        (expr, bind, semi)
    }

    fn pop(&mut self, kind: tt::Kind) -> Option<u32> {
        self.raw
            .pop_kind(kind)
            .map(|t| t.offset() as u32)
    }

    fn insert(&self, stmt: Statement) -> StatementId {
        self.raw.tree().borrow_mut().push_statement(stmt)
    }
}

//
//  Implementation Details (Tuple)
//
fn parse_constructor_impl<'a, 'tree, T: Copy>(
    raw: &mut RawParser<'a, 'tree>,
    inner_parser: fn(&mut RawParser<'a, 'tree>) -> Id<T>,
    separator: tt::Kind,
    type_: TypeId,
)
    -> (Constructor<T>, Range)
    where
        Tree: Store<T> + MultiStore<Id<T>>
{
    let arguments = if let Some(tt::Node::Braced(o, ns, c)) = raw.peek() {
        assert_eq!(o.kind(), tt::Kind::ParenthesisOpen);

        raw.pop_node();
        let tree = raw.tree();
        raw.parse_tuple(tree, inner_parser, separator, ns, o, c)
    } else {
        Default::default()
    };

    let range =
        raw.tree().borrow().get_type_range(type_).extend(arguments.span());

    (Constructor { type_, arguments }, range)
}

fn parse_statements_impl<'a, 'tree>(raw: &mut RawParser<'a, 'tree>)
    -> Id<[StatementId]>
{
    let mut stmts = vec!();

    while let Some(tok) = raw.peek().map(|n| n.front()) {
        if tok.kind() != tt::Kind::KeywordReturn &&
            tok.kind() != tt::Kind::KeywordSet &&
            tok.kind() != tt::Kind::KeywordVar {
            break;
        }
        stmts.push(parse_statement(raw));
    }

    raw.tree().borrow_mut().push_statement_ids(&stmts)
}

//
//  Tests
//

#[cfg(test)]
mod tests {
    use std::ops;
    use crate::model::ast::*;
    use super::super::com::tests::Env;

    #[test]
    fn basic_add() {
        let env = LocalEnv::new(b"1 + 2");
        let e = env.factory().expr();
        e.bin_op(e.int(1, 0), e.int(2, 4)).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_var() {
        let env = LocalEnv::new(b" :var fool := 1234;");
        let (e, _, p, s, _, _) = env.factories();
        s.var(p.var(6, 4), e.int(1234, 14)).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn basic_var_automatic_insertion() {
        let env = LocalEnv::new(b" :var fool 1234");
        let (e, _, p, s, _, _) = env.factories();
        s.var(p.var(6, 4), e.int(1234, 11))
            .bind(0)
            .semi_colon(14)
            .build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn basic_bytes() {
        let env = LocalEnv::new(b"b'1 + 2'");
        let e = env.factory().expr();
        e.literal(0, 8).push_text(2, 5).bytes().build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_constructor() {
        let env = LocalEnv::new(b"True");
        let (e, _, _, _, _, t) = env.factories();
        e.constructor(t.simple(0, 4)).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_constructor_arguments() {
        let env = LocalEnv::new(b"Some(1)");
        let (e, _, _, _, _, t) = env.factories();
        e.constructor(t.simple(0, 4)).push(e.int(1, 5)).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_nested_constructor() {
        let env = LocalEnv::new(b"Bool::True");
        let (e, _, _, _, _, t) = env.factories();
        e.constructor(t.nested(6, 4).push(0, 4).build()).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_function_call() {
        let env = LocalEnv::new(b"basic(1, 2)");
        let (e, _, _, _, _, _) = env.factories();
        let (one, two) = (e.int(1, 6), e.int(2, 9));
        e.function_call(e.var(0, 5), 5, 10).push(one).push(two).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_function_call_statement() {
        let env = LocalEnv::new(b":var a := basic(1, 2);");
        let (e, _, p, s, _, _) = env.factories();
        let (one, two) = (e.int(1, 16), e.int(2, 19));
        let fun = e.function_call(e.var(10, 5), 15, 20)
            .push(one)
            .push(two)
            .build();
        s.var(p.var(5, 1), fun).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn basic_if_else() {
        let env = LocalEnv::new(b":if true { 1 } :else { 0 }");
        let e = env.factory().expr();
        let (cond, one, two) = (e.bool_(true, 4), e.int(1, 11), e.int(0, 23));
        e.if_else(cond, e.block(one).build(), e.block(two).build()).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn basic_string() {
        let env = LocalEnv::new(b"'1 + 2'");
        let e = env.factory().expr();
        e.literal(0, 7).push_text(1, 5).string().build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn block_basic() {
        let env = LocalEnv::new(b"{\n    :var fool := 1234;\n    fool\n}");
        let (e, _, p, s, _, _) = env.factories();
        let int = e.int(1234, 19);
        e.block(e.var(29, 4))
            .range(0, 35)
            .push_stmt(s.var(p.var(11, 4), int).build())
            .build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn block_return() {
        let env = LocalEnv::new(b"{ :return 1; }");
        let (e, _, _, s, _, _) = env.factories();
        e.block_expression_less()
            .push_stmt(s.ret().expr(e.int(1, 10)).build())
            .build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn boolean_basic() {
        let env = LocalEnv::new(b":var x := true;");
        let (e, _, p, s, _, _) = env.factories();
        s.var(p.var(5, 1), e.bool_(true, 10)).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn constructor_keyed() {
        let env = LocalEnv::new(b"Person(.name := jack_jack, .age := 1)");
        let (e, _, _, _, _, t) = env.factories();
        e.constructor(t.simple(0, 6))
            .name(7, 5).separator(13).push(e.var(16, 9))
            .name(27, 4).separator(32).push(e.int(1, 35))
            .build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn field_access_basic() {
        let env = LocalEnv::new(b"tup.42");
        let e = env.factory().expr();
        e.field_access(e.var(0, 3)).index(42).range(3, 3).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn field_access_keyed() {
        let env = LocalEnv::new(b"tup.x");
        let e = env.factory().expr();
        e.field_access(e.var(0, 3)).name(3, 2).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn field_access_recursive() {
        let env = LocalEnv::new(b"tup.42.53");
        let e = env.factory().expr();
        let field = e.field_access(e.var(0, 3)).index(42).range(3, 3).build();
        e.field_access(field).index(53).range(6, 3).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn loop_empty() {
        let env = LocalEnv::new(b":loop { }");
        let e = env.factory().expr();
        e.loop_(0).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn set_basic() {
        let env = LocalEnv::new(b" :set fool := 1234;");
        let (e, _, _, s, _, _) = env.factories();
        s.set(e.var(6, 4), e.int(1234, 14)).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn set_field() {
        let env = LocalEnv::new(b" :set foo.0 := 1234;");
        let (e, _, _, s, _, _) = env.factories();
        s.set(
            e.field_access(e.var(6, 3)).index(0).build(),
            e.int(1234, 15)
        ).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn var_constructor() {
        let env = LocalEnv::new(b":var Some(x) := Some(1);");
        let (e, _, p, s, _, t) = env.factories();
        s.var(
            p.constructor(t.simple(5, 4)).push(p.var(10, 1)).build(),
            e.constructor(t.simple(16, 4)).push(e.int(1, 21)).build(),
        ).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn var_constructor_keyed() {
        let env = LocalEnv::new(b":var Person(.name: n, .age: a) := p;");
        let (e, _, p, s, _, t) = env.factories();
        let pat = p.constructor(t.simple(5, 6))
            .name(12, 5).push(p.var(19, 1))
            .name(22, 4).push(p.var(28, 1))
            .build();
        s.var(pat, e.var(34, 1)).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn var_ignored() {
        let env = LocalEnv::new(b":var _ := 1;");
        let (e, _, p, s, _, _) = env.factories();
        s.var(p.ignored(5), e.int(1, 10)).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn var_ignored_nested() {
        let env = LocalEnv::new(b":var (_, b) := (1, 2);");
        let (e, _, p, s, _, _) = env.factories();
        s.var(
            p.tuple().push(p.ignored(6)).push(p.var(9, 1)).build(),
            e.tuple().push(e.int(1, 16)).push(e.int(2, 19)).build(),
        ).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn var_tuple() {
        let env = LocalEnv::new(b":var (a, b) := (1, 2);");
        let (e, _, p, s, _, _) = env.factories();
        s.var(
            p.tuple().push(p.var(6, 1)).push(p.var(9, 1)).build(),
            e.tuple().push(e.int(1, 16)).push(e.int(2, 19)).build(),
        ).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn var_tuple_keyed() {
        let env = LocalEnv::new(b":var (.x: a, .y: b) := foo();");
        let (e, _, p, s, _, _) = env.factories();
        s.var(
            p.tuple()
                .name(6, 2).push(p.var(10, 1))
                .name(13, 2).push(p.var(17, 1))
                .build(),
            e.function_call(e.var(23, 3), 26, 27).build(),
        ).build();

        assert_eq!(env.actual_statement(), env.expected_tree());
    }

    #[test]
    fn shunting_yard_prefix() {
        let env = LocalEnv::new(b":not a :or b :and c");
        let e = env.factory().expr();
        let (a, b, c) = (e.var(5, 1), e.var(11, 1), e.var(18, 1));
        e.bin_op(
            e.pre_op(a).build(),
            e.bin_op(b, c).and().build()
        ).or().build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn shunting_yard_simple() {
        let env = LocalEnv::new(b"1 + 2 * 3 < 4 // 5");
        let e = env.factory().expr();

        let (two, three, one) = (e.int(2, 4), e.int(3, 8), e.int(1, 0));
        let mult = e.bin_op(two, three).times().build();
        let (four, five) = (e.int(4, 12), e.int(5, 17));

        let left = e.bin_op(one, mult).build();
        let right = e.bin_op(four, five).floor_by().build();

        e.bin_op(left, right).less_than().build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn tuple_basic() {
        let env = LocalEnv::new(b"(1)");
        let e = env.factory().expr();
        e.tuple().push(e.int(1, 1)).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn tuple_keyed() {
        let env = LocalEnv::new(b"(.x := 1, .y := 2)");
        let e = env.factory().expr();
        e.tuple()
            .name(1, 2).separator(4).push(e.int(1, 7))
            .name(10, 2).separator(13).push(e.int(2, 16))
            .build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn tuple_keyed_named() {
        let env = LocalEnv::new(b"(.x := 1, .y := 2)");
        let e = env.factory().expr();
        e.tuple()
            .name(1, 2).separator(4).push(e.int(1, 7))
            .name(10, 2).separator(13).push(e.int(2, 16))
            .build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    #[test]
    fn tuple_nested() {
        let env = LocalEnv::new(b"(1, (2, 3), 4)");
        let e = env.factory().expr();

        let one = e.int(1, 1);
        let inner = e.tuple().push(e.int(2, 5)).push(e.int(3, 8)).build();
        e.tuple().push(one).push(inner).push(e.int(4, 12)).build();

        assert_eq!(env.actual_expression(), env.expected_tree());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_expression(&self) -> Tree {
            let mut raw = self.env.raw();
            super::parse_expression(&mut raw);
            let result = self.env.actual_tree().borrow().clone();
            println!("actual_expression: {:#?}", result);
            println!();
            result
        }

        fn actual_statement(&self) -> Tree {
            let mut raw = self.env.raw();
            super::parse_statement(&mut raw);
            let result = self.env.actual_tree().borrow().clone();
            println!("actual_statement: {:#?}", result);
            println!();
            result
        }

        fn expected_tree(&self) -> Tree {
            let result = self.env.expected_tree().borrow().clone();
            println!("expected_tree: {:#?}", result);
            println!();
            result
        }
    }

    impl ops::Deref for LocalEnv {
        type Target = Env;

        fn deref(&self) -> &Env { &self.env }
    }
}
