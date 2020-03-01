//! Syntactic pass, aka parsing.
//!
//! Function parser.

use std::cell;

use crate::basic::com::Span;
use crate::basic::mem;
use crate::model::ast::*;
use crate::model::tt;

use super::com::RawParser;
use super::{expr, typ};

pub struct FunParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>
}

pub fn parse_function<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> FunctionId {
    let previous_tree: &'tree cell::RefCell<Tree> = raw.tree();

    let tree = cell::RefCell::default();
    {
        let mut inner_raw = raw.rescope(&tree);
        let mut parser = FunParser::new(inner_raw);

        parser.parse();
        inner_raw = parser.into_raw();

        *raw = inner_raw.rescope(previous_tree);
    }

    raw.module().borrow_mut().push_function(tree.into_inner())
}

impl<'a, 'tree> FunParser<'a, 'tree> {
    pub fn new(raw: RawParser<'a, 'tree>) -> FunParser<'a, 'tree> {
        FunParser { raw: raw }
    }

    pub fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    pub fn parse(&mut self) {
        use self::tt::Kind::*;

        let (keyword, name) = {
            let keyword = self.pop_token(KeywordFun).unwrap();
            let name = self.pop_token(NameValue).expect("Function Name");

            (keyword.offset() as u32, self.raw.resolve_variable(name))
        };

        let (open, arguments, close) = {
            let (o, a, c) = match self.raw.peek() {
                Some(tt::Node::Braced(open, a, close)) => (open, a, close),
                _ => unimplemented!(),
            };

            self.raw.pop_node();

            assert_eq!(o.kind(), tt::Kind::ParenthesisOpen);
            assert_eq!(c.kind(), tt::Kind::ParenthesisClose);

            let arguments = self.parse_arguments(a);

            (o.offset() as u32, arguments, c.offset() as u32)
        };

        let (arrow, result) =
            if let Some(arrow) = self.pop_token(SignArrowSingle) {
                (arrow.offset() as u32, typ::parse_type(&mut self.raw))
            } else {
                unimplemented!()
            };

        let body = expr::parse_expression(&mut self.raw);

        let body = if let Expression::Block(_) =
            self.raw.tree().borrow().get_expression(body)
        {
            body
        }
        else
        {
            let range = self.raw.tree().borrow().get_expression_range(body);
            let block = Block {
                statements: Id::empty(),
                expression: Some(body),
                open: range.offset() as u32,
                close: range.end_offset() as u32 - 1
            };
            self.raw.tree().borrow_mut().push_expression(block.into(), range)
        };

        let fun = Function {
            name,
            arguments,
            result,
            keyword,
            open,
            close,
            arrow,
        };

        self.raw.tree().borrow_mut().set_root(Root::Function(fun, body));
    }
}

//
//  Implementation Details
//
impl<'a, 'tree> FunParser<'a, 'tree> {
    fn parse_arguments(&self, arguments: &'a [tt::Node]) -> Id<[Argument]> {
        use self::tt::Kind::*;

        if arguments.is_empty() {
            return Id::empty();
        }

        let mut raw = self.raw.spawn(arguments);
        let mut buffer = vec!();

        while raw.peek().is_some() {
            let name = raw.pop_kind(NameValue).expect("Name");
            let name = self.raw.resolve_variable(name);

            let colon = raw.pop_kind(SignColon)
                .map(|t| t.offset() as u32).unwrap_or(0);

            let type_ =
                if name.id() == mem::InternId::self_value() &&
                   colon == 0 &&
                   buffer.is_empty()
                {
                    self.materialize_self(name)
                } else {
                    typ::parse_type(&mut raw)
                };

            let type_range = self.raw.tree().borrow().get_type_range(type_);

            let comma = raw.pop_kind(SignComma)
                .map(|t| t.offset() as u32)
                .unwrap_or(type_range.end_offset() as u32 - 1);

            buffer.push(Argument { name, type_, colon, comma, });
        }

        self.raw.tree().borrow_mut().push_arguments(&buffer)
    }

    fn pop_token(&mut self, kind: tt::Kind) -> Option<tt::Token> {
        self.raw.pop_kind(kind)
    }

    fn materialize_self(&self, name: VariableIdentifier) -> TypeId {
        debug_assert!(name.id() == mem::InternId::self_value());

        let range = name.span();
        let self_ = Type::Simple(TypeIdentifier(mem::InternId::self_type(), range));
        self.raw.tree().borrow_mut().push_type(self_, range)
    }
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
    fn basic_argument_less() {
        let env = LocalEnv::new(b":fun add() -> Int { 1 + 2 }");
        let (e, i, _, _, _, t) = env.factories();
        i.function(
            5,
            3,
            t.simple(14, 3),
            e.block(e.bin_op(e.int(1, 20), e.int(2, 24)).build()).build(),
        ).build();

        assert_eq!(env.actual_function(), env.expected_module());
    }

    #[test]
    fn basic_add() {
        let env = LocalEnv::new(b":fun add(a: Int, b: Int) -> Int { a + b }");
        let (e, i, _, _, _, t) = env.factories();
        let (a_type, b_type) = (t.simple(12, 3), t.simple(20, 3));
        i.function(
            5,
            3,
            t.simple(28, 3),
            e.block(e.bin_op(e.var(34, 1), e.var(38, 1)).build()).build(),
        )
            .push(9, 1, a_type)
            .push(17, 1, b_type)
            .build();

        assert_eq!(env.actual_function(), env.expected_module());
    }

    #[test]
    fn self_typed() {
        let env = LocalEnv::new(b":fun id(self: Int) -> Int { self }");
        let (e, i, _, _, _, t) = env.factories();
        let self_type = t.simple(14, 3);
        i.function(
            5,
            2,
            t.simple(22, 3),
            e.block(e.var(28, 4)).build(),
        )
            .push(8, 4, self_type)
            .build();

        assert_eq!(env.actual_function(), env.expected_module());
    }

    #[test]
    fn self_untyped() {
        let env = LocalEnv::new(b":fun id(self) -> Int { self }");
        let (e, i, _, _, _, t) = env.factories();
        let self_type = t.self_(8, 4);
        i.function(
            5,
            2,
            t.simple(17, 3),
            e.block(e.var(23, 4)).build(),
        )
            .push(8, 4, self_type)
            .colon(0)
            .build();

        assert_eq!(env.actual_function(), env.expected_module());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_function(&self) -> Module {
            let mut raw = self.env.raw();
            super::parse_function(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_function: {:#?}", result);
            println!();
            result
        }

        fn expected_module(&self) -> Module {
            let result = self.env.expected_module().borrow().clone();
            println!("expected_module: {:#?}", result);
            println!();
            result
        }
    }

    impl ops::Deref for LocalEnv {
        type Target = Env;

        fn deref(&self) -> &Env { &self.env }
    }
}
