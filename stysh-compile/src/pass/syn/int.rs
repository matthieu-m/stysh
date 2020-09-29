//! Syntactic pass, aka parsing.
//!
//! Interface parser.

use crate::model::tt::Kind;
use crate::model::ast::*;

use super::{body, gen};
use super::com::RawParser;

/// Parses an interface.
pub fn parse_interface<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> InterfaceId {
    let mut parser = InterfaceParser::new(*raw);
    let i = parser.parse_interface();
    *raw = parser.into_raw();
    i
}

//
//  Implementation Details
//
struct InterfaceParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

impl<'a, 'tree> InterfaceParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self { Self { raw } }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse_interface(&mut self) -> InterfaceId {
        //  Expects:
        //  -   :int
        //  -   type-identifier
        //  -   <parameters>
        //  -   <body>
        let keyword = self.raw.pop_kind(Kind::KeywordInt).expect(":int");

        let name =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| self.raw.resolve_type(t))
                .unwrap_or(TypeIdentifier::default());

        let parameters = gen::try_parse_generic_parameters(&mut self.raw);

        let body = body::parse_body(&mut self.raw, TypeId::default());

        let int = Interface {
            name,
            parameters,
            functions: body.functions,
            keyword: keyword.offset() as u32,
            open: body.open,
            close: body.close,
        };

        let int = self.raw.module().borrow_mut().push_interface(int);

        let function_ids: Vec<_> =
            self.raw.module().borrow().get_function_ids(body.functions).iter().copied().collect();

        for &fun in &function_ids {
            self.raw.module().borrow_mut().set_function_scope(fun, Scope::Int(int));
        }

        int
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
    fn interface_empty() {
        let env = LocalEnv::new(b":int Empty { }");
        let i = env.factory().item();

        i.interface(5, 5).braces(11, 13).build();

        assert_eq!(env.actual_interface(), env.expected_module());
    }

    #[test]
    fn interface_single_function() {
        let env = LocalEnv::new(b":int Simple { :fun id() -> Simple { Simple } }");
        let (e, _, i, _, _, tm, t) = env.factories();

        let fun = i.function(
            19,
            2,
            tm.simple(27, 6),
        ).build();
        e.block(e.constructor(t.simple(36, 6)).build()).build_body(fun);

        i.interface(5, 6)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_interface(), env.expected_module());
    }

    #[test]
    fn interface_three_functions() {
        let env = LocalEnv::new(
            b":int Simple { :fun one() -> O { O } :fun two() -> W { W } :fun three() -> R { R } }"
        );
        let (e, _, i, _, _, tm, t) = env.factories();

        let one = i.function(
            19,
            3,
            tm.simple(28, 1),
        ).build();
        e.block(e.constructor(t.simple(32, 1)).build()).build_body(one);

        env.reset_expected_tree();

        let two = i.function(
            41,
            3,
            tm.simple(50, 1),
        ).build();
        e.block(e.constructor(t.simple(54, 1)).build()).build_body(two);

        env.reset_expected_tree();

        let three = i.function(
            63,
            5,
            tm.simple(74, 1),
        ).build();
        e.block(e.constructor(t.simple(78, 1)).build()).build_body(three);

        i.interface(5, 6)
            .push_function(one)
            .push_function(two)
            .push_function(three)
            .build();

        assert_eq!(env.actual_interface(), env.expected_module());
    }

    #[test]
    fn interface_generic() {
        let env = LocalEnv::new(b":int Simple[T] { :fun id() -> Simple { Simple } }");
        let (e, g, i, _, _, tm, t) = env.factories();

        let parameters = g.parameters().push(g.parameter(12, 1)).build();

        let fun = i.function(
            22,
            2,
            tm.simple(30, 6),
        ).build();
        e.block(e.constructor(t.simple(39, 6)).build()).build_body(fun);

        i.interface(5, 6)
            .parameters(parameters)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_interface(), env.expected_module());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_interface(&self) -> Module {
            let mut raw = self.env.raw();
            super::parse_interface(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_interface: {:#?}", result);
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
