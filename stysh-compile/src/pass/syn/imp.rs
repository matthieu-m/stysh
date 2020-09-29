//! Syntactic pass, aka parsing.
//!
//! Implementation parser.

use crate::model::tt::Kind;
use crate::model::ast::*;

use super::{body, gen, typ};
use super::com::RawParser;

/// Parses an implementation.
pub fn parse_implementation<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> ImplementationId {
    let mut parser = ImplementationParser::new(*raw);
    let i = parser.parse_implementation();
    *raw = parser.into_raw();
    i
}

//
//  Implementation Details
//
struct ImplementationParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

impl<'a, 'tree> ImplementationParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self { Self { raw } }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse_implementation(&mut self) -> ImplementationId {
        //  Expects:
        //  -   :imp
        //  -   <parameters>
        //  -   type
        //  -   :for
        //  -   type
        //  -   <body>
        let keyword = self.raw.pop_kind(Kind::KeywordImp).expect(":imp");

        let parameters = gen::try_parse_generic_parameters(&mut self.raw);

        let implemented = typ::parse_type(&mut self.raw);

        let for_ = self.raw.pop_kind(Kind::KeywordFor).expect(":for");

        let extended = typ::parse_type(&mut self.raw);

        let body = body::parse_body(&mut self.raw, extended);

        let imp = Implementation {
            implemented,
            extended,
            parameters,
            functions: body.functions,
            keyword: keyword.offset() as u32,
            for_: for_.offset() as u32,
            open: body.open,
            close: body.close,
        };

        let imp = self.raw.module().borrow_mut().push_implementation(imp);

        let function_ids: Vec<_> =
            self.raw.module().borrow().get_function_ids(body.functions).iter().copied().collect();

        for &fun in &function_ids {
            self.raw.module().borrow_mut().set_function_scope(fun, Scope::Imp(imp));
        }

        imp
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
    fn implementation_empty() {
        let env = LocalEnv::new(b":imp Magic :for Empty { }");
        let i = env.factory().item();

        i.implementation(5, 5, 5).braces(22, 24).build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    #[test]
    fn implementation_single_function() {
        let env = LocalEnv::new(b":imp Magic :for Simple { :fun id() -> Simple { Simple } }");
        let (e, _, i, _, _, tm, t) = env.factories();

        let mut imp = i.implementation(5, 5, 6);

        let fun = i.function(
            30,
            2,
            tm.simple(38, 6),
        ).build();
        e.block(e.constructor(t.simple(47, 6)).build()).build_body(fun);

        imp.push_function(fun)
            .build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    #[test]
    fn implementation_three_functions() {
        let env = LocalEnv::new(
            b":imp Magic :for Simple { :fun one() -> O { O } :fun two() -> W { W } :fun three() -> R { R } }"
        );
        let (e, _, i, _, _, tm, t) = env.factories();

        let mut imp = i.implementation(5, 5, 6);

        let one = i.function(
            30,
            3,
            tm.simple(39, 1),
        ).build();
        e.block(e.constructor(t.simple(43, 1)).build()).build_body(one);

        env.reset_expected_tree();

        let two = i.function(
            52,
            3,
            tm.simple(61, 1),
        ).build();
        e.block(e.constructor(t.simple(65, 1)).build()).build_body(two);

        env.reset_expected_tree();

        let three = i.function(
            74,
            5,
            tm.simple(85, 1),
        ).build();
        e.block(e.constructor(t.simple(89, 1)).build()).build_body(three);

        imp.push_function(one)
            .push_function(two)
            .push_function(three)
            .build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    #[test]
    fn implementation_nested() {
        let env = LocalEnv::new(b":imp A::B :for X::Y { :fun id() -> Self { Self } }");
        let (e, _, i, _, _, tm, t) = env.factories();

        let imp = tm.nested(8, 1).push(5, 1).build();
        let ext = tm.nested(18, 1).push(15, 1).build();

        let fun = i.function(
            27,
            2,
            tm.simple(35, 4),
        ).build();
        e.block(e.constructor(t.simple(42, 4)).build()).build_body(fun);

        i.implementation_typed(imp, ext)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    #[test]
    fn implementation_generic() {
        let env = LocalEnv::new(b":imp[T] Magic[T] :for Simple[T] { :fun id() -> Self { Self } }");
        let (e, g, i, _, _, tm, t) = env.factories();

        let parameters = g.parameters().push(g.parameter(5, 1)).build();
        let magic = {
            let variables = g.variables().push(g.variable_type(tm.simple(14, 1))).build();
            tm.generic(8, 5).variables(variables).build()
        };
        let simple = {
            let variables = g.variables().push(g.variable_type(tm.simple(29, 1))).build();
            tm.generic(22, 6).variables(variables).build()
        };

        let fun = i.function(
            39,
            2,
            tm.simple(47, 4),
        ).build();
        e.block(e.constructor(t.simple(54, 4)).build()).build_body(fun);

        i.implementation_typed(magic, simple)
            .parameters(parameters)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    #[test]
    fn implementation_tuple() {
        let env = LocalEnv::new(b":imp Magic :for (A, B) { :fun id() -> Simple { Simple } }");
        let (e, _, i, _, _, tm, t) = env.factories();

        let imp = tm.simple(5, 5);
        let ext = {
            let a = tm.simple(17, 1);
            let b = tm.simple(20, 1);
            tm.tuple().push(a).push(b).build()
        };

        let fun = i.function(
            30,
            2,
            tm.simple(38, 6),
        ).build();
        e.block(e.constructor(t.simple(47, 6)).build()).build_body(fun);

        i.implementation_typed(imp, ext)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_implementation(&self) -> Module {
            let mut raw = self.env.raw();
            super::parse_implementation(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_implementation: {:#?}", result);
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
