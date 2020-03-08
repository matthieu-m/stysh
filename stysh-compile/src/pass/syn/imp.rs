//! Syntactic pass, aka parsing.
//!
//! Implementation parser.

use crate::model::tt::Kind;
use crate::model::ast::*;

use super::body;
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
        //  -   type-identifier
        //  -   :for
        //  -   type-identifier
        //  -   <body>
        let keyword = self.raw.pop_kind(Kind::KeywordInt).expect(":int");

        let interface =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| self.raw.resolve_type(t))
                .unwrap_or(TypeIdentifier::default());

        let for_ = self.raw.pop_kind(Kind::KeywordFor).expect(":for");

        let extended =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| self.raw.resolve_type(t))
                .unwrap_or(TypeIdentifier::default());

        let body = body::parse_body(&mut self.raw, extended);

        let imp = Implementation {
            interface,
            extended,
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
        let (e, i, _, _, tm, t) = env.factories();

        let fun = i.function(
            30,
            2,
            tm.simple(38, 6),
        ).build();
        e.block(e.constructor(t.simple(47, 6)).build()).build_body(fun);

        i.implementation(5, 5, 6)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_implementation(), env.expected_module());
    }

    #[test]
    fn implementation_three_functions() {
        let env = LocalEnv::new(
            b":imp Magic :for Simple { :fun one() -> O { O } :fun two() -> W { W } :fun three() -> R { R } }"
        );
        let (e, i, _, _, tm, t) = env.factories();

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

        i.implementation(5, 5, 6)
            .push_function(one)
            .push_function(two)
            .push_function(three)
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
