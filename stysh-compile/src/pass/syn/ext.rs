//! Syntactic pass, aka parsing.
//!
//! Extension parser.

use crate::model::tt::Kind;
use crate::model::ast::*;

use super::{body, typ};
use super::com::RawParser;

/// Parses an extension.
pub fn parse_extension<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> ExtensionId {
    let mut parser = ExtensionParser::new(*raw);
    let e = parser.parse_extension();
    *raw = parser.into_raw();
    e
}

//
//  Implementation Details
//
struct ExtensionParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

impl<'a, 'tree> ExtensionParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self { Self { raw } }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse_extension(&mut self) -> ExtensionId {
        //  Expects:
        //  -   :ext
        //  -   type
        //  -   <body>
        let keyword = self.raw.pop_kind(Kind::KeywordExt).expect(":ext");

        let extended = typ::parse_type(&mut self.raw);

        let body = body::parse_body(&mut self.raw, extended);

        let ext = Extension {
            extended,
            functions: body.functions,
            keyword: keyword.offset() as u32,
            open: body.open,
            close: body.close,
        };

        let ext = self.raw.module().borrow_mut().push_extension(ext);

        let function_ids: Vec<_> =
            self.raw.module().borrow().get_function_ids(body.functions).iter().copied().collect();

        for &fun in &function_ids {
            self.raw.module().borrow_mut().set_function_scope(fun, Scope::Ext(ext));
        }

        ext
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
    fn extension_empty() {
        let env = LocalEnv::new(b":ext Empty { }");
        let i = env.factory().item();

        i.extension(5, 5).braces(11, 13).build();

        assert_eq!(env.actual_extension(), env.expected_module());
    }

    #[test]
    fn extension_single_function() {
        let env = LocalEnv::new(b":ext Simple { :fun id() -> Simple { Simple } }");
        let (e, i, _, _, tm, t) = env.factories();

        let mut ext = i.extension(5, 6);

        let fun = i.function(
            19,
            2,
            tm.simple(27, 6),
        ).build();
        e.block(e.constructor(t.simple(36, 6)).build()).build_body(fun);

        ext.push_function(fun)
            .build();

        assert_eq!(env.actual_extension(), env.expected_module());
    }

    #[test]
    fn extension_three_functions() {
        let env = LocalEnv::new(
            b":ext Simple { :fun one() -> O { O } :fun two() -> W { W } :fun three() -> R { R } }"
        );
        let (e, i, _, _, tm, t) = env.factories();

        let mut ext = i.extension(5, 6);

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

        ext.push_function(one)
            .push_function(two)
            .push_function(three)
            .build();

        assert_eq!(env.actual_extension(), env.expected_module());
    }

    #[test]
    fn extension_nested() {
        let env = LocalEnv::new(b":ext Nested::Rec { :fun id() -> Self { Self } }");
        let (e, i, _, _, tm, t) = env.factories();

        let ext = tm.nested(13, 3).push(5, 6).build();

        let fun = i.function(
            24,
            2,
            tm.simple(32, 4),
        ).build();
        e.block(e.constructor(t.simple(39, 4)).build()).build_body(fun);

        i.extension_typed(ext)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_extension(), env.expected_module());
    }

    #[test]
    fn extension_tuple() {
        let env = LocalEnv::new(b":ext (Int, String) { :fun id() -> Self { Self } }");
        let (e, i, _, _, tm, t) = env.factories();

        let ext = {
            let int = tm.simple(6, 3);
            let string = tm.simple(11, 6);
            tm.tuple().push(int).push(string).build()
        };

        let fun = i.function(
            26,
            2,
            tm.simple(34, 4),
        ).build();
        e.block(e.constructor(t.simple(41, 4)).build()).build_body(fun);

        i.extension_typed(ext)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_extension(), env.expected_module());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_extension(&self) -> Module {
            let mut raw = self.env.raw();
            super::parse_extension(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_extension: {:#?}", result);
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
