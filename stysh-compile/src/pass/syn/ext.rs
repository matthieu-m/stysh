//! Syntactic pass, aka parsing.
//!
//! Extension parser.

use crate::model::tt::{Kind, Node};
use crate::model::ast::*;

use super::com::RawParser;
use super::fun;

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
        //  -   type-identifier
        //  -   {
        //  -       functions
        //  -   }
        let keyword = self.raw.pop_kind(Kind::KeywordExt).expect(":ext");

        let name =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| self.raw.resolve_type(t))
                .unwrap_or(TypeIdentifier::default());

        let mut function_ids = vec!();
        let (open, close) = match self.raw.peek() {
            Some(Node::Braced(o, ns, c)) => {
                self.raw.pop_node();
                let mut raw = self.raw.spawn(ns);

                while let Some(node) = raw.peek() {
                    match node.front().kind() {
                        Kind::KeywordFun => {
                            function_ids.push(fun::parse_function(&mut raw));
                        },
                        _ => unimplemented!("parse_extension - expected :fun in {:?}", name),
                    }
                }

                (o, c)
            }
            _ => unimplemented!("parse_extension - unbraced extension {:?}", name),
        };

        let functions = self.raw.module().borrow_mut().push_function_ids(&function_ids);

        let ext = Extension {
            name,
            functions,
            keyword: keyword.offset() as u32,
            open: open.offset() as u32,
            close: close.offset() as u32,
        };

        let ext = self.raw.module().borrow_mut().push_extension(ext);

        for &fun in &function_ids {
            self.raw.module().borrow_mut().set_function_extension(fun, ext);
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
        let (e, i, _, _, _, t) = env.factories();

        let fun = i.function(
            19,
            2,
            t.simple(27, 6),
            e.block(e.constructor(t.simple(36, 6)).build()).build(),
        ).build();

        i.extension(5, 6)
            .push_function(fun)
            .build();

        assert_eq!(env.actual_extension(), env.expected_module());
    }

    #[test]
    fn extension_three_functions() {
        let env = LocalEnv::new(
            b":ext Simple { :fun one() -> O { O } :fun two() -> W { W } :fun three() -> R { R } }"
        );
        let (e, i, _, _, _, t) = env.factories();

        let one = i.function(
            19,
            3,
            t.simple(28, 1),
            e.block(e.constructor(t.simple(32, 1)).build()).build(),
        ).build();

        env.reset_expected_tree();

        let two = i.function(
            41,
            3,
            t.simple(50, 1),
            e.block(e.constructor(t.simple(54, 1)).build()).build(),
        ).build();

        env.reset_expected_tree();

        let three = i.function(
            63,
            5,
            t.simple(74, 1),
            e.block(e.constructor(t.simple(78, 1)).build()).build(),
        ).build();

        i.extension(5, 6)
            .push_function(one)
            .push_function(two)
            .push_function(three)
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
