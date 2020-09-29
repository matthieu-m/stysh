//! Syntactic passes, aka parsing.
//!
//! Type Parser.

use std::cell;

use crate::basic::com::{Range, Span, Store, MultiStore};

use crate::model::tt::{Kind, Node};
use crate::model::ast::*;

use super::com::RawParser;
use super::gen;

pub fn parse_enum<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> EnumId {
    let mut parser = EnumRecParser::new(*raw);
    let e = parser.parse_enum();
    *raw = parser.into_raw();
    e
}

pub fn parse_record<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> RecordId {
    let mut parser = EnumRecParser::new(*raw);
    let r = parser.parse_record();
    *raw = parser.into_raw();
    r
}

pub fn parse_type<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> TypeId {
    let module = raw.module();
    parse_type_impl(raw, module)
}

pub fn try_parse_type<'a, 'tree, 'store, S>(raw: &mut RawParser<'a, 'tree>, store: &'store cell::RefCell<S>, path: Path)
    -> Option<TypeId>
    where
        S: Store<Type> + Store<GenericVariablePack> + MultiStore<GenericVariable> + MultiStore<Id<Type>> + MultiStore<Identifier> + MultiStore<u32>
{
    let mut parser = TypeParser::new(*raw, store);
    let t = parser.try_parse(path);
    *raw = parser.into_raw();
    t
}

//
//  Implementation Details
//
struct EnumRecParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

struct TypeParser<'a, 'tree, 'store, S> {
    raw: RawParser<'a, 'tree>,
    store: &'store cell::RefCell<S>,
}

impl<'a, 'tree> EnumRecParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self {
        EnumRecParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse_enum(&mut self) -> EnumId {
        //  Expects:
        //  -   :enum
        //  -   type-identifier
        //  -   [
        //  -       generic parameters (with trailing comma)
        //  -   ]?
        //  -   {
        //  -       variants (with trailing comma)
        //  -   }
        let keyword = self.raw.pop_kind(Kind::KeywordEnum).expect(":enum");

        let name =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| self.raw.resolve_type(t))
                .unwrap_or(TypeIdentifier::default());

        let parameters = self.try_parse_generic_parameters();

        let enum_ = match self.raw.peek() {
            Some(Node::Braced(o, ns, c)) => {
                self.raw.pop_node();

                let mut parser = EnumRecParser::new(self.raw.spawn(ns));

                let mut variants = vec!();
                let mut commas = vec!();
                while let Some(identifier) =
                    parser.raw.pop_kind(Kind::NameType)
                        .map(|n| self.raw.resolve_type(n))
                {
                    let (v, c) = parser.parse_inner_record(Kind::SignComma, identifier);

                    let v = self.raw.module().borrow_mut()
                        .push_record(Record { inner: v, parameters, keyword: 0, semi_colon: 0 });
                    variants.push(v);
                    commas.push(c);
                }

                let mut module = self.raw.module().borrow_mut();

                Enum {
                    name,
                    parameters,
                    variants: module.push_record_ids(&variants),
                    keyword: keyword.offset() as u32,
                    open: o.offset() as u32,
                    close: c.offset() as u32,
                    commas: module.push_positions(&commas),
                }
            },
            _ => Enum {
                name: name,
                parameters,
                variants: Id::empty(),
                keyword: keyword.offset() as u32,
                open: 0,
                close: 0,
                commas: Id::empty(),
            },
        };

        self.raw.module().borrow_mut().push_enum(enum_)
    }

    fn parse_record(&mut self) -> RecordId {
        let keyword = self.raw.pop_kind(Kind::KeywordRec).expect(":rec");

        let identifier =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|n| self.raw.resolve_type(n))
                .expect("Record name");

        let parameters = self.try_parse_generic_parameters();

        let (inner, semi) = self.parse_inner_record(Kind::SignSemiColon, identifier);

        let record = Record {
            inner,
            parameters,
            keyword: keyword.offset() as u32,
            semi_colon: semi
        };

        self.raw.module().borrow_mut().push_record(record)
    }

    fn parse_inner_record(&mut self, end: Kind, identifier: TypeIdentifier) -> (InnerRecord, u32) {
        let variant = self.parse_inner_record_variant(identifier);

        let semi =
            self.raw
                .pop_kind(end)
                .map(|c| c.offset())
                .unwrap_or(identifier.span().end_offset() - 1) as u32;

        (variant, semi)
    }

    fn parse_inner_record_variant(&mut self, identifier: TypeIdentifier) -> InnerRecord {
        if let Some(_) = self.raw.peek_braced(Kind::ParenthesisOpen) {
            let mut inner = TypeParser::new(self.raw.clone(), self.raw.module());
            let tuple = inner.parse_tuple();
            self.raw = inner.into_raw();

            InnerRecord::Tuple(identifier, tuple)
        } else {
            InnerRecord::Unit(identifier)
        }
    }

    fn try_parse_generic_parameters(&mut self) -> Option<Id<GenericParameterPack>> {
        gen::try_parse_generic_parameters(&mut self.raw)
    }
}

impl<'a, 'tree, 'store, S> TypeParser<'a, 'tree, 'store, S>
    where
        S: Store<Type> + Store<GenericVariablePack> + MultiStore<GenericVariable> + MultiStore<Id<Type>> + MultiStore<Identifier> + MultiStore<u32>
{
    fn new(raw: RawParser<'a, 'tree>, store: &'store cell::RefCell<S>) -> Self {
        TypeParser { raw, store, }
    }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse(&mut self, path: Path) -> TypeId {
        if let Some(ty) = self.try_parse(path) {
            return ty;
        }

        let range = if let Some(n) = self.raw.peek() {
            Range::new(n.span().offset(), 0)
        } else {
            Range::new(0, 0)
        };

        self.store.borrow_mut().push(Type::Missing(range), range)
    }

    fn parse_tuple(&mut self) -> Tuple<Type> {
        if let Some((o, ns, c)) = self.raw.peek_braced(Kind::ParenthesisOpen) {
            self.raw.pop_node();

            let store = self.store;
            let parser = |raw: &mut RawParser<'a, 'tree>| {
                parse_type_impl(raw, store)
            };
            return self.raw.parse_tuple(self.store, parser, Kind::SignColon, ns, o, c);
        }

        panic!("Unreachable: should only be called on Node::Braced");
    }

    fn try_parse(&mut self, path: Path) -> Option<TypeId> {
        self.raw.peek().and_then(|node| {
            match node {
                Node::Run(..) => {
                    let typ = if let Some(typ) = self.raw.pop_kind(Kind::NameType) {
                        typ
                    } else {
                        return None;
                    };

                    let typ = self.raw.resolve_type(typ);

                    let variables = gen::try_parse_generic_variables(&mut self.raw, &self.store);

                    Some(match variables {
                        Some(variables) => Type::Generic(typ, variables, path),
                        None if path.is_empty() => Type::Simple(typ),
                        None => Type::Nested(typ, path),
                    })
                },
                Node::Braced(..) => {
                    let tup = self.parse_tuple();
                    Some(Type::Tuple(tup))
                },
                _ => unimplemented!()
            }
        }).map(|ty| {
            let range = ty.range(&*self.store.borrow());
            self.store.borrow_mut().push(ty, range)
        })
    }
}

fn parse_type_impl<'a, 'tree, S>(
    raw: &mut RawParser<'a, 'tree>,
    store: &cell::RefCell<S>,
)
    -> TypeId
    where
        S: Store<Type> + Store<GenericVariablePack> + MultiStore<GenericVariable> + MultiStore<Id<Type>> + MultiStore<Identifier> + MultiStore<u32>
{
    let path = raw.parse_path(store);
    let mut parser = TypeParser::new(*raw, store);
    let t = parser.parse(path);
    *raw = parser.into_raw();
    t
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
    fn enum_empty() {
        let env = LocalEnv::new(b":enum Empty {}");
        let i = env.factory().item();
        i.enum_(6, 5).braces(12, 13).build();

        assert_eq!(env.actual_enum(), env.expected_module());
    }

    #[test]
    fn enum_unit_single() {
        let env = LocalEnv::new(b":enum Simple { First }");
        let i = env.factory().item();
        i.enum_(6, 6).push_unit(15, 5).build();

        assert_eq!(env.actual_enum(), env.expected_module());
    }

    #[test]
    fn enum_unit_single_trailing_comma() {
        let env = LocalEnv::new(b":enum Simple { First ,}");
        let i = env.factory().item();
        i.enum_(6, 6).braces(13, 22).push_unit(15, 5).comma(21).build();

        assert_eq!(env.actual_enum(), env.expected_module());
    }

    #[test]
    fn enum_unit_multiple() {
        let env = LocalEnv::new(b":enum Simple { First, Second, Third }");
        let i = env.factory().item();
        i.enum_(6, 6)
            .push_unit(15, 5)
            .push_unit(22, 6)
            .push_unit(30, 5)
            .build();

        assert_eq!(env.actual_enum(), env.expected_module());
    }

    #[test]
    fn enum_generic_variants() {
        let env = LocalEnv::new(b":enum Option [T] { Some(T), None }");
        let (_, g, i, _, _, t, _) = env.factories();

        let parameters = g.parameters().push(g.parameter(14, 1)).build();
        let tuple = t.tuple().push(t.simple(24, 1)).build_tuple();

        i.enum_(6, 6)
            .parameters(parameters)
            .braces(17, 33)
            .push_tuple(19, 4, tuple)
            .push_unit(28, 4)
            .build();

        assert_eq!(env.actual_enum(), env.expected_module());
    }

    #[test]
    fn rec_tuple() {
        let env = LocalEnv::new(b":rec Tup(Int, String);");
        let (_, _, i, _, _, t, _) = env.factories();
        let tuple = t.tuple().push(t.simple(9, 3)).push(t.simple(14, 6)).build_tuple();
        i.record(5, 3).tuple(tuple).build();

        assert_eq!(env.actual_record(), env.expected_module());
    }

    #[test]
    fn rec_tuple_keyed() {
        let env = LocalEnv::new(b":rec Person(.name: String, .age: Int);");
        let (_, _, i, _, _, t, _) = env.factories();
        let tuple = t.tuple()
                    .name(12, 5).push(t.simple(19, 6))
                    .name(27, 4).push(t.simple(33, 3))
                    .build_tuple();
        i.record(5, 6).tuple(tuple).build();

        assert_eq!(env.actual_record(), env.expected_module());
    }

    #[test]
    fn rec_unit() {
        let env = LocalEnv::new(b":rec Simple;");
        let i = env.factory().item();
        i.record(5, 6).build();

        assert_eq!(env.actual_record(), env.expected_module());
    }

    #[test]
    fn rec_generic_pair() {
        let env = LocalEnv::new(b":rec Pair[T, U](T, U);");
        let (_, g, i, _, _, t, _) = env.factories();
        let parameters = g.parameters()
            .push(g.parameter(10, 1))
            .push(g.parameter(13, 1))
            .build();
        let tuple = t.tuple().push(t.simple(16, 1)).push(t.simple(19, 1)).build_tuple();
        i.record(5, 4).parameters(parameters).tuple(tuple).build();

        assert_eq!(env.actual_record(), env.expected_module());
    }

    #[test]
    fn type_simple() {
        //  By default, types are parsed in the Tree, so test with the Module
        //  to ensure that the non-default path works just as well.
        let env = LocalEnv::new(b"Int");
        env.factory().type_module().simple(0, 3);

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn type_nested() {
        let env = LocalEnv::new(b"Enum::Variant");
        env.factory().type_module().nested(6, 7).push(0, 4).build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn type_nested_nested() {
        let env = LocalEnv::new(b"Enum::Other::Variant");
        env.factory().type_module().nested(13, 7).push(0, 4).push(6, 5).build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_unit() {
        let env = LocalEnv::new(b"()");
        env.factory().type_module().tuple().parens(0, 1).build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_unit_spaced() {
        let env = LocalEnv::new(b"( )");
        env.factory().type_module().tuple().parens(0, 2).build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_simple() {
        let env = LocalEnv::new(b"(Int)");
        let t = env.factory().type_module();
        t.tuple().push(t.simple(1, 3)).build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_simple_trailing_comma() {
        let env = LocalEnv::new(b"(Int,)");
        let t = env.factory().type_module();
        t.tuple().push(t.simple(1, 3)).comma(4).build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_few() {
        let env = LocalEnv::new(b"(Int,Int ,Int)");
        let t = env.factory().type_module();
        t.tuple()
            .push(t.simple(1, 3))
            .push(t.simple(5, 3))
            .comma(9)
            .push(t.simple(10, 3))
            .build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_few_spaces_and_commas() {
        let env = LocalEnv::new(b" ( Int , Int, Int , )");
        let t = env.factory().type_module();
        t.tuple()
            .parens(1, 20)
            .push(t.simple(3, 3))
            .comma(7)
            .push(t.simple(9, 3))
            .push(t.simple(14, 3))
            .comma(18)
            .build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_keyed() {
        let env = LocalEnv::new(b"(.name: String, .age: Int)");
        let t = env.factory().type_module();
        t.tuple()
            .name(1, 5).push(t.simple(8, 6))
            .name(16, 4).push(t.simple(22, 3))
            .build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    #[test]
    fn tuple_nested() {
        let env = LocalEnv::new(b"((Int, Int), Int, )");
        let t = env.factory().type_module();
        t.tuple()
            .parens(0, 18)
            .push(
                t.tuple()
                    .push(t.simple(2, 3))
                    .push(t.simple(7, 3))
                    .build(),
            )
            .push(t.simple(13, 3))
            .comma(16)
            .build();

        assert_eq!(env.actual_type(), env.expected_module());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_enum(&self) -> Module {
            let mut raw = self.env.raw();
            super::parse_enum(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_enum: {:#?}", result);
            println!();
            result
        }

        fn actual_record(&self) -> Module {
            let mut raw = self.env.raw();
            super::parse_record(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_record: {:#?}", result);
            println!();
            result
        }

        fn actual_type(&self) -> Module {
            let mut raw = self.env.raw();
            let module = raw.module();
            super::parse_type_impl(&mut raw, module);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_type: {:#?}", result);
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
