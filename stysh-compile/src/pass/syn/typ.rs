//! Syntactic passes, aka parsing.
//!
//! Type Parser.

use basic::com::{self, Span};

use model::tt::{Kind, Node};
use model::ast::*;

use super::com::RawParser;

pub fn parse_enum<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Enum<'g>
{
    let mut parser = EnumRecParser::new(*raw);
    let e = parser.parse_enum();
    *raw = parser.into_raw();
    e
}

pub fn parse_record<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Record<'g>
{
    let mut parser = EnumRecParser::new(*raw);
    let r = parser.parse_record();
    *raw = parser.into_raw();
    r
}

pub fn parse_type<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Type<'g>
{
    let mut parser = TypeParser::new(*raw);
    let t = parser.parse();
    *raw = parser.into_raw();
    t
}

pub fn try_parse_type<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Option<Type<'g>>
{
    let mut parser = TypeParser::new(*raw);
    let t = parser.try_parse();
    *raw = parser.into_raw();
    t
}

//
//  Implementation Details
//
struct EnumRecParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>,
}

struct TypeParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>,
}

impl<'a, 'g, 'local> EnumRecParser<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> EnumRecParser<'a, 'g, 'local> {
        EnumRecParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse_enum(&mut self) -> Enum<'g> {
        //  Expects:
        //  -   :enum
        //  -   type-identifier
        //  -   {
        //  -       variants (with trailing comma)
        //  -   }
        let keyword = self.raw.pop_kind(Kind::KeywordEnum).expect(":enum");

        let name =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| self.raw.resolve_type(t))
                .unwrap_or(TypeIdentifier::default());

        match self.raw.peek() {
            Some(Node::Braced(o, ns, c)) => {
                let mut parser = EnumRecParser::new(self.raw.spawn(ns));

                let mut variants = self.raw.local_array();
                let mut commas = self.raw.local_array();
                while let Some((v, c)) =
                    parser.parse_inner_record(Kind::SignComma)
                {
                    variants.push(v);
                    commas.push(c);
                }

                Enum {
                    name: name,
                    variants: self.raw.intern_slice(variants.into_slice()),
                    keyword: keyword.offset() as u32,
                    open: o.offset() as u32,
                    close: c.offset() as u32,
                    commas: self.raw.intern_slice(commas.into_slice()),
                }
            },
            _ => Enum {
                name: name,
                variants: &[],
                keyword: keyword.offset() as u32,
                open: 0,
                close: 0,
                commas: &[],
            },
        }
    }

    fn parse_record(&mut self) -> Record<'g> {
        let keyword = self.raw.pop_kind(Kind::KeywordRec).expect(":rec");
        let missing = com::Range::new(keyword.span().end_offset(), 0);

        let (inner, semi) =
            self.parse_inner_record(Kind::SignSemiColon)
                .unwrap_or((InnerRecord::Missing(missing), 0));

        Record {
            inner: inner,
            keyword: keyword.offset() as u32,
            semi_colon: semi
        }
    }

    fn parse_inner_record(&mut self, end: Kind) -> Option<(InnerRecord<'g>, u32)> {
        if self.raw.peek().is_none() {
            return None;
        }

        let variant = self.parse_inner_record_variant();

        let semi =
            self.raw
                .pop_kind(end)
                .map(|c| c.offset() as u32);

        match (variant, semi) {
            (Some(variant), Some(semi)) => Some((variant, semi)),
            (Some(variant), None)
                => Some((variant, variant.span().end_offset() as u32 - 1)),
            (None, Some(semi)) => Some((
                InnerRecord::Missing(com::Range::new(semi as usize, 0)),
                semi
            )),
            (None, None) => unimplemented!(),
        }
    }

    fn parse_inner_record_variant(&mut self) -> Option<InnerRecord<'g>> {
        let identifier =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|n| self.raw.resolve_type(n));

        if let Some(identifier) = identifier {
            if let Some(Node::Braced(..)) = self.raw.peek() {
                let mut inner = TypeParser::new(self.raw.clone());
                let tuple = inner.parse_tuple();
                self.raw = inner.into_raw();

                return Some(InnerRecord::Tuple(identifier, tuple));
            }

            return Some(InnerRecord::Unit(identifier));
        }

        None
    }
}

impl<'a, 'g, 'local> TypeParser<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> TypeParser<'a, 'g, 'local> {
        TypeParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse(&mut self) -> Type<'g> {
        self.try_parse()
            .or_else(||
                self.raw.peek().map(|n|
                    Type::Missing(com::Range::new(n.span().offset(), 0))
                )
            )
            .unwrap_or(Type::Missing(com::Range::new(0, 0)))
    }

    fn parse_tuple(&mut self) -> Tuple<'g, Type<'g>> {
        if let Some(Node::Braced(o, ns, c)) = self.raw.peek() {
            self.raw.pop_node();
            return self.raw.parse_tuple(parse_type, Kind::SignColon, ns, o, c);
        }

        panic!("Unreachable: should only be called on Node::Braced");
    }

    fn try_parse(&mut self) -> Option<Type<'g>> {
        self.raw.peek().and_then(|node| {
            match node {
                Node::Run(run) => {
                    let mut components = self.raw.local_array();
                    let mut colons = self.raw.local_array();

                    if run[0].kind() != Kind::NameType {
                        return None;
                    }

                    let mut t =
                        self.raw.pop_kind(Kind::NameType).expect("Type");

                    while let Some(c) =
                        self.raw.pop_kind(Kind::SignDoubleColon)
                    {
                        components.push(self.raw.resolve_type(t));
                        colons.push(c.offset() as u32);

                        t = self.raw.pop_kind(Kind::NameType).expect("Type");
                    }

                    if components.is_empty() {
                        Some(Type::Simple(self.raw.resolve_type(t)))
                    } else {
                        let path = Path {
                            components:
                                self.raw.intern_slice(components.into_slice()),
                            colons: self.raw.intern_slice(colons.into_slice()),
                        };
                        Some(Type::Nested(self.raw.resolve_type(t), path))
                    }
                },
                Node::Braced(..) => Some(Type::Tuple(self.parse_tuple())),
                _ => unimplemented!()
            }
        })
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::super::com::tests::{Env, LocalEnv};
    use model::ast::*;

    #[test]
    fn enum_empty() {
        let env = Env::new();
        let local = env.local(b":enum Empty {}");
        let i = local.factory().item();

        assert_eq!(
            enumit(&local),
            i.enum_(6, 5).braces(12, 13).build()
        );
    }

    #[test]
    fn enum_unit_single() {
        let env = Env::new();
        let local = env.local(b":enum Simple { First }");
        let i = local.factory().item();

        assert_eq!(
            enumit(&local),
            i.enum_(6, 6).push_unit(15, 5).build()
        );
    }

    #[test]
    fn enum_unit_single_trailing_comma() {
        let env = Env::new();
        let local = env.local(b":enum Simple { First ,}");
        let i = local.factory().item();

        assert_eq!(
            enumit(&local),
            i.enum_(6, 6).braces(13, 22).push_unit(15, 5).comma(21).build()
        ); 
    }

    #[test]
    fn enum_unit_multiple() {
        let env = Env::new();
        let local = env.local(b":enum Simple { First, Second, Third }");
        let i = local.factory().item();

        assert_eq!(
            enumit(&local),
            i.enum_(6, 6)
                .push_unit(15, 5)
                .push_unit(22, 6)
                .push_unit(30, 5)
                .build()
        );
    }

    #[test]
    fn rec_tuple() {
        let env = Env::new();
        let local = env.local(b":rec Tup(Int, String);");
        let (_, i, _, _, t) = local.factories();

        assert_eq!(
            recit(&local),
            i.record(5, 3).tuple(
                t.tuple()
                    .push(t.simple(9, 3))
                    .push(t.simple(14, 6))
                    .build()
            ).build()
        );
    }

    #[test]
    fn rec_tuple_keyed() {
        let env = Env::new();
        let local = env.local(b":rec Person(.name: String, .age: Int);");
        let (_, i, _, _, t) = local.factories();

        assert_eq!(
            recit(&local),
            i.record(5, 6).tuple(
                t.tuple()
                    .name(12, 5).push(t.simple(19, 6))
                    .name(27, 4).push(t.simple(33, 3))
                    .build()
            ).build()
        );
    }

    #[test]
    fn rec_unit() {
        let env = Env::new();
        let local = env.local(b":rec Simple;");
        let i = local.factory().item();

        assert_eq!(
            recit(&local),
            i.record(5, 6).build()
        );
    }

    #[test]
    fn type_simple() {
        let env = Env::new();
        let local = env.local(b"Int");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.simple(0, 3)
        );
    }

    #[test]
    fn type_nested() {
        let env = Env::new();
        let local = env.local(b"Enum::Variant");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.nested(6, 7).push(0, 4).build()
        );
    }

    #[test]
    fn type_nested_nested() {
        let env = Env::new();
        let local = env.local(b"Enum::Other::Variant");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.nested(13, 7).push(0, 4).push(6, 5).build()
        );
    }

    #[test]
    fn tuple_unit() {
        let env = Env::new();
        let local = env.local(b"()");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple().parens(0, 1).build()
        );
    }

    #[test]
    fn tuple_unit_spaced() {
        let env = Env::new();
        let local = env.local(b"( )");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple().parens(0, 2).build()
        );
    }

    #[test]
    fn tuple_simple() {
        let env = Env::new();
        let local = env.local(b"(Int)");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple().push(t.simple(1, 3)).build()
        );
    }

    #[test]
    fn tuple_simple_trailing_comma() {
        let env = Env::new();
        let local = env.local(b"(Int,)");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple().push(t.simple(1, 3)).comma(4).build()
        );
    }

    #[test]
    fn tuple_few() {
        let env = Env::new();
        let local = env.local(b"(Int,Int ,Int)");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple()
                .push(t.simple(1, 3))
                .push(t.simple(5, 3))
                .comma(9)
                .push(t.simple(10, 3))
                .build()
        );
    }

    #[test]
    fn tuple_few_spaces_and_commas() {
        let env = Env::new();
        let local = env.local(b" ( Int , Int, Int , )");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple()
                .parens(1, 20)
                .push(t.simple(3, 3))
                .comma(7)
                .push(t.simple(9, 3))
                .push(t.simple(14, 3))
                .comma(18)
                .build()
        );
    }

    #[test]
    fn tuple_keyed() {
        let env = Env::new();
        let local = env.local(b"(.name: String, .age: Int)");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
            t.tuple()
                .name(1, 5).push(t.simple(8, 6))
                .name(16, 4).push(t.simple(22, 3))
                .build()
        );
    }

    #[test]
    fn tuple_nested() {
        let env = Env::new();
        let local = env.local(b"((Int, Int), Int, )");
        let t = local.factory().type_();

        assert_eq!(
            typeit(&local),
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
                .build()
        );
    }

    fn enumit<'g>(local: &LocalEnv<'g>) -> Enum<'g> {
        let mut raw = local.raw();
        super::parse_enum(&mut raw)
    }

    fn recit<'g>(local: &LocalEnv<'g>) -> Record<'g> {
        let mut raw = local.raw();
        super::parse_record(&mut raw)
    }

    fn typeit<'g>(local: &LocalEnv<'g>) -> Type<'g> {
        let mut raw = local.raw();
        super::parse_type(&mut raw)
    }
}
