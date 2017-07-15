//! Syntactic passes, aka parsing.
//!
//! Type Parser.

use basic::com;

use model::tt::{Kind, Node};
use model::syn::{Enum, InnerRecord, Record, Type, TypeIdentifier, Tuple};

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
    -> Record
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

        let name = TypeIdentifier(
            self.raw
                .pop_kind(Kind::NameType)
                .map(|t| t.range())
                .unwrap_or(com::Range::new(keyword.range().end_offset(), 0))
        );

        match self.raw.peek() {
            Some(Node::Braced(o, ns, c)) => {
                let mut parser = EnumRecParser::new(self.raw.spawn(ns));

                let mut variants = self.raw.local_array();
                let mut semis = self.raw.local_array();
                while let Some((v, c)) =
                    parser.parse_inner_record(Kind::SignComma)
                {
                    variants.push(v);
                    semis.push(c);
                }

                Enum {
                    name: name,
                    variants: self.raw.intern_slice(variants.into_slice()),
                    keyword: keyword.offset() as u32,
                    open: o.offset() as u32,
                    close: c.offset() as u32,
                    commas: self.raw.intern_slice(semis.into_slice()),
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

    fn parse_record(&mut self) -> Record {
        let keyword = self.raw.pop_kind(Kind::KeywordRec).expect(":rec");
        let missing = com::Range::new(keyword.range().end_offset(), 0);

        let (inner, semi) =
            self.parse_inner_record(Kind::SignSemiColon)
                .unwrap_or((InnerRecord::Missing(missing), 0));

        Record {
            inner: inner,
            keyword: keyword.offset() as u32,
            semi_colon: semi
        }
    }

    fn parse_inner_record(&mut self, end: Kind) -> Option<(InnerRecord, u32)> {
        if self.raw.peek().is_none() {
            return None;
        }

        let variant =
            self.raw
                .pop_kind(Kind::NameType)
                .map(|n| InnerRecord::Unit(TypeIdentifier(n.range())));

        let semi =
            self.raw
                .pop_kind(end)
                .map(|c| c.offset() as u32);

        match (variant, semi) {
            (Some(variant), Some(semi)) => Some((variant, semi)),
            (Some(variant), None) => Some((variant, 0)),
            (None, Some(semi)) => Some((
                InnerRecord::Missing(com::Range::new(semi as usize, 0)),
                semi
            )),
            (None, None) => unimplemented!(),
        }
    }
}

impl<'a, 'g, 'local> TypeParser<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> TypeParser<'a, 'g, 'local> {
        TypeParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse(&mut self) -> Type<'g> {
        self.maybe_parse()
            .or_else(||
                self.raw.peek().map(|n|
                    Type::Missing(com::Range::new(n.range().offset(), 0))
                )
            )
            .unwrap_or(Type::Missing(com::Range::new(0, 0)))
    }

    fn maybe_parse(&mut self) -> Option<Type<'g>> {
        self.raw.peek().map(|node| {
            match node {
                Node::Run(_) => {
                    let t = self.raw.pop_kind(Kind::NameType).expect("Type");
                    Type::Simple(TypeIdentifier(t.range()))
                },
                Node::Braced(o, ns, c) => {
                    self.raw.pop_node();

                    assert_eq!(o.kind(), Kind::ParenthesisOpen);
                    let mut inner = TypeParser { raw: self.raw.spawn(ns) };

                    let mut fields = self.raw.local_array();
                    let mut commas = self.raw.local_array();

                    while let Some(t) = inner.maybe_parse() {
                        fields.push(t);
                        if let Some(c) = inner.raw.pop_kind(Kind::SignComma) {
                            commas.push(c.range().offset() as u32)
                        } else {
                            commas.push(t.range().end_offset() as u32 - 1)
                        };
                    }

                    assert!(inner.into_raw().peek().is_none());

                    Type::Tuple(Tuple {
                        fields: self.raw.intern_slice(fields.into_slice()),
                        commas: self.raw.intern_slice(commas.into_slice()),
                        open: o.offset() as u32,
                        close: c.offset() as u32,
                    })
                },
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
    use basic::{com, mem};
    use model::syn::*;

    #[test]
    fn enum_empty() {
        let global = mem::Arena::new();

        assert_eq!(
            enumit(&global, b":enum Empty {}"),
            Enum {
                name: typeid(6, 5),
                variants: &[],
                keyword: 0,
                open: 12,
                close: 13,
                commas: &[]
            }
        );
    }

    #[test]
    fn enum_unit_single() {
        let global = mem::Arena::new();

        assert_eq!(
            enumit(&global, b":enum Simple { First }"),
            Enum {
                name: typeid(6, 6),
                variants: &[ InnerRecord::Unit(typeid(15, 5)) ],
                keyword: 0,
                open: 13,
                close: 21,
                commas: &[ 0 ]
            }
        );

        assert_eq!(
            enumit(&global, b":enum Simple { First ,}"),
            Enum {
                name: typeid(6, 6),
                variants: &[ InnerRecord::Unit(typeid(15, 5)) ],
                keyword: 0,
                open: 13,
                close: 22,
                commas: &[ 21 ]
            }
        );
    }

    #[test]
    fn enum_unit_multiple() {
        let global = mem::Arena::new();

        assert_eq!(
            enumit(&global, b":enum Simple { First, Second, Third }"),
            Enum {
                name: typeid(6, 6),
                variants: &[
                    InnerRecord::Unit(typeid(15, 5)),
                    InnerRecord::Unit(typeid(22, 6)),
                    InnerRecord::Unit(typeid(30, 5)),
                ],
                keyword: 0,
                open: 13,
                close: 36,
                commas: &[ 20, 28, 0 ]
            }
        );
    }

    #[test]
    fn rec_unit() {
        let global = mem::Arena::new();

        assert_eq!(
            recit(&global, b":rec Simple;"),
            Record {
                inner: InnerRecord::Unit(typeid(5, 6)),
                keyword: 0,
                semi_colon: 11,
            }
        );
    }

    #[test]
    fn tuple_unit() {
        let global = mem::Arena::new();

        assert_eq!(
            typeit(&global, b"()"),
            Type::Tuple(Tuple {
                fields: &[],
                commas: &[],
                open: 0,
                close: 1,
            })
        );

        assert_eq!(
            typeit(&global, b"( )"),
            Type::Tuple(Tuple {
                fields: &[],
                commas: &[],
                open: 0,
                close: 2,
            })
        );
    }

    #[test]
    fn tuple_simple() {
        let global = mem::Arena::new();

        assert_eq!(
            typeit(&global, b"(Int)"),
            Type::Tuple(Tuple {
                fields: &[simple_type(1, 3)],
                commas: &[3],
                open: 0,
                close: 4,
            })
        );

        assert_eq!(
            typeit(&global, b"(Int,)"),
            Type::Tuple(Tuple {
                fields: &[simple_type(1, 3)],
                commas: &[4],
                open: 0,
                close: 5,
            })
        );
    }

    #[test]
    fn tuple_few() {
        let global = mem::Arena::new();

        assert_eq!(
            typeit(&global, b"(Int,Int ,Int)"),
            Type::Tuple(Tuple {
                fields: &[
                    simple_type(1, 3),
                    simple_type(5, 3),
                    simple_type(10, 3)
                ],
                commas: &[4, 9, 12],
                open: 0,
                close: 13,
            })
        );

        assert_eq!(
            typeit(&global, b" ( Int , Int, Int , )"),
            Type::Tuple(Tuple {
                fields: &[
                    simple_type(3, 3),
                    simple_type(9, 3),
                    simple_type(14, 3)
                ],
                commas: &[7, 12, 18],
                open: 1,
                close: 20,
            })
        );
    }

    #[test]
    fn tuple_nested() {
        let global = mem::Arena::new();

        assert_eq!(
            typeit(&global, b"((Int, Int), Int, )"),
            Type::Tuple(Tuple {
                fields: &[
                    Type::Tuple(Tuple {
                        fields: &[simple_type(2, 3), simple_type(7, 3)],
                        commas: &[5, 9],
                        open: 1,
                        close: 10,
                    }),
                    simple_type(13, 3),
                ],
                commas: &[11, 16],
                open: 0,
                close: 18,
            })
        );
    }

    fn enumit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Enum<'g> {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let e = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_enum(&mut raw)
        };
        local_arena.recycle();

        e
    }

    fn recit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Record {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let r = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_record(&mut raw)
        };
        local_arena.recycle();

        r
    }

    fn typeit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Type<'g> {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let t = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_type(&mut raw)
        };
        local_arena.recycle();

        t
    }

    fn simple_type(offset: usize, length: usize) -> Type<'static> {
        Type::Simple(typeid(offset, length))
    }

    fn typeid(offset: usize, length: usize) -> TypeIdentifier {
        TypeIdentifier(range(offset, length))
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
