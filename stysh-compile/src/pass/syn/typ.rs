//! Syntactic passes, aka parsing.
//!
//! Type Parser.

use basic::com;

use model::syn::{Type, TypeIdentifier, TypeTuple};

use super::com::RawParser;

pub struct TypeParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>
}

pub fn parse_type<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Type<'g>
{
    let mut parser = TypeParser::new(*raw);
    let t = parser.parse();
    *raw = parser.into_raw();
    t
}

impl<'a, 'g, 'local> TypeParser<'a, 'g, 'local> {
    pub fn new(raw: RawParser<'a, 'g, 'local>) -> TypeParser<'a, 'g, 'local> {
        TypeParser { raw: raw }
    }

    pub fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    pub fn parse(&mut self) -> Type<'g> {
        self.maybe_parse()
            .or_else(||
                self.raw.peek().map(|n|
                    Type::Missing(com::Range::new(n.range().offset(), 0))
                )
            )
            .unwrap_or(Type::Missing(com::Range::new(0, 0)))
    }

    pub fn maybe_parse(&mut self) -> Option<Type<'g>> {
        use model::tt::{Kind, Node};

        self.raw.peek().map(|node| {
            match node {
                Node::Run(_) => {
                    let t = self.raw.pop_kind(Kind::NameType).expect("Type");
                    Type::Simple(TypeIdentifier(t.range()))
                },
                Node::Braced(o, ns, c) => {
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

                    self.raw.pop_node();

                    Type::Tuple(TypeTuple {
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
    fn tuple_unit() {
        let global = mem::Arena::new();

        assert_eq!(
            typeit(&global, b"()"),
            Type::Tuple(TypeTuple {
                fields: &[],
                commas: &[],
                open: 0,
                close: 1,
            })
        );

        assert_eq!(
            typeit(&global, b"( )"),
            Type::Tuple(TypeTuple {
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
            Type::Tuple(TypeTuple {
                fields: &[simple_type(1, 3)],
                commas: &[3],
                open: 0,
                close: 4,
            })
        );

        assert_eq!(
            typeit(&global, b"(Int,)"),
            Type::Tuple(TypeTuple {
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
            Type::Tuple(TypeTuple {
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
            Type::Tuple(TypeTuple {
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
            Type::Tuple(TypeTuple {
                fields: &[
                    Type::Tuple(TypeTuple {
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

    fn typeit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Type<'g> {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let e = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_type(&mut raw)
        };
        local_arena.recycle();

        e
    }

    fn simple_type(offset: usize, length: usize) -> Type<'static> {
        Type::Simple(TypeIdentifier(range(offset, length)))
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
