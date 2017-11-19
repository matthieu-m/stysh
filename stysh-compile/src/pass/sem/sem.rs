//! Semantic passes, aka name resolution, type checking, ...
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use basic::{com, mem};

use model::{syn, sem};

use super::nmr::{self, scp};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'a, 'g, 'local>
    where 'g: 'a
{
    code_fragment: com::CodeFragment,
    scope: &'a scp::Scope<'g>,
    registry: &'a sem::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> GraphBuilder<'a, 'g, 'local>
    where 'g: 'a
{
    /// Creates a new instance of the graph builder.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(
        source: com::CodeFragment,
        scope: &'a scp::Scope<'g>,
        registry: &'a sem::Registry<'g>,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> GraphBuilder<'a, 'g, 'local>
    {
        GraphBuilder {
            code_fragment: source,
            scope: scope,
            registry: registry,
            global_arena: global,
            local_arena: local
        }
    }

    /// Extracts the prototypes of an item.
    pub fn prototype(&mut self, item: &syn::Item) -> sem::Prototype<'g> {
        match *item {
            syn::Item::Enum(e) => self.enum_prototype(e),
            syn::Item::Fun(fun) => self.fun_prototype(fun),
            syn::Item::Rec(r) => self.rec_prototype(r),
        }
    }

    /// Translates a stand-alone expression.
    pub fn expression(&mut self, e: &syn::Expression) -> sem::Value<'g> {
        self.resolver(self.scope).value_of(e)
    }

    /// Translates a full-fledged item.
    pub fn item(&mut self, proto: &'g sem::Prototype<'g>, item: &syn::Item)
        -> sem::Item<'g>
    {
        use model::syn::Item;
        use model::sem::Prototype::*;

        debug_assert!(
            item.range().offset() == proto.range().offset(),
            "Mismatched item and prototype: {} vs {}",
            item.range(),
            proto.range()
        );

        match (*item, proto) {
            (Item::Enum(i), &Enum(ref p)) => self.enum_item(i, p),
            (Item::Fun(i), &Fun(ref p)) => self.fun_item(i, p),
            (Item::Rec(r), &Rec(ref p)) => self.rec_item(r, p),
            (Item::Enum(_), &p) => panic!("Expected enum {:?}", p),
            (Item::Fun(_), &p) => panic!("Expected function {:?}", p),
            (Item::Rec(_), &p) => panic!("Expected record {:?}", p),
        }
    }
}

//
//  Implementation Details
//
impl<'a, 'g, 'local> GraphBuilder<'a, 'g, 'local>
    where 'g: 'a
{
    fn enum_prototype(&mut self, e: syn::Enum) -> sem::Prototype<'g> {
        sem::Prototype::Enum(
            sem::EnumProto {
                name: e.name.into(),
                range: e.keyword().range().extend(e.name.range()),
            }
        )
    }

    fn fun_prototype(&mut self, fun: syn::Function) -> sem::Prototype<'g> {
        let mut arguments =
            mem::Array::with_capacity(fun.arguments.len(), self.global_arena);

        for a in fun.arguments {
            arguments.push(sem::Binding::Argument(
                sem::ValueIdentifier(a.name.0),
                self.resolver(self.scope).type_of(&a.type_),
                a.range()
            ));
        }

        sem::Prototype::Fun(
            sem::FunctionProto {
                name: sem::ItemIdentifier(fun.name.0),
                range: com::Range::new(
                    fun.keyword as usize,
                    fun.result.range().end_offset() - (fun.keyword as usize)
                ),
                arguments: arguments.into_slice(),
                result: self.resolver(self.scope).type_of(&fun.result),
            }
        )
    }

    fn rec_prototype(&mut self, r: syn::Record) -> sem::Prototype<'g> {
        sem::Prototype::Rec(sem::RecordProto {
            name: r.name().into(),
            range: r.range(),
            enum_: sem::ItemIdentifier::unresolved(),
        })
    }

    fn enum_item(&mut self, e: syn::Enum, p: &'g sem::EnumProto)
        -> sem::Item<'g>
    {
        let mut variants =
            mem::Array::with_capacity(e.variants.len(), self.global_arena);

        for ev in e.variants {
            use self::syn::InnerRecord::*;

            match *ev {
                Unit(name) => variants.push(sem::Record {
                    prototype: sem::RecordProto {
                        name: name.into(),
                        range: ev.range(),
                        enum_: p.name.into()
                    },
                }),
                Missing(_) | Unexpected(_) => (),
            }
        }

        sem::Item::Enum(sem::Enum {
            prototype: p,
            variants: variants.into_slice(),
        })
    }

    fn fun_item(&mut self, fun: syn::Function, p: &'g sem::FunctionProto<'g>)
        -> sem::Item<'g>
    {
        let scope = self.function_scope(self.scope, p);

        sem::Item::Fun(sem::Function {
            prototype: p,
            body: self.resolver(&scope).value_of(&fun.body)
        })
    }

    fn rec_item(&mut self, _: syn::Record, p: &'g sem::RecordProto)
        -> sem::Item<'g>
    {
        sem::Item::Rec(sem::Record { prototype: *p })
    }

    fn function_scope<'b>(
        &'b self,
        parent: &'b scp::Scope<'g>,
        p: &'b sem::FunctionProto<'b>,
    )
        -> scp::FunctionScope<'b, 'g>
    {
        scp::FunctionScope::new(
            &*self.code_fragment,
            parent,
            p,
            self.global_arena,
            self.local_arena,
        )
    }

    fn resolver<'b>(&'b self, scope: &'b scp::Scope<'g>)
        -> nmr::NameResolver<'b, 'g, 'local>
    {
        nmr::NameResolver::new(
            &*self.code_fragment,
            scope,
            self.registry,
            self.global_arena,
            self.local_arena
        )
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::syn;
    use model::sem::*;
    use model::sem::mocks::MockRegistry;
    use super::scp::mocks::MockScope;

    #[test]
    fn prototype_enum() {
        fn unit(offset: usize, length: usize) -> syn::InnerRecord {
            syn::InnerRecord::Unit(syn::TypeIdentifier(range(offset, length)))
        }

        let global_arena = mem::Arena::new();
        let env = Env::new(b":enum Simple { One, Two }", &global_arena);

        assert_eq!(
            env.proto_of(
                &syn::Item::Enum(syn::Enum {
                    name: syn::TypeIdentifier(range(6, 6)),
                    variants: &[unit(15, 3), unit(20, 3)],
                    keyword: 0,
                    open: 13,
                    close: 24,
                    commas: &[18, 0],
                }),
            ),
            Prototype::Enum(EnumProto {
                name: ItemIdentifier(range(6, 6)),
                range: range(0, 12)
            })
        );
    }

    #[test]
    fn item_enum() {
        fn unit(offset: usize, length: usize) -> syn::InnerRecord {
            syn::InnerRecord::Unit(syn::TypeIdentifier(range(offset, length)))
        }

        fn item_id(offset: usize, length: usize) -> ItemIdentifier {
            ItemIdentifier(range(offset, length))
        }

        let global_arena = mem::Arena::new();
        let env = Env::new(b":enum Simple { One, Two }", &global_arena);

        let enum_prototype = EnumProto {
            name: item_id(6, 6),
            range: range(0, 12)
        };

        assert_eq!(
            env.item_of(
                &Prototype::Enum(enum_prototype),
                &syn::Item::Enum(syn::Enum {
                    name: syn::TypeIdentifier(range(6, 6)),
                    variants: &[unit(15, 3), unit(20, 3)],
                    keyword: 0,
                    open: 13,
                    close: 24,
                    commas: &[18, 0],
                }),
            ),
            Item::Enum(Enum {
                prototype: &enum_prototype,
                variants: &[
                    Record {
                        prototype: RecordProto {
                            name: item_id(15, 3),
                            range: range(15, 3),
                            enum_: enum_prototype.name,
                        },
                    },
                    Record {
                        prototype: RecordProto {
                            name: item_id(20, 3),
                            range: range(20, 3),
                            enum_: enum_prototype.name,
                        },
                    },
                ],
            })
        );
    }

    #[test]
    fn prototype_rec() {
        fn unit(offset: usize, length: usize) -> syn::InnerRecord {
            syn::InnerRecord::Unit(syn::TypeIdentifier(range(offset, length)))
        }

        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Simple;", &global_arena);

        assert_eq!(
            env.proto_of(
                &syn::Item::Rec(syn::Record {
                    inner: unit(5, 6),
                    keyword: 0,
                    semi_colon: 11,
                }),
            ),
            Prototype::Rec(RecordProto {
                name: ItemIdentifier(range(5, 6)),
                range: range(0, 12),
                enum_: ItemIdentifier::unresolved(),
            })
        );
    }

    #[test]
    fn item_rec() {
        fn unit(offset: usize, length: usize) -> syn::InnerRecord {
            syn::InnerRecord::Unit(syn::TypeIdentifier(range(offset, length)))
        }

        fn item_id(offset: usize, length: usize) -> ItemIdentifier {
            ItemIdentifier(range(offset, length))
        }

        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Simple;", &global_arena);

        let rec_prototype = RecordProto {
            name: item_id(5, 6),
            range: range(0, 12),
            enum_: ItemIdentifier::unresolved(),
        };

        assert_eq!(
            env.item_of(
                &Prototype::Rec(rec_prototype),
                &syn::Item::Rec(syn::Record {
                    inner: unit(5, 6),
                    keyword: 0,
                    semi_colon: 11,
                }),
            ),
            Item::Rec(Record { prototype: rec_prototype })
        );
    }

    #[test]
    fn prototype_fun() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add(a: Int, b: Int) -> Int { 1 + 2 }",
            &global_arena
        );
        let int = Type::Builtin(BuiltinType::Int);

        assert_eq!(
            env.proto_of(
                &syn::Item::Fun(
                    syn::Function {
                        name: syn::VariableIdentifier(range(5, 3)),
                        arguments: &[
                            syn::Argument {
                                name: syn::VariableIdentifier(range(9, 1)),
                                type_: type_simple(12, 3),
                                colon: 10,
                                comma: 15,
                            },
                            syn::Argument {
                                name: syn::VariableIdentifier(range(17, 1)),
                                type_: type_simple(20, 3),
                                colon: 18,
                                comma: 0,
                            }
                        ],
                        result: type_simple(28, 3),
                        body: syn::Expression::Var(
                            syn::VariableIdentifier(range(0, 0))
                        ),
                        keyword: 0,
                        open: 0,
                        close: 0,
                        arrow: 0,
                    }
                )
            ),
            Prototype::Fun(
                FunctionProto {
                    name: ItemIdentifier(range(5, 3)),
                    range: range(0, 31),
                    arguments: &[
                        Binding::Argument(
                            ValueIdentifier(range(9, 1)),
                            int,
                            range(9, 7),
                        ),
                        Binding::Argument(
                            ValueIdentifier(range(17, 1)),
                            int,
                            range(17, 6),
                        ),
                    ],
                    result: int,
                }
            )
        );
    }

    #[test]
    fn item_fun() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add(a: Int, b: Int) -> Int { a + b }",
            &global_arena,
        );
        let int = Type::Builtin(BuiltinType::Int);

        let function_proto = FunctionProto {
            name: ItemIdentifier(range(5, 3)),
            range: range(0, 31),
            arguments: &[
                Binding::Argument(value(9, 1), int, range(9, 7)),
                Binding::Argument(value(17, 1), int, range(17, 6)),
            ],
            result: int,
        };

        let prototype = Prototype::Fun(function_proto);

        let arg = resolved_argument;

        assert_eq!(
            env.item_of(
                &prototype,
                &syn::Item::Fun(
                    syn::Function {
                        name: syn::VariableIdentifier(range(5, 3)),
                        arguments: &[
                            syn::Argument {
                                name: syn::VariableIdentifier(range(9, 1)),
                                type_: type_simple(12, 3),
                                colon: 10,
                                comma: 15,
                            },
                            syn::Argument {
                                name: syn::VariableIdentifier(range(17, 1)),
                                type_: type_simple(20, 3),
                                colon: 18,
                                comma: 0,
                            }
                        ],
                        result: type_simple(28, 3),
                        body: syn::Expression::Block(
                            &[],
                            &syn::Expression::BinOp(
                                syn::BinaryOperator::Plus,
                                36,
                                &syn::Expression::Var(
                                    syn::VariableIdentifier(range(34, 1))
                                ),
                                &syn::Expression::Var(
                                    syn::VariableIdentifier(range(38, 1))
                                ),
                            ),
                            range(32, 9),
                        ),
                        keyword: 0,
                        open: 0,
                        close: 0,
                        arrow: 0,
                    }
                )
            ),
            Item::Fun(Function {
                prototype: &function_proto,
                body: Value {
                    type_: int,
                    range: range(32, 9),
                    expr: Expr::Block(
                        &[],
                        &Value {
                            type_: int,
                            range: range(34, 5),
                            expr: Expr::Call(
                                Callable::Builtin(BuiltinFunction::Add),
                                &[
                                    arg(value(9, 1), range(34, 1), int),
                                    arg(value(17, 1), range(38, 1), int),
                                ]
                            ),
                        },
                    ),
                }
            })
        );
    }

    #[test]
    fn item_fun_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add() -> (Int, Int) { (1, 2) }",
            &global_arena,
        );

        fn lit_int(offset: usize, length: usize) -> syn::Expression<'static> {
            syn::Expression::Lit(syn::Literal::Integral, range(offset, length))
        }

        fn value_int(value: i64, offset: usize, length: usize)
            -> Value<'static>
        {
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(offset, length),
                expr: Expr::BuiltinVal(BuiltinValue::Int(value)),
            }
        }

        let int = Type::Builtin(BuiltinType::Int);

        let tuple_type = Tuple { fields: &[int, int] };

        let function_proto = FunctionProto {
            name: ItemIdentifier(range(5, 3)),
            range: range(0, 24),
            arguments: &[],
            result: Type::Tuple(tuple_type),
        };

        let prototype = Prototype::Fun(function_proto);

        assert_eq!(
            env.item_of(
                &prototype,
                &syn::Item::Fun(
                    syn::Function {
                        name: syn::VariableIdentifier(range(5, 3)),
                        arguments: &[],
                        result: syn::Type::Tuple(syn::Tuple {
                            fields: &[type_simple(15, 3), type_simple(20, 3)],
                            commas: &[],
                            open: 0,
                            close: 0,
                        }),
                        body: syn::Expression::Block(
                            &[],
                            &syn::Expression::Tuple(syn::Tuple {
                                fields: &[lit_int(28, 1), lit_int(31, 1)],
                                commas: &[],
                                open: 27,
                                close: 32,
                            }),
                            range(25, 10),
                        ),
                        keyword: 0,
                        open: 0,
                        close: 0,
                        arrow: 0,
                    }
                )
            ),
            Item::Fun(Function {
                prototype: &function_proto,
                body: Value {
                    type_: Type::Tuple(Tuple { fields: &[int, int] }),
                    range: range(25, 10),
                    expr: Expr::Block(
                        &[],
                        &Value {
                            type_: Type::Tuple(Tuple { fields: &[int, int] }),
                            range: range(27, 6),
                            expr: Expr::Tuple(Tuple {
                                fields: &[
                                    value_int(1, 28, 1),
                                    value_int(2, 31, 1)
                                ]
                            }),
                        },
                    ),
                }
            })
        );
    }

    struct Env<'g> {
        scope: MockScope<'g>,
        registry: MockRegistry<'g>,
        fragment: &'g [u8],
        arena: &'g mem::Arena,
    }

    impl<'g> Env<'g> {
        fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> Env<'g> {
            Env {
                scope: MockScope::new(fragment, arena),
                registry: MockRegistry::new(arena),
                fragment: fragment,
                arena: arena,
            }
        }

        fn builder<'a, 'local>(&'a self, local: &'local mem::Arena)
            -> super::GraphBuilder<'a, 'g, 'local>
        {
            super::GraphBuilder::new(
                com::CodeFragment::new(self.fragment.to_vec()),
                &self.scope,
                &self.registry, 
                self.arena, 
                local
            )
        }

        fn proto_of(&self, item: &syn::Item) -> Prototype<'g> {
            let mut local_arena = mem::Arena::new();
            let result = self.builder(&local_arena).prototype(item);
            local_arena.recycle();
            result
        }

        fn item_of(&self, proto: &Prototype, item: &syn::Item) -> Item<'g> {
            let mut local_arena = mem::Arena::new();
            let proto = self.arena.intern_ref(proto);
            let result = self.builder(&local_arena).item(proto, item);
            local_arena.recycle();
            result
        }
    }

    fn resolved_argument<'a>(
        value: ValueIdentifier,
        range: com::Range,
        type_: Type<'a>
    )
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: range,
            expr: Expr::ArgumentRef(value),
        }
    }

    fn type_simple(offset: usize, length: usize) -> syn::Type<'static> {
        syn::Type::Simple(syn::TypeIdentifier(range(offset, length)))
    }

    fn value(start: usize, length: usize) -> ValueIdentifier {
        ValueIdentifier(range(start, length))
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
