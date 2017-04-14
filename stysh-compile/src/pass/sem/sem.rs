//! Semantic passes, aka name resolution, type checking, ...
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use basic::{com, mem};

use model::{syn, sem};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'g, 'local> {
    code_fragment: com::CodeFragment,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> GraphBuilder<'g, 'local> {
    /// Creates a new instance of the graph builder.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(
        source: com::CodeFragment,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> GraphBuilder<'g, 'local>
    {
        GraphBuilder {
            code_fragment: source,
            global_arena: global,
            local_arena: local
        }
    }

    /// Extracts the prototype of an item.
    pub fn prototype(&mut self, item: &syn::Item) -> sem::Prototype<'g> {
        match *item {
            syn::Item::Fun(fun) => self.fun_prototype(fun),
        }
    }

    /// Translates a stand-alone expression.
    pub fn expression(&mut self, e: &syn::Expression) -> sem::Value<'g> {
        use super::nmr::NameResolver;

        let scope = ();

        let mut resolver = NameResolver::new(
            &*self.code_fragment,
            &scope,
            self.global_arena,
            self.local_arena
        );

        resolver.value(e)
    }

    /// Translates a full-fledged item.
    pub fn item(&mut self, proto: &'g sem::Prototype<'g>, item: &syn::Item)
        -> sem::Item<'g>
    {
        debug_assert!(
            item.range().offset() == proto.range.offset(),
            "Mismatched item and prototype: {} vs {}",
            item.range(),
            proto.range
        );

        match (*item, &proto.proto) {
            (syn::Item::Fun(i), &sem::Proto::Fun(ref p)) => self.fun_item(i, p),
        }
    }
}

//
//  Implementation Details
//
impl<'g, 'local> GraphBuilder<'g, 'local> {
    fn fun_prototype(&mut self, fun: syn::Function) -> sem::Prototype<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        for a in fun.arguments {
            buffer.push(sem::Binding::Argument(
                sem::ValueIdentifier(a.name.0),
                sem::Type::Builtin(sem::BuiltinType::Int),
                a.range()
            ));
        }

        let arguments = self.global_arena.insert_slice(buffer.into_slice());

        sem::Prototype {
            name: sem::ItemIdentifier(fun.name.0),
            range: com::Range::new(
                fun.keyword as usize,
                fun.result.0.end_offset() - (fun.keyword as usize)
            ),
            proto: sem::Proto::Fun(
                sem::FunctionProto {
                    arguments: arguments,
                    result: sem::Type::Builtin(sem::BuiltinType::Int),
                }
            ),
        }
    }

    fn fun_item(&mut self, fun: syn::Function, p: &'g sem::FunctionProto<'g>)
        -> sem::Item<'g>
    {
        use super::nmr::NameResolver;
        use super::nmr::scp::FunctionScope;

        let scope =
            FunctionScope::new(&*self.code_fragment, p, self.local_arena);

        let mut resolver = NameResolver::new(
            &*self.code_fragment,
            &scope,
            self.global_arena,
            self.local_arena
        );

        sem::Item::Fun(sem::Function {
            prototype: p,
            body: resolver.value(&fun.body)
        })
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

    #[test]
    fn prototype_fun() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        assert_eq!(
            protoit(
                &global_arena,
                b":fun add(a: Int, b: Int) -> Int { 1 + 2 }",
                &syn::Item::Fun(
                    syn::Function {
                        name: syn::VariableIdentifier(range(5, 3)),
                        arguments: &[
                            syn::Argument {
                                name: syn::VariableIdentifier(range(9, 1)),
                                type_: syn::TypeIdentifier(range(12, 3)),
                                colon: 10,
                                comma: 15,
                            },
                            syn::Argument {
                                name: syn::VariableIdentifier(range(17, 1)),
                                type_: syn::TypeIdentifier(range(20, 3)),
                                colon: 18,
                                comma: 0,
                            }
                        ],
                        result: syn::TypeIdentifier(range(28, 3)),
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
            Prototype {
                name: ItemIdentifier(range(5, 3)),
                range: range(0, 31),
                proto: Proto::Fun(
                    FunctionProto {
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
                ),
            }
        );
    }

    #[test]
    fn item_fun() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        let function_proto = FunctionProto {
            arguments: &[
                Binding::Argument(value(9, 1), int, range(9, 7)),
                Binding::Argument(value(17, 1), int, range(17, 6)),
            ],
            result: int,
        };

        let prototype = Prototype {
            name: ItemIdentifier(range(5, 3)),
            range: range(0, 31),
            proto: Proto::Fun(function_proto),
        };

        let arg = resolved_argument;

        assert_eq!(
            itemit(
                &global_arena,
                b":fun add(a: Int, b: Int) -> Int { a + b }",
                &prototype,
                &syn::Item::Fun(
                    syn::Function {
                        name: syn::VariableIdentifier(range(5, 3)),
                        arguments: &[
                            syn::Argument {
                                name: syn::VariableIdentifier(range(9, 1)),
                                type_: syn::TypeIdentifier(range(12, 3)),
                                colon: 10,
                                comma: 15,
                            },
                            syn::Argument {
                                name: syn::VariableIdentifier(range(17, 1)),
                                type_: syn::TypeIdentifier(range(20, 3)),
                                colon: 18,
                                comma: 0,
                            }
                        ],
                        result: syn::TypeIdentifier(range(28, 3)),
                        body: syn::Expression::Block(
                            &[],
                            &syn::Expression::BinOp(
                                syn::BinaryOperator::Plus,
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
                        &Expr::BuiltinCall(
                            BuiltinFunction::Add,
                            &[
                                arg(value(9, 1), range(34, 1), int),
                                arg(value(17, 1), range(38, 1), int),
                            ]
                        ),
                    ),
                }
            })
        );
    }

    fn protoit<'g>(
        global_arena: &'g mem::Arena,
        fragment: &[u8],
        item: &syn::Item
    )
        -> Prototype<'g>
    {
        use super::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let fragment = com::CodeFragment::new(fragment.to_vec());

        let result =
            GraphBuilder::new(fragment, global_arena, &local_arena)
                .prototype(item);
        local_arena.recycle();

        result
    }

    fn itemit<'g>(
        global_arena: &'g mem::Arena,
        fragment: &[u8],
        proto: &'g Prototype,
        item: &syn::Item
    )
        -> Item<'g>
    {
        use super::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let fragment = com::CodeFragment::new(fragment.to_vec());

        let result =
            GraphBuilder::new(fragment, global_arena, &local_arena)
                .item(proto, item);
        local_arena.recycle();

        result
    }

    fn resolved_argument(value: ValueIdentifier, range: com::Range, type_: Type)
        -> Value<'static>
    {
        Value {
            type_: type_,
            range: range,
            expr: Expr::ArgumentRef(value),
        }
    }

    fn value(start: usize, length: usize) -> ValueIdentifier {
        ValueIdentifier(range(start, length))
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
