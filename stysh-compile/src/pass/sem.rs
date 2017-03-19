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

    /// Translates an expresion into a value.
    pub fn value(&mut self, e: &syn::Expression) -> sem::Value<'g> {
        self.value_of_expr(e)
    }
}

//
//  Implementation Details
//
impl<'g, 'local> GraphBuilder<'g, 'local> {
    fn fun_prototype(&mut self, fun: syn::Function) -> sem::Prototype<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        for a in fun.arguments {
            buffer.push(sem::Value {
                type_: sem::Type::Builtin(sem::BuiltinType::Int),
                range: a.range(),
                expr: sem::Expr::Binding(a.name.0),
            })
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

    fn value_of_expr(&mut self, expr: &syn::Expression)
        -> sem::Value<'g>
    {
        use model::syn::Expression;

        match *expr {
            Expression::BinOp(op, left, right) =>
                self.value_of_binary_operator(op, left, right),
            Expression::Lit(lit, range) => self.value_of_literal(lit, range),
            _ => unimplemented!(),
        }
    }

    fn value_of_binary_operator(
        &mut self,
        op: syn::BinaryOperator,
        left: &syn::Expression,
        right: &syn::Expression
    )
        -> sem::Value<'g>
    {
        let range = left.range().extend(right.range());

        let left = self.value_of_expr(left);
        let right = self.value_of_expr(right);

        let op = match op {
            syn::BinaryOperator::Plus => sem::BuiltinFunction::Add,
        };

        let mut buffer = mem::Array::with_capacity(2, self.local_arena);
        buffer.push(left);
        buffer.push(right);

        let arguments = self.global_arena.insert_slice(buffer.into_slice());

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range,
            expr: sem::Expr::BuiltinCall(op, arguments),
        }
    }

    fn value_of_literal(&mut self, lit: syn::Literal, range: com::Range)
        -> sem::Value<'g>
    {
        match lit {
            syn::Literal::Integral => self.value_of_literal_integral(range),
        }
    }

    fn value_of_literal_integral(&mut self, range: com::Range)
        -> sem::Value<'g>
    {
        let mut value = 0;
        for byte in self.source(range) {
            match *byte {
                b'0'...b'9' => value += (byte - b'0') as i64,
                b'_' => (),
                _ => unimplemented!(),
            }
        }

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range,
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(value)),
        }
    }

    fn source(&self, range: com::Range) -> &[u8] {
        &self.code_fragment[range]
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

        let fragment =
            com::CodeFragment::new(
                b":fun add(a: Int, b: Int) -> Int { 1 + 2 }".to_vec()
            );

        assert_eq!(
            protoit(
                &global_arena,
                fragment,
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
                            Value {
                                type_: Type::Builtin(BuiltinType::Int),
                                range: range(9, 7),
                                expr: Expr::Binding(range(9, 1)),
                            },
                            Value {
                                type_: Type::Builtin(BuiltinType::Int),
                                range: range(17, 6),
                                expr: Expr::Binding(range(17, 1)),
                            }
                        ],
                        result: Type::Builtin(BuiltinType::Int),
                    }
                ),
            }
        );
    }

    #[test]
    fn value_basic_add() {
        let global_arena = mem::Arena::new();

        let fragment = com::CodeFragment::new(b"1 + 2".to_vec());

        let (left_range, right_range) = (range(0, 1), range(4, 1));

        let left_hand = lit_integral(left_range);
        let right_hand = lit_integral(right_range);

        let expr = syn::Expression::BinOp(
            syn::BinaryOperator::Plus,
            &left_hand,
            &right_hand,
        );

        assert_eq!(
            valueit(&global_arena, fragment, &expr),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(0, 5),
                expr: Expr::BuiltinCall(
                    BuiltinFunction::Add,
                    &[ int(1, left_range), int(2, right_range) ],
                )
            }
        );
    }

    fn protoit<'g>(
        global_arena: &'g mem::Arena,
        fragment: com::CodeFragment,
        item: &syn::Item
    )
        -> Prototype<'g>
    {
        use super::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(fragment, global_arena, &local_arena)
                .prototype(item);
        local_arena.recycle();

        result
    }

    fn valueit<'g>(
        global_arena: &'g mem::Arena,
        fragment: com::CodeFragment,
        expr: &syn::Expression
    )
        -> Value<'g>
    {
        use super::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(fragment, global_arena, &local_arena)
                .value(expr);
        local_arena.recycle();

        result
    }

    fn lit_integral(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Lit(syn::Literal::Integral, range)
    }

    fn int(value: i64, range: com::Range) -> Value<'static> {
        Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: range,
            expr: Expr::BuiltinVal(BuiltinValue::Int(value)),
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
