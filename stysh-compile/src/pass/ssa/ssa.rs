//! Control Flow passes, aka building the Control Flow Graph.
//!
//! This module is in charge of transforming the Abstract Semantic Graph (often
//! confusingly dubbed AST) into a CFG in a variant of the SSA form.

use basic::com;
use basic::mem::{self, CloneInto};
use model::{sem, sir};

/// Stysh CFG builder.
///
/// Builds the Control-Flow Graph.
pub struct GraphBuilder<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> GraphBuilder<'g, 'local>
    where 'g: 'local
{
    /// Creates a new instance of a GraphBuilder.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(global: &'g mem::Arena, local: &'local mem::Arena)
        -> GraphBuilder<'g, 'local>
    {
        GraphBuilder { global_arena: global, local_arena: local }
    }

    /// Translates a semantic expression into its control-flow graph.
    pub fn from_value(&self, expr: &sem::Value<'g>)
        -> sir::ControlFlowGraph<'g>
    {
        let mut imp =
            GraphBuilderImpl::new(self.global_arena, self.local_arena, &[]);

        imp.from_value(expr);

        sir::ControlFlowGraph {
            blocks: self.global_arena.insert_slice(imp.blocks.into_slice())
        }
    }

    /// Translates a semantic function into its control-flow graph.
    pub fn from_function(&self, fun: &sem::Function<'g>)
        -> sir::ControlFlowGraph<'g>
    {
        let mut bindings =
            mem::Array::with_capacity(
                fun.prototype.arguments.len(),
                self.local_arena
            );

        for &a in fun.prototype.arguments {
            if let sem::Binding::Argument(value, type_, _) = a {
                bindings.push((value.0, type_));
                continue;
            }
            panic!("All arguments should be of type Binding::Argument");
        }

        let mut imp =
            GraphBuilderImpl::new(
                self.global_arena,
                self.local_arena,
                bindings.into_slice()
            );

        imp.from_value(&fun.body);

        sir::ControlFlowGraph {
            blocks: self.global_arena.insert_slice(imp.blocks.into_slice())
        }
    }
}

//
//  Implementation Details
//
struct GraphBuilderImpl<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
    arguments: &'local [(com::Range, sem::Type<'local>)],
    blocks: mem::Array<'local, sir::BasicBlock<'g>>,
}

struct BlockBuilderImpl<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
    arguments: &'local [(com::Range, sem::Type<'local>)],
    variables: mem::Array<'local, (com::Range, sir::ValueId)>,
    instrs: mem::Array<'local, sir::Instruction<'g>>,
}

impl<'g, 'local> GraphBuilderImpl<'g, 'local>
    where 'g: 'local
{
    fn new(
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
        arguments: &'local [(com::Range, sem::Type)],
    )
        -> GraphBuilderImpl<'g, 'local>
    {
        GraphBuilderImpl {
            global_arena: global_arena,
            local_arena: local_arena,
            arguments: arguments,
            blocks: mem::Array::new(local_arena),
        }
    }

    fn from_value(&mut self, value: &sem::Value<'g>) {
        let mut imp =
            BlockBuilderImpl::new(
                self.global_arena,
                self.local_arena,
                self.arguments
            );

        imp.from_value(value);

        let return_value = sir::ValueId::new_instruction(imp.instrs.len() - 1);

        let mut arguments =
            mem::Array::with_capacity(self.arguments.len(), self.global_arena);

        for &(_, type_) in self.arguments {
            arguments.push(type_.clone_into(self.global_arena));
        }

        self.blocks.push(sir::BasicBlock {
            arguments: arguments.into_slice(),
            instructions: self.global_arena.insert_slice(&imp.instrs),
            exit: sir::TerminatorInstruction::Return(return_value),
        });
    }
}

impl<'g, 'local> BlockBuilderImpl<'g, 'local>
    where 'g: 'local
{
    fn new(
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
        arguments: &'local [(com::Range, sem::Type)]
    )
        -> BlockBuilderImpl<'g, 'local>
    {
        BlockBuilderImpl {
            global_arena: global_arena,
            local_arena: local_arena,
            arguments: arguments,
            variables: mem::Array::new(local_arena),
            instrs: mem::Array::new(local_arena),
        }
    }

    fn from_value(&mut self, value: &sem::Value<'g>) -> sir::ValueId {
        let index = self.instrs.len();

        match value.expr {
            sem::Expr::ArgumentRef(id) => {
                for (index, &arg) in self.arguments.iter().enumerate() {
                    if arg.0 == id.0 {
                        return sir::ValueId::new_argument(index);
                    }
                }
                panic!("Unresolved argument: {}", id.0);
            },
            sem::Expr::Block(stmts, v) => {
                for &s in stmts {
                    match s {
                        sem::Stmt::Var(sem::Binding::Variable(var, value, _))
                            =>
                        {
                            let id = self.from_value(&value);
                            self.variables.push((var.0, id));
                        },
                        _ => unimplemented!(),
                    }
                }
                self.from_value(&sem::Value {
                    type_: value.type_, 
                    range: value.range,
                    expr: v.expr
                });
            },
            sem::Expr::BuiltinCall(fun, args) => {
                let mut arguments = mem::Array::new(self.local_arena);
                for a in args {
                    arguments.push(self.from_value(a));
                }
                let arguments = self.global_arena.insert_slice(&arguments);
                self.instrs.push(
                    sir::Instruction::CallFunction(fun, arguments, value.range)
                );
            },
            sem::Expr::BuiltinVal(val) => {
                self.instrs.push(sir::Instruction::Load(val, value.range))
            },
            sem::Expr::Tuple(tuple) => {
                let mut arguments = mem::Array::with_capacity(
                    tuple.fields.len(),
                    self.global_arena
                );
                for a in tuple.fields {
                    arguments.push(self.from_value(a));
                }
                self.instrs.push(
                    sir::Instruction::New(
                        value.type_,
                        arguments.into_slice(),
                        value.range
                    )
                );
            }
            sem::Expr::VariableRef(id) => {
                for &(range, value_id) in &self.variables {
                    if range == id.0 {
                        return value_id;
                    }
                }
                panic!("Unresolved variable: {}", id.0);
            }
            _ => unimplemented!(),
        };

        sir::ValueId::new_instruction(index)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::sem;
    use model::sir::*;


    #[test]
    fn value_simple() {
        let global_arena = mem::Arena::new();

        let (left, right) = (lit_integral(1, 0, 1), lit_integral(2, 4, 1));
        let arguments = &[left, right];
        let expr_range = range(0, 5);

        let val = sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: expr_range,
            expr: sem::Expr::BuiltinCall(
                sem::BuiltinFunction::Add,
                arguments,
            )
        };

        assert_eq!(
            valueit(&global_arena, &val).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := add($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn tuple_simple() {
        let global_arena = mem::Arena::new();

        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        assert_eq!(
            valueit(
                &global_arena,
                &sem::Value {
                    type_: sem::Type::Tuple(sem::Tuple { fields: &[int, int] }),
                    range: range(0, 5),
                    expr: sem::Expr::Tuple(sem::Tuple {
                        fields: &[lit_integral(1, 0, 1), lit_integral(2, 4, 1)]
                    }),
                }
            ).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := new (Int, Int) ($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn block_simple() {
        let global_arena = mem::Arena::new();
        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        //  { :var a := 1; :var b := 2; a + b }
        let (a, b) = (value(7, 1), value(20, 1));

        assert_eq!(
            valueit(
                &global_arena,
                &sem::Value {
                    type_: sem::Type::Builtin(sem::BuiltinType::Int),
                    range: range(0, 35),
                    expr: sem::Expr::Block(
                        &[
                            sem::Stmt::Var(sem::Binding::Variable(
                                a,
                                lit_integral(1, 12, 1),
                                range(2, 12)
                            )),
                            sem::Stmt::Var(sem::Binding::Variable(
                                b,
                                lit_integral(2, 25, 1),
                                range(15, 12)
                            )),
                        ],
                        &sem::Value {
                            type_: int,
                            range: range(28, 5),
                            expr: sem::Expr::BuiltinCall(
                                sem::BuiltinFunction::Add,
                                &[
                                    resolved_variable(a, int),
                                    resolved_variable(b, int),
                                ]
                            ),
                        },
                    )
                }
            ).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@12",
                "    $1 := load 2 ; 1@25",
                "    $2 := add($0, $1) ; 35@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn fun_simple() {
        let global_arena = mem::Arena::new();
        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        let (first, second) = (value(9, 1), value(17, 1));

        assert_eq!(
            funit(
                &global_arena,
                &sem::Function {
                    prototype: &sem::FunctionProto {
                        arguments: &[
                            argument(first, int),
                            argument(second, int),
                        ],
                        result: int,
                    },
                    body: sem::Value {
                        type_: int,
                        range: range(34, 5),
                        expr: sem::Expr::BuiltinCall(
                            sem::BuiltinFunction::Add,
                            &[
                                resolved_argument(first, int),
                                resolved_argument(second, int),
                            ]
                        ),
                    }
                }
            ).to_string(),
            cat(&[
                "0:",
                "    $0 := add(@0, @1) ; 5@34",
                "    return $0",
                ""
            ])
        );
    }

    fn valueit<'g>(global_arena: &'g mem::Arena, expr: &sem::Value<'g>)
        -> ControlFlowGraph<'g>
    {
        use pass::ssa::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(global_arena, &local_arena)
                .from_value(expr);
        local_arena.recycle();

        result
    }

    fn funit<'g>(global_arena: &'g mem::Arena, fun: &sem::Function<'g>)
        -> ControlFlowGraph<'g>
    {
        use pass::ssa::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(global_arena, &local_arena)
                .from_function(fun);
        local_arena.recycle();

        result
    }

    fn argument<'a>(value: sem::ValueIdentifier, type_: sem::Type<'a>)
        -> sem::Binding<'a>
    {
        sem::Binding::Argument(value, type_, range(0, 0))
    }

    fn lit_integral(value: i64, offset: usize, length: usize)
        -> sem::Value<'static>
    {
        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range(offset, length),
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(value)),
        }
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn resolved_argument<'a>(value: sem::ValueIdentifier, type_: sem::Type<'a>)
        -> sem::Value<'a>
    {
        sem::Value {
            type_: type_,
            range: range(0, 0),
            expr: sem::Expr::ArgumentRef(value),
        }
    }

    fn resolved_variable<'a>(value: sem::ValueIdentifier, type_: sem::Type<'a>)
        -> sem::Value<'a>
    {
        sem::Value {
            type_: type_,
            range: range(0, 0),
            expr: sem::Expr::VariableRef(value),
        }
    }

    fn value(start: usize, length: usize) -> sem::ValueIdentifier {
        sem::ValueIdentifier(range(start, length))
    }

    fn cat(lines: &[&str]) -> String {
        let mut result = String::from("");
        for i in lines.iter() {
            result.push_str(i);
            result.push_str("\n");
        }
        result
    }
}
