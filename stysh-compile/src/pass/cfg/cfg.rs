//! Control Flow passes, aka building the Control Flow Graph.
//!
//! This module is in charge of transforming the Abstract Semantic Graph (often
//! confusingly dubbed AST) into a CFG in a variant of the SSA form.

use basic::{com, mem};
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
    arguments: &'local [(com::Range, sem::Type)],
    blocks: mem::Array<'local, sir::BasicBlock<'g>>,
}

struct BlockBuilderImpl<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
    arguments: &'local [(com::Range, sem::Type)],
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
            arguments.push(type_);
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
            sem::Expr::BuiltinVal(val) =>
                self.instrs.push(sir::Instruction::Load(val, value.range)),
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
        use pass::cfg::GraphBuilder;

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
        use pass::cfg::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(global_arena, &local_arena)
                .from_function(fun);
        local_arena.recycle();

        result
    }

    fn argument(value: sem::ValueIdentifier, type_: sem::Type)
        -> sem::Binding<'static>
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

    fn resolved_argument(value: sem::ValueIdentifier, type_: sem::Type)
        -> sem::Value<'static>
    {
        sem::Value {
            type_: type_,
            range: range(0, 0),
            expr: sem::Expr::ArgumentRef(value),
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
