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

    /// Translates a semantic expression into a control-flow graph for this
    /// expression.
    pub fn from_expression(&self, expr: &sem::Value<'g>)
        -> sir::ControlFlowGraph<'g>
    {
        let mut imp =
            GraphBuilderImpl::new(self.global_arena, self.local_arena);

        imp.from_expression(expr);

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
    blocks: mem::Array<'local, sir::BasicBlock<'g>>,
}

struct BlockBuilderImpl<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
    instrs: mem::Array<'local, sir::Instruction<'g>>,
}

impl<'g, 'local> GraphBuilderImpl<'g, 'local>
    where 'g: 'local
{
    fn new(global_arena: &'g mem::Arena, local_arena: &'local mem::Arena)
        -> GraphBuilderImpl<'g, 'local>
    {
        GraphBuilderImpl {
            global_arena: global_arena,
            local_arena: local_arena,
            blocks: mem::Array::new(local_arena),
        }
    }

    fn from_expression(&mut self, expr: &sem::Value<'g>) {
        let mut imp =
            BlockBuilderImpl::new(self.global_arena, self.local_arena);

        imp.from_expression(expr);

        let return_value = sir::ValueId::new_instruction(imp.instrs.len() - 1);

        self.blocks.push(sir::BasicBlock {
            instructions: self.global_arena.insert_slice(&imp.instrs),
            exit: sir::TerminatorInstruction::Return(return_value),
        });
    }
}

impl<'g, 'local> BlockBuilderImpl<'g, 'local>
    where 'g: 'local
{
    fn new(global_arena: &'g mem::Arena, local_arena: &'local mem::Arena)
        -> BlockBuilderImpl<'g, 'local>
    {
        BlockBuilderImpl {
            global_arena: global_arena,
            local_arena: local_arena,
            instrs: mem::Array::new(local_arena),
        }
    }

    fn from_expression(&mut self, expr: &sem::Value<'g>) -> sir::ValueId {
        let index = self.instrs.len();
        match *expr {
            sem::Value::BuiltinCall(fun, args, r) => {
                let mut arguments = mem::Array::new(self.local_arena);
                for a in args {
                    arguments.push(self.from_expression(a));
                }
                let arguments = self.global_arena.insert_slice(&arguments);
                self.instrs.push(
                    sir::Instruction::CallFunction(fun, arguments, r)
                );
            },
            sem::Value::BuiltinVal(val, r) =>
                self.instrs.push(sir::Instruction::Load(val, r)),
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
    fn expr_simple() {
        let global_arena = mem::Arena::new();

        let (left, right) = (lit_integral(1, 0, 1), lit_integral(2, 4, 1));
        let arguments = &[left, right];
        let expr_range = range(0, 5);

        let expr = sem::Value::BuiltinCall(
            sem::BuiltinFunction::Add,
            arguments,
            expr_range
        );

        assert_eq!(
            cfg_expr(&global_arena, &expr).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := add($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        )
    }

    fn cfg_expr<'g>(global_arena: &'g mem::Arena, expr: &sem::Value<'g>)
        -> ControlFlowGraph<'g>
    {
        use pass::cfg::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(global_arena, &local_arena)
                .from_expression(expr);
        local_arena.recycle();

        result
    }

    fn lit_integral(value: i64, offset: usize, length: usize)
        -> sem::Value<'static>
    {
        sem::Value::BuiltinVal(
            sem::BuiltinValue::Int(value),
            range(offset, length)
        )
    }

    fn instr_id(id: usize) -> ValueId { ValueId::new_instruction(id) }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
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
