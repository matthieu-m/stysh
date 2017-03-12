//! The interpreter.
//!
//! This module defines the entry point of the interpreter.

use basic::mem;
use model::{sem, sir};

/// Stysh Interpreter.
///
/// Interprets a SIR control flow graph based on a type dictionary, producing
/// either a value, or an error if the interpretation cannot succeed (missing
/// definitions, FFI call, ...).
pub struct Interpreter<'g, 'local> {
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> Interpreter<'g, 'local> {
    /// Creates a new instance of an interpreter.
    pub fn new(global: &'g mem::Arena, local: &'local mem::Arena)
        -> Interpreter<'g, 'local>
    {
        Interpreter { global_arena: global, local_arena: local }
    }

    /// Returns the value evaluated from the SIR.
    pub fn evaluate(&self, cfg: &'local sir::ControlFlowGraph)
        -> sem::Value<'g>
    {
        let frame = FrameInterpreter::new(self.local_arena);
        self.duplicate(frame.evaluate(cfg))
    }
}

//
//  Implementation Details
//
impl<'g, 'local> Interpreter<'g, 'local> {
    fn duplicate(&self, value: sem::Value<'local>) -> sem::Value<'g> {
        match value.expr {
            sem::Expr::BuiltinVal(v) => sem::Value {
                type_: value.type_,
                range: value.range,
                expr: sem::Expr::BuiltinVal(v),
            },
            _ => unimplemented!(),
        }
    }
}

struct FrameInterpreter<'a> {
    arena: &'a mem::Arena,
}

impl<'a> FrameInterpreter<'a> {
    fn new(arena: &'a mem::Arena) -> FrameInterpreter<'a> {
        FrameInterpreter { arena: arena }
    }

    fn evaluate(&self, cfg: &'a sir::ControlFlowGraph<'a>) -> sem::Value<'a> {
        let mut interpreter = BlockInterpreter::new(self.arena);

        match interpreter.evaluate(&cfg.blocks[0]) {
            BlockResult::Return(v) => sem::Value {
                type_: sem::Type::Builtin(sem::BuiltinType::Int),
                range: cfg.range(),
                expr: sem::Expr::BuiltinVal(v),
            }
        }
    }
}

struct BlockInterpreter<'a> {
    arena: &'a mem::Arena,
    bindings: mem::Array<'a, sem::BuiltinValue>,
}

impl<'a> BlockInterpreter<'a> {
    fn new(arena: &'a mem::Arena) -> BlockInterpreter<'a> {
        BlockInterpreter { arena: arena, bindings: mem::Array::new(arena) }
    }

    fn evaluate(&mut self, block: &'a sir::BasicBlock<'a>) -> BlockResult {
        use model::sir::TerminatorInstruction::*;

        for i in block.instructions {
            let value = self.eval_instr(i);
            self.bindings.push(value);
        }

        match block.exit {
            Return(index) => BlockResult::Return(self.get_value(index)),
            Unreachable => unimplemented!(),
        }
    }

    fn eval_instr(&self, instr: &'a sir::Instruction<'a>) -> sem::BuiltinValue {
        use model::sir::Instruction::*;

        match *instr {
            CallFunction(fun, args, _) => self.eval_fun(fun, args),
            Load(value, _) => value,
        }
    }

    fn eval_fun(&self, fun: sem::BuiltinFunction, args: &'a [sir::ValueId])
        -> sem::BuiltinValue
    {
        match fun {
            sem::BuiltinFunction::Add => self.eval_binary_fun(fun, args),
        }
    }

    fn eval_binary_fun(
        &self,
        fun: sem::BuiltinFunction,
        args: &'a [sir::ValueId]
    )
        -> sem::BuiltinValue
    {
        assert_eq!(args.len(), 2);

        let left = self.get_integral(args[0]);
        let right = self.get_integral(args[1]);

        match fun {
            sem::BuiltinFunction::Add => sem::BuiltinValue::Int(left + right),
        }
    }

    fn get_value(&self, id: sir::ValueId) -> sem::BuiltinValue {
        let instr = id.as_instruction()
            .expect("Arguments support not implemented");
        self.bindings[instr]
    }

    fn get_integral(&self, id: sir::ValueId) -> i64 {
        match self.get_value(id) {
            sem::BuiltinValue::Int(i) => i,
        }
    }
}

enum BlockResult {
    Return(sem::BuiltinValue),
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::{sem, sir};

    #[test]
    fn add() {
        let global_arena = mem::Arena::new();

        let arguments = [ val_instr(0), val_instr(1) ];
        let instructions = [
            instr_load_int(1),
            instr_load_int(2),
            instr_call(sem::BuiltinFunction::Add, &arguments),
        ];
        let blocks = [ block_return(&instructions) ];
        let cfg = sir::ControlFlowGraph { blocks: &blocks };

        assert_eq!(
            eval(&global_arena, &cfg),
            sem::Value {
                type_: sem::Type::Builtin(sem::BuiltinType::Int),
                range: com::Range::new(0, 0),
                expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(3)),
            }
        );
    }

    fn eval<'g>(global_arena: &'g mem::Arena, cfg: &sir::ControlFlowGraph)
        -> sem::Value<'g>
    {
        use super::Interpreter;

        let mut local_arena = mem::Arena::new();

        let result = Interpreter::new(global_arena, &local_arena).evaluate(cfg);
        local_arena.recycle();

        result
    }

    fn val_instr(index: usize) -> sir::ValueId {
        sir::ValueId::new_instruction(index)
    }

    fn instr_call<'a>(fun: sem::BuiltinFunction, args: &'a [sir::ValueId])
        -> sir::Instruction<'a>
    {
        sir::Instruction::CallFunction(fun, args, com::Range::new(0, 0))
    }

    fn instr_load_int(i: i64) -> sir::Instruction<'static> {
        sir::Instruction::Load(sem::BuiltinValue::Int(i), com::Range::new(0, 0))
    }

    fn block_return<'a>(code: &'a [sir::Instruction<'a>])
        -> sir::BasicBlock<'a>
    {
        sir::BasicBlock {
            instructions: code,
            exit: sir::TerminatorInstruction::Return(val_instr(code.len() - 1)),
        }
    }
}
