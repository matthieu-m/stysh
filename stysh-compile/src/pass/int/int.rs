//! The interpreter.
//!
//! This module defines the entry point of the interpreter.

use basic::{com, mem};
use model::{sem, sir};

/// Stysh Interpreter.
///
/// Interprets a SIR control flow graph based on a type dictionary, producing
/// either a value, or an error if the interpretation cannot succeed (missing
/// definitions, FFI call, ...).
pub struct Interpreter<'g, 'local> {
    #[allow(dead_code)]
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
    pub fn evaluate(
        &self,
        cfg: &'local sir::ControlFlowGraph,
        arguments: &'local [sem::Value<'local>],
    )
        -> sem::Value<'g>
    {
        let frame = FrameInterpreter::new(self.local_arena);
        self.global_arena.intern(&frame.evaluate(cfg, arguments))
    }
}

//
//  Implementation Details
//
struct FrameInterpreter<'a> {
    arena: &'a mem::Arena,
}

impl<'a> FrameInterpreter<'a> {
    fn new(arena: &'a mem::Arena) -> FrameInterpreter<'a> {
        FrameInterpreter { arena: arena }
    }

    fn evaluate(
        &self,
        cfg: &'a sir::ControlFlowGraph<'a>,
        arguments: &'a [sem::Value<'a>]
    )
        -> sem::Value<'a>
    {
        use self::BlockResult::*;

        fn interpret_block<'a>(
            arena: &'a mem::Arena,
            block: &'a sir::BasicBlock<'a>,
            arguments: &'a [sem::Value<'a>],
        )
            -> BlockResult<'a>
        {
            let mut interpreter = BlockInterpreter::new(arena, arguments);
            interpreter.evaluate(block)
        }

        let mut index = 0;
        let mut arguments = arguments;

        //  TODO(matthieum): add way to parameterize fuel.
        for _ in 0..1000 {
            let (i, args) = match interpret_block(
                self.arena,
                &cfg.blocks[index],
                arguments
            ) 
            {
                Jump(index, args) => (index, args),
                Return(v) => return sem::Value {
                    type_: v.type_,
                    range: com::Range::new(0, 0),
                    expr: v.expr,
                },
            };
            index = i.index();
            arguments = args;
        }

        unreachable!()
    }
}

struct BlockInterpreter<'a> {
    arena: &'a mem::Arena,
    arguments: &'a [sem::Value<'a>],
    bindings: mem::Array<'a, sem::Value<'a>>,
}

impl<'a> BlockInterpreter<'a> {
    fn new(arena: &'a mem::Arena, arguments: &'a [sem::Value<'a>])
        -> BlockInterpreter<'a>
    {
        BlockInterpreter {
            arena: arena,
            arguments: arguments,
            bindings: mem::Array::new(arena)
        }
    }

    fn evaluate(&mut self, block: &'a sir::BasicBlock<'a>) -> BlockResult<'a> {
        use model::sir::TerminatorInstruction::*;

        for i in block.instructions {
            let value = self.eval_instr(i);
            self.bindings.push(value);
        }

        match block.exit {
            Branch(index, jumps) => self.jump(&jumps[self.get_branch(index)]),
            Jump(jump) => self.jump(&jump),
            Return(index) => BlockResult::Return(self.get_value(index)),
            _ => unimplemented!(),
        }
    }

    fn eval_instr(&self, instr: &'a sir::Instruction<'a>) -> sem::Value<'a> {
        use model::sir::Instruction::*;

        match *instr {
            Call(fun, args, _) => self.eval_call(fun, args),
            Load(value, range) => self.load(value, range),
            New(type_, fields, range) => self.eval_new(type_, fields, range),
        }
    }

    fn eval_call(&self, fun: sem::Callable<'a>, args: &'a [sir::ValueId])
        -> sem::Value<'a>
    {
        match fun {
            sem::Callable::Builtin(b) => self.eval_builtin(b, args),
            _ => unimplemented!(),
        }
    }

    fn eval_builtin(&self, fun: sem::BuiltinFunction, args: &'a [sir::ValueId])
        -> sem::Value<'a>
    {
        self.eval_binary_fun(fun, args)
    }

    fn eval_binary_fun(
        &self,
        fun: sem::BuiltinFunction,
        args: &'a [sir::ValueId]
    )
        -> sem::Value<'a>
    {
        use model::sem::BuiltinFunction::*;
        use model::sem::BuiltinValue::{Bool, Int};

        assert_eq!(args.len(), 2);

        fn get_builtin<'a>(value: sem::Value<'a>) -> sem::BuiltinValue<'a> {
            match value.expr {
                sem::Expr::BuiltinVal(b) => b,
                _ => unreachable!(),
            }
        }

        fn to_int(value: sem::BuiltinValue) -> i64 {
            use std::convert::Into;
            Into::<i64>::into(value)
        }

        let left = get_builtin(self.get_value(args[0]));
        let right = get_builtin(self.get_value(args[1]));

        let value = match fun {
            Add => Int(to_int(left) + to_int(right)),
            Differ => Bool(left != right),
            Equal => Bool(left == right),
            FloorDivide => Int(to_int(left) / to_int(right)),
            GreaterThan => Bool(left > right),
            GreaterThanOrEqual => Bool(left >= right),
            LessThan => Bool(left < right),
            LessThanOrEqual => Bool(left <= right),
            Multiply => Int(to_int(left) * to_int(right)),
            Substract => Int(to_int(left) - to_int(right)),
        };

        sem::Value {
            type_: fun.result_type(),
            range: com::Range::new(0, 0),
            expr: sem::Expr::BuiltinVal(value),
        }
    }

    fn eval_new(
        &self,
        type_: sem::Type<'a>,
        fields: &[sir::ValueId],
        range: com::Range
    )
        -> sem::Value<'a>
    {
        let mut elements = mem::Array::with_capacity(fields.len(), self.arena);

        for id in fields {
            elements.push(self.get_value(*id))
        }

        sem::Value {
            type_: type_,
            range: range,
            expr: sem::Expr::Tuple(sem::Tuple{ fields: elements.into_slice() }),
        }
    }

    fn load(&self, v: sem::BuiltinValue, range: com::Range) -> sem::Value<'a> {
        let type_ = match v {
            sem::BuiltinValue::Bool(_) => sem::BuiltinType::Bool,
            sem::BuiltinValue::Int(_) => sem::BuiltinType::Int,
            sem::BuiltinValue::String(_) => sem::BuiltinType::String,
        };

        let value = sem::Value {
            type_: sem::Type::Builtin(type_),
            range: range,
            expr: sem::Expr::BuiltinVal(v),
        };

        self.arena.intern(&value)
    }

    fn get_value(&self, id: sir::ValueId) -> sem::Value<'a> {
        if let Some(i) = id.as_instruction() {
            self.bindings[i]
        } else {
            self.arguments[id.as_argument().expect("Either instr or arg!")]
        }
    }

    fn get_branch(&self, index: sir::ValueId) -> usize {
        match self.get_value(index).expr {
            sem::Expr::BuiltinVal(sem::BuiltinValue::Int(i)) => i as usize,
            sem::Expr::BuiltinVal(sem::BuiltinValue::Bool(cond))
                => if cond { 0 } else { 1 },
            _ => unreachable!(),
        }
    }

    fn jump(&self, jump: &sir::Jump) -> BlockResult<'a> {
        let mut arguments =
            mem::Array::with_capacity(jump.arguments.len(), self.arena);

        for &a in jump.arguments {
            arguments.push(self.get_value(a));
        }

        BlockResult::Jump(jump.dest, arguments.into_slice())
    }
}

enum BlockResult<'a> {
    Jump(sir::BlockId, &'a [sem::Value<'a>]),
    Return(sem::Value<'a>),
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::{sem, sir};

    #[test]
    fn add_no_arguments() {
        let global_arena = mem::Arena::new();

        let arguments = [ val_instr(0), val_instr(1) ];
        let instructions = [
            instr_load_int(1),
            instr_load_int(2),
            instr_builtin(sem::BuiltinFunction::Add, &arguments),
        ];
        let blocks = [ block_return(&[], &instructions) ];
        let cfg = sir::ControlFlowGraph { blocks: &blocks };

        assert_eq!(
            eval(&global_arena, &[], &cfg),
            sem_int(3)
        );
    }

    #[test]
    fn add_with_arguments() {
        let global_arena = mem::Arena::new();
        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        assert_eq!(
            eval(
                &global_arena,
                &[sem_int(1), sem_int(2)],
                &sir::ControlFlowGraph {
                    blocks: &[block_return(
                        &[int, int],
                        &[
                            instr_builtin(
                                sem::BuiltinFunction::Add,
                                &[ val_arg(0), val_arg(1) ]
                            ),
                        ]
                    )]
                }
            ),
            sem_int(3)
        );
    }

    #[test]
    fn branch_if() {
        use model::sir::TerminatorInstruction::*;

        let global_arena = mem::Arena::new();
        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        for &(condition, result) in &[(true, sem_int(1)), (false, sem_int(2))] {
            assert_eq!(
                eval(
                    &global_arena,
                    &[],
                    &sir::ControlFlowGraph {
                        blocks: &[
                            sir::BasicBlock {
                                arguments: &[],
                                instructions: &[instr_load_bool(condition)],
                                exit: Branch(val_instr(0), &[
                                    exit_jump(1, &[]),
                                    exit_jump(2, &[]),
                                ]),
                            },
                            sir::BasicBlock {
                                arguments: &[],
                                instructions: &[instr_load_int(1)],
                                exit: Jump(exit_jump(3, &[val_instr(0)])),
                            },
                            sir::BasicBlock {
                                arguments: &[],
                                instructions: &[instr_load_int(2)],
                                exit: Jump(exit_jump(3, &[val_instr(0)])),
                            },
                            block_return(&[int], &[]),
                        ]
                    }
                ),
                result
            )
        }
    }

    #[test]
    fn new_tuple() {
        let global_arena = mem::Arena::new();

        let int = sem::Type::Builtin(sem::BuiltinType::Int);
        let inner_type = [int, int];
        let type_ = sem::Type::Tuple(sem::Tuple { fields: &inner_type });

        assert_eq!(
            eval(
                &global_arena,
                &[],
                &sir::ControlFlowGraph {
                    blocks: &[block_return(
                        &[],
                        &[
                            instr_load_int(1),
                            instr_load_int(2),
                            instr_new(
                                type_,
                                &[val_instr(0), val_instr(1)]
                            ),
                        ]
                    )]
                }
            ),
            sem::Value {
                type_: type_,
                range: range(0, 0),
                expr: sem::Expr::Tuple(sem::Tuple {
                    fields: &[sem_int(1), sem_int(2)],
                })
            }
        );
    }

    #[test]
    fn return_helloworld() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            eval(
                &global_arena,
                &[],
                &sir::ControlFlowGraph {
                    blocks: &[
                        block_return(
                            &[],
                            &[
                                instr_load_string(b"Hello, World!")
                            ]
                        ),
                    ]
                }
            ),
            sem_string(b"Hello, World!")
        );
    }

    fn eval<'g>(
        global_arena: &'g mem::Arena,
        arguments: &[sem::Value],
        cfg: &sir::ControlFlowGraph
    )
        -> sem::Value<'g>
    {
        use super::Interpreter;

        let mut local_arena = mem::Arena::new();

        let result =
            Interpreter::new(global_arena, &local_arena)
                .evaluate(cfg, arguments);
        local_arena.recycle();

        result
    }

    fn sem_int(i: i64) -> sem::Value<'static> {
        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range(0, 0),
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(i)),
        }
    }

    fn sem_string<'a>(s: &'a [u8]) -> sem::Value<'a> {
        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::String),
            range: range(0, 0),
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::String(s)),
        }
    }

    fn val_arg(index: usize) -> sir::ValueId {
        sir::ValueId::new_argument(index)
    }

    fn val_instr(index: usize) -> sir::ValueId {
        sir::ValueId::new_instruction(index)
    }

    fn instr_builtin<'a>(fun: sem::BuiltinFunction, args: &'a [sir::ValueId])
        -> sir::Instruction<'a>
    {
        sir::Instruction::Call(sem::Callable::Builtin(fun), args, range(0, 0))
    }

    fn instr_load_bool(value: bool) -> sir::Instruction<'static> {
        sir::Instruction::Load(sem::BuiltinValue::Bool(value), range(0, 0))
    }

    fn instr_load_int(i: i64) -> sir::Instruction<'static> {
        sir::Instruction::Load(sem::BuiltinValue::Int(i), range(0, 0))
    }

    fn instr_load_string<'a>(s: &'a [u8]) -> sir::Instruction<'a> {
        sir::Instruction::Load(sem::BuiltinValue::String(s), range(0, 0))
    }

    fn instr_new<'a>(type_: sem::Type<'a>, values: &'a [sir::ValueId])
        -> sir::Instruction<'a>
    {
        sir::Instruction::New(type_, values, range(0, 0))
    }

    fn exit_jump<'a>(index: usize, args: &'a [sir::ValueId]) -> sir::Jump<'a> {
        sir::Jump {
            dest: sir::BlockId::new(index),
            arguments: args,
        }
    }

    fn block_return<'a>(
        args: &'a [sem::Type],
        code: &'a [sir::Instruction<'a>]
    )
        -> sir::BasicBlock<'a>
    {
        let result = if code.len() > 0 {
            val_instr(code.len() - 1)
        } else {
            val_arg(0)
        };

        sir::BasicBlock {
            arguments: args,
            instructions: code,
            exit: sir::TerminatorInstruction::Return(result),
        }
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
