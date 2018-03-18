//! The interpreter.
//!
//! This module defines the entry point of the interpreter.

use basic::{com, mem};
use model::{hir, sir};
use super::reg::Registry;

/// Stysh Interpreter.
///
/// Interprets a SIR control flow graph based on a type dictionary, producing
/// either a value, or an error if the interpretation cannot succeed (missing
/// definitions, FFI call, ...).
pub struct Interpreter<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    registry: &'a Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> Interpreter<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    /// Creates a new instance of an interpreter.
    pub fn new(
        registry: &'a Registry<'g>,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> Interpreter<'a, 'g, 'local>
    {
        Interpreter {
            registry: registry,
            global_arena: global,
            local_arena: local
        }
    }

    /// Returns the value evaluated from the SIR.
    pub fn evaluate(
        &self,
        cfg: &sir::ControlFlowGraph<'g>,
        arguments: &'local [hir::Value<'local>],
    )
        -> hir::Value<'g>
    {
        let frame = FrameInterpreter::new(self.registry, self.local_arena);
        self.global_arena.intern(&frame.evaluate(cfg, arguments))
    }
}

//
//  Implementation Details
//
struct FrameInterpreter<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    registry: &'a Registry<'g>,
    arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> FrameInterpreter<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    fn new(registry: &'a Registry<'g>, arena: &'local mem::Arena)
        -> FrameInterpreter<'a, 'g, 'local>
    {
        FrameInterpreter { registry: registry, arena: arena }
    }

    fn evaluate(
        &self,
        cfg: &sir::ControlFlowGraph<'g>,
        arguments: &'local [hir::Value<'local>]
    )
        -> hir::Value<'local>
    {
        use self::BlockResult::*;

        fn interpret_block<'a, 'g, 'local>(
            registry: &'a Registry<'g>,
            arena: &'local mem::Arena,
            block: &'g sir::BasicBlock<'g>,
            arguments: &'local [hir::Value<'local>],
        )
            -> BlockResult<'local>
        where
            'g: 'a + 'local
        {
            let mut interpreter =
                BlockInterpreter::new(registry, arena, arguments);
            interpreter.evaluate(block)
        }

        let mut index = 0;
        let mut arguments = arguments;

        //  TODO(matthieum): add way to parameterize fuel.
        for _ in 0..1000 {
            let (i, args) = match interpret_block(
                self.registry,
                self.arena,
                &cfg.blocks[index],
                arguments
            ) 
            {
                Jump(index, args) => (index, args),
                Return(v) => return hir::Value {
                    type_: v.type_,
                    range: com::Range::new(0, 0),
                    expr: v.expr,
                    gvn: Default::default(),
                },
            };
            index = i.index();
            arguments = args;
        }

        unreachable!()
    }
}

struct BlockInterpreter<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    registry: &'a Registry<'g>,
    arena: &'local mem::Arena,
    arguments: &'local [hir::Value<'local>],
    bindings: mem::Array<'local, hir::Value<'local>>,
}

impl<'a, 'g, 'local> BlockInterpreter<'a, 'g, 'local>
    where 'g: 'local
{
    fn new(
        registry: &'a Registry<'g>,
        arena: &'local mem::Arena,
        arguments: &'local [hir::Value<'local>]
    )
        -> BlockInterpreter<'a, 'g, 'local>
    {
        BlockInterpreter {
            registry: registry,
            arena: arena,
            arguments: arguments,
            bindings: mem::Array::new(arena)
        }
    }

    fn evaluate(&mut self, block: &sir::BasicBlock<'g>)
        -> BlockResult<'local>
    {
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

    fn eval_instr(&self, instr: &sir::Instruction<'g>) -> hir::Value<'local>
    {
        use model::sir::Instruction::*;

        match *instr {
            Call(fun, args, _) => self.eval_call(fun, args),
            Field(_, value, index, _) => self.eval_field(value, index),
            Load(value, range) => self.load(value, range),
            New(type_, fields, range) => self.eval_new(type_, fields, range),
        }
    }

    fn eval_call(&self, fun: hir::Callable, args: &[sir::ValueId])
        -> hir::Value<'local>
    {
        match fun {
            hir::Callable::Builtin(b) => self.eval_builtin(b, args),
            hir::Callable::Function(ref f) => self.eval_function(f, args),
            _ => panic!("unimplemented - eval_call - {:?}", fun),
        }
    }

    fn eval_builtin(&self, fun: hir::BuiltinFunction, args: &[sir::ValueId])
        -> hir::Value<'local>
    {
        self.eval_binary_fun(fun, args)
    }

    fn eval_field(&self, value: sir::ValueId, index: u16)
        -> hir::Value<'local>
    {
        use self::hir::Expr::*;

        let index = index as usize;

        match self.get_value(value).expr {
            Constructor(c) => self.arena.intern(&c.arguments.fields[index]),
            Tuple(tup) => self.arena.intern(&tup.fields[index]),
            _ => unreachable!(),
        }
    }

    fn eval_function(
        &self,
        fun: &hir::FunctionProto,
        args: &[sir::ValueId]
    )
        -> hir::Value<'local>
    {
        let cfg = self.registry.lookup_cfg(fun.name).expect("CFG present");
        let interpreter = FrameInterpreter::new(self.registry, self.arena);

        let mut arguments = mem::Array::with_capacity(args.len(), self.arena);
        for &a in args {
            arguments.push(self.get_value(a));
        }

        self.arena.intern(&interpreter.evaluate(&cfg, arguments.into_slice()))
    }

    fn eval_binary_fun(
        &self,
        fun: hir::BuiltinFunction,
        args: &[sir::ValueId]
    )
        -> hir::Value<'local>
    {
        use model::hir::BuiltinFunction::*;
        use model::hir::BuiltinValue::{Bool, Int};

        assert_eq!(args.len(), 2);

        fn get_builtin<'a>(value: hir::Value<'a>) -> hir::BuiltinValue<'a> {
            match value.expr {
                hir::Expr::BuiltinVal(b) => b,
                _ => unreachable!(),
            }
        }

        fn to_bool(value: hir::BuiltinValue) -> bool {
            use std::convert::Into;
            Into::<bool>::into(value)
        }

        fn to_int(value: hir::BuiltinValue) -> i64 {
            use std::convert::Into;
            Into::<i64>::into(value)
        }

        let left = || get_builtin(self.get_value(args[0]));
        let right = || get_builtin(self.get_value(args[1]));

        let value = match fun {
            And => Bool(to_bool(left()) && to_bool(right())),
            Add => Int(to_int(left()) + to_int(right())),
            Differ => Bool(left() != right()),
            Equal => Bool(left() == right()),
            FloorDivide => Int(to_int(left()) / to_int(right())),
            GreaterThan => Bool(left() > right()),
            GreaterThanOrEqual => Bool(left() >= right()),
            LessThan => Bool(left() < right()),
            LessThanOrEqual => Bool(left() <= right()),
            Multiply => Int(to_int(left()) * to_int(right())),
            Not => Bool(!to_bool(left())),
            Or => Bool(to_bool(left()) || to_bool(right())),
            Substract => Int(to_int(left()) - to_int(right())),
            Xor => Bool(to_bool(left()) ^ to_bool(right())),
        };

        hir::Value {
            type_: fun.result_type(),
            range: com::Range::new(0, 0),
            expr: hir::Expr::BuiltinVal(value),
            gvn: Default::default(),
        }
    }

    fn eval_new(
        &self,
        type_: hir::Type<'local>,
        fields: &[sir::ValueId],
        range: com::Range
    )
        -> hir::Value<'local>
    {
        let mut elements = mem::Array::with_capacity(fields.len(), self.arena);

        for id in fields {
            elements.push(self.get_value(*id))
        }

        hir::Value {
            type_: type_,
            range: range,
            expr: hir::Expr::Tuple(hir::Tuple{
                fields: elements.into_slice(),
                names: &[],
            }),
            gvn: Default::default(),
        }
    }

    fn load(&self, v: hir::BuiltinValue, range: com::Range)
        -> hir::Value<'local>
    {
        let type_ = match v {
            hir::BuiltinValue::Bool(_) => hir::BuiltinType::Bool,
            hir::BuiltinValue::Int(_) => hir::BuiltinType::Int,
            hir::BuiltinValue::String(_) => hir::BuiltinType::String,
        };

        let value = hir::Value {
            type_: hir::Type::Builtin(type_),
            range: range,
            expr: hir::Expr::BuiltinVal(v),
            gvn: Default::default(),
        };

        self.arena.intern(&value)
    }

    fn get_value(&self, id: sir::ValueId) -> hir::Value<'local> {
        if let Some(i) = id.as_instruction() {
            debug_assert!(
                i < self.bindings.len(), "{} not in {:?}", i, self.bindings
            );
            self.bindings[i]
        } else if let Some(a) = id.as_argument() {
            debug_assert!(
                a < self.arguments.len(), "{} not in {:?}", a, self.arguments
            );
            self.arguments[a]
        } else {
            unreachable!()
        }
    }

    fn get_branch(&self, index: sir::ValueId) -> usize {
        match self.get_value(index).expr {
            hir::Expr::BuiltinVal(hir::BuiltinValue::Int(i)) => i as usize,
            hir::Expr::BuiltinVal(hir::BuiltinValue::Bool(cond))
                => if cond { 0 } else { 1 },
            _ => unreachable!(),
        }
    }

    fn jump(&self, jump: &sir::Jump) -> BlockResult<'local> {
        let mut arguments =
            mem::Array::with_capacity(jump.arguments.len(), self.arena);

        for &a in jump.arguments {
            arguments.push(self.get_value(a));
        }

        BlockResult::Jump(jump.dest, arguments.into_slice())
    }
}

enum BlockResult<'a> {
    Jump(sir::BlockId, &'a [hir::Value<'a>]),
    Return(hir::Value<'a>),
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::{hir, sir};
    use super::super::reg::{Registry, SimpleRegistry};

    #[test]
    fn add_no_arguments() {
        let global_arena = mem::Arena::new();

        let arguments = [ val_instr(0), val_instr(1) ];
        let instructions = [
            instr_load_int(1),
            instr_load_int(2),
            instr_builtin(hir::BuiltinFunction::Add, &arguments),
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
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

        assert_eq!(
            eval(
                &global_arena,
                &[sem_int(1), sem_int(2)],
                &sir::ControlFlowGraph {
                    blocks: &[block_return(
                        &[int, int],
                        &[
                            instr_builtin(
                                hir::BuiltinFunction::Add,
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
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

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
    fn call_user_defined_function() {
        let global_arena = mem::Arena::new();
        let mut registry = SimpleRegistry::new(&global_arena);

        let id = hir::ItemIdentifier(Default::default(), range(42, 5));
        let int = hir::Type::Builtin(hir::BuiltinType::Int);
        
        registry.insert(
            id,
            global_arena.intern(&sir::ControlFlowGraph {
                blocks: &[
                    block_return(
                        &[],
                        &[
                            instr_load_int(1),
                        ],
                    ),
                ],
            })
        );

        let cfg = global_arena.intern(
            &sir::ControlFlowGraph {
                blocks: &[block_return(
                    &[],
                    &[
                        sir::Instruction::Call(
                            hir::Callable::Function(hir::FunctionProto {
                                name: id,
                                range: range(0, 0),
                                arguments: &[],
                                result: int,
                            }),
                            &[],
                            range(0, 0),
                        )
                    ]
                )],
            }
        );

        assert_eq!(
            eval_with_registry(
                &global_arena,
                &registry,
                &[],
                &cfg
            ),
            sem_int(1)
        );
    }

    #[test]
    fn new_tuple() {
        let global_arena = mem::Arena::new();

        let int = hir::Type::Builtin(hir::BuiltinType::Int);
        let inner_type = [int, int];
        let type_ = hir::Type::Tuple(
            hir::Tuple { fields: &inner_type, names: &[] }
        );

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
            hir::Value {
                type_: type_,
                range: range(0, 0),
                expr: hir::Expr::Tuple(hir::Tuple {
                    fields: &[sem_int(1), sem_int(2)],
                    names: &[],
                }),
                gvn: Default::default(),
            }
        );
    }

    #[test]
    fn record_field() {
        let global_arena = mem::Arena::new();

        let int = hir::Type::Builtin(hir::BuiltinType::Int);
        let rec = hir::Type::Rec(hir::RecordProto {
            name: hir::ItemIdentifier(Default::default(), range(5, 4)),
            range: range(0, 20),
            enum_: hir::ItemIdentifier::unresolved(),
        });

        assert_eq!(
            eval(
                &global_arena,
                &[],
                &sir::ControlFlowGraph {
                    blocks: &[block_return(
                        &[],
                        &[
                            instr_load_int(4),
                            instr_load_int(42),
                            instr_new(rec, &[val_instr(0), val_instr(1)]),
                            instr_field(int, val_instr(2), 1)
                        ]
                    )]
                }
            ),
            sem_int(42)
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
        arguments: &[hir::Value<'g>],
        cfg: &sir::ControlFlowGraph<'g>
    )
        -> hir::Value<'g>
    {
        let registry = SimpleRegistry::new(global_arena);
        global_arena.intern(
            &eval_with_registry(global_arena, &registry, arguments, cfg)
        )
    }

    fn eval_with_registry<'g>(
        global_arena: &'g mem::Arena,
        registry: &'g Registry<'g>,
        arguments: &[hir::Value<'g>],
        cfg: &sir::ControlFlowGraph<'g>,
    )
        -> hir::Value<'g>
    {
        use super::Interpreter;

        let mut local_arena = mem::Arena::new();
        let result = Interpreter::new(registry, global_arena, &local_arena)
            .evaluate(cfg, arguments);
        local_arena.recycle();

        result
    }

    fn sem_int(i: i64) -> hir::Value<'static> {
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::Int),
            range: range(0, 0),
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::Int(i)),
            gvn: Default::default(),
        }
    }

    fn sem_string<'a>(s: &'a [u8]) -> hir::Value<'a> {
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::String),
            range: range(0, 0),
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::String(s)),
            gvn: Default::default(),
        }
    }

    fn val_arg(index: usize) -> sir::ValueId {
        sir::ValueId::new_argument(index)
    }

    fn val_instr(index: usize) -> sir::ValueId {
        sir::ValueId::new_instruction(index)
    }

    fn instr_builtin<'a>(fun: hir::BuiltinFunction, args: &'a [sir::ValueId])
        -> sir::Instruction<'a>
    {
        sir::Instruction::Call(hir::Callable::Builtin(fun), args, range(0, 0))
    }

    fn instr_field<'a>(type_: hir::Type<'a>, value: sir::ValueId, index: u16)
        -> sir::Instruction<'a>
    {
        sir::Instruction::Field(type_, value, index, range(0, 0))
    }

    fn instr_load_bool(value: bool) -> sir::Instruction<'static> {
        sir::Instruction::Load(hir::BuiltinValue::Bool(value), range(0, 0))
    }

    fn instr_load_int(i: i64) -> sir::Instruction<'static> {
        sir::Instruction::Load(hir::BuiltinValue::Int(i), range(0, 0))
    }

    fn instr_load_string<'a>(s: &'a [u8]) -> sir::Instruction<'a> {
        sir::Instruction::Load(hir::BuiltinValue::String(s), range(0, 0))
    }

    fn instr_new<'a>(type_: hir::Type<'a>, values: &'a [sir::ValueId])
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
        args: &'a [hir::Type],
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
