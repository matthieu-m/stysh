//! The interpreter.
//!
//! This module defines the entry point of the interpreter.

use basic::com;
use basic::mem::DynArray;
use model::{hir, sir};
use super::reg::Registry;

/// Stysh Interpreter.
///
/// Interprets a SIR control flow graph based on a type dictionary, producing
/// either a value, or an error if the interpretation cannot succeed (missing
/// definitions, FFI call, ...).
pub struct Interpreter<'a> {
    registry: &'a Registry,
}

impl<'a> Interpreter<'a> {
    /// Creates a new instance of an interpreter.
    pub fn new(registry: &'a Registry) -> Interpreter<'a> {
        Interpreter { registry }
    }

    /// Returns the value evaluated from the SIR.
    pub fn evaluate(
        &self,
        cfg: &sir::ControlFlowGraph,
        arguments: DynArray<hir::Value>,
    )
        -> hir::Value
    {
        let frame = FrameInterpreter::new(self.registry);
        frame.evaluate(cfg, arguments)
    }
}

//
//  Implementation Details
//
struct FrameInterpreter<'a> {
    registry: &'a Registry,
}

impl<'a> FrameInterpreter<'a> {
    fn new(registry: &'a Registry) -> FrameInterpreter<'a> {
        FrameInterpreter { registry }
    }

    fn evaluate(
        &self,
        cfg: &sir::ControlFlowGraph,
        arguments: DynArray<hir::Value>,
    )
        -> hir::Value
    {
        use self::BlockResult::*;

        fn interpret_block<'a>(
            registry: &'a Registry,
            block: &sir::BasicBlock,
            arguments: DynArray<hir::Value>,
        )
            -> BlockResult
        {
            let mut interpreter = BlockInterpreter::new(registry, arguments);
            interpreter.evaluate(block)
        }

        let mut index = 0;
        let mut arguments = arguments;

        //  TODO(matthieum): add way to parameterize fuel.
        for _ in 0..1000 {
            let (i, args) = match interpret_block(
                self.registry,
                &cfg.blocks.at(index),
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

struct BlockInterpreter<'a> {
    registry: &'a Registry,
    arguments: DynArray<hir::Value>,
    bindings: DynArray<hir::Value>,
}

impl<'a> BlockInterpreter<'a> {
    fn new(
        registry: &'a Registry,
        arguments: DynArray<hir::Value>,
    )
        -> BlockInterpreter<'a>
    {
        BlockInterpreter {
            registry: registry,
            arguments: arguments,
            bindings: DynArray::new(vec!()),
        }
    }

    fn evaluate(&mut self, block: &sir::BasicBlock)
        -> BlockResult
    {
        use model::sir::TerminatorInstruction::*;

        for i in &block.instructions {
            let value = self.eval_instr(&i);
            self.bindings.push(value);
        }

        match &block.exit {
            Branch(index, jumps) => self.jump(&jumps.at(self.get_branch(*index))),
            Jump(jump) => self.jump(&jump),
            Return(index) => BlockResult::Return(self.get_value(*index)),
            _ => unimplemented!(),
        }
    }

    fn eval_instr(&self, instr: &sir::Instruction) -> hir::Value
    {
        use model::sir::Instruction::*;

        match instr {
            Call(fun, args, _) => self.eval_call(fun, args.clone()),
            Field(_, value, index, _) => self.eval_field(*value, *index),
            Load(value, range) => self.load(value, *range),
            New(type_, fields, range) => self.eval_new(type_.clone(), fields.clone(), *range),
        }
    }

    fn eval_call(&self, fun: &hir::Callable, args: DynArray<sir::ValueId>)
        -> hir::Value
    {
        match fun {
            hir::Callable::Builtin(b) => self.eval_builtin(b, args),
            hir::Callable::Function(ref f) => self.eval_function(f, args),
            _ => panic!("unimplemented - eval_call - {:?}", fun),
        }
    }

    fn eval_builtin(&self, fun: &hir::BuiltinFunction, args: DynArray<sir::ValueId>)
        -> hir::Value
    {
        self.eval_binary_fun(fun, args)
    }

    fn eval_field(&self, value: sir::ValueId, index: u16)
        -> hir::Value
    {
        use self::hir::Expr::*;

        let index = index as usize;

        match self.get_value(value).expr {
            Constructor(c) => c.arguments.fields.at(index),
            Tuple(tup) => tup.fields.at(index),
            _ => unreachable!(),
        }
    }

    fn eval_function(
        &self,
        fun: &hir::FunctionProto,
        args: DynArray<sir::ValueId>,
    )
        -> hir::Value
    {
        let cfg = self.registry.lookup_cfg(fun.name).expect("CFG present");
        let interpreter = FrameInterpreter::new(self.registry);

        let arguments = DynArray::with_capacity(args.len());
        for a in args {
            arguments.push(self.get_value(a));
        }

        interpreter.evaluate(&cfg, arguments)
    }

    fn eval_binary_fun(
        &self,
        fun: &hir::BuiltinFunction,
        args: DynArray<sir::ValueId>,
    )
        -> hir::Value
    {
        use model::hir::BuiltinFunction::*;
        use model::hir::BuiltinValue::{Bool, Int};

        assert_eq!(args.len(), 2);

        fn get_builtin(value: hir::Value) -> hir::BuiltinValue {
            match value.expr {
                hir::Expr::BuiltinVal(b) => b.clone(),
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

        let left = || get_builtin(self.get_value(args.at(0)));
        let right = || get_builtin(self.get_value(args.at(1)));

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
        type_: hir::Type,
        fields: DynArray<sir::ValueId>,
        range: com::Range
    )
        -> hir::Value
    {
        let elements = DynArray::with_capacity(fields.len());

        for id in fields {
            elements.push(self.get_value(id))
        }

        hir::Value {
            type_: type_,
            range: range,
            expr: hir::Expr::Tuple(hir::Tuple{
                fields: elements,
                names: DynArray::default(),
            }),
            gvn: Default::default(),
        }
    }

    fn load(&self, v: &hir::BuiltinValue, range: com::Range)
        -> hir::Value
    {
        let type_ = match v {
            hir::BuiltinValue::Bool(_) => hir::BuiltinType::Bool,
            hir::BuiltinValue::Int(_) => hir::BuiltinType::Int,
            hir::BuiltinValue::String(_) => hir::BuiltinType::String,
        };

        let value = hir::Value {
            type_: hir::Type::Builtin(type_),
            range: range,
            expr: hir::Expr::BuiltinVal(v.clone()),
            gvn: Default::default(),
        };

        value
    }

    fn get_value(&self, id: sir::ValueId) -> hir::Value {
        if let Some(i) = id.as_instruction() {
            debug_assert!(
                i < self.bindings.len(), "{} not in {:?}", i, self.bindings
            );
            self.bindings.at(i)
        } else if let Some(a) = id.as_argument() {
            debug_assert!(
                a < self.arguments.len(), "{} not in {:?}", a, self.arguments
            );
            self.arguments.at(a)
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

    fn jump(&self, jump: &sir::Jump) -> BlockResult {
        let arguments = DynArray::with_capacity(jump.arguments.len());

        for a in &jump.arguments {
            arguments.push(self.get_value(a));
        }

        BlockResult::Jump(jump.dest, arguments)
    }
}

enum BlockResult {
    Jump(sir::BlockId, DynArray<hir::Value>),
    Return(hir::Value),
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::com;
    use basic::mem::DynArray;
    use model::{hir, sir};
    use super::super::reg::{Registry, SimpleRegistry};

    #[test]
    fn add_no_arguments() {
        let arguments = [ val_instr(0), val_instr(1) ];
        let instructions = [
            instr_load_int(1),
            instr_load_int(2),
            instr_builtin(hir::BuiltinFunction::Add, &arguments),
        ];
        let cfg = cfg( &[ block_return(&[], &instructions) ] );

        assert_eq!(
            eval(&[], &cfg),
            sem_int(3)
        );
    }

    #[test]
    fn add_with_arguments() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

        assert_eq!(
            eval(
                &[sem_int(1), sem_int(2)],
                &cfg(&[
                    block_return(
                        &[int.clone(), int],
                        &[
                            instr_builtin(
                                hir::BuiltinFunction::Add,
                                &[ val_arg(0), val_arg(1) ]
                            ),
                        ]
                    )
                ])
            ),
            sem_int(3)
        );
    }

    #[test]
    fn branch_if() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

        for &(condition, ref result) in &[(true, sem_int(1)), (false, sem_int(2))] {
            assert_eq!(
                eval(
                    &[],
                    &cfg(&[
                        block_branch(
                            &[],
                            &[instr_load_bool(condition)],
                            val_instr(0),
                            &[ exit_jump(1, &[]), exit_jump(2, &[]) ],
                        ),
                        block_jump(
                            &[],
                            &[instr_load_int(1)],
                            exit_jump(3, &[val_instr(0)]),
                        ),
                        block_jump (
                            &[],
                            &[instr_load_int(2)],
                            exit_jump(3, &[val_instr(0)]),
                        ),
                        block_return(&[int.clone()], &[]),
                    ])
                ),
                result.clone()
            )
        }
    }

    #[test]
    fn call_user_defined_function() {
        let mut registry = SimpleRegistry::new();

        let id = hir::ItemIdentifier(Default::default(), range(42, 5));
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

        registry.insert(
            id,
            cfg(&[
                block_return(
                    &[],
                    &[ instr_load_int(1) ],
                ),
            ])
        );

        let cfg = cfg(&[
            block_return(
                &[],
                &[
                    sir::Instruction::Call(
                        hir::Callable::Function(hir::FunctionProto {
                            name: id,
                            range: range(0, 0),
                            arguments: DynArray::default(),
                            result: int,
                        }),
                        DynArray::default(),
                        range(0, 0),
                    )
                ]
            )
        ]);

        assert_eq!(
            eval_with_registry(
                &registry,
                &[],
                &cfg
            ),
            sem_int(1)
        );
    }

    #[test]
    fn new_tuple() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);
        let inner_type = [int.clone(), int];
        let type_ = hir::Type::Tuple(
            hir::Tuple { fields: dyn_array(&inner_type), names: DynArray::default() },
            Default::default(),
        );

        assert_eq!(
            eval(
                &[],
                &cfg(&[
                    block_return(
                        &[],
                        &[
                            instr_load_int(1),
                            instr_load_int(2),
                            instr_new(
                                type_.clone(),
                                &[val_instr(0), val_instr(1)]
                            ),
                        ]
                    )
                ]),
            ),
            hir::Value {
                type_: type_,
                range: range(0, 0),
                expr: hir::Expr::Tuple(hir::Tuple {
                    fields: dyn_array(&[sem_int(1), sem_int(2)]),
                    names: DynArray::default(),
                }),
                gvn: Default::default(),
            }
        );
    }

    #[test]
    fn record_field() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);
        let rec = hir::Type::UnresolvedRec(
            hir::RecordProto {
                name: hir::ItemIdentifier(Default::default(), range(5, 4)),
                range: range(0, 20),
                enum_: hir::ItemIdentifier::unresolved(),
            },
            Default::default(),
            Default::default(),
        );

        assert_eq!(
            eval(
                &[],
                &cfg(&[
                    block_return(
                        &[],
                        &[
                            instr_load_int(4),
                            instr_load_int(42),
                            instr_new(rec, &[val_instr(0), val_instr(1)]),
                            instr_field(int, val_instr(2), 1)
                        ]
                    )
                ]),
            ),
            sem_int(42)
        );
    }

    #[test]
    fn return_helloworld() {
        assert_eq!(
            eval(
                &[],
                &cfg(&[
                    block_return(
                        &[],
                        &[
                            instr_load_string(b"Hello, World!")
                        ]
                    ),
                ]),
            ),
            sem_string(b"Hello, World!")
        );
    }

    fn eval(
        arguments: &[hir::Value],
        cfg: &sir::ControlFlowGraph,
    )
        -> hir::Value
    {
        let registry = SimpleRegistry::new();
        eval_with_registry(&registry, arguments, cfg)
    }

    fn eval_with_registry(
        registry: &Registry,
        arguments: &[hir::Value],
        cfg: &sir::ControlFlowGraph,
    )
        -> hir::Value
    {
        use super::Interpreter;

        Interpreter::new(registry).evaluate(cfg, dyn_array(arguments))
    }

    fn sem_int(i: i64) -> hir::Value {
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::Int),
            range: range(0, 0),
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::Int(i)),
            gvn: Default::default(),
        }
    }

    fn sem_string(s: &[u8]) -> hir::Value {
        let value = s.iter().cloned().collect();
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::String),
            range: range(0, 0),
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::String(value)),
            gvn: Default::default(),
        }
    }

    fn val_arg(index: usize) -> sir::ValueId {
        sir::ValueId::new_argument(index)
    }

    fn val_instr(index: usize) -> sir::ValueId {
        sir::ValueId::new_instruction(index)
    }

    fn instr_builtin(fun: hir::BuiltinFunction, args: &[sir::ValueId])
        -> sir::Instruction
    {
        sir::Instruction::Call(hir::Callable::Builtin(fun), dyn_array(args), range(0, 0))
    }

    fn instr_field(type_: hir::Type, value: sir::ValueId, index: u16)
        -> sir::Instruction
    {
        sir::Instruction::Field(type_, value, index, range(0, 0))
    }

    fn instr_load_bool(value: bool) -> sir::Instruction {
        sir::Instruction::Load(hir::BuiltinValue::Bool(value), range(0, 0))
    }

    fn instr_load_int(i: i64) -> sir::Instruction {
        sir::Instruction::Load(hir::BuiltinValue::Int(i), range(0, 0))
    }

    fn instr_load_string(s: &[u8]) -> sir::Instruction {
        let value = s.iter().cloned().collect();
        sir::Instruction::Load(hir::BuiltinValue::String(value), range(0, 0))
    }

    fn instr_new(type_: hir::Type, values: &[sir::ValueId])
        -> sir::Instruction
    {
        sir::Instruction::New(type_, dyn_array(values), range(0, 0))
    }

    fn exit_jump(index: usize, args: &[sir::ValueId]) -> sir::Jump {
        sir::Jump {
            dest: sir::BlockId::new(index),
            arguments: dyn_array(args),
        }
    }

    fn block_branch(
        args: &[hir::Type],
        code: &[sir::Instruction],
        condition: sir::ValueId,
        jumps: &[sir::Jump],
    )
        -> sir::BasicBlock
    {
        sir::BasicBlock {
            arguments: dyn_array(args),
            instructions: dyn_array(code),
            exit: sir::TerminatorInstruction::Branch(condition, dyn_array(jumps)),
        }
    }

    fn block_jump(
        args: &[hir::Type],
        code: &[sir::Instruction],
        jump: sir::Jump,
    )
        -> sir::BasicBlock
    {
        sir::BasicBlock {
            arguments: dyn_array(args),
            instructions: dyn_array(code),
            exit: sir::TerminatorInstruction::Jump(jump),
        }
    }

    fn block_return(
        args: &[hir::Type],
        code: &[sir::Instruction]
    )
        -> sir::BasicBlock
    {
        let result = if code.len() > 0 {
            val_instr(code.len() - 1)
        } else {
            val_arg(0)
        };

        sir::BasicBlock {
            arguments: dyn_array(args),
            instructions: dyn_array(code),
            exit: sir::TerminatorInstruction::Return(result),
        }
    }

    fn cfg(blocks: &[sir::BasicBlock]) -> sir::ControlFlowGraph {
        sir::ControlFlowGraph { blocks: dyn_array(blocks) }
    }

    fn dyn_array<T: Clone>(elements: &[T]) -> DynArray<T> {
        let array = DynArray::with_capacity(elements.len());
        for e in elements { array.push(e.clone()); }
        array
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
