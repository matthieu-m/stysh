//! The interpreter.
//!
//! This module defines the entry point of the interpreter.

use basic::mem::{DynArray, InternerSnapshot};
use model::{hir, sir};
use super::reg::Registry;

/// Stysh Interpreter.
///
/// Interprets a SIR control flow graph based on a type dictionary, producing
/// either a value, or an error if the interpretation cannot succeed (missing
/// definitions, FFI call, ...).
pub struct Interpreter<'a> {
    interner: InternerSnapshot<'a>,
    registry: &'a Registry,
}

impl<'a> Interpreter<'a> {
    /// Creates a new instance of an interpreter.
    pub fn new(interner: InternerSnapshot<'a>, registry: &'a Registry)
        -> Interpreter<'a>
    {
        Interpreter { interner, registry }
    }

    /// Returns the value evaluated from the SIR.
    pub fn evaluate(
        &self,
        cfg: &sir::ControlFlowGraph,
        arguments: DynArray<Value>,
    )
        -> Value
    {
        let frame = FrameInterpreter::new(self.interner, self.registry);
        frame.evaluate(cfg, arguments)
    }
}

//
//  Values
//

/// A value.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Value {
    /// Boolean.
    Bool(bool),
    /// Integral.
    Int(i64),
    /// String.
    String(Vec<u8>),
    /// A constructor call.
    Constructor(hir::Constructor<Value>),
    /// A tuple.
    Tuple(hir::Tuple<Value>),
}

//
//  Implementation Details
//
struct FrameInterpreter<'a> {
    interner: InternerSnapshot<'a>,
    registry: &'a Registry,
}

impl<'a> FrameInterpreter<'a> {
    fn new(interner: InternerSnapshot<'a>, registry: &'a Registry)
        -> FrameInterpreter<'a>
    {
        FrameInterpreter { interner, registry }
    }

    fn evaluate(
        &self,
        cfg: &sir::ControlFlowGraph,
        arguments: DynArray<Value>,
    )
        -> Value
    {
        use self::BlockResult::*;

        fn interpret_block<'a>(
            interner: InternerSnapshot<'a>,
            registry: &'a Registry,
            block: &sir::BasicBlock,
            arguments: DynArray<Value>,
        )
            -> BlockResult
        {
            let mut interpreter = BlockInterpreter::new(interner, registry, arguments);
            interpreter.evaluate(block)
        }

        let mut index = 0;
        let mut arguments = arguments;

        //  TODO(matthieum): add way to parameterize fuel.
        for _ in 0..1000 {
            let (i, args) = match interpret_block(
                self.interner,
                self.registry,
                &cfg.blocks.at(index),
                arguments
            ) 
            {
                Jump(index, args) => (index, args),
                Return(v) => return v,
            };
            index = i.index();
            arguments = args;
        }

        unreachable!()
    }
}

struct BlockInterpreter<'a> {
    interner: InternerSnapshot<'a>,
    registry: &'a Registry,
    arguments: DynArray<Value>,
    bindings: DynArray<Value>,
}

impl<'a> BlockInterpreter<'a> {
    fn new(
        interner: InternerSnapshot<'a>,
        registry: &'a Registry,
        arguments: DynArray<Value>
    )
        -> BlockInterpreter<'a>
    {
        let bindings = Default::default();
        BlockInterpreter { interner, registry, arguments, bindings }
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

    fn eval_instr(&self, instr: &sir::Instruction) -> Value
    {
        use model::sir::Instruction::*;

        match instr {
            Call(fun, args, _) => self.eval_call(fun, args.clone()),
            Field(_, value, index, _) => self.eval_field(*value, *index),
            Load(value, _) => self.load(value),
            New(type_, fields, _) => self.eval_new(type_.clone(), fields.clone()),
        }
    }

    fn eval_call(&self, fun: &sir::Callable, args: DynArray<sir::ValueId>)
        -> Value
    {
        match fun {
            sir::Callable::Builtin(b) => self.eval_builtin(b, args),
            sir::Callable::Function(f) => self.eval_function(f, args),
        }
    }

    fn eval_builtin(&self, fun: &hir::BuiltinFunction, args: DynArray<sir::ValueId>)
        -> Value
    {
        self.eval_builtin_fun(fun, args)
    }

    fn eval_field(&self, value: sir::ValueId, index: u16) -> Value {
        use self::Value::*;

        let index = index as usize;

        match self.get_value(value) {
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
        -> Value
    {
        let cfg = self.registry.lookup_cfg(fun.name).expect("CFG present");
        let interpreter = FrameInterpreter::new(self.interner, self.registry);

        let arguments = DynArray::with_capacity(args.len());
        for a in args {
            arguments.push(self.get_value(a));
        }

        interpreter.evaluate(&cfg, arguments)
    }

    fn eval_builtin_fun(
        &self,
        fun: &hir::BuiltinFunction,
        args: DynArray<sir::ValueId>,
    )
        -> Value
    {
        use model::hir::BuiltinFunction::*;
        use self::Value::{Bool, Int};

        let left = args.get(0).map(|v| self.get_value(v));
        let right = args.get(1).map(|v| self.get_value(v));

        fn to_bool(value: Option<Value>) -> bool {
            if let Some(Bool(b)) = value {
                return b;
            }
            unreachable!("Expected boolean, got: {:?}", value);
        }

        fn to_int(value: Option<Value>) -> i64 {
            if let Some(Int(i)) = value {
                return i;
            }
            unreachable!("Expected integral, got: {:?}", value);
        }

        match fun {
            And => Bool(to_bool(left) && to_bool(right)),
            Add => Int(to_int(left) + to_int(right)),
            Differ => Bool(left != right),
            Equal => Bool(left == right),
            FloorDivide => Int(to_int(left) / to_int(right)),
            GreaterThan => Bool(left > right),
            GreaterThanOrEqual => Bool(left >= right),
            LessThan => Bool(left < right),
            LessThanOrEqual => Bool(left <= right),
            Multiply => Int(to_int(left) * to_int(right)),
            Not => Bool(!to_bool(left)),
            Or => Bool(to_bool(left) || to_bool(right)),
            Substract => Int(to_int(left) - to_int(right)),
            Xor => Bool(to_bool(left) ^ to_bool(right)),
        }
    }

    fn eval_new(
        &self,
        type_: hir::Type,
        elements: DynArray<sir::ValueId>,
    )
        -> Value
    {
        let fields = DynArray::with_capacity(elements.len());

        for id in elements {
            fields.push(self.get_value(id))
        }

        let names = match type_ {
            hir::Type::Rec(rec, _, _) => rec.definition.names,
            hir::Type::Tuple(tuple, _) => tuple.names,
            t => unreachable!("Expected tuple, got {:?}", t),
        };

        Value::Tuple(hir::Tuple{ fields, names })
    }

    fn load(&self, v: &hir::BuiltinValue) -> Value {
        use model::hir::BuiltinValue::*;

        match v {
            Bool(b) => Value::Bool(*b),
            Int(i) => Value::Int(*i),
            String(id) => {
                let slice = self.interner.get(*id).expect("Unknown InternId");
                Value::String(slice.iter().cloned().collect())
            },
        }
    }

    fn get_value(&self, id: sir::ValueId) -> Value {
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
        match self.get_value(index) {
            Value::Int(i) => i as usize,
            Value::Bool(cond) => if cond { 0 } else { 1 },
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
    Jump(sir::BlockId, DynArray<Value>),
    Return(Value),
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::com;
    use basic::mem::{DynArray, InternId, Interner, InternerSnapshot};
    use model::{hir, sir};
    use super::Value;
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
            Value::Int(3)
        );
    }

    #[test]
    fn add_with_arguments() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

        assert_eq!(
            eval(
                &[val_int(1), val_int(2)],
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
            val_int(3)
        );
    }

    #[test]
    fn branch_if() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);

        for &(condition, ref result) in &[(true, val_int(1)), (false, val_int(2))] {
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
        let interner = Interner::new();
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
                        sir::Callable::Function(hir::FunctionProto {
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
                interner.snapshot(),
                &registry,
                &[],
                &cfg
            ),
            val_int(1)
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
            Value::Tuple(hir::Tuple {
                fields: dyn_array(&[val_int(1), val_int(2)]),
                names: DynArray::default(),
            })
        );
    }

    #[test]
    fn record_field() {
        let int = hir::Type::Builtin(hir::BuiltinType::Int);
        let rec = hir::Type::Rec(
            hir::Record {
                prototype: hir::RecordProto {
                    name: hir::ItemIdentifier(Default::default(), range(5, 4)),
                    range: range(0, 20),
                    enum_: hir::ItemIdentifier::unresolved(),
                },
                definition: hir::Tuple {
                    fields: dyn_array(&[int.clone(), int.clone()]),
                    names: DynArray::default(),
                },
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
            val_int(42)
        );
    }

    #[test]
    fn return_helloworld() {
        let interner = Interner::new();
        let registry = SimpleRegistry::new();

        let id = interner.insert(b"Hello, World!");

        assert_eq!(
            eval_with_registry(
                interner.snapshot(),
                &registry,
                &[],
                &cfg(&[
                    block_return(
                        &[],
                        &[
                            instr_load_string(id)
                        ]
                    ),
                ]),
            ),
            val_string(b"Hello, World!")
        );
    }

    fn eval(
        arguments: &[Value],
        cfg: &sir::ControlFlowGraph,
    )
        -> Value
    {
        let interner = Interner::new();
        let registry = SimpleRegistry::new();
        eval_with_registry(interner.snapshot(), &registry, arguments, cfg)
    }

    fn eval_with_registry(
        interner: InternerSnapshot<'_>,
        registry: &Registry,
        arguments: &[Value],
        cfg: &sir::ControlFlowGraph,
    )
        -> Value
    {
        use super::Interpreter;

        Interpreter::new(interner, registry).evaluate(cfg, dyn_array(arguments))
    }

    fn val_int(i: i64) -> Value { Value::Int(i) }

    fn val_string(s: &[u8]) -> Value {
        let value = s.iter().cloned().collect();
        Value::String(value)
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
        sir::Instruction::Call(sir::Callable::Builtin(fun), dyn_array(args), range(0, 0))
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

    fn instr_load_string(s: InternId) -> sir::Instruction {
        sir::Instruction::Load(hir::BuiltinValue::String(s), range(0, 0))
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
