//! The interpreter.
//!
//! This module defines the entry point of the interpreter.

use crate::basic::mem::InternerSnapshot;
use crate::model::{hir, sir};
use crate::model::hir::ItemId;
use super::reg;

/// Stysh Interpreter.
///
/// Interprets a SIR control flow graph based on a type dictionary, producing
/// either a value, or an error if the interpretation cannot succeed (missing
/// definitions, FFI call, ...).
pub struct Interpreter<'a> {
    external: External<'a>
}

impl<'a> Interpreter<'a> {
    /// Creates a new instance of an interpreter.
    pub fn new(
        interner: InternerSnapshot,
        hir_registry: &'a dyn hir::Registry,
        cfg_registry: &'a dyn reg::Registry,
    )
        -> Interpreter<'a>
    {
        let external = External { interner, hir_registry, cfg_registry };
        Interpreter { external }
    }

    /// Returns the value evaluated from the SIR.
    pub fn evaluate(
        &self,
        cfg: &sir::Graph,
        arguments: Vec<Value>,
    )
        -> Value
    {
        let frame = FrameInterpreter::new(self.external.clone());
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
    Constructor(hir::TypeId, Vec<Value>),
    /// A tuple.
    Tuple(Vec<Value>),
}

//
//  Implementation Details
//
struct FrameInterpreter<'a> {
    external: External<'a>
}

impl<'a> FrameInterpreter<'a> {
    fn new(external: External<'a>) -> FrameInterpreter<'a> {
        FrameInterpreter { external }
    }

    fn evaluate(
        &self,
        cfg: &sir::Graph,
        arguments: Vec<Value>,
    )
        -> Value
    {
        use self::BlockResult::*;

        fn interpret_block(
            external: External<'_>,
            graph: &sir::Graph,
            block: &sir::Block,
            arguments: Vec<Value>,
        )
            -> BlockResult
        {
            let mut interpreter = BlockInterpreter::new(
                external, graph, block, arguments
            );
            interpreter.evaluate()
        }

        let mut index = sir::BlockId::new(0);
        let mut arguments = arguments;

        //  TODO(matthieum): add way to parameterize fuel.
        for _ in 0..1000 {
            let (i, args) = match interpret_block(
                self.external.clone(),
                cfg,
                cfg.get_block(index),
                arguments
            ) 
            {
                Jump(index, args) => (index, args),
                Return(v) => return v,
            };
            index = i;
            arguments = args;
        }

        unreachable!()
    }
}

struct BlockInterpreter<'a> {
    external: External<'a>,
    graph: &'a sir::Graph,
    block: &'a sir::Block,
    arguments: Vec<Value>,
    bindings: Vec<Value>,
}

impl<'a> BlockInterpreter<'a> {
    fn new(
        external: External<'a>,
        graph: &'a sir::Graph,
        block: &'a sir::Block,
        arguments: Vec<Value>
    )
        -> BlockInterpreter<'a>
    {
        let bindings = Default::default();
        BlockInterpreter { external, graph, block, arguments, bindings }
    }

    fn evaluate(&mut self) -> BlockResult  {
        use self::sir::TerminatorInstruction::*;

        for i in self.block.get_instructions() {
            let value = self.eval_instruction(i);
            self.bindings.push(value);
        }

        match self.block.get_terminator() {
            Branch(index, jumps) => {
                let jumps = self.block.get_jump_ids(jumps);
                let branch = self.get_branch(index);
                self.jump(jumps[branch])
            },
            Jump(jump) => self.jump(jump),
            Return(index) => BlockResult::Return(self.get_value(index)),
            _ => unimplemented!(),
        }
    }

    fn eval_instruction(
        &self,
        instr: sir::ValueId,
    )
        -> Value
    {
        use self::sir::Instruction::*;

        match self.block.get_instruction(instr) {
            Call(_, fun, args) => self.eval_call(fun, args),
            Field(_, value, index) => self.eval_field(value, index),
            Load(value) => self.load(value),
            New(type_, fields) => self.eval_new(type_, fields),
        }
    }

    fn eval_call(&self, fun: sir::Callable, args: sir::Id<[sir::ValueId]>)
        -> Value
    {
        let args = self.block.get_instruction_ids(args);
        match fun {
            sir::Callable::Builtin(b) => self.eval_builtin(b, args),
            sir::Callable::Function(f) => self.eval_function(f, args),
        }
    }

    fn eval_builtin(&self, fun: hir::BuiltinFunction, args: &[sir::ValueId])
        -> Value
    {
        self.eval_builtin_fun(fun, args)
    }

    fn eval_field(&self, value: sir::ValueId, index: u16) -> Value {
        use self::Value::*;

        let index = index as usize;

        match self.get_value(value) {
            Constructor(_, tup) | Tuple(tup) => tup[index as usize].clone(),
            _ => unreachable!(),
        }
    }

    fn eval_function(
        &self,
        fun: hir::FunctionId,
        args: &[sir::ValueId],
    )
        -> Value
    {
        let cfg = self.external.cfg_registry.lookup_cfg(fun).expect("CFG present");
        let interpreter = FrameInterpreter::new(self.external.clone());

        let mut arguments = Vec::with_capacity(args.len());
        for &a in args {
            arguments.push(self.get_value(a));
        }

        interpreter.evaluate(cfg, arguments)
    }

    fn eval_builtin_fun(
        &self,
        fun: hir::BuiltinFunction,
        args: &[sir::ValueId],
    )
        -> Value
    {
        use self::hir::BuiltinFunction::*;
        use self::Value::{Bool, Int};

        let left = args.get(0).map(|&v| self.get_value(v));
        let right = args.get(1).map(|&v| self.get_value(v));

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
        ty: hir::TypeId,
        arguments: sir::Id<[sir::ValueId]>,
    )
        -> Value
    {
        let arguments = self.block.get_instruction_ids(arguments);
        let mut fields = Vec::with_capacity(arguments.len());

        for &id in arguments {
            fields.push(self.get_value(id))
        }

        if let hir::Type::Tuple(..) = self.get_type(ty) {
            Value::Tuple(fields)
        } else {
            Value::Constructor(ty, fields)
        }
    }

    fn load(&self, v: hir::BuiltinValue) -> Value {
        use self::hir::BuiltinValue::*;

        match v {
            Bool(b) => Value::Bool(b),
            Int(i) => Value::Int(i),
            String(id) => {
                let slice = self.external.interner.get(id)
                    .expect("Unknown InternId");
                Value::String(slice.iter().cloned().collect())
            },
        }
    }

    fn get_value(&self, id: sir::ValueId) -> Value {
        if let Some(i) = id.as_instruction() {
            let i = i as usize;
            debug_assert!(
                i < self.bindings.len(), "{} not in {:?}", i, self.bindings
            );
            self.bindings[i].clone()
        } else if let Some(a) = id.as_argument() {
            let a = a as usize;
            debug_assert!(
                a < self.arguments.len(), "{} not in {:?}", a, self.arguments
            );
            self.arguments[a].clone()
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

    fn get_type(&self, ty: hir::TypeId) -> hir::Type {
        if let Some(b) = ty.builtin() {
            hir::Type::Builtin(b)
        } else if ty.is_tree() {
            self.graph.get_type(ty)
        } else {
            self.external.hir_registry.get_type(ty)
        }
    }

    fn jump(&self, jump: sir::JumpId) -> BlockResult {
        let jump = self.block.get_jump(jump);
        let args = self.block.get_instruction_ids(jump.arguments);

        let mut arguments = Vec::with_capacity(args.len());

        for &a in args {
            arguments.push(self.get_value(a));
        }

        BlockResult::Jump(jump.destination, arguments)
    }
}

enum BlockResult {
    Jump(sir::BlockId, Vec<Value>),
    Return(Value),
}

#[derive(Clone)]
struct External<'a> {
    interner: InternerSnapshot,
    hir_registry: &'a dyn hir::Registry,
    cfg_registry: &'a dyn reg::Registry,
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::mem;
    use crate::basic::com::Range;
    use crate::basic::mem::Interner;
    use crate::model::{hir, sir};
    use self::hir::builder::*;
    use super::Value;
    use super::super::reg::SimpleRegistry;

    #[test]
    fn add_no_arguments() {
        let mut env = Env::new();

        let a = env.push_load_int(1);
        let b = env.push_load_int(2);
        let r = env.push_call_builtin(hir::BuiltinFunction::Add, &[a, b]);
        env.push_return(r);
        env.push_block(&[]);

        assert_eq!(env.eval(&[]), Value::Int(3));
    }

    #[test]
    fn add_with_arguments() {
        let mut env = Env::new();

        let r = env.push_call_builtin(
            hir::BuiltinFunction::Add,
            &[ val_arg(0), val_arg(1) ],
        );
        env.push_return(r);
        env.push_block(&[]);

        assert_eq!(env.eval(&[val_int(1), val_int(2)]), val_int(3));
    }

    #[test]
    fn branch_if() {
        let int = hir::TypeId::int();

        for &(condition, ref result) in &[(true, val_int(1)), (false, val_int(2))] {
            let mut env = Env::new();

            {
                let condition = env.push_load_bool(condition);
                env.push_jump(1, &[]);
                env.push_jump(2, &[]);
                env.push_branch(condition);
                env.push_block(&[]);
            }

            {
                let value = env.push_load_int(1);
                env.push_jump(3, &[value]);
                env.push_block(&[]);
            }

            {
                let value = env.push_load_int(2);
                env.push_jump(3, &[value]);
                env.push_block(&[]);
            }

            {
                env.push_return(val_arg(0));
                env.push_block(&[int]);
            }

            assert_eq!(env.eval(&[]), result.clone());
        }
    }

    #[test]
    fn call_user_defined_function() {
        let mut env = Env::new();

        let id = hir::ItemIdentifier(Default::default(), range(42, 5));

        let fun = {
            let hir = env.hir();
            let (i, t) = (hir.item(), hir.type_module());
            i.fun(id, t.int()).build();
            env.module.borrow().lookup_function(id)
                .expect("Function to be registered")
        };

        {
            let a = env.push_load_int(1);
            env.push_return(a);
            env.push_block(&[]);
            env.push_graph(fun);
        }

        {
            let a = env.push_call_function(fun, hir::TypeId::int(), &[]);
            env.push_return(a);
            env.push_block(&[]);
        }

        assert_eq!(env.eval(&[]), val_int(1));
    }

    #[test]
    fn new_tuple() {
        let mut env = Env::new();

        let tup = {
            let t = env.hir().type_module();
            t.tuple().push(t.int()).push(t.int()).build()
        };

        {
            let a = env.push_load_int(1);
            let b = env.push_load_int(2);
            let c = env.push_new(tup, &[a, b]);
            env.push_return(c);
            env.push_block(&[]);
        }

        assert_eq!(env.eval(&[]), Value::Tuple(vec![val_int(1), val_int(2)]));
    }

    #[test]
    fn record_field() {
        let mut env = Env::new();

        let rec = {
            let hir = env.hir();
            let (i, t) = (hir.item(), hir.type_module());

            let name = hir::ItemIdentifier(Default::default(), range(5, 4));
            let rec = i.rec(name)
                .push(t.int())
                .push(t.int())
                .build();
            t.record(rec).build()
        };

        {
            let a = env.push_load_int(4);
            let b = env.push_load_int(42);
            let c = env.push_new(rec, &[a, b]);
            let d = env.push_field(hir::TypeId::int(), c, 1);
            env.push_return(d);
            env.push_block(&[]);
        }

        assert_eq!(env.eval(&[]), val_int(42));
    }

    #[test]
    fn return_helloworld() {
        let mut env = Env::new();

        let r = env.push_load_string(b"Hello, World!");
        env.push_return(r);
        env.push_block(&[]);

        assert_eq!(env.eval(&[]), val_string(b"Hello, World!"));
    }

    struct Env {
        interner: Interner,
        registry: SimpleRegistry,
        module: RcModule,
        tree: RcTree,
        graph: sir::Graph,
        block: sir::Block,
    }

    impl Env {
        fn new() -> Env {
            Env {
                interner: Interner::new(),
                registry: SimpleRegistry::new(),
                module: RcModule::default(),
                tree: RcTree::default(),
                graph: sir::Graph::default(),
                block: sir::Block::default(),
            }
        }

        fn eval(&self, arguments: &[Value]) -> Value {
            use std::ops::Deref;
            use super::Interpreter;

            let snapshot = self.interner.snapshot();
            let module = self.module.borrow();
            let interpreter = Interpreter::new(snapshot, module.deref(), &self.registry);
            interpreter.evaluate(&self.graph, arguments.to_vec())
        }

        fn hir(&self) -> Factory {
            Factory::new(self.module.clone(), self.tree.clone())
        }

        fn push_call_builtin(&mut self, fun: hir::BuiltinFunction, args: &[sir::ValueId]) -> sir::ValueId {
            let ty = fun.result_type_id();
            self.push_call(ty, sir::Callable::Builtin(fun), args)
        }

        fn push_call_function(
            &mut self,
            fun: hir::FunctionId,
            ty: hir::TypeId,
            args: &[sir::ValueId]
        )
            -> sir::ValueId
        {
            self.push_call(ty, sir::Callable::Function(fun), args)
        }

        fn push_call(&mut self, ty: hir::TypeId, callable: sir::Callable, args: &[sir::ValueId]) -> sir::ValueId {
            let args = self.block.push_instruction_ids(args.iter().cloned());
            self.push_instruction(sir::Instruction::Call(ty, callable, args))
        }

        fn push_field(&mut self, ty: hir::TypeId, value: sir::ValueId, index: u16) -> sir::ValueId {
            self.push_instruction(sir::Instruction::Field(ty, value, index))
        }

        fn push_load_bool(&mut self, value: bool) -> sir::ValueId {
            self.push_load(hir::BuiltinValue::Bool(value))
        }

        fn push_load_int(&mut self, i: i64) -> sir::ValueId {
            self.push_load(hir::BuiltinValue::Int(i))
        }

        fn push_load_string(&mut self, s: &[u8]) -> sir::ValueId {
            let id = self.interner.insert(s);
            self.push_load(hir::BuiltinValue::String(id))
        }

        fn push_load(&mut self, value: hir::BuiltinValue) -> sir::ValueId {
            self.push_instruction(sir::Instruction::Load(value))
        }

        fn push_new(&mut self, ty: hir::TypeId, args: &[sir::ValueId]) -> sir::ValueId {
            let args = self.block.push_instruction_ids(args.iter().cloned());
            self.push_instruction(sir::Instruction::New(ty, args))
        }

        fn push_instruction(&mut self, instr: sir::Instruction) -> sir::ValueId {
            self.block.push_instruction(instr, range(0, 0))
        }

        fn push_jump(&mut self, index: u32, args: &[sir::ValueId]) {
            let arguments = self.block.push_instruction_ids(args.iter().cloned());
            let jump = self.block.push_jump(sir::Jump {
                destination: sir::BlockId::new(index),
                arguments,
            });
            if self.block.len_jumps() == 1 {
                self.push_terminator(sir::TerminatorInstruction::Jump(jump));
            } else {
                self.push_terminator(sir::TerminatorInstruction::Unreachable);
            }
        }

        fn push_branch(&mut self, condition: sir::ValueId) {
            debug_assert!(self.block.len_jumps() > 1);
            let jumps = self.block.push_jump_ids(self.block.get_jumps());
            self.push_terminator(sir::TerminatorInstruction::Branch(condition, jumps));
        }

        fn push_return(&mut self, value: sir::ValueId) {
            debug_assert!(self.block.len_jumps() == 0);
            self.push_terminator(sir::TerminatorInstruction::Return(value));
        }

        fn push_terminator(&mut self, terminator: sir::TerminatorInstruction) {
            self.block.set_terminator(terminator, range(0, 0));
        }

        fn push_block(&mut self, arguments: &[hir::TypeId]) {
            let mut block = mem::replace(&mut self.block, sir::Block::default());
            let arguments = self.graph.push_type_ids(arguments.iter().cloned());
            block.set_arguments(arguments);
            self.graph.push_block(block);
        }

        fn push_graph(&mut self, fun: hir::FunctionId) {
            let mut graph = mem::replace(&mut self.graph, sir::Graph::default());
            graph.set_source(fun);
            self.registry.insert(fun, graph);
        }
    }

    fn val_int(i: i64) -> Value { Value::Int(i) }

    fn val_string(s: &[u8]) -> Value {
        let value = s.iter().cloned().collect();
        Value::String(value)
    }

    fn val_arg(index: u32) -> sir::ValueId {
        sir::ValueId::new_argument(index)
    }

    fn range(offset: usize, length: usize) -> Range {
        Range::new(offset, length)
    }
}
