//! Tree
//!
//! A stand-alone flat representation of the tree which composes an item,
//! either the body of a function, or a stand-alone expression.
//!
//! A stand-alone representation has the benefit that the Tree may be
//! understood in isolation.
//!
//! A flat representation has 3 key benefits:
//! -   Direct addressing: it is possible to keep the ID of any node.
//! -   Efficient allocation: few allocations, and compact representation.
//! -   Efficient in-place mutation.
//!
//! Furthermore, those benefits are achieved whilst retaining control over
//! mutability, allowing to "freeze" the graph at some point, unlike inner
//! mutability which persists long after it ceased to be necessary.
//!
//! Note:   this layout is inspired by the realization that ECS are a great fit
//!         for Rust.

use std::fmt;

use crate::basic::com::{IdIterator, Range};
use crate::basic::sea::{MultiTable, Table};

use crate::model::hir::{self, ItemId};

use super::*;

/// A Control Flow Graph.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Graph {
    /// The source of the ControlFlowGraph, tying it back to its HIR.
    source: hir::FunctionId,

    /// Blocks.
    block: Table<BlockId, Block>,

    /// Types.
    type_: Table<hir::TypeId, hir::Type>,

    /// Names of constructors, records and tuples.
    names: KeyedMulti<hir::ValueIdentifier>,
    /// Path components.
    paths: KeyedMulti<hir::PathComponent>,
    /// Types of enums and tuples.
    type_ids: KeyedMulti<hir::TypeId>,
}

impl Graph {
    /// Creates an instance.
    pub fn new(source: hir::FunctionId) -> Graph {
        Graph { source, .. Default::default() }
    }

    /// Returns the source of the graph.
    pub fn source(&self) -> hir::FunctionId { self.source }

    /// Sets the source of the graph.
    pub fn set_source(&mut self, source: hir::FunctionId) { self.source = source; }

    /// Returns the range spanned by the graph.
    pub fn range(&self) -> Range {
        let initial = self.block.at(&BlockId::new(0)).range();
        self.block.iter().fold(initial, |acc, b| acc.extend(b.range()))
    }


    //
    //  Blocks
    //

    /// Returns the number of blocks.
    pub fn len_blocks(&self) -> usize { self.block.len() }

    /// Returns the blocks.
    pub fn get_blocks(&self) -> IdIterator<BlockId> {
        IdIterator::new(0, self.block.len() as u32)
    }

    /// Returns the block.
    pub fn get_block(&self, id: BlockId) -> &Block { self.block.at(&id) }

    /// Inserts a new block.
    ///
    /// Returns the BlockId created for it.
    pub fn push_block(&mut self, block: Block) -> BlockId {
        self.block.extend(block)
    }


    //
    //  Types
    //

    /// Initializes all types.
    pub fn initialize_types(&mut self, tree: &hir::Tree) {
        for ty in 0..tree.len_types() as u32 {
            let ty = hir::TypeId::new(ty);
            self.type_.extend(tree.get_type(ty));
        }

        for n in 0..tree.len_names() as u32 {
            self.names.create(tree.get_names(Id::new(n)).iter().cloned());
        }

        for p in 0..tree.len_paths() as u32 {
            self.paths.create(tree.get_path(Id::new(p)).iter().cloned());
        }

        for t in 0..tree.len_type_ids() as u32 {
            self.type_ids.create(tree.get_type_ids(Id::new(t)).iter().cloned());
        }
    }

    /// Returns the names associated to the id.
    pub fn get_names(&self, id: Id<[hir::ValueIdentifier]>) -> &[hir::ValueIdentifier] {
        if id.is_empty() { &[] } else { self.names.get(&id) }
    }

    /// Returns the path associated to the id.
    pub fn get_path(&self, id: Id<[hir::PathComponent]>) -> &[hir::PathComponent] {
        if id.is_empty() { &[] } else { self.paths.get(&id) }
    }

    /// Returns the number of types.
    pub fn len_types(&self) -> usize { self.type_.len() }

    /// Returns the type.
    pub fn get_type(&self, id: hir::TypeId) -> hir::Type { *self.type_.at(&id) }

    /// Returns the types associated to the id.
    pub fn get_type_ids(&self, id: Id<[hir::TypeId]>) -> &[hir::TypeId] {
        if id.is_empty() { &[] } else { self.type_ids.get(&id) }
    }

    /// Inserts a new array of types.
    ///
    /// Returns the id created for it.
    pub fn push_type_ids<I>(&mut self, type_ids: I) -> Id<[hir::TypeId]>
        where
            I: IntoIterator<Item = hir::TypeId>,
    {
        self.type_ids.create(type_ids).unwrap_or(Id::empty())
    }
}


/// Block
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Block {
    //
    //  Top-level
    //

    /// Arguments.
    arguments: Id<[hir::TypeId]>,
    /// Instructions.
    instruction: Table<ValueId, Instruction>,
    /// Range associated to a given instruction.
    instruction_range: Table<ValueId, Range>,
    /// Terminators.
    terminator: TerminatorInstruction,
    /// Range associated to a given terminator.
    terminator_range: Range,

    //
    //  Satellites.
    //

    /// Jumps.
    jump: Table<JumpId, Jump>,

    /// Instructions.
    instruction_ids: KeyedMulti<ValueId>,
    /// Multi-branch Jumps.
    jump_ids: KeyedMulti<JumpId>,
}

impl Block {
    /// Creates a new instance.
    pub fn new() -> Self { Self::default() }

    /// Returns the range of the block.
    pub fn range(&self) -> Range {
        if let Some(last) = self.instruction_range.iter().last() {
            last.extend(self.terminator_range)
        } else {
            self.terminator_range
        }
    }

    //
    //  Arguments
    //

    /// Returns the arguments.
    pub fn get_arguments(&self) -> Id<[hir::TypeId]> { self.arguments }

    /// Sets the arguments.
    pub fn set_arguments(&mut self, arguments: Id<[hir::TypeId]>) {
        self.arguments = arguments;
    }


    //
    //  Instructions
    //

    /// Returns the number of instructions.
    pub fn len_instructions(&self) -> usize { self.instruction.len() }

    /// Returns the instructions.
    pub fn get_instructions(&self) -> IdIterator<Instruction> {
        IdIterator::new(0, self.instruction.len() as u32)
    }

    /// Returns the instruction.
    pub fn get_instruction(&self, id: ValueId) -> Instruction {
        *self.instruction.at(&id)
    }

    /// Returns the range associated to an instruction.
    pub fn get_instruction_range(&self, id: ValueId) -> Range {
        *self.instruction_range.at(&id)
    }

    /// Inserts a new instruction.
    ///
    /// Returns the ValueId created for it.
    pub fn push_instruction(&mut self, instruction: Instruction, range: Range)
        -> ValueId
    {
        debug_assert!(self.instruction.len() == self.instruction_range.len());

        let id = self.instruction_range.extend(range);
        self.instruction.push(&id, instruction);

        id
    }


    //
    //  Terminator
    //

    /// Returns the terminator.
    pub fn get_terminator(&self) -> TerminatorInstruction { self.terminator }

    /// Returns the range associated to the terminator.
    pub fn get_terminator_range(&self) -> Range { self.terminator_range }

    /// Sets the terminator.
    pub fn set_terminator(&mut self, terminator: TerminatorInstruction, range: Range) {
        self.terminator = terminator;
        self.terminator_range = range;
    }


    //
    //  Satellites
    //

    /// Returns the number of jumps.
    pub fn len_jumps(&self) -> usize { self.jump.len() }

    /// Returns the jumps.
    pub fn get_jumps(&self) -> IdIterator<Jump> {
        IdIterator::new(0, self.jump.len() as u32)
    }

    /// Returns the jump.
    pub fn get_jump(&self, id: JumpId) -> Jump { *self.jump.at(&id) }

    /// Inserts a jump.
    ///
    /// Returns the id created for it.
    pub fn push_jump(&mut self, jump: Jump) -> JumpId { self.jump.extend(jump) }

    /// Returns the values associated to the id.
    pub fn get_instruction_ids(&self, id: Id<[ValueId]>) -> &[ValueId] {
        if id.is_empty() { &[] } else { self.instruction_ids.get(&id) }
    }

    /// Inserts a new array of values.
    ///
    /// Returns the id created for it.
    pub fn push_instruction_ids<I>(&mut self, instruction_ids: I) -> Id<[ValueId]>
        where
            I: IntoIterator<Item = ValueId>,
    {
        self.instruction_ids.create(instruction_ids).unwrap_or(Id::empty())
    }

    /// Returns the values associated to the id.
    pub fn get_jump_ids(&self, id: Id<[JumpId]>) -> &[JumpId] {
        if id.is_empty() { &[] } else { self.jump_ids.get(&id) }
    }

    /// Inserts a new array of values.
    ///
    /// Returns the id created for it.
    pub fn push_jump_ids<I>(&mut self, jump_ids: I) -> Id<[JumpId]>
        where
            I: IntoIterator<Item = JumpId>,
    {
        self.jump_ids.create(jump_ids).unwrap_or(Id::empty())
    }
}


//
//  Display
//

/// Displays the graph in a human readable form.
pub fn display_graph(graph: &Graph, registry: &dyn hir::Registry) -> String {
    let displayer = GraphDisplayer{ graph, registry };
    
    format!("{}", displayer)
}

struct GraphDisplayer<'a> {
    graph: &'a Graph,
    registry: &'a dyn hir::Registry,
}

impl<'a> fmt::Display for GraphDisplayer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let (graph, registry) = (self.graph, self.registry);

        for (index, block) in graph.block.iter().enumerate() {
            let block = BlockDisplayer { block, graph, registry };
            write!(f, "{} ", index)?;
            block.display(f)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}

struct BlockDisplayer<'a> {
    block: &'a Block,
    graph: &'a Graph,
    registry: &'a dyn hir::Registry,
}

impl<'a> BlockDisplayer<'a> {
    fn display(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.display_tuple_types(self.block.arguments, f)?;
        write!(f, ":")?;
        self.display_instructions(f)?;
        write!(f, "\n")
    }

    fn display_instructions(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "\n")?;
        for (index, instruction) in self.block.get_instructions().enumerate() {
            write!(f, "    ${} := ", index)?;
            self.display_instruction(instruction, f)?;
            write!(f, "\n")?;
        }
        write!(f, "    ")?;
        self.display_terminator(f)
    }

    fn display_instruction(&self, id: ValueId, f: &mut fmt::Formatter)
        -> Result<(), fmt::Error>
    {
        use self::Instruction::*;

        match self.block.get_instruction(id) {
            Call(_, fun, args) => {
                self.display_callable(fun, f)?;
                self.display_arguments(args, f)?;
            },
            Field(_, val, i) => write!(f, "field {} of {}", i, val)?,
            Load(val) => write!(f, "load {}", val)?,
            New(ty, args) => {
                write!(f, "new ")?;
                self.display_type(ty, f)?;
                write!(f, " ")?;
                self.display_arguments(args, f)?;
            },
        };

        let range = self.block.get_instruction_range(id);
        write!(f, " ; {}", range)
    }

    fn display_terminator(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::TerminatorInstruction::*;

        match self.block.terminator {
            Branch(value, jumps) => {
                write!(f, "branch {} in [", value)?;
                for (i, &jump) in self.block.get_jump_ids(jumps).iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{} => ", i)?;
                    self.display_jump(jump, f)?;
                }
                write!(f, "]")
            },
            Jump(jump) => {
                write!(f, "jump ")?;
                self.display_jump(jump, f)
            },
            Return(v) => write!(f, "return {}", v),
            Unreachable => write!(f, "unreachable"),
        }
    }

    fn display_arguments(&self, arguments: Id<[ValueId]>, f: &mut fmt::Formatter)
        -> Result<(), fmt::Error>
    {
        let arguments = self.block.get_instruction_ids(arguments);
        write!(f, "(")?;

        for (i, v) in arguments.iter().enumerate() {
            if i != 0 { write!(f, ", ")?; }
            write!(f, "{}", v)?;
        }

        write!(f, ")")
    }

    fn display_callable(&self, callable: Callable, f: &mut fmt::Formatter)
        -> Result<(), fmt::Error>
    {
        use self::Callable::*;

        match callable {
            Builtin(b) => write!(f, "{}", b),
            Function(fun) => {
                let fun = self.registry.get_function(fun);
                write!(f, "<{}>", fun.name.1)
            },
        }
    }

    fn display_jump(&self, jump: JumpId, f: &mut fmt::Formatter)
        -> Result<(), fmt::Error>
    {
        let jump = self.block.get_jump(jump);

        write!(f, "<{}> ", jump.destination.value())?;
        self.display_arguments(jump.arguments, f)
    }

    fn display_type(&self, ty: hir::TypeId, f: &mut fmt::Formatter)
        -> Result<(), fmt::Error>
    {
        use self::hir::Type::*;

        if let Some(b) = ty.builtin() {
            return write!(f, "{}", b);
        }

        let type_ = if ty.is_tree() {
            println!("display_type - {:?}", ty);
            self.graph.get_type(ty)
        } else {
            self.registry.get_type(ty)
        };

        match type_ {
            Builtin(b) => write!(f, "{}", b),
            Enum(id, _) => write!(f, "<{}>", self.registry.get_enum(id).name.1),
            Rec(id, _) => write!(f, "<{}>", self.registry.get_record(id).name.1),
            Tuple(tup) => self.display_tuple_types(tup.fields, f),
            Unresolved(..) => unimplemented!("Unresolved Type!"),
        }
    }

    fn display_tuple_types(&self, tuple: Id<[hir::TypeId]>, f: &mut fmt::Formatter)
        -> Result<(), fmt::Error>
    {
        write!(f, "(")?;

        for (index, &ty) in self.graph.get_type_ids(tuple).iter().enumerate() {
            if index > 0 { write!(f, ", ")?; }
            self.display_type(ty, f)?;
        }

        write!(f, ")")
    }
}

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let prefix = if self.is_argument() { '@' } else { '$' };
        let index =
            self.as_argument().unwrap_or_else(
                || self.as_instruction().unwrap()
            );
        write!(f, "{}{}", prefix, index)
    }
}


//
//  Private Types
//

type KeyedMulti<T> = MultiTable<Id<[T]>, T>;
