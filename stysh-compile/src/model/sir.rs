//! Stysh Intermediate Representation, aka Control Flow Graph.
//!
//! This is the model describing the execution semantics of the program. It uses
//! a variant of Static Single Assignment.
//!
//! Normally, in SSA form, the function is decomposed in Basic Blocks:
//! -   each Basic Block has a single point of entry and exit,
//! -   variable names are scoped within the function,
//! -   and Phi nodes are used upon entering a Basic Block to reconcile
//!     converging branches.
//!
//! The SIR conserves the idea of Basic Blocks with a single entry/exit, but:
//! -   makes names local to each Basic Block,
//! -   replaces Phi Nodes on entry by instead having Basic Block accept
//!     a set of inputs.
//!
//! This requires building the SIR backward (starting from diverging BBs),
//! however the representation seems to have several advantages over regular
//! SSA because it promotes local reasoning: each BB can be checked in near
//! complete isolation, with only the exit branch having to check a few other
//! BBs to ensure the result being well-formed.
//!
//! There are two importants points about the SIR:
//! -   only executable code is lowered down, constants and types are not,
//!     this means expressions (for repl) and functions,
//! -   SIR is target-agnostic, thus compile-time functions have the same result
//!     for all supported targets unless explicitly fiddling with target
//!     specific functions (such as `std::mem::size_of`).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

use std;

use basic::com;
use basic::mem::DynArray;

use model::hir;

/// A Control Flow Graph.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ControlFlowGraph {
    /// The list of basic blocks of this graph.
    ///
    /// Note: the basic block refer to each others by indices in this list.
    pub blocks: DynArray<BasicBlock>
}

/// A Basic Block.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BasicBlock {
    /// The list of arguments of this basic block.
    pub arguments: DynArray<hir::TypeDefinition>,
    /// The list of instructions of this basic block.
    pub instructions: DynArray<Instruction>,
    /// The last instruction of this basic block.
    pub exit: TerminatorInstruction,
}

/// A unique identifier for a basic block in a control flow graph.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BlockId(u32);

/// A unique identifier for an argument or instruction result in a basic block.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ValueId(u16);

/// An instruction.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Instruction {
    /// A function call.
    Call(Callable, DynArray<ValueId>, com::Range),
    /// A field load.
    Field(hir::TypeDefinition, ValueId, u16, com::Range),
    /// A value load.
    Load(hir::BuiltinValue, com::Range),
    /// A composite creation.
    New(hir::TypeDefinition, DynArray<ValueId>, com::Range),
}

/// A Callable.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Callable {
    /// A Builtin.
    Builtin(hir::BuiltinFunction),
    /// User-defined.
    Function(hir::FunctionProto),
}

/// An instruction.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum TerminatorInstruction {
    /// Branch instruction, the destination of which is determined by the value
    /// of its ValueId which MUST represent a valid index into the array.
    ///
    /// Note:   the array should contain at least two elements:
    ///         -   no element would make the index necessarily invalid,
    ///         -   a single element is better represent by the "Jump" case.
    ///
    /// Note:   for ease of use, a boolean can be used to index a 2 elements
    ///         array, in which case "true" maps to 0 and "false" to 1.
    Branch(ValueId, DynArray<Jump>),
    /// Unconditional jump to another block.
    Jump(Jump),
    /// Return the control to the caller.
    Return(ValueId),
    /// Unreachable, this may occur as a result of optimizations.
    Unreachable
}

/// A jump to another block.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Jump {
    /// ID of the block to jump to.
    pub dest: BlockId,
    /// Arguments to pass to the block.
    pub arguments: DynArray<ValueId>,
}

impl ControlFlowGraph {
    /// Returns the range spanned by the graph.
    pub fn range(&self) -> com::Range {
        let initial = self.blocks.at(0).range();
        self.blocks.iter().fold(initial, |acc, b| acc.extend(b.range()))
    }
}

impl BasicBlock {
    /// Returns the range spanned by the block.
    pub fn range(&self) -> com::Range {
        let initial = self.instructions.at(0).range();
        self.instructions
            .iter()
            .fold(initial, |acc, instr| acc.extend(instr.range()))
    }
}

impl Instruction {
    /// Returns the range spanned by the instruction.
    pub fn range(&self) -> com::Range {
        use self::Instruction::*;

        match *self {
            Call(_, _, r) => r,
            Field(_, _, _, r) => r,
            Load(_, r) => r,
            New(_, _, r) => r,
        }
    }

    /// Returns the resulting type of the instruction.
    pub fn result_type(&self) -> hir::TypeDefinition {
        use self::Instruction::*;

        match self {
            Call(fun, _, _) => fun.result_type(),
            Field(type_, _, _, _) => type_.clone(),
            Load(builtin, _) => type_of_value(*builtin),
            New(type_, _, _) => type_.clone(),
        }
    }
}

impl Callable {
    /// Returns the type of the result of the function.
    pub fn result_type(&self) -> hir::TypeDefinition {
        use self::Callable::*;

        match self {
            Builtin(fun) => type_of_function(*fun),
            Function(fun) => fun.result.clone(),
        }
    }
}

impl BlockId {
    /// Returns a BlockId from a block index.
    pub fn new(index: usize) -> BlockId { BlockId(index as u32) }

    /// Returns the index of the block.
    pub fn index(&self) -> usize { self.0 as usize }
}

impl ValueId {
    /// Returns a ValueId from an argument index.
    pub fn new_argument(index: usize) -> ValueId {
        ValueId(index as u16 | VALUE_ID_ARGUMENT_MASK)
    }

    /// Returns a ValueId from an instruction index.
    pub fn new_instruction(index: usize) -> ValueId { ValueId(index as u16) }

    /// Returns true if and only if this ValueId indexes an argument.
    pub fn is_argument(&self) -> bool { self.0 & VALUE_ID_ARGUMENT_MASK != 0 }

    /// Returns true if and only if this ValueId indexes an instruction.
    pub fn is_instruction(&self) -> bool { !self.is_argument() }

    /// Returns the index of the argument this ValueId represents, or None.
    pub fn as_argument(&self) -> Option<usize> {
        if self.is_argument() {
            Some((self.0 - VALUE_ID_ARGUMENT_MASK) as usize)
        } else {
            None
        }
    }

    /// Returns the index of the instruction this ValueId represents, or None.
    pub fn as_instruction(&self) -> Option<usize> {
        if self.is_instruction() {
            Some(self.0 as usize)
        } else {
            None
        }
    }
}

//
//  Implementation Details
//
const VALUE_ID_ARGUMENT_MASK: u16 = 1u16 << 15;

fn type_of_function(b: hir::BuiltinFunction) -> hir::TypeDefinition {
    use self::hir::BuiltinFunction::*;

    match b {
        Add | FloorDivide | Multiply | Substract => hir::TypeDefinition::int(),
        And | Differ | Equal | GreaterThan | GreaterThanOrEqual |
        LessThan | LessThanOrEqual | Not | Or | Xor => hir::TypeDefinition::bool_(),
    }
}

fn type_of_value(b: hir::BuiltinValue) -> hir::TypeDefinition {
    use self::hir::BuiltinValue::*;

    match b {
        Bool(_) => hir::TypeDefinition::bool_(),
        Int(_) => hir::TypeDefinition::int(),
        String(_) => hir::TypeDefinition::string(),
    }
}

impl std::fmt::Display for ControlFlowGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for (index, block) in self.blocks.iter().enumerate() {
            write!(f, "{} (", index)?;

            for (i, type_) in block.arguments.iter().enumerate() {
                if i != 0 { write!(f, ", ")?; }
                write!(f, "{}", type_)?;
            }

            write!(f, "):{}\n", block)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "\n")?;
        for (index, instr) in self.instructions.iter().enumerate() {
            write!(f, "    ${} := {}\n", index, instr)?;
        }
        write!(f, "    {}\n", self.exit)
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::Call(fun, vals, r) => {
                write!(f, "{}(", fun)?;
                for (i, v) in vals.iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, ") ; {}", r)
            },
            Instruction::Field(_, val, i, r) => {
                write!(f, "field {} of {} ; {}", i, val, r)
            },
            Instruction::Load(val, r) => {
                write!(f, "load {} ; {}", val, r)
            },
            Instruction::New(type_, vals, r) => {
                write!(f, "new {} (", type_)?;
                for (i, v) in vals.iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, ") ; {}", r)
            },
        }
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use self::Callable::*;

        match self {
            Builtin(b) => write!(f, "{}", b),
            Function(fun) => write!(f, "{}", fun.name),
        }
    }
}

impl std::fmt::Display for TerminatorInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use self::TerminatorInstruction::*;

        match self {
            Branch(value, jumps) => {
                write!(f, "branch {} in [", value)?;
                for (i, j) in jumps.iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{} => {}", i, j)?;
                }
                write!(f, "]")
            },
            Jump(jump) => write!(f, "jump {}", jump),
            Return(v) => write!(f, "return {}", v),
            Unreachable => write!(f, "unreachable"),
        }
    }
}

impl std::fmt::Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "<{}> (", self.dest.0)?;
        for (i, a) in self.arguments.iter().enumerate() {
            if i != 0 { write!(f, ", ")?; }
            write!(f, "{}", a)?;
        }
        write!(f, ")")
    }
}

impl std::fmt::Display for ValueId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let prefix = if self.is_argument() { '@' } else { '$' };
        let index =
            self.as_argument().unwrap_or_else(
                || self.as_instruction().unwrap()
            );
        write!(f, "{}{}", prefix, index)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::ValueId;

    #[test]
    fn value_id_argument() {
        let arg = ValueId::new_argument(3);

        assert_eq!(arg.is_argument(), true);
        assert_eq!(arg.as_argument(), Some(3));

        assert_eq!(arg.is_instruction(), false);
        assert_eq!(arg.as_instruction(), None);
    }

    #[test]
    fn value_id_instruction() {
        let arg = ValueId::new_instruction(3);

        assert_eq!(arg.is_instruction(), true);
        assert_eq!(arg.as_instruction(), Some(3));

        assert_eq!(arg.is_argument(), false);
        assert_eq!(arg.as_argument(), None);
    }
}
