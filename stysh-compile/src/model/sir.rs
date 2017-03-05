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

use basic::com;
use model::sem;

/// A Control Flow Graph.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ControlFlowGraph<'a> {
    /// The list of basic blocks of this graph.
    ///
    /// Note: the basic block refer to each others by indices in this list.
    pub blocks: &'a [BasicBlock<'a>]
}

/// A Basic Block.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BasicBlock<'a> {
    /// The list of instructions of this basic block.
    pub instructions: &'a [Instruction<'a>],
    /// The last instruction of this basic block.
    pub exit: TerminatorInstruction,
}

/// A unique identifier for a basic block in a control flow graph.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct BlockId(u32);

/// A unique identifier for an argument or instruction result in a basic block.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ValueId(u16);

/// An instruction.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Instruction<'a> {
    /// A function call.
    CallFunction(sem::BuiltinFunction, &'a [ValueId], com::Range),
    /// A value load.
    Load(sem::BuiltinValue, com::Range),
}

/// An instruction.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum TerminatorInstruction {
    /// Return the control to the caller.
    Return(ValueId),
    /// Unreachable, this may occur as a result of optimizations.
    Unreachable
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
