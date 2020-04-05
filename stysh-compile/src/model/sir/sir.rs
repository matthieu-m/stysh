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

pub use crate::basic::com::Id;

use crate::model::hir;
use super::Block;

/// Block ID.
pub type BlockId = Id<Block>;
/// Jump ID.
pub type JumpId = Id<Jump>;
/// Value ID.
pub type ValueId = Id<Instruction>;

/// An instruction.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Instruction {
    /// A function call.
    Call(hir::TypeId, Callable, Id<[ValueId]>),
    /// A cast to an interface.
    Cast(hir::TypeId, ValueId),
    /// A field load.
    Field(hir::TypeId, ValueId, u16),
    /// A value load.
    Load(hir::BuiltinValue),
    /// A composite creation.
    New(hir::TypeId, Id<[ValueId]>),
}

/// A Callable.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Callable {
    /// A Builtin.
    Builtin(hir::BuiltinFunction),
    /// User-defined, statically resolved.
    Function(hir::FunctionId),
    /// User-defined, dynamically resolved.
    Method(hir::InterfaceId, hir::FunctionId),
}

/// An instruction.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
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
    Branch(ValueId, Id<[JumpId]>),
    /// Unconditional jump to another block.
    Jump(JumpId),
    /// Return the control to the caller.
    Return(ValueId),
    /// Unreachable, this may occur as a result of optimizations.
    Unreachable
}

impl Default for TerminatorInstruction {
    fn default() -> Self { TerminatorInstruction::Unreachable }
}

/// A jump to another block.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Jump {
    /// ID of the block to jump to.
    pub destination: BlockId,
    /// Arguments to pass to the block.
    pub arguments: Id<[ValueId]>,
}

impl Instruction {
    /// Returns the Type of the instruction.
    pub fn result_type_id(&self) -> hir::TypeId {
        use self::Instruction::*;

        match *self {
            Call(ty, ..) | Cast(ty, ..) | Field(ty, ..) | New(ty, ..) => ty,
            Load(b) => b.result_type_id(),
        }
    }
}

impl ValueId {
    /// Returns a ValueId from an argument index.
    pub fn new_argument(index: u32) -> ValueId {
        ValueId::new(index + VALUE_ID_ARGUMENT_MASK)
    }

    /// Returns a ValueId from an instruction index.
    pub fn new_instruction(index: u32) -> ValueId { ValueId::new(index) }

    /// Returns true if and only if this ValueId indexes an argument.
    pub fn is_argument(&self) -> bool { self.value() >= VALUE_ID_ARGUMENT_MASK }

    /// Returns true if and only if this ValueId indexes an instruction.
    pub fn is_instruction(&self) -> bool { !self.is_argument() }

    /// Returns the index of the argument this ValueId represents, or None.
    pub fn as_argument(&self) -> Option<u32> {
        if self.is_argument() {
            Some(self.value() - VALUE_ID_ARGUMENT_MASK)
        } else {
            None
        }
    }

    /// Returns the index of the instruction this ValueId represents, or None.
    pub fn as_instruction(&self) -> Option<u32> {
        if self.is_instruction() {
            Some(self.value())
        } else {
            None
        }
    }
}

//
//  Implementation Details
//
const VALUE_ID_ARGUMENT_MASK: u32 = 1u32 << 31;

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
