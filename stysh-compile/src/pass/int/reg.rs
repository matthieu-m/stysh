//! The registry.
//!
//! This module defines a `Registry` trait, which is used by the interpreter to
//! look-up the implementation of the function calls it encounters.
//!
//! This modules also defines a `SimpleRegistry` structure, a simple
//! implementation of the `Registry` trait.

use basic::com::Span;
use basic::mem;
use model::{hir, sir};

/// Registry
///
/// A `Registry` is used by the interpreter to look-up the implementation
/// corresponding to a function.
pub trait Registry<'a> {
    /// Look-up a ControlFlowGraph.
    fn lookup_cfg(&self, key: hir::ItemIdentifier)
        -> Option<sir::ControlFlowGraph<'a>>;
}

/// SimpleRegistry
///
/// A simple implementation of a `Registry`.
pub struct SimpleRegistry<'a> {
    cfgs: mem::ArrayMap<'a, hir::ItemIdentifier, sir::ControlFlowGraph<'a>>,
}

impl<'a> SimpleRegistry<'a> {
    /// Creates an instance of the `SimpleRegistry`.
    pub fn new(arena: &'a mem::Arena) -> SimpleRegistry<'a> {
        SimpleRegistry { cfgs: mem::ArrayMap::new(arena) }
    }

    /// Adds a cfg to the registry.
    pub fn insert(
        &mut self,
        id: hir::ItemIdentifier,
        cfg: sir::ControlFlowGraph<'a>
    )
    {
        assert!(
            self.cfgs.insert(id, cfg).is_none(),
            "{} already known", id.span()
        );
    }
}

impl<'a> Registry<'a> for SimpleRegistry<'a> {
    fn lookup_cfg(&self, key: hir::ItemIdentifier)
        -> Option<sir::ControlFlowGraph<'a>>
    {
        self.cfgs.get(&key).cloned()
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
}
