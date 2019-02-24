//! The registry.
//!
//! This module defines a `Registry` trait, which is used by the interpreter to
//! look-up the implementation of the function calls it encounters.
//!
//! This modules also defines a `SimpleRegistry` structure, a simple
//! implementation of the `Registry` trait.

use std::collections::HashMap;

use basic::com::Span;
use model::{hir, sir};

/// Registry
///
/// A `Registry` is used by the interpreter to look-up the implementation
/// corresponding to a function.
pub trait Registry {
    /// Look-up a ControlFlowGraph.
    fn lookup_cfg(&self, key: hir::ItemIdentifier)
        -> Option<sir::ControlFlowGraph>;
}

/// SimpleRegistry
///
/// A simple implementation of a `Registry`.
pub struct SimpleRegistry {
    cfgs: HashMap<hir::ItemIdentifier, sir::ControlFlowGraph>,
}

impl SimpleRegistry {
    /// Creates an instance of the `SimpleRegistry`.
    pub fn new() -> SimpleRegistry {
        SimpleRegistry { cfgs: HashMap::new() }
    }

    /// Adds a cfg to the registry.
    pub fn insert(
        &mut self,
        id: hir::ItemIdentifier,
        cfg: sir::ControlFlowGraph,
    )
    {
        assert!(
            self.cfgs.insert(id, cfg).is_none(),
            "{} already known", id.span()
        );
    }
}

impl Registry for SimpleRegistry {
    fn lookup_cfg(&self, key: hir::ItemIdentifier)
        -> Option<sir::ControlFlowGraph>
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
