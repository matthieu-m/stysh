//! Core Nested Entity Fetcher

use std::cell;

use crate::model::hir::*;

use super::{Context, RegRef, Scope};

/// Core Fetcher
#[derive(Clone, Copy, Debug)]
pub struct CoreFetcher<'a> {
    /// Context.
    pub context: &'a Context,
    /// Scope.
    pub scope: &'a dyn Scope,
    /// Registry.
    pub registry: &'a dyn Registry,
    /// Tree.
    pub tree: &'a cell::RefCell<Tree>,
}

impl<'a> CoreFetcher<'a> {
    /// Creates a new instance.
    pub fn new(
        context: &'a Context,
        scope: &'a dyn Scope,
        registry: &'a dyn Registry,
        tree: &'a cell::RefCell<Tree>,
    )
        -> Self 
    {
        CoreFetcher { context, scope, registry, tree }
    }

    /// Returns a reference to the unified Registry.
    pub fn registry(&self) -> RegRef<'a> {
        RegRef::new(self.registry, self.tree())
    }

    /// Returns a reference to the Tree.
    pub fn tree(&self) -> cell::Ref<'a, Tree> { self.tree.borrow() }

    /// Returns a mutable reference to the Tree.
    pub fn tree_mut(&self) -> cell::RefMut<'a, Tree> { self.tree.borrow_mut() }
}

/// Status
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Status {
    /// Fetched, whether already fetched or freshly fetched.
    Fetched,
    /// Unfetched, still, though progress may have been made.
    Unfetched,
}

impl Status {
    /// Combine two statuses.
    pub fn combine(self, other: Status) -> Status {
        use self::Status::*;

        match (self, other) {
            (Fetched, Fetched) => Fetched,
            _ => Unfetched,
        }
    }
}
