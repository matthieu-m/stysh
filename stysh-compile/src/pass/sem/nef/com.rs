//! Core Nested Entity Fetcher

use std::cell;

use model::hir::*;

use super::Context;

/// Core Fetcher
#[derive(Clone, Copy, Debug)]
pub struct CoreFetcher<'a> {
    /// Context.
    pub context: &'a Context,
    /// Registry.
    pub registry: &'a Registry,
    /// Tree.
    pub tree: &'a cell::RefCell<Tree>,
}

/// Status
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Status {
    /// Fetched, whether already fetched or freshly fetched.
    Fetched,
    /// Unfetched, still, though progress may have been made.
    Unfetched,
}

//
//  Public interface of CoreFetcher
//

impl<'a> CoreFetcher<'a> {
    /// Creates a new instance.
    pub fn new(
        context: &'a Context,
        registry: &'a Registry,
        tree: &'a cell::RefCell<Tree>,
    )
        -> Self 
    {
        CoreFetcher { context, registry, tree }
    }

    /// Returns a reference to the Tree.
    pub fn tree(&self) -> cell::Ref<'a, Tree> { self.tree.borrow() }

    /// Returns a mutable reference to the Tree.
    pub fn tree_mut(&self) -> cell::RefMut<'a, Tree> { self.tree.borrow_mut() }
}

//
//  Public interface of Status
//

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
