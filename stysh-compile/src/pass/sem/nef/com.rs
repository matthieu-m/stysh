//! Core Nested Entity Fetcher

use basic::mem;

use model::hir::*;

use super::Context;

/// Core Fetcher
#[derive(Clone, Copy, Debug)]
pub struct CoreFetcher<'a, 'g>
    where 'g: 'a
{
    /// Context.
    pub context: &'a Context<'g>,
    /// Registry.
    pub registry: &'a Registry<'g>,
    /// Arena.
    pub global_arena: &'g mem::Arena,
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

impl<'a, 'g> CoreFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(
        context: &'a Context<'g>,
        registry: &'a Registry<'g>,
        global_arena: &'g mem::Arena,
    )
        -> Self
    {
        CoreFetcher { registry, context, global_arena }
    }

    /// Inserts an item into the arena.
    pub fn insert<T: 'g>(&self, e: T) -> &'g T {
        self.global_arena.insert(e)
    }
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
