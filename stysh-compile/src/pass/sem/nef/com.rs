//! Core Nested Entity Fetcher

use model::hir::*;

use super::Context;

/// Core Fetcher
#[derive(Clone, Copy, Debug)]
pub struct CoreFetcher<'a> {
    /// Context.
    pub context: &'a Context,
    /// Registry.
    pub registry: &'a Registry,
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
    pub fn new(context: &'a Context, registry: &'a Registry) -> Self {
        CoreFetcher { registry, context }
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
