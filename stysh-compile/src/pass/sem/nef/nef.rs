//! Semantic pass: Nested Entity Fetching.

use std::cell;

use model::hir::{Gvn, Registry, Tree};

use super::{fld, typ, Context, Scope};
use super::com::*;

/// NestedEntityFetcher.
#[derive(Clone)]
pub struct NestedEntityFetcher<'a> {
    core: CoreFetcher<'a>,
}

//
//  Public interface of NestedEntityFetcher
//

impl<'a> NestedEntityFetcher<'a> {
    /// Creates a new instance.
    pub fn new(
        context: &'a Context,
        scope: &'a Scope,
        registry: &'a Registry,
        tree: &'a cell::RefCell<Tree>,
    )
        -> Self
    {
        NestedEntityFetcher { core: CoreFetcher::new(context, scope, registry, tree) }
    }

    /// Attempts to fetch all entities for this iteration.
    ///
    /// Returns the number of entities successfully fetched.
    pub fn fetch_all(&self) -> usize {
        while let Some(gvn) = self.core.context.pop_unfetched() {
            let status = self.fetch_entity(gvn);

            if status == Status::Fetched {
                self.core.context.push_fetched(gvn);
            }
        }
        self.core.context.fetched()
    }
}

//
//  Implementation of NestedEntityFetcher
//

impl<'a> NestedEntityFetcher<'a> {
    fn fetch_entity(&self, gvn: Gvn) -> Status {
        self.fetch_type(gvn)
            .combine(self.fetch_field(gvn))
    }

    fn fetch_field(&self, gvn: Gvn) -> Status {
        fld::FieldFetcher::new(self.core, gvn).fetch()
    }

    fn fetch_type(&self, gvn: Gvn) -> Status {
        typ::TypeFetcher::new(self.core, gvn).fetch()
    }
}
