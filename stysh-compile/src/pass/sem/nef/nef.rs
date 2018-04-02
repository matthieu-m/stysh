//! Semantic pass: Nested Entity Fetching.

use basic::mem;

use model::hir::Registry;

use super::{flat, fld, typ, Context};
use super::com::*;

/// NestedEntityFetcher.
#[derive(Clone)]
pub struct NestedEntityFetcher<'a, 'g: 'a> {
    core: CoreFetcher<'a, 'g>,
}

//
//  Public interface of NestedEntityFetcher
//

impl<'a, 'g> NestedEntityFetcher<'a, 'g> {
    /// Creates a new instance.
    pub fn new(
        context: &'a Context<'g>,
        registry: &'a Registry<'g>,
        global_arena: &'g mem::Arena,
    )
        -> Self
    {
        NestedEntityFetcher {
            core: CoreFetcher::new(context, registry, global_arena)
        }
    }

    /// Fetch nested entities from the context.
    pub fn fetch_all(&self) {
        while let Some(e) = self.core.context.pop_unfetched() {
            let status = self.fetch_entity(e);

            if status != Status::Fetched {
                self.core.context.push_unfetched(e);
            }
        }
    }
}

//
//  Implementation of NestedEntityFetcher
//

impl<'a, 'g> NestedEntityFetcher<'a, 'g> {
    fn fetch_entity(&self, e: flat::ValueHandle<'a, 'g>) -> Status {
        self.fetch_type(e)
            .combine(self.fetch_field(e))
    }

    fn fetch_field(&self, e: flat::ValueHandle<'a, 'g>) -> Status {
        fld::FieldFetcher::new(self.core, e).fetch()
    }

    fn fetch_type(&self, e: flat::ValueHandle<'a, 'g>) -> Status {
        typ::TypeFetcher::new(self.core, e).fetch()
    }
}
