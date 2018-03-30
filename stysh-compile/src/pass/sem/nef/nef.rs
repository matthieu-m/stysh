//! Semantic pass: Nested Entity Fetching.
//!
//! Fetches:
//! -   Compound entities,
//! -   Fields,
//! -   Methods.

use basic::mem;

use model::hir::*;
use super::{common, pat, stmt, typ, val, Context, Resolution};

/// The Nested Entity Fetcher.
///
/// Fetches inner entities.
#[derive(Clone, Debug)]
pub struct NestedEntityFetcher<'a, 'g>
    where 'g: 'a
{
    core: common::CoreFetcher<'a, 'g>,
}

//
//  Public Interface
//

impl<'a, 'g> NestedEntityFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(
        registry: &'a Registry<'g>,
        context: &'a Context<'g>,
        global_arena: &'g mem::Arena,
    )
        -> Self
    {
        NestedEntityFetcher {
            core: common::CoreFetcher::new(registry, context, global_arena),
        }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch_pattern(&self, p: Pattern<'g>)
        -> Resolution<Pattern<'g>>
    {
        pat::PatternFetcher::new(self.core).fetch(p)
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch_statement(&self, s: Stmt<'g>) -> Resolution<Stmt<'g>> {
        stmt::StatementFetcher::new(self.core).fetch(s)
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch_type(&self, t: Type<'g>) -> Resolution<Type<'g>> {
        typ::TypeFetcher::new(self.core).fetch(t)
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch_value(&self, v: Value<'g>) -> Resolution<Value<'g>> {
        val::ValueFetcher::new(self.core).fetch(v)
    }
}
