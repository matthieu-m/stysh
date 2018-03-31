//! Semantic pass: Type Unifying & Propagating.
//!
//! Unifies:
//! -   variants to parent enum.
//!
//! Propagates unified types to references.

use basic::mem;

use model::hir::*;
use super::{common, pat, stmt, val, Alteration, Context};

/// The Type Unifier.
///
/// Unifies inner entities.
#[derive(Clone, Debug)]
pub struct TypeUnifier<'a, 'g>
    where 'g: 'a 
{
    core: common::CoreUnifier<'a, 'g>,
}

//
//  Public Interface
//

impl<'a, 'g> TypeUnifier<'a, 'g>
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
        TypeUnifier {
            core: common::CoreUnifier::new(registry, context, global_arena)
        }
    }

    /// Unifies the inner entities, recursively.
    pub fn unify_pattern(&self, p: Pattern<'g>, ty: Type<'g>)
        -> Alteration<Pattern<'g>>
    {
        pat::PatternUnifier::new(self.core).unify(p, ty)
    }

    /// Unifies the inner entities, recursively.
    pub fn unify_statement(&self, s: Stmt<'g>) -> Alteration<Stmt<'g>> {
        stmt::StatementUnifier::new(self.core).unify(s)
    }

    /// Unifies the inner entities, recursively.
    pub fn unify_value(&self, v: Value<'g>, ty: Type<'g>)
        -> Alteration<Value<'g>>
    {
        val::ValueUnifier::new(self.core).unify(v, ty)
    }
}
