//! Semantic pass: Type Unifying & Propagating.
//!
//! Unifies:
//! -   variants to parent enum.
//!
//! Propagates unified types to references.

use model::hir::*;
use super::{common, pat, stmt, val, Alteration, Context};

/// The Type Unifier.
///
/// Unifies inner entities.
#[derive(Clone, Debug)]
pub struct TypeUnifier<'a> {
    core: common::CoreUnifier<'a>,
}

//
//  Public Interface
//

impl<'a> TypeUnifier<'a> {
    /// Creates a new instance.
    pub fn new(context: &'a Context, registry: &'a Registry) -> Self {
        TypeUnifier {
            core: common::CoreUnifier::new(context, registry)
        }
    }

    /// Unifies the inner entities, recursively.
    pub fn unify_pattern(&self, p: Pattern, ty: Type)
        -> Alteration<Pattern>
    {
        pat::PatternUnifier::new(self.core).unify(p, ty)
    }

    /// Unifies the inner entities, recursively.
    pub fn unify_statement(&self, s: Stmt) -> Alteration<Stmt> {
        stmt::StatementUnifier::new(self.core).unify(s)
    }

    /// Unifies the inner entities, recursively.
    pub fn unify_value(&self, v: Value, ty: Type)
        -> Alteration<Value>
    {
        val::ValueUnifier::new(self.core).unify(v, ty)
    }
}
