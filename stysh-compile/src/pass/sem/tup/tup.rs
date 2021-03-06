//! Semantic pass: Type Unifying & Propagating.
//!
//! Unifies:
//! -   variants to parent enum.
//!
//! Propagates unified types to references.

use std::cell;

use crate::model::hir::{self, *};
use super::{common, expr, pat, Context};

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
    pub fn new(
        context: &'a Context,
        registry: &'a dyn hir::Registry,
        tree: &'a cell::RefCell<hir::Tree>,
    )
        -> Self
    {
        TypeUnifier {
            core: common::CoreUnifier::new(context, registry, tree)
        }
    }

    /// Finalizes the Tree by inserting Implicit casts as appropriate.
    pub fn finalize(&self) {
        let expressions = self.core.tree().get_expressions();
        for e in expressions {
            self.finalize_expression(e);
        }
    }

    /// Attempts to unify all entities for this iteration.
    pub fn unify_all(&self) {
        while let Some(gvn) = self.core.context.pop_diverging() {
            let status = self.unify_entity(gvn);

            if status == common::Status::Unified {
                self.core.context.push_unified(gvn);
            }
        }
    }
}

//
//  Implementation of TypeUnifier
//

impl<'a> TypeUnifier<'a> {
    fn finalize_expression(&self, e: ExpressionId) {
        expr::ExprUnifier::new(self.core).finalize(e)
    }

    fn unify_entity(&self, gvn: Gvn) -> common::Status {
        if let Some(e) = gvn.as_expression() {
            self.unify_expression(e)
        } else if let Some(p) = gvn.as_pattern() {
            self.unify_pattern(p)
        } else {
            panic!("Neither expression nor pattern: {:?}", gvn)
        }
    }

    fn unify_expression(&self, e: ExpressionId) -> common::Status {
        expr::ExprUnifier::new(self.core).unify(e)
    }

    fn unify_pattern(&self, p: PatternId) -> common::Status {
        pat::PatternUnifier::new(self.core).unify(p)
    }
}
