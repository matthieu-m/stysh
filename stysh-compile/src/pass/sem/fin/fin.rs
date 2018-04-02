//! Semantic pass: Graph Finalizing.
//!
//! Rewrites the graph by substituting the elements from the Context when they
//! have changed.

use basic::mem;

use model::hir::*;
use super::{com, pat, stmt, typ, /*val,*/ Alteration, Context};

/// The Graph Finalizer.
///
/// Rewrites the graph.
#[derive(Clone, Debug)]
pub struct GraphFinalizer<'a, 'g>
    where 'g: 'a
{
    core: com::CoreFinalizer<'a, 'g>,
}

//
//  Public Interface
//

impl<'a, 'g> GraphFinalizer<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(
        context: &'a Context<'g>,
        global_arena: &'g mem::Arena,
    )
        -> Self
    {
        GraphFinalizer {
            core: com::CoreFinalizer::new(context, global_arena),
        }
    }

    /// Finalize the pattern.
    pub fn finalize_pattern(&self, p: Pattern<'g>) -> Alteration<Pattern<'g>>
    {
        pat::PatternFinalizer::new(self.core).finalize(p)
    }

    /// Finalize the statement.
    pub fn finalize_statement(&self, s: Stmt<'g>) -> Alteration<Stmt<'g>> {
        stmt::StatementFinalizer::new(self.core).finalize(s)
    }

    /// Finalize the type.
    pub fn finalize_type(&self, t: Type<'g>, gvn: Gvn) -> Alteration<Type<'g>> {
        typ::TypeFinalizer::new(self.core).finalize(t, gvn)
    }

    /// Finalize the value.
    pub fn finalize_value(&self, v: Value<'g>) -> Alteration<Value<'g>> {
        //val::ValueFinalizer::new(self.core).finalize(v)
        Alteration::forward(v)
    }
}
