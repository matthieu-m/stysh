//! Statement Finalizer.

use model::hir::*;
use super::{com, pat, val, Alteration};

/// Statement Finalizer.
#[derive(Clone, Debug)]
pub struct StatementFinalizer<'a, 'g>
    where 'g: 'a
{
    core: com::CoreFinalizer<'a, 'g>,
}

//
//  Public interface of StatementFinalizer
//

impl<'a, 'g> StatementFinalizer<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: com::CoreFinalizer<'a, 'g>) -> Self {
        StatementFinalizer { core }
    }

    /// Finalizes the statement.
    pub fn finalize(&self, s: Stmt<'g>) -> Alteration<Stmt<'g>> {
        use self::Stmt::*;

        match s {
            Return(r) => self.finalize_return(r).combine(s, |r| Return(r)),
            Set(r) => self.finalize_rebinding(r).combine(s, |r| Set(r)),
            Var(b) => self.finalize_binding(b).combine(s, |b| Var(b)),
        }
    }
}

//
//  Implementation Details
//

impl<'a, 'g> StatementFinalizer<'a, 'g>
    where 'g: 'a
{
    fn finalize_binding(&self, b: Binding<'g>) -> Alteration<Binding<'g>> {
        let left = self.finalize_pattern(b.left);
        let right = self.finalize_value(b.right);
        let range = b.range;

        left.combine2(b, right, |left, right| Binding { left, right, range })
    }

    fn finalize_rebinding(&self, r: ReBinding<'g>) -> Alteration<ReBinding<'g>> {
        let left = self.finalize_value(r.left);
        let right = self.finalize_value(r.right);
        let range = r.range;

        left.combine2(r, right, |left, right| ReBinding { left, right, range })
    }

    fn finalize_return(&self, r: Return<'g>) -> Alteration<Return<'g>> {
        let value = self.finalize_value(r.value);
        let range = r.range;

        value.combine(r, |value| Return { value, range })
    }

    fn finalize_pattern(&self, p: Pattern<'g>) -> Alteration<Pattern<'g>> {
        pat::PatternFinalizer::new(self.core).finalize(p)
    }

    fn finalize_value(&self, v: Value<'g>) -> Alteration<Value<'g>> {
        val::ValueFinalizer::new(self.core).finalize(v)
    }
}
