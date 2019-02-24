//! Statement Unifier & Propagator.

use model::hir::*;
use super::{common, pat, val, Alteration};

/// Statement Unifier.
#[derive(Clone, Debug)]
pub struct StatementUnifier<'a,> {
    core: common::CoreUnifier<'a>,
}

//
//  Public interface of StatementUnifier
//

impl<'a> StatementUnifier<'a> {
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a>) -> Self {
        StatementUnifier { core }
    }

    /// Unifies the inner entities, recursively.
    pub fn unify(&self, s: Stmt) -> Alteration<Stmt> {
        use self::Stmt::*;

        match s.clone() {
            Return(r) => self.unify_return(r).combine(s, |_, r| Return(r)),
            Set(r) => self.unify_rebinding(r).combine(s, |_, r| Set(r)),
            Var(b) => self.unify_binding(b).combine(s, |_, b| Var(b)),
        }
    }
}

//
//  Implementation Details of StatementUnifier
//

impl<'a> StatementUnifier<'a> {
    fn unify_binding(&self, b: Binding) -> Alteration<Binding> {
        let clone = b.clone();

        let right = self.unify_value(clone.right, Type::unresolved());
        let left = self.unify_pattern(clone.left, right.entity.type_.clone());
        let range = clone.range;

        left.combine2(b, right, |_, left, right| Binding { left, right, range })
    }

    fn unify_rebinding(&self, r: ReBinding) -> Alteration<ReBinding> {
        let left_clone = r.left.clone();
        let right_clone = r.right.clone();
        let range = r.range;

        let right = self.unify_value(right_clone, r.left.type_.clone());
        let left = if r.left.type_ != right.entity.type_ {
            Alteration::forward(right.entity.type_.clone())
        } else {
            Alteration::forward(r.left.type_.clone())
        }.map(|t| left_clone.with_type(t));

        left.combine2(r, right, |_, left, right| ReBinding { left, right, range })
    }

    fn unify_return(&self, r: Return) -> Alteration<Return> {
        let clone = r.clone();
        let value = self.unify_value(clone.value, Type::unresolved());
        let range = clone.range;

        value.combine(r, |_, value| Return { value, range })
    }

    fn unify_pattern(&self, p: Pattern, ty: Type)
        -> Alteration<Pattern>
    {
        pat::PatternUnifier::new(self.core).unify(p, ty)
    }

    fn unify_value(&self, v: Value, ty: Type) -> Alteration<Value> {
        val::ValueUnifier::new(self.core).unify(v, ty)
    }
}
