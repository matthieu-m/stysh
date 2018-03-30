//! Statement Unifier & Propagator.

use model::hir::*;
use super::{common, pat, val, Resolution};

/// Statement Unifier.
#[derive(Clone, Debug)]
pub struct StatementUnifier<'a, 'g>
    where 'g: 'a
{
    core: common::CoreUnifier<'a, 'g>,
}

//
//  Public interface of StatementUnifier
//

impl<'a, 'g> StatementUnifier<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a, 'g>) -> Self {
        StatementUnifier { core }
    }

    /// Unifies the inner entities, recursively.
    pub fn unify(&self, s: Stmt<'g>) -> Resolution<Stmt<'g>> {
        use self::Stmt::*;

        match s {
            Return(r) => self.unify_return(r).combine(s, |r| Return(r)),
            Set(r) => self.unify_rebinding(r).combine(s, |r| Set(r)),
            Var(b) => self.unify_binding(b).combine(s, |b| Var(b)),
        }
    }
}

//
//  Implementation Details of StatementUnifier
//

impl<'a, 'g> StatementUnifier<'a, 'g>
    where 'g: 'a
{
    fn unify_binding(&self, b: Binding<'g>) -> Resolution<Binding<'g>> {
        let right = self.unify_value(b.right, Type::unresolved());
        let left = self.unify_pattern(b.left, right.entity.type_);
        let range = b.range;

        left.combine2(b, right, |left, right| Binding { left, right, range })
    }

    fn unify_rebinding(&self, r: ReBinding<'g>) -> Resolution<ReBinding<'g>> {
        let right = self.unify_value(r.right, r.left.type_);
        let left = if r.left.type_ != right.entity.type_ {
            Resolution::forward(right.entity.type_)
        } else {
            Resolution::forward(r.left.type_)
        }.map(|t| r.left.with_type(t));
        let range = r.range;

        left.combine2(r, right, |left, right| ReBinding { left, right, range })
    }

    fn unify_return(&self, r: Return<'g>) -> Resolution<Return<'g>> {
        let value = self.unify_value(r.value, Type::unresolved());
        let range = r.range;

        value.combine(r, |value| Return { value, range })
    }

    fn unify_pattern(&self, p: Pattern<'g>, ty: Type<'g>)
        -> Resolution<Pattern<'g>>
    {
        pat::PatternUnifier::new(self.core).unify(p, ty)
    }

    fn unify_value(&self, v: Value<'g>, ty: Type<'g>) -> Resolution<Value<'g>> {
        val::ValueUnifier::new(self.core).unify(v, ty)
    }
}
