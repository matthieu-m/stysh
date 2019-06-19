//! Unifier of Expression Types.

use model::hir::*;
use super::common::{self, Status};
use super::typ::{self, Action};

/// Expression Unifier.
#[derive(Clone, Debug)]
pub struct ExprUnifier<'a> {
    core: common::CoreUnifier<'a>,
}

//
//  Public interface of ExprUnifier
//

impl<'a> ExprUnifier<'a> {
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a>) -> Self {
        ExprUnifier { core }
    }

    /// Unifies the type of the expression.
    pub fn unify(&self, e: ExpressionId) -> Status {
        let ty = self.core.tree().get_expression_type_id(e);

        if let Some(a) = self.determine_from_expression(e) {
            self.apply_action(e, ty, a)
        } else if let Some(a) = self.unify_type(ty) {
            self.apply_action(e, ty, a)
        } else {
            Status::Diverging
        }
    }
}

//
//  Implementation Details of ExprUnifier
//

impl<'a> ExprUnifier<'a> {
    /// Apply Action.
    fn apply_action(&self, e: ExpressionId, ty: TypeId, a: Action) -> Status {
        match a {
            Action::Update(target) =>
                self.update(e, ty, target),
            Action::Cast(target) =>
                self.cast(e, ty, target),
            Action::Unified =>
                Status::Unified,
        }
    }

    /// Determine from expression.
    fn determine_from_expression(&self, e: ExpressionId) -> Option<Action> {
        use self::Expr::*;

        let expr = *self.core.tree().get_expression(e);

        match expr {
            BuiltinVal(value) =>
                Some(Action::Update(value.result_type())),
            Call(fun, _) =>
                self.determine_from_callable(fun),
            Block(..) | Constructor(..) | FieldAccess(..) | If(..) |
            Implicit(..) | Loop(..) | Ref(..) | Tuple(..) | UnresolvedRef(..) =>
                None,
        }
    }

    /// Determine from callable.
    fn determine_from_callable(&self, fun: Callable) -> Option<Action> {
        use self::Callable::*;

        match fun {
            Builtin(fun) =>
                Some(Action::Update(fun.result_type())),
            Function(_, _, id) =>
                Some(Action::Update(*self.core.tree().get_type(id))),
            Unknown(_) | Unresolved(_) =>
                None,
        }
    }

    /// Determine the necessary action to unify the type.
    fn unify_type(&self, ty: TypeId) -> Option<Action> {
        typ::TypeUnifier::new(self.core).unify(ty)
    }

    /// Update the type of the expression to unify it.
    fn update(&self, _e: ExpressionId, ty: TypeId, target: Type) -> Status {
        self.core.tree_mut().set_type(ty, target);
        Status::Unified
    }

    /// Cast the expression to a new type to unify it.
    fn cast(&self, _e: ExpressionId, _ty: TypeId, _target: Type) -> Status {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::{ExprUnifier, Status};
    use super::super::Relation;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn name() {
        let env = Env::default();
        let local = env.local(b":var a := 1; a");
        let a = local.value_id(5, 1);

        let expr = {
            let (_, p, _, s, _, _, v) = env.source_factories();
            let pat = p.var_typed(a, Type::int());
            let expr = v.name_ref(a, 17).build();
            s.var(pat, v.int(1, 10));

            local.link_types_of(expr.into(), Relation::Identical(pat.into()));

            expr
        };

        {
            let (_, p, _, s, _, _, v) = env.target_factories();
            let pat = p.var_typed(a, Type::int());
            s.var(pat, v.int(1, 10));
            v.int_ref(a, 17).build();
        }

        assert_eq!(unify(&local, expr), Status::Unified);
    }

    fn unify<'g>(local: &LocalEnv<'g>, e: ExpressionId) -> Status {
        local.resolve_trees();

        println!("source before: {:?}", local.source());
        println!();

        let status = ExprUnifier::new(local.core()).unify(e);

        println!("source after: {:?}", local.source());
        println!();
        println!("target: {:?}", local.target());
        println!();

        assert_eq!(local.source(), local.target());

        status
    }
}