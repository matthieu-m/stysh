//! Unifier of Expression Types.

use crate::model::hir::*;
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
            self.apply_action(e, a)
        } else if let Some(a) = self.unify_type(ty) {
            self.apply_action(e, a)
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
    fn apply_action(&self, e: ExpressionId, a: Action) -> Status {
        match a {
            Action::Update(target) =>
                self.update(e, target),
            Action::Cast(target) =>
                self.cast(e, target),
            Action::Unified =>
                Status::Unified,
        }
    }

    /// Determine from expression.
    fn determine_from_expression(&self, e: ExpressionId) -> Option<Action> {
        use self::Expression::*;

        let expr = self.core.tree().get_expression(e);

        match expr {
            BuiltinVal(value) =>
                Some(Action::Update(value.result_type())),
            Call(fun, _, _) =>
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
            Function(id) | Method(id) => {
                let registry = self.core.registry();
                let function = registry.get_function(id);
                Some(Action::Update(registry.get_type(function.result)))
            },
            Unknown(_) | Unresolved(_) =>
                None,
        }
    }

    /// Determine the necessary action to unify the type.
    fn unify_type(&self, ty: TypeId) -> Option<Action> {
        typ::TypeUnifier::new(self.core).unify(ty)
    }

    /// Update the type of the expression to unify it.
    fn update(&self, e: ExpressionId, target: Type) -> Status {
        self.core.tree_mut().set_expression_type(e, target);
        Status::Unified
    }

    /// Cast the expression to a new type to unify it.
    fn cast(&self, e: ExpressionId, target: Type) -> Status {
        let implicit = match target {
            Type::Enum(id, ..) => Implicit::ToEnum(id, e),
            _ => unreachable!("{:?}", target),
        };

        let implicit = Expression::Implicit(implicit);

        let implicit = {
            let mut tree = self.core.tree_mut();
            let range = tree.get_expression_range(e);
            tree.push_expression(target, implicit, range)
        };

        for gvn in self.core.context.get_gvn_links(e.into()) {
            if let Some(expr) = gvn.as_expression() {
                self.replace_in_expression(expr, e, implicit);
            }
        }

        Status::Unified
    }

    fn replace_in_expression(
        &self,
        expr: ExpressionId,
        from: ExpressionId,
        to_: ExpressionId,
    )
    {
        use self::Expression::*;

        let replacement = match self.core.tree().get_expression(expr) {
            BuiltinVal(..) | FieldAccess(..) | Implicit(..) | Loop(..) |
            Ref(..) | UnresolvedRef(..) =>
                unreachable!("replace_in_expression {:?}", expr),
            Block(stmts, e) =>
                if e == Some(from) {
                    Block(stmts, Some(to_))
                } else {
                    Block(stmts, e)
                },
            Call(c, r, args) =>
                if r == Some(from) {
                    Call(c, Some(to_), args)
                } else {
                    Call(c, r, self.replace_in_expression_tuple(args, from, to_))
                },
            Constructor(args) =>
                Constructor(self.replace_in_expression_tuple(args, from, to_)),
            If(c, t, f) =>
                if c == from {
                    If(to_, t, f)
                } else if t == from {
                    If(c, to_, f)
                } else if f == from {
                    If(c, t, to_)
                } else {
                    If(c, t, f)
                },
            Tuple(args) =>
                Tuple(self.replace_in_expression_tuple(args, from, to_)),
        };

        self.core.tree_mut().set_expression(expr, replacement);
    }

    fn replace_in_expression_tuple(
        &self,
        tuple: Tuple<ExpressionId>,
        from: ExpressionId,
        to_: ExpressionId,
    )
        -> Tuple<ExpressionId>
    {
        let fields: Vec<_> = {
            let tree = self.core.tree();
            let fields = tree.get_expression_ids(tuple.fields);

            if !fields.contains(&from) {
                return tuple;
            }

            fields.iter().copied()
                .map(|e| if e == from { to_ } else { e })
                .collect()
        };

        let fields = self.core.tree_mut().push_expression_ids(fields);

        Tuple { fields, names: tuple.names, }
    }
}

#[cfg(test)]
mod tests {
    use crate::model::hir::*;

    use super::{common, Action, ExprUnifier, Status};
    use super::super::Relation;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn name() {
        let env = Env::default();
        let local = env.local(b":var a := 1; a");
        let a = local.value_id(5, 1);

        let expr = {
            let (_, p, s, _, _, v) = env.source_factories();
            let pat = p.var_typed(a, Type::int());
            s.var(pat, v.int(1, 10));

            let expr = v.name_ref(a, 17).build();
            local.link_types_of(expr.into(), Relation::Identical(pat.into()));

            expr
        };

        {
            let (_, p, s, _, _, v) = env.target_factories();
            let pat = p.var_typed(a, Type::int());
            s.var(pat, v.int(1, 10));
            v.int_ref(a, 17).build();
        }

        assert_eq!(unify(&local, expr), Status::Unified);
    }

    #[test]
    fn cast_to_enum() {
        let env = Env::default();
        let local = env.local(b"{ A() } :enum X { A }");

        let (x, a) = {
            let (i, _, _, _, _, _) = env.source_factories();
            let x = local.item_id(14, 1);
            let a = local.item_id(18, 1);

            let x_id = local.module().borrow_mut().push_enum_name(x);

            let a = i.unit_of_enum(a, x_id);
            let x = i.enum_(x).push(a).build();

            (x, a)
        };

        let expr = {
            let (_, _, _, t, _, v) = env.source_factories();
            let constructor = v.constructor(t.record(a).build())
                .range(2, 3)
                .build();
            let block = v.block(constructor).build();

            local.link_gvns(&[constructor.into(), block.into()]);

            constructor
        };

        {
            let (_, _, _, t, _, v) = env.target_factories();
            let constructor = v.constructor(t.record(a).build())
                .range(2, 3)
                .build();
            let block = v.block(constructor).build();

            let implicit = v.implicit(constructor)
                .type_(Type::Enum(x, PathId::empty()))
                .build();

            local.target().borrow_mut()
                .set_expression(block, Expression::Block(Id::empty(), Some(implicit)));
        }

        apply_action(&local, expr, Action::Cast(Type::Enum(x, PathId::empty())));
    }

    fn apply_action(local: &LocalEnv, e: ExpressionId, a: Action) {
        println!("source before: {:?}", local.source());
        println!();

        let module = local.module().borrow();
        let core = common::CoreUnifier::new(
            local.context(),
            &*module,
            &*local.source()
        );
        ExprUnifier::new(core).apply_action(e, a);

        println!("source after: {:?}", local.source());
        println!();
        println!("target: {:?}", local.target());
        println!();

        assert_eq!(local.source(), local.target());
    }

    fn unify(local: &LocalEnv, e: ExpressionId) -> Status {
        println!("source before: {:?}", local.source());
        println!();

        let module = local.module().borrow();
        let core = common::CoreUnifier::new(
            local.context(),
            &*module,
            &*local.source()
        );
        let status = ExprUnifier::new(core).unify(e);

        println!("source after: {:?}", local.source());
        println!();
        println!("target: {:?}", local.target());
        println!();

        assert_eq!(local.source(), local.target());

        status
    }
}
