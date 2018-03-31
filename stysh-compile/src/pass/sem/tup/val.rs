//! Value Unifier & Propagator.

use basic::mem;

use model::hir::*;
use super::{common, stmt, Alteration};
use super::typ::{self, Action};

/// Value Unifier.
#[derive(Clone, Debug)]
pub struct ValueUnifier<'a, 'g>
    where 'g: 'a
{
    core: common::CoreUnifier<'a, 'g>,
}

//
//  Public interface of ValueUnifier
//

impl<'a, 'g> ValueUnifier<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a, 'g>) -> Self {
        ValueUnifier { core }
    }

    /// Unifies the inner entities, recursively.
    pub fn unify(&self, v: Value<'g>, ty: Type<'g>) -> Alteration<Value<'g>> {
        use self::Action::*;

        let e = ExprUnifier { core: self.core, expr: v.expr, type_: ty };
        let (expr, type_) = e.unify();

        match type_ {
            Cast(Type::Enum(e, p)) => {
                let value = if expr.altered > 0 {
                    self.core.insert(v)
                } else {
                    self.core.insert(v.with_expr(expr.entity).with_type(v.type_))
                };
                Alteration::update(Value {
                    type_: Type::Enum(e, p),
                    range: v.range,
                    expr: Expr::Implicit(Implicit::ToEnum(*e.prototype, value)),
                    gvn: self.core.context.gvn(),
                })
            },
            Cast(t) => unreachable!("Inappropriate Cast target {:?}", t),
            Keep(type_) => {
                let type_ = Alteration::forward(type_)
                    .with_altered(if type_ != v.type_ { 1 } else { 0 });
                expr.combine2(v, type_, |expr, type_| {
                    v.with_expr(expr).with_type(type_)
                })
            },
            Update(type_) => {
                let type_ = Alteration::update(type_);
                expr.combine2(v, type_, |expr, type_| {
                    v.with_expr(expr).with_type(type_)
                })
            },
        }
    }
}

//
//  Implementation Details of ValueUnifier
//

struct ExprUnifier<'a, 'g>
    where 'g: 'a
{
    core: common::CoreUnifier<'a, 'g>,
    expr: Expr<'g>,
    type_: Type<'g>,
}

type Result<'g> = (Alteration<Expr<'g>>, Action<'g>);

impl<'a, 'g> ExprUnifier<'a, 'g>
    where 'g: 'a
{
    fn unify_block(&self, stmts: &'g [Stmt<'g>], v: Option<&'g Value<'g>>)
        -> Result<'g>
    {
        let stmts = self.core.unify_slice(stmts, |_, s| self.unify_statement(s));
        let v = self.core.unify_option(v, |v| self.unify_ref(v, self.type_));

        (
            stmts.combine2(self.expr, v, |stmts, v| Expr::Block(stmts, v)),
            v.entity
                .map(|v| Action::Update(v.type_))
                .unwrap_or(Action::Update(Type::void())),
        )
    }

    fn unify_call(&self, c: Callable<'g>, args: &'g [Value<'g>]) -> Result<'g> {
        use self::Callable::*;

        match c {
            Builtin(f) => self.unify_call_builtin(f, args),
            Function(f) => self.unify_call_function(f, args),
            Unknown(..) | Unresolved(..)
                => (Alteration::forward(self.expr), Action::Keep(self.type_)),
        }
    }

    fn unify_call_builtin(&self, f: BuiltinFunction, args: &'g [Value<'g>])
        -> Result<'g>
    {
        let bool_ = Type::bool_();

        let args = match (f.number_arguments(), args.len()) {
            (1, 1)
                => self.core.unify_slice(args,
                    |_, v| self.unify_value(v, bool_)),
            (2, 2) => {
                let (t0, t1) = self.merge(args[0].type_, args[1].type_);
                self.core.unify_slice(args, |i, v| {
                    self.unify_value(
                        v,
                        if i == 0 { t0.type_() } else { t1.type_() }
                    )
                })
            },
            (_, _) => unimplemented!(
                "Mismatched {:?} does not take arguments {:?}",
                f, args
            ),
        };

        (
            args.combine(self.expr, |v| Expr::Call(Callable::Builtin(f), v)),
            self.transform_into(f.result_type(), self.type_),
        )
    }

    fn unify_call_function(&self, f: FunctionProto<'g>, args: &'g [Value<'g>])
        -> Result<'g>
    {
        let args = if f.arguments.len() == args.len() {
            self.core.unify_slice(args, |i, v| {
                self.unify_value(v, f.arguments[i].type_)
            })
        } else {
            unimplemented!(
                "Mismatched {:?} does not take arguments {:?}",
                f, args
            );
        };

        (
            args.combine(self.expr, |v| Expr::Call(Callable::Function(f), v)),
            self.transform_into(f.result, self.type_),
        )
    }

    fn unify_constructor(&self, c: Constructor<'g, Value<'g>>)
        -> Result<'g>
    {
        use self::Type::*;

        let forward =
            (Alteration::forward(self.expr), Action::Keep(self.type_));

        let (record, path) = match c.type_ {
            Rec(r, p) => (r, p),
            Unresolved(..) | UnresolvedRec(..) => return forward,
            _ => unimplemented!("Constructor type {:?}", c.type_),
        };

        let (arguments, _) =
            self.unify_tuple_impl(c.arguments, Type::Tuple(record.definition));
        let type_ = Type::Rec(record, path);
        let range = c.range;

        (
            arguments.combine(self.expr, |arguments| {
                Expr::Constructor(Constructor { type_, arguments, range })
            }),
            self.transform_into(type_, self.type_),
        )
    }

    fn unify(&self) -> (Alteration<Expr<'g>>, Action<'g>) {
        use self::Expr::*;

        match self.expr {
            Implicit(..) | UnresolvedRef(..) | UnresolvedField(..)
                => (Alteration::forward(self.expr), Action::Keep(self.type_)),
            BuiltinVal(v) => {
                let action = self.merge(v.result_type(), self.type_).1;
                (Alteration::forward(self.expr), action)
            },
            Block(stmts, v) => self.unify_block(stmts, v),
            Call(c, args) => self.unify_call(c, args),
            Constructor(c) => self.unify_constructor(c),
            FieldAccess(v, i) => self.unify_field_access(v, i),
            If(c, t, f) => self.unify_if(c, t, f),
            Loop(stmts) => self.unify_loop(stmts),
            Ref(name, _) => self.unify_variable_ref(name),
            Tuple(t) => self.unify_tuple(t),
        }
    }

    fn unify_field_access(&self, v: &'g Value<'g>, i: u16) -> Result<'g> {
        let value = self.unify_ref(v, Type::unresolved());

        let result_type =
            self.core.fields_of(value.entity.type_)
                .and_then(|t| t.fields.get(i as usize).cloned())
                .unwrap_or(Type::unresolved());

        (
            value.combine(self.expr, |v| Expr::FieldAccess(v, i)),
            self.transform_into(result_type, self.type_),
        )
    }

    fn unify_if(&self, c: &'g Value<'g>, t: &'g Value<'g>, f: &'g Value<'g>)
        -> Result<'g>
    {
        let c = self.unify_ref(c, Type::bool_());

        let (t_type, f_type) = self.merge(t.type_, f.type_);
        let t = self.unify_ref(t, t_type.type_());
        let f = self.unify_ref(f, f_type.type_());

        let result_type = self.select(t_type.type_(), f_type.type_());

        (
            c.combine3(self.expr, t, f, |c, t, f| Expr::If(c, t, f)),
            self.transform_into(result_type, self.type_),
        )
    }

    fn unify_loop(
        &self,
        stmts: &'g [Stmt<'g>],
    )
        -> (Alteration<Expr<'g>>, Action<'g>)
    {
        let stmts = self.core.unify_slice(stmts, |_, s| self.unify_statement(s));
        (
            stmts.combine(self.expr, |stmts| Expr::Loop(stmts)),
            self.transform_into(Type::void(), self.type_),
        )
    }

    fn unify_tuple(&self, t: Tuple<'g, Value<'g>>) -> Result<'g> {
        let (t, ty) = self.unify_tuple_impl(t, self.type_);
        (
            t.combine(self.expr, |t| Expr::Tuple(t)),
            ty,
        )
    }

    fn unify_variable_ref(&self, name: ValueIdentifier) -> Result<'g> {
        let (_, result) = self.merge(self.core.type_of(name), self.type_);

        if let Action::Update(to) = result {
            self.core.context.update_binding(name, to);
        }

        (Alteration::forward(self.expr), result)
    }

    fn merge(&self, t0: Type<'g>, t1: Type<'g>) -> (Action<'g>, Action<'g>) {
        typ::TypeUnifier::new(self.core).merge(t0, t1)
    }

    fn select(&self, t0: Type<'g>, t1: Type<'g>) -> Type<'g> {
        typ::TypeUnifier::new(self.core).select(t0, t1)
    }

    fn transform_into(&self, expr: Type<'g>, r: Type<'g>) -> Action<'g> {
        self.merge(expr, r).0
    }

    fn unify_ref(&self, v: &'g Value<'g>, ty: Type<'g>)
        -> Alteration<&'g Value<'g>>
    {
        self.unify_value(*v, ty).map(|v| self.core.insert(v))
    }

    fn unify_statement(&self, s: Stmt<'g>) -> Alteration<Stmt<'g>> {
        stmt::StatementUnifier::new(self.core).unify(s)
    }

    fn unify_tuple_impl(&self, t: Tuple<'g, Value<'g>>, ty: Type<'g>)
        -> (Alteration<Tuple<'g, Value<'g>>>, Action<'g>)
    {
        let v = self.core.unify_tuple(t, ty, |v, ty| self.unify_value(v, ty));
        let action = self.unify_tuple_type(v.entity, ty);

        (v, action)
    }

    fn unify_tuple_type(&self, v: Tuple<'g, Value<'g>>, ty: Type<'g>)
        -> Action<'g>
    {
        use self::Action::*;

        if let Type::Tuple(ty) = ty {
            if v.len() == ty.len() {
                for (&f, &ty) in v.fields.iter().zip(ty.fields.iter()) {
                    match self.merge(f.type_, ty).1 {
                        Cast(_) => unimplemented!(),
                        Keep(_) => continue,
                        Update(_) => return Update(self.type_of(v)),
                    }
                }

                Keep(Type::Tuple(ty))
            } else {
                Update(self.type_of(v))
            }
        } else {
            Update(self.type_of(v))
        }
    }

    fn type_of(&self, v: Tuple<'g, Value<'g>>) -> Type<'g> {
        let mut fields =
            mem::Array::with_capacity(v.len(), self.core.global_arena);
        for v in v.fields {
            fields.push(v.type_);
        }
        Type::Tuple(Tuple {
            fields: fields.into_slice(),
            names: v.names,
        })
    }

    fn unify_value(&self, v: Value<'g>, ty: Type<'g>) -> Alteration<Value<'g>> {
        ValueUnifier::new(self.core).unify(v, ty)
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::ValueUnifier;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn tuple() {
        let env = Env::default();
        let (_, _, _, _, _, v) = env.factories();

        let local = env.local(b"(0, 1)");

        assert_eq!(
            unify(
                &local, 3,
                v.tuple()
                    .push(v.int(0, 1))
                    .push(v.int(1, 3))
                    .build()
                    .without_type(),
                Type::unresolved(),
            ),
            v.tuple().push(v.int(0, 1)).push(v.int(1, 3)).build()
        );
    }

    fn unify<'g>(
        local: &LocalEnv<'g>,
        altered: u32,
        v: Value<'g>,
        ty: Type<'g>,
    )
        -> Value<'g>
    {
        let v = local.resolver().resolve_value(v);
        let ty = local.resolver().resolve_type(ty);

        let resolution =
            ValueUnifier::new(local.core())
                .unify(v, ty)
                .map(|v| local.scrubber().scrub_value(v));

        assert_eq!(resolution.altered, altered);

        resolution.entity
    }
}
