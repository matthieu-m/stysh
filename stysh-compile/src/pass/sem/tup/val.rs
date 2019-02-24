//! Value Unifier & Propagator.

use basic::mem::{DynArray, Ptr};

use model::hir::*;
use super::{common, stmt, Alteration};
use super::typ::{self, Action};

/// Value Unifier.
#[derive(Clone, Debug)]
pub struct ValueUnifier<'a> {
    core: common::CoreUnifier<'a>,
}

//
//  Public interface of ValueUnifier
//

impl<'a> ValueUnifier<'a> {
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a>) -> Self {
        ValueUnifier { core }
    }

    /// Unifies the inner entities, recursively.
    pub fn unify(&self, v: Value, ty: Type) -> Alteration<Value> {
        use self::Action::*;

        let e = ExprUnifier::new(self.core, v.expr.clone(), ty);
        let (expr, type_) = e.unify();
        let range = v.range;

        match type_ {
            Cast(Type::Enum(e, p, gin)) => {
                let prototype = e.prototype.clone();
                let value = if expr.altered > 0 {
                    Ptr::new(v)
                } else {
                    Ptr::new(v.with_expr(expr.entity))
                };

                Alteration::update(Value {
                    type_: Type::Enum(e, p, gin),
                    range: range,
                    expr: Expr::Implicit(Implicit::ToEnum(prototype, value)),
                    gvn: self.core.context.gvn(),
                })
            },

            Cast(t) => unreachable!("Inappropriate Cast target {:?}", t),

            Keep(type_) => {
                let altered = if type_ != v.type_ { 1 } else { 0 };
                let type_ = Alteration::forward(type_).with_altered(altered);
                expr.combine2(v, type_, |v, expr, type_| {
                    v.with_expr(expr).with_type(type_)
                })
            },

            Update(type_) => {
                let type_ = Alteration::update(type_);
                expr.combine2(v, type_, |v, expr, type_| {
                    v.with_expr(expr).with_type(type_)
                })
            },
        }
    }
}

//
//  Implementation Details of ValueUnifier
//

struct ExprUnifier<'a> {
    helper: ExprHelper<'a>,
    expr: Expr,
    type_: Type,
}

#[derive(Clone, Copy)]
struct ExprHelper<'a> {
    core: common::CoreUnifier<'a>,
}

type Result = (Alteration<Expr>, Action);

impl<'a> ExprUnifier<'a> {
    fn new(core: common::CoreUnifier<'a>, expr: Expr, type_: Type) -> Self {
        ExprUnifier {
            helper: ExprHelper { core },
            expr,
            type_,
        }
    }

    fn unify_block(self, stmts: DynArray<Stmt>, v: Option<Ptr<Value>>)
        -> Result
    {
        let helper = self.helper;
        let type_ = self.type_;

        let stmts = helper.core.unify_array(stmts, |_, s| helper.unify_statement(s));
        let v = helper.core.unify_option(v, |v| helper.unify_ptr(v, type_));

        (
            stmts.combine2(self.expr, v.clone(), |_, stmts, v| Expr::Block(stmts, v)),
            v.entity
                .map(|v| Action::Update(v.get().type_))
                .unwrap_or(Action::Update(Type::void())),
        )
    }

    fn unify_call(self, c: Callable, args: DynArray<Value>) -> Result {
        use self::Callable::*;

        match c {
            Builtin(f) => self.unify_call_builtin(f, args),
            Function(f) => self.unify_call_function(f, args),
            Unknown(..) | Unresolved(..)
                => (Alteration::forward(self.expr), Action::Keep(self.type_)),
        }
    }

    fn unify_call_builtin(self, f: BuiltinFunction, args: DynArray<Value>)
        -> Result
    {
        let bool_ = Type::bool_();
        let helper = self.helper;

        let args = match (f.number_arguments(), args.len()) {
            (1, 1)
                => helper.core.unify_array(args,
                    |_, v| helper.unify_value(v, bool_.clone())),
            (2, 2) => {
                let (t0, t1) = helper.merge(args.at(0).type_, args.at(1).type_);
                helper.core.unify_array(args, |i, v| {
                    helper.unify_value(
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
            args.combine(self.expr, |_, args| Expr::Call(Callable::Builtin(f), args)),
            helper.transform_into(f.result_type(), self.type_),
        )
    }

    fn unify_call_function(self, f: FunctionProto, args: DynArray<Value>)
        -> Result
    {
        let helper = self.helper;

        let args = if f.arguments.len() == args.len() {
            helper.core.unify_array(args, |i, v| {
                helper.unify_value(v, f.arguments.at(i).type_)
            })
        } else {
            unimplemented!(
                "Mismatched {:?} does not take arguments {:?}",
                f, args
            );
        };

        (
            args.combine(self.expr, |_, v| Expr::Call(Callable::Function(f.clone()), v)),
            helper.transform_into(f.result, self.type_),
        )
    }

    fn unify_constructor(self, c: Constructor<Value>)
        -> Result
    {
        use self::Type::*;

        let helper = self.helper;

        let forward =
            (Alteration::forward(self.expr), Action::Keep(self.type_));

        let (record, path, gin) = match c.type_ {
            Rec(r, p, g) => (r, p, g),
            Unresolved(..) | UnresolvedRec(..) => return forward,
            _ => unimplemented!("Constructor type {:?}", c.type_),
        };

        let (arguments, _) = helper.unify_tuple_impl(
            c.arguments,
            Type::Tuple(record.definition.clone(), gin),
        );
        let type_ = Type::Rec(record, path, gin);
        let range = c.range;

        (
            arguments.combine(forward.0.entity, |_, arguments| {
                Expr::Constructor(Constructor { type_: type_.clone(), arguments, range })
            }),
            helper.transform_into(type_, forward.1.into_type()),
        )
    }

    fn unify(self) -> (Alteration<Expr>, Action) {
        use self::Expr::*;

        let helper = self.helper;

        match self.expr.clone() {
            Implicit(..) | UnresolvedRef(..)
                => (Alteration::forward(self.expr), Action::Keep(self.type_)),
            BuiltinVal(v) => {
                let action = helper.merge(v.result_type(), self.type_).1;
                (Alteration::forward(self.expr), action)
            },
            Block(stmts, v) => self.unify_block(stmts, v),
            Call(c, args) => self.unify_call(c, args),
            Constructor(c) => self.unify_constructor(c),
            FieldAccess(v, f) => self.unify_field_access(v, f),
            If(c, t, f) => self.unify_if(c, t, f),
            Loop(stmts) => self.unify_loop(stmts),
            Ref(_, g) => self.unify_variable_ref(g),
            Tuple(t) => self.unify_tuple(t),
        }
    }

    fn unify_field_access(self, v: Ptr<Value>, f: Field) -> Result {
        let helper = self.helper;

        let value = helper.unify_ptr(v, Type::unresolved());

        let result_type = value.entity.get().type_.field(f);

        (
            value.combine(self.expr, |_, v| Expr::FieldAccess(v, f)),
            helper.transform_into(result_type, self.type_),
        )
    }

    fn unify_if(self, c: Ptr<Value>, t: Ptr<Value>, f: Ptr<Value>)
        -> Result
    {
        let helper = self.helper;

        let c = helper.unify_ptr(c, Type::bool_());

        let (t_type, f_type) = helper.merge(t.get().type_, f.get().type_);
        let t = helper.unify_ptr(t, t_type.type_());
        let f = helper.unify_ptr(f, f_type.type_());

        let result_type = helper.select(t_type.into_type(), f_type.into_type());

        (
            c.combine3(self.expr, t, f, |_, c, t, f| Expr::If(c, t, f)),
            helper.transform_into(result_type, self.type_),
        )
    }

    fn unify_loop(self, stmts: DynArray<Stmt>) -> Result {
        let helper = self.helper;

        let stmts = helper.core.unify_array(stmts, |_, s| helper.unify_statement(s));
        (
            stmts.combine(self.expr, |_, stmts| Expr::Loop(stmts)),
            helper.transform_into(Type::void(), self.type_),
        )
    }

    fn unify_tuple(self, t: Tuple<Value>) -> Result {
        let helper = self.helper;

        let (t, ty) = helper.unify_tuple_impl(t, self.type_);
        (
            t.combine(self.expr, |_, t| Expr::Tuple(t)),
            ty,
        )
    }

    fn unify_variable_ref(self, gvn: Gvn) -> Result {
        let helper = self.helper;

        let (_, result) = helper.merge(helper.core.type_of(gvn), self.type_);

        if let Action::Update(to) = result.clone() {
            helper.core.context.value(gvn).set_type(to);
        }

        (Alteration::forward(self.expr), result)
    }
}

impl<'a> ExprHelper<'a> {
    fn merge(&self, t0: Type, t1: Type) -> (Action, Action) {
        typ::TypeUnifier::new(self.core).merge(t0, t1)
    }

    fn select(&self, t0: Type, t1: Type) -> Type {
        typ::TypeUnifier::new(self.core).select(t0, t1)
    }

    fn transform_into(&self, expr: Type, r: Type) -> Action {
        self.merge(expr, r).0
    }

    fn unify_ptr(&self, v: Ptr<Value>, ty: Type)
        -> Alteration<Ptr<Value>>
    {
        self.unify_value(v.get(), ty).map(|v| Ptr::new(v))
    }

    fn unify_statement(&self, s: Stmt) -> Alteration<Stmt> {
        stmt::StatementUnifier::new(self.core).unify(s)
    }

    fn unify_tuple_impl(&self, t: Tuple<Value>, ty: Type)
        -> (Alteration<Tuple<Value>>, Action)
    {
        let v =
            self.core.unify_tuple(t, ty.clone(), |v, ty| self.unify_value(v, ty));
        let action = self.unify_tuple_type(v.entity.clone(), ty);

        (v, action)
    }

    fn unify_tuple_type(&self, v: Tuple<Value>, ty: Type)
        -> Action
    {
        use self::Action::*;

        if let Type::Tuple(ty, gin) = ty {
            if v.len() == ty.len() {
                for (f, ty) in v.fields.iter().zip(ty.fields.iter()) {
                    match self.merge(f.type_, ty).1 {
                        Cast(_) => unimplemented!(),
                        Keep(_) => continue,
                        Update(_) => return Update(self.type_of(v, gin)),
                    }
                }

                Keep(Type::Tuple(ty, gin))
            } else {
                Update(self.type_of(v, gin))
            }
        } else {
            Update(self.type_of(v, ty.gin()))
        }
    }

    fn type_of(&self, v: Tuple<Value>, gin: Gin) -> Type {
        let fields = DynArray::with_capacity(v.len());
        for v in v.fields {
            fields.push(v.type_);
        }
        Type::Tuple(
            Tuple {
                fields: fields,
                names: v.names,
            },
            gin,
        )
    }

    fn unify_value(&self, v: Value, ty: Type) -> Alteration<Value> {
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
        v: Value,
        ty: Type,
    )
        -> Value
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
