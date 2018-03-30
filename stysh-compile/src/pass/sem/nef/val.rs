//! Value Fetcher.

use model::hir::*;
use super::{common, stmt, typ, Resolution};

/// Value Fetcher.
#[derive(Clone, Debug)]
pub struct ValueFetcher<'a, 'g>
    where 'g: 'a
{
    core: common::CoreFetcher<'a, 'g>,
}

//
//  Public interface of ValueFetcher
//

impl<'a, 'g> ValueFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreFetcher<'a, 'g>) -> Self {
        ValueFetcher { core }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self, v: Value<'g>) -> Resolution<Value<'g>> {
        let type_ = self.fetch_type(v.type_);
        let expr = self.fetch_expression(v.expr);
        let (range, gvn) = (v.range, v.gvn);

        type_.combine2(v, expr, |type_, expr| {
            Value { type_, range, expr, gvn }
        })
    }
}

//
//  Implementation Details
//

impl<'a, 'g> ValueFetcher<'a, 'g>
    where 'g: 'a
{
    fn fetch_block(
        &self,
        e: Expr<'g>,
        stmts: &'g [Stmt<'g>],
        v: Option<&'g Value<'g>>
    )
        -> Resolution<Expr<'g>>
    {
        let stmts = self.core.fetch_slice(stmts, |s| self.fetch_statement(s));
        let v = self.core.fetch_option(v, |v| {
            self.fetch(*v).map(|v| self.core.insert(v))
        });

        stmts.combine2(e, v, |stmts, v| Expr::Block(stmts, v))
    }

    fn fetch_call(
        &self,
        e: Expr<'g>,
        c: Callable<'g>,
        v: &'g [Value<'g>],
    )
        -> Resolution<Expr<'g>>
    {
        let v = self.core.fetch_slice(v, |v| self.fetch(v));
        v.combine(e, |v| Expr::Call(c, v))
    }

    fn fetch_constructor(&self, c: Constructor<'g, Value<'g>>)
        -> Resolution<Constructor<'g, Value<'g>>>
    {
        let type_ = self.fetch_type(c.type_);
        let arguments = self.fetch_tuple(c.arguments);
        let range = c.range;

        type_.combine2(c, arguments, |type_, arguments| {
            Constructor { type_, arguments, range }
        })
    }

    fn fetch_expression(&self, e: Expr<'g>) -> Resolution<Expr<'g>> {
        use self::Expr::*;

        match e {
            ArgumentRef(..) | BuiltinVal(..) | UnresolvedRef(..)
                | VariableRef(..)
                    => Resolution::forward(e),
            Block(stmts, v) => self.fetch_block(e, stmts, v),
            Call(c, args) => self.fetch_call(e, c, args),
            Constructor(c)
                => self.fetch_constructor(c).combine(e, |c| Constructor(c)),
            FieldAccess(v, i) => self.fetch_ref(v).combine(e, |v| FieldAccess(v, i)),
            If(c, t, f) => self.fetch_if(e, c, t, f),
            Implicit(i) => self.fetch_implicit(i).combine(e, |i| Implicit(i)),
            Loop(stmts) => self.fetch_loop(e, stmts),
            Tuple(t) => self.fetch_tuple(t).combine(e, |t| Tuple(t)),
            UnresolvedField(v, id) => self.fetch_unresolved_field(e, v, id),
        }

    }

    fn fetch_if(
        &self,
        e: Expr<'g>,
        c: &'g Value<'g>,
        t: &'g Value<'g>,
        f: &'g Value<'g>,
    )
        -> Resolution<Expr<'g>>
    {
        let c = self.fetch_ref(c);
        let t = self.fetch_ref(t);
        let f = self.fetch_ref(f);

        c.combine3(e, t, f, |c, t, f| Expr::If(c, t, f))
    }

    fn fetch_implicit(&self, i: Implicit<'g>) -> Resolution<Implicit<'g>> {
        use self::Implicit::*;

        match i {
            ToEnum(e, v) => self.fetch_ref(v).combine(i, |v| ToEnum(e, v)),
        }
    }

    fn fetch_loop(&self, e: Expr<'g>, stmts: &'g [Stmt<'g>])
        -> Resolution<Expr<'g>>
    {
        let stmts = self.core.fetch_slice(stmts, |s| self.fetch_statement(s));

        stmts.combine(e, |stmts| Expr::Loop(stmts))
    }

    fn fetch_tuple(&self, t: Tuple<'g, Value<'g>>)
        -> Resolution<Tuple<'g, Value<'g>>>
    {
        self.core.fetch_tuple(t, |v| self.fetch(v))
    }

    fn fetch_unresolved_field(
        &self,
        e: Expr<'g>,
        v: &'g Value<'g>,
        name: ValueIdentifier,
    )
        -> Resolution<Expr<'g>>
    {
        use self::Type::*;

        let v = self.fetch_ref(v);

        let index = match v.entity.type_ {
            Builtin(_) | Enum(..) | Unresolved(..)
                => return v.combine(e, |v| Expr::UnresolvedField(v, name)),
            Rec(..) | Tuple(..) => self.field_of(v.entity.type_, name),
        };

        match index {
            None => v.combine(e, |v| Expr::UnresolvedField(v, name)),
            Some(index) => v.map(|v| Expr::FieldAccess(v, index)),
        }
    }

    fn fetch_ref(&self, v: &'g Value<'g>) -> Resolution<&'g Value<'g>> {
        self.fetch(*v).map(|v| self.core.insert(v))
    }

    fn fetch_statement(&self, s: Stmt<'g>) -> Resolution<Stmt<'g>> {
        stmt::StatementFetcher::new(self.core).fetch(s)
    }

    fn fetch_type(&self, t: Type<'g>) -> Resolution<Type<'g>> {
        typ::TypeFetcher::new(self.core).fetch(t)
    }

    fn field_of(&self, t: Type<'g>, name: ValueIdentifier) -> Option<u16> {
        use self::Type::*;

        let tuple = match t {
            Rec(r, _)
                => self.core.registry.lookup_record(r.name).map(|r| r.definition),
            Tuple(t) => Some(t),
            _ => None,
        };

        let result = tuple.and_then(|t|
            t.names.iter().position(|n| n.id() == name.id()).map(|u| u as u16)
        );

        if result.is_some() { self.fetched_value(name); }

        result
    }

    fn fetched_value(&self, v: ValueIdentifier) {
        self.core.context.fetched_value(v);
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::ValueFetcher;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn constructor_nested() {
        let env = Env::default();
        let (i, _, p, _, t, v) = env.factories();

        let mut local = env.local(b":enum E { A, B };    E::B");
        local.mark_unfetched_items(&[i.id(24, 1)]);

        let e =
            i.enum_(p.enum_(i.id(6, 1)).build())
                .push(i.unit(10, 1))
                .push(i.unit(13, 1))
                .build();
        local.insert_enum(e);

        let parent = Type::Enum(*e.prototype, Default::default());

        let unresolved = t.unresolved(i.id(24, 1)).push(parent).build();
        let fetched = t.record(i.id(13, 1), 13).push(parent).build();

        assert_eq!(
            fetch(
                &local, 1,
                v.constructor(unresolved, 21, 4).build_value().without_type()
            ),
            v.constructor(fetched, 21, 4).build_value().without_type()
        );
    }

    fn fetch<'g>(local: &LocalEnv<'g>, altered: u32, v: Value<'g>) -> Value<'g> {
        let v = local.resolver().resolve_value(v);

        let resolution =
            ValueFetcher::new(local.core())
                .fetch(v)
                .map(|v| local.scrubber().scrub_value(v));

        assert_eq!(resolution.altered, altered);
        assert_eq!(resolution.introduced, 0);

        resolution.entity
    }
}
