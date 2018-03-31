//! Value Fetcher.

use basic::com::Span;

use model::hir::*;
use super::{common, stmt, typ, Alteration};

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
    pub fn fetch(&self, v: Value<'g>) -> Alteration<Value<'g>> {
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
        -> Alteration<Expr<'g>>
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
        -> Alteration<Expr<'g>>
    {
        let v = self.core.fetch_slice(v, |v| self.fetch(v));
        v.combine(e, |v| Expr::Call(c, v))
    }

    fn fetch_constructor(&self, c: Constructor<'g, Value<'g>>)
        -> Alteration<Constructor<'g, Value<'g>>>
    {
        let type_ = self.fetch_type(c.type_);
        let arguments = self.fetch_tuple(c.arguments);
        let range = c.range;

        type_.combine2(c, arguments, |type_, arguments| {
            Constructor { type_, arguments, range }
        })
    }

    fn fetch_expression(&self, e: Expr<'g>) -> Alteration<Expr<'g>> {
        use self::Expr::*;

        match e {
            BuiltinVal(..) | Ref(..) | UnresolvedRef(..)
                => Alteration::forward(e),
            Block(stmts, v) => self.fetch_block(e, stmts, v),
            Call(c, args) => self.fetch_call(e, c, args),
            Constructor(c)
                => self.fetch_constructor(c).combine(e, |c| Constructor(c)),
            FieldAccess(v, f) => self.fetch_field_access(e, v, f),
            If(c, t, f) => self.fetch_if(e, c, t, f),
            Implicit(i) => self.fetch_implicit(i).combine(e, |i| Implicit(i)),
            Loop(stmts) => self.fetch_loop(e, stmts),
            Tuple(t) => self.fetch_tuple(t).combine(e, |t| Tuple(t)),
        }

    }

    fn fetch_field_access(
        &self,
        e: Expr<'g>,
        v: &'g Value<'g>,
        f: Field,
    )
        -> Alteration<Expr<'g>>
    {
        let v = self.fetch_ref(v);
        let f = self.field_of(v.entity.type_, f);

        v.combine2(e, f, |v, f| Expr::FieldAccess(v, f))
    }

    fn fetch_if(
        &self,
        e: Expr<'g>,
        c: &'g Value<'g>,
        t: &'g Value<'g>,
        f: &'g Value<'g>,
    )
        -> Alteration<Expr<'g>>
    {
        let c = self.fetch_ref(c);
        let t = self.fetch_ref(t);
        let f = self.fetch_ref(f);

        c.combine3(e, t, f, |c, t, f| Expr::If(c, t, f))
    }

    fn fetch_implicit(&self, i: Implicit<'g>) -> Alteration<Implicit<'g>> {
        use self::Implicit::*;

        match i {
            ToEnum(e, v) => self.fetch_ref(v).combine(i, |v| ToEnum(e, v)),
        }
    }

    fn fetch_loop(&self, e: Expr<'g>, stmts: &'g [Stmt<'g>])
        -> Alteration<Expr<'g>>
    {
        let stmts = self.core.fetch_slice(stmts, |s| self.fetch_statement(s));

        stmts.combine(e, |stmts| Expr::Loop(stmts))
    }

    fn fetch_tuple(&self, t: Tuple<'g, Value<'g>>)
        -> Alteration<Tuple<'g, Value<'g>>>
    {
        self.core.fetch_tuple(t, |v| self.fetch(v))
    }

    fn fetch_ref(&self, v: &'g Value<'g>) -> Alteration<&'g Value<'g>> {
        self.fetch(*v).map(|v| self.core.insert(v))
    }

    fn fetch_statement(&self, s: Stmt<'g>) -> Alteration<Stmt<'g>> {
        stmt::StatementFetcher::new(self.core).fetch(s)
    }

    fn fetch_type(&self, t: Type<'g>) -> Alteration<Type<'g>> {
        typ::TypeFetcher::new(self.core).fetch(t)
    }

    fn field_of(&self, t: Type<'g>, field: Field) -> Alteration<Field> {
        use self::Type::*;

        let name = match field {
            Field::Index(..) => return Alteration::forward(field),
            Field::Unresolved(n) => n,
        };

        let tuple = match t {
            Rec(r, _) => r.definition,
            Tuple(t) => t,
            _ => return Alteration::forward(field),
        };

        tuple.names.iter()
            .position(|n| n.id() == name.id())
            .map(|u| Alteration::update(Field::Index(u as u16, field.span())))
            .unwrap_or(Alteration::forward(field))
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

        let e =
            i.enum_(p.enum_(i.id(6, 1)).build())
                .push(i.unit(10, 1))
                .push(i.unit(13, 1))
                .build();
        local.insert_enum(e);

        let parent = Type::UnresolvedEnum(*e.prototype, Default::default());

        let unresolved = t.unresolved(i.id(24, 1)).push(parent).build();
        let fetched = t.unresolved_record(i.id(13, 1), 13).push(parent).build();

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

        resolution.entity
    }
}