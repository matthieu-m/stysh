//! Value Finalizer.

use model::hir::*;
use super::{com, flat, stmt, typ, Alteration};

/// Value Finalizer.
#[derive(Clone, Debug)]
pub struct ValueFinalizer<'a, 'g: 'a> {
    core: com::CoreFinalizer<'a, 'g>,
}

//
//  Public interface of ValueFinalizer
//

impl<'a, 'g: 'a> ValueFinalizer<'a, 'g> {
    /// Creates a new instance.
    pub fn new(core: com::CoreFinalizer<'a, 'g>) -> Self {
        ValueFinalizer { core }
    }

    /// Finalizes the value.
    pub fn finalize(&self, v: Value<'g>) -> Alteration<Value<'g>> {
        let type_ = self.finalize_type(v.type_, v.gvn);
        let expr = self.finalize_expression(v.expr, v.gvn);
        let (range, gvn) = (v.range, v.gvn);

        type_.combine2(v, expr, |type_, expr| {
            Value { type_, range, expr, gvn }
        })
    }
}

//
//  Implementation Details
//

impl<'a, 'g: 'a> ValueFinalizer<'a, 'g> {
    fn finalize_block(
        &self,
        e: Expr<'g>,
        stmts: &'g [Stmt<'g>],
        v: Option<&'g Value<'g>>
    )
        -> Alteration<Expr<'g>>
    {
        let stmts = self.core.finalize_slice(stmts, |s| self.finalize_statement(s));
        let v = self.core.finalize_option(v, |v| {
            self.finalize(*v).map(|v| self.core.insert(v))
        });

        stmts.combine2(e, v, |stmts, v| Expr::Block(stmts, v))
    }

    fn finalize_call(
        &self,
        e: Expr<'g>,
        c: Callable<'g>,
        v: &'g [Value<'g>],
    )
        -> Alteration<Expr<'g>>
    {
        let v = self.core.finalize_slice(v, |v| self.finalize(v));
        v.combine(e, |v| Expr::Call(c, v))
    }

    fn finalize_constructor(&self, c: Constructor<'g, Value<'g>>, gvn: Gvn)
        -> Alteration<Constructor<'g, Value<'g>>>
    {
        let type_ = self.finalize_type(c.type_, gvn);
        let arguments = self.finalize_tuple(c.arguments);
        let range = c.range;

        type_.combine2(c, arguments, |type_, arguments| {
            Constructor { type_, arguments, range }
        })
    }

    fn finalize_expression(&self, e: Expr<'g>, gvn: Gvn)
        -> Alteration<Expr<'g>>
    {
        use self::Expr::*;

        match e {
            BuiltinVal(..) | Ref(..) | UnresolvedRef(..)
                => Alteration::forward(e),
            Block(stmts, v) => self.finalize_block(e, stmts, v),
            Call(c, args) => self.finalize_call(e, c, args),
            Constructor(c)
                => self.finalize_constructor(c, gvn)
                    .combine(e, |c| Constructor(c)),
            FieldAccess(v, f) => self.finalize_field_access(e, gvn, v, f),
            If(c, t, f) => self.finalize_if(e, c, t, f),
            Implicit(i) => self.finalize_implicit(i).combine(e, |i| Implicit(i)),
            Loop(stmts) => self.finalize_loop(e, stmts),
            Tuple(t) => self.finalize_tuple(t).combine(e, |t| Tuple(t)),
        }

    }

    fn finalize_field_access(
        &self,
        e: Expr<'g>,
        g: Gvn,
        v: &'g Value<'g>,
        f: Field,
    )
        -> Alteration<Expr<'g>>
    {
        let children = self.core.context.value(g).children();
        let field = if let flat::ValueChildren::FieldOf(field, _) = children {
            field
        } else {
            panic!("Unexpected children for FieldAccess: {:?}", children);
        };

        let v = self.finalize_ref(v);
        let f = Alteration::update_if(field, f != field);

        v.combine2(e, f, |v, f| Expr::FieldAccess(v, f))
    }

    fn finalize_if(
        &self,
        e: Expr<'g>,
        c: &'g Value<'g>,
        t: &'g Value<'g>,
        f: &'g Value<'g>,
    )
        -> Alteration<Expr<'g>>
    {
        let c = self.finalize_ref(c);
        let t = self.finalize_ref(t);
        let f = self.finalize_ref(f);

        c.combine3(e, t, f, |c, t, f| Expr::If(c, t, f))
    }

    fn finalize_implicit(&self, i: Implicit<'g>) -> Alteration<Implicit<'g>> {
        use self::Implicit::*;

        match i {
            ToEnum(e, v) => self.finalize_ref(v).combine(i, |v| ToEnum(e, v)),
        }
    }

    fn finalize_loop(&self, e: Expr<'g>, stmts: &'g [Stmt<'g>])
        -> Alteration<Expr<'g>>
    {
        let stmts = self.core.finalize_slice(stmts, |s| self.finalize_statement(s));

        stmts.combine(e, |stmts| Expr::Loop(stmts))
    }

    fn finalize_tuple(&self, t: Tuple<'g, Value<'g>>)
        -> Alteration<Tuple<'g, Value<'g>>>
    {
        self.core.finalize_tuple(t, |v| self.finalize(v))
    }

    fn finalize_ref(&self, v: &'g Value<'g>) -> Alteration<&'g Value<'g>> {
        self.finalize(*v).map(|v| self.core.insert(v))
    }

    fn finalize_statement(&self, s: Stmt<'g>) -> Alteration<Stmt<'g>> {
        stmt::StatementFinalizer::new(self.core).finalize(s)
    }

    fn finalize_type(&self, t: Type<'g>, gvn: Gvn) -> Alteration<Type<'g>> {
        typ::TypeFinalizer::new(self.core).finalize(t, gvn)
    }
}
