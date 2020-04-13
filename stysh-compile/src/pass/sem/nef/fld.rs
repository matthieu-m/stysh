//! Field Fetcher

use crate::basic::com::Span;

use crate::model::hir::*;

use super::com::*;
use super::super::scp::TypeScope;

/// Field Fetcher.
#[derive(Clone, Debug)]
pub struct FieldFetcher<'a> {
    core: CoreFetcher<'a>,
    gvn: Gvn,
}

//
//  Public interface of FieldFetcher
//

impl<'a> FieldFetcher<'a> {
    /// Creates an instance.
    pub fn new(core: CoreFetcher<'a>, gvn: Gvn) -> Self {
        FieldFetcher { core, gvn }
    }

    /// Fetches the fields.
    pub fn fetch(&self) -> Status {
        self.fetch_children()
    }
}

//
//  Implementation of FieldFetcher
//

impl<'a> FieldFetcher<'a> {
    fn fetch_children(&self) -> Status {
        use self::Expression as E;

        if let Some(e) = self.gvn.as_expression() {
            let expr = self.core.tree().get_expression(e);

            match expr {
                E::FieldAccess(accessed, field)
                    => self.fetch_field(e, field, accessed),
                E::Call(Callable::Unknown(name), Some(receiver), arguments)
                    => self.fetch_method(e, name, receiver, arguments),
                _ => Status::Fetched,
            }
        } else {
            Status::Fetched
        }
    }

    fn fetch_field(
        &self,
        e: ExpressionId,
        field: Field,
        accessed: ExpressionId
    )
        -> Status
    {
        use self::Field::*;

        match field {
            Index(..) => Status::Fetched,
            Unresolved(n) => self.fetch_field_unresolved(e, n, accessed),
        }
    }

    fn fetch_field_unresolved(
        &self,
        e: ExpressionId,
        name: ValueIdentifier,
        accessed: ExpressionId,
    )
        -> Status
    {
        if let Some((field, typ)) = self.fetch_field_unresolved_impl(accessed, name) {
            let typ = self.core.registry().get_type(typ);
            let mut tree = self.core.tree_mut();

            tree.set_expression(e, Expression::FieldAccess(accessed, field));
            tree.set_expression_type(e, typ);

            return Status::Fetched;
        }

        Status::Unfetched
    }

    fn fetch_field_unresolved_impl(
        &self,
        accessed: ExpressionId,
        name: ValueIdentifier
    )
        -> Option<(Field, TypeId)>
    {
        use self::Type::*;

        let tree = self.core.tree();
        let registry = self.core.registry();

        let definition = match tree.get_expression_type(accessed) {
            Rec(id) => registry.get_record(id).definition,
            Tuple(t) => t,
            _ => return None,
        };

        let types = registry.get_type_ids(definition.fields);
        let names = registry.get_names(definition.names);
        debug_assert!(names.len() == types.len());

        names.iter()
            .position(|n| *n == name.id())
            .map(|i| (Field::Index(i as u16, name.span()), types[i]))
    }

    fn fetch_method(
        &self,
        e: ExpressionId,
        name: ValueIdentifier,
        receiver: ExpressionId,
        arguments: Tuple<ExpressionId>,
    )
        -> Status
    {
        use self::Type::*;

        let typ = self.core.tree().get_expression_type(receiver);

        match typ {
            Builtin(..) | Enum(..) | Int(..) | Rec(..)
                => self.fetch_method_impl(e, name, typ, receiver, arguments),
            Tuple(..)
                => unimplemented!("fetch_method of Tuple"),
            Unresolved
                => Status::Unfetched
        }
    }

    fn fetch_method_impl(
        &self,
        e: ExpressionId,
        name: ValueIdentifier,
        typ: Type,
        receiver: ExpressionId,
        arguments: Tuple<ExpressionId>,
    )
        -> Status
    {
        use self::Callable::*;

        let candidate = {
            let scope = TypeScope::new(self.core.scope, typ);
            scope.lookup_associated_method(name, self.core.registry)
        };

        let callable = candidate.into_callable(&mut *self.core.tree_mut());

        let result = match callable {
            Builtin(b)
                => Some(b.result_type()),
            Function(f) | Method(f) => {
                let typ = self.core.registry.get_function(f).result;
                Some(self.core.registry.get_type(typ))
            },
            _ => None,
        };

        self.core.tree_mut()
            .set_expression(e, Expression::Call(callable, Some(receiver), arguments));

        if let Some(typ) = result {
            self.core.tree_mut().set_expression_type(e, typ);
        }

        //  All the information necessary to resolving the method call should
        //  be available; if it cannot be resolved now, there is no point in
        //  retrying later.
        Status::Fetched
    }
}
