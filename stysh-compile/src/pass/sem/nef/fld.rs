//! Field Fetcher

use basic::com::Span;

use model::hir::*;

use super::com::*;

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
        if let Some(e) = self.gvn.as_expression() {
            let expr = self.core.tree().get_expression(e);

            match expr {
                Expression::FieldAccess(accessed, field)
                    => self.fetch_field(e, field, accessed),
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
            Unresolved(n) => self.fetch_unresolved(e, n, accessed),
        }
    }

    fn fetch_unresolved(
        &self,
        e: ExpressionId,
        name: ValueIdentifier,
        accessed: ExpressionId,
    )
        -> Status
    {
        if let Some((field, typ)) = self.fetch_unresolved_impl(accessed, name) {
            let typ = self.core.registry().get_type(typ);
            let mut tree = self.core.tree_mut();

            tree.set_expression(e, Expression::FieldAccess(accessed, field));
            tree.set_expression_type(e, typ);

            return Status::Fetched;
        }

        Status::Unfetched
    }

    fn fetch_unresolved_impl(
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
            Rec(id, _) => registry.get_record(id).definition,
            Tuple(t) => t,
            _ => return None,
        };

        let types = registry.get_type_ids(definition.fields);
        let names = registry.get_names(definition.names);

        names.iter()
            .position(|n| n.id() == name.id())
            .map(|i| (Field::Index(i as u16, name.span()), types[i]))
    }
}
