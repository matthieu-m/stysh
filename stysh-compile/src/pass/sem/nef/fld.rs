//! Field Fetcher

use basic::com::Span;

use model::hir::*;

use super::flat::{ValueChildren, ValueHandle};
use super::com::*;

/// Field Fetcher.
#[derive(Clone, Debug)]
pub struct FieldFetcher<'a, 'g: 'a> {
    core: CoreFetcher<'a, 'g>,
    handle: ValueHandle<'a, 'g>,
}

//
//  Public interface of FieldFetcher
//

impl<'a, 'g: 'a> FieldFetcher<'a, 'g> {
    /// Creates an instance.
    pub fn new(core: CoreFetcher<'a, 'g>, handle: ValueHandle<'a, 'g>)
        -> Self
    {
        FieldFetcher { core, handle }
    }

    /// Fetches the fields.
    pub fn fetch(&self) -> Status {
        self.fetch_children()
    }
}

//
//  Implementation of FieldFetcher
//

impl<'a, 'g: 'a> FieldFetcher<'a, 'g> {
    fn fetch_children(&self) -> Status {
        use self::ValueChildren::*;

        match self.handle.children() {
            FieldOf(field, accessed) => self.fetch_field(field, accessed),
            Leaf => Status::Fetched,
        }
    }

    fn fetch_field(&self, field: Field, accessed: Gvn) -> Status {
        use self::Field::*;

        match field {
            Index(..) => Status::Fetched,
            Unresolved(n) => self.fetch_unresolved(accessed, n),
        }
    }

    fn fetch_unresolved(&self, accessed: Gvn, name: ValueIdentifier) -> Status {
        use self::Type::*;

        let fields = match self.core.context.value(accessed).type_() {
            Rec(r, ..) => r.definition,
            Tuple(t, ..) => t,
            _ => return Status::Unfetched,
        };

        if let Some(i) = fields.names.iter().position(|n| n.id() == name.id()) {
            let field = Field::Index(i as u16, name.span());
            self.handle.set_children(ValueChildren::FieldOf(field, accessed));
            //  TODO:   queue GVN for TUP (once migrated).
        }

        //  Job done, whether it was found or not:
        //  -   if it was found, it's really done.
        //  -   if it was not, it never will be anyway.
        Status::Fetched
    }
}
