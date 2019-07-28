//! Type Fetcher

use model::hir::*;

use super::com::*;

/// Type Fetcher.
#[derive(Clone, Debug)]
pub struct TypeFetcher<'a> {
    core: CoreFetcher<'a>,
    ty: TypeId,
}

//
//  Public interface of TypeFetcher
//

impl<'a> TypeFetcher<'a> {
    /// Creates a new instance.
    pub fn new(core: CoreFetcher<'a>, gvn: Gvn) -> Self {
        let ty = core.tree().get_gvn_type_id(gvn);
        TypeFetcher { core, ty }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self) -> Status {
        self.fetch_type(self.ty)
    }
}

//
//  Implementation Details
//

impl<'a> TypeFetcher<'a> {
    fn fetch_type(&self, ty: TypeId) -> Status {
        use self::Type::*;

        //  Force borrow to end early.
        let type_ = self.core.registry().get_type(ty);

        match type_ {
            Tuple(t) => self.fetch_tuple(t),
            Unresolved(u, p) => self.fetch_unresolved(ty, u, p),
            _ => Status::Fetched,
        }
    }

    fn fetch_tuple(&self, t: Tuple<TypeId>) -> Status {
        //  Local copy of slice to avoid keeping a borrow on the tree.
        let tys: Vec<_> =
            self.core.registry().get_type_ids(t.fields).iter().cloned().collect();

        let mut status = Status::Fetched;

        for ty in tys {
            status = status.combine(self.fetch_type(ty));
        }

        status
    }

    fn fetch_unresolved(&self, ty: TypeId, name: ItemIdentifier, p: PathId)
        -> Status
    {
        let (parent, status) = self.fetch_path(p);

        if status == Status::Unfetched {
            return status;
        }

        if let Some(e) = parent {
            return self.fetch_enum_variant(ty, p, name, e);
        }

        let type_ = self.core.scope.lookup_type(name);

        match type_ {
            Type::Enum(..) | Type::Rec(..) => {
                self.core.tree_mut().set_type(ty, type_.with_path(p));
                Status::Fetched
            },
            _ => Status::Unfetched,
        }
    }

    fn fetch_path(&self, path: PathId) -> (Option<EnumId>, Status) {
        let registry = self.core.registry();

        let path = registry.get_path_components(path);
        assert!(path.len() <= 1, "Deeply nested types not implemented.");

        match path.first() {
            Some(PathComponent::Enum(e, _)) => (Some(*e), Status::Fetched),
            Some(_) => (None, Status::Unfetched),
            None => (None, Status::Fetched),
        }
    }

    fn fetch_enum_variant(
        &self,
        ty: TypeId,
        path: PathId,
        name: ItemIdentifier,
        e: EnumId,
    )
        -> Status
    {
        let mut record = None;
        {
            let registry = self.core.registry();
            for r in registry.get_record_ids(registry.get_enum(e).variants) {
                if name.id() == registry.get_record(*r).name.id() {
                    record = Some(Type::Rec(*r, path));
                    break;
                }
            }
        }

        if let Some(r) = record {
            self.core.tree_mut().set_type(ty, r);
            Status::Fetched
        } else {
            Status::Unfetched
        }
    }
}
