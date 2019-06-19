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
        let type_ = self.core.tree().get_type(ty);

        match type_ {
            Tuple(t) => self.fetch_tuple(t),
            Unresolved(u, p) => self.fetch_unresolved(ty, u, p),
            _ => Status::Fetched,
        }
    }

    fn fetch_tuple(&self, t: Tuple<TypeId>) -> Status {
        //  Local copy of slice to avoid keeping a borrow on the tree.
        let tys: Vec<_> =
            self.core.tree().get_type_ids(t.fields).iter().cloned().collect();

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
            return self.fetch_enum_variant(ty, p, name, &e);
        }

        if let Some(e) = self.core.registry.lookup_enum(name) {
            let mut tree = self.core.tree_mut();

            let e = tree.insert_enum(&e);
            tree.set_type(ty, e);

            return Status::Fetched;
        }

        if let Some(r) = self.core.registry.lookup_record(name) {
            let mut tree = self.core.tree_mut();

            let r = tree.insert_record(&r);
            tree.set_type(ty, r);

            return Status::Fetched;
        }

        Status::Unfetched
    }

    fn fetch_path(&self, path: PathId) -> (Option<Enum>, Status) {
        let tree = self.core.tree();

        let path = tree.get_path(path);
        assert!(path.len() <= 1, "Deeply nested types not implemented.");

        if let Some(first) = path.first() {
            if let Some(e) = self.core.registry.lookup_enum(*first) {
                (Some(e), Status::Fetched)
            } else {
                (None, Status::Unfetched)
            }
        } else {
            (None, Status::Fetched)
        }
    }

    fn fetch_enum_variant(
        &self,
        ty: TypeId,
        path: PathId,
        name: ItemIdentifier,
        e: &Enum
    )
        -> Status
    {
        self.core.tree_mut().insert_enum(e);

        for r in &e.variants {
            if name.id() == r.prototype.name.id() {
                let r = self.core.tree_mut().insert_record(&r);
                self.core.tree_mut().set_type(ty, r.with_path(path));
                return Status::Fetched;
            }
        }

        Status::Unfetched
    }
}
