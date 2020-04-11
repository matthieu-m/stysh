//! Type Fetcher

use crate::model::hir::*;

use super::com::*;

/// Type Fetcher.
#[derive(Clone, Debug)]
pub struct TypeFetcher<'a> {
    core: CoreFetcher<'a>,
    gvn: Gvn,
}

//
//  Public interface of TypeFetcher
//

impl<'a> TypeFetcher<'a> {
    /// Creates a new instance.
    pub fn new(core: CoreFetcher<'a>, gvn: Gvn) -> Self {
        TypeFetcher { core, gvn }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self) -> Status {
        if let Some(e) = self.core.tree().get_gvn_elaborate_type_id(self.gvn) {
            let t = self.core.tree().get_gvn_type_id(self.gvn);
            self.fetch_type(t, e)
        } else {
            //  No elaborate type, no nesting.
            Status::Fetched
        }
    }
}

//
//  Implementation Details
//

impl<'a> TypeFetcher<'a> {
    fn fetch_type(&self, ty: TypeId, e: ElaborateTypeId) -> Status {
        use self::ElaborateType::*;

        //  Force borrow to end early.
        let type_ = self.core.registry().get_type(ty);
        let elaborate = self.core.registry().get_elaborate_type(e);

        match (type_, elaborate) {
            (Type::Tuple(t), ElaborateType::Tuple(e)) => self.fetch_tuple(t, e),
            (_, Unresolved(u, p)) => self.fetch_unresolved(ty, e, u, p),
            _ => Status::Fetched,
        }
    }

    fn fetch_tuple(
        &self,
        t: Tuple<TypeId>,
        e: Tuple<ElaborateTypeId>,
    )
        -> Status
    {
        //  Local copy of slice to avoid keeping a borrow on the tree.
        let types: Vec<_> = {
            let registry = self.core.registry();

            let tys = registry.get_type_ids(t.fields).iter().copied();
            let es = registry.get_elaborate_type_ids(e.fields).iter().copied();

            tys.zip(es).collect()
        };

        let mut status = Status::Fetched;

        for (ty, e) in types {
            status = status.combine(self.fetch_type(ty, e));
        }

        status
    }

    fn fetch_unresolved(
        &self,
        ty: TypeId,
        e: ElaborateTypeId,
        name: ItemIdentifier,
        p: PathId,
    )
        -> Status
    {
        let (parent, status) = self.fetch_path(p);

        if status == Status::Unfetched {
            return status;
        }

        if let Some(enum_) = parent {
            return self.fetch_enum_variant(ty, e, p, name, enum_);
        }

        let type_ = self.core.scope.lookup_type(name);

        match type_ {
            Type::Enum(..) | Type::Rec(..) => {
                let elaborate = type_.elaborate(Default::default(), p);
                self.core.tree_mut().set_type(ty, type_);
                self.core.tree_mut().set_elaborate_type(e, elaborate);
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
        e: ElaborateTypeId,
        path: PathId,
        name: ItemIdentifier,
        enum_: EnumId,
    )
        -> Status
    {
        use std::convert::TryInto;

        let mut record = None;
        {
            let registry = self.core.registry();
            for r in registry.get_record_ids(registry.get_enum(enum_).variants) {
                if name.id() == registry.get_record(*r).name.id() {
                    record = Some(ElaborateType::Rec(*r, path));
                    break;
                }
            }
        }

        if let Some(r) = record {
            self.core.tree_mut().set_type(ty, r.try_into().expect("Record"));
            self.core.tree_mut().set_elaborate_type(e, r);
            Status::Fetched
        } else {
            Status::Unfetched
        }
    }
}
