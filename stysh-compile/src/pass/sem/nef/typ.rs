//! Type Fetcher

use basic::mem::DynArray;

use model::hir::*;

use super::Alteration;
use super::com::*;
use super::flat::ValueHandle;

/// Type Fetcher.
#[derive(Clone, Debug)]
pub struct TypeFetcher<'a> {
    core: CoreFetcher<'a>,
    handle: ValueHandle<'a>,
}

//
//  Public interface of TypeFetcher
//

impl<'a> TypeFetcher<'a> {
    /// Creates a new instance.
    pub fn new(core: CoreFetcher<'a>, handle: ValueHandle<'a>) -> Self {
        TypeFetcher { core, handle }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self) -> Status {
        let (ty, status) = self.fetch_type(self.handle.type_());
        self.handle.set_type(ty.entity);
        status
    }
}

//
//  Implementation Details
//

type Result = (Alteration<Type>, Status);

impl<'a> TypeFetcher<'a> {
    fn fetch_tuple(&self, t: Tuple<Type>, gin: Gin) -> Result {
        let (fields, status) =
            self.fetch_array(t.fields.clone(), |t| self.fetch_type(t));
        let fields = fields.combine(t, |t, f| Tuple { fields: f, names: t.names });

        (fields.map(|f| Type::Tuple(f, gin)), status)
    }

    fn fetch_type(&self, ty: Type) -> Result {
        use self::Type::*;

        match self.handle.type_() {
            Tuple(t, g) => self.fetch_tuple(t, g),
            Unresolved(u, p, _) => self.fetch_unresolved(ty, u, p),
            UnresolvedEnum(e, p, _) => self.fetch_unresolved_enum(ty, e, p),
            UnresolvedRec(r, p, _) => self.fetch_unresolved_record(ty, r, p),
            _ => (Alteration::forward(ty), Status::Fetched),
        }
    }

    fn fetch_unresolved(&self, ty: Type, i: ItemIdentifier, p: Path)
        -> Result
    {
        //  Cannot fetch anything without a root.
        if let Some(Type::Unresolved(..)) = p.components.first() {
            return (Alteration::forward(ty), Status::Unfetched);
        }

        let mut last = None;
        let mut result = Status::Fetched;
        let mut altered = 0;

        for i in 0..p.components.len() {
            let element = p.components.replace_with_default(i);

            let (alteration, status) = {
                let (result, status) = if let Some(parent) = last {
                    self.fetch_nested(element, parent)
                } else {
                    (Alteration::forward(element), Status::Fetched)
                };
                last = Some(result.entity.clone());
                (result, status)
            };

            altered += alteration.altered;
            p.components.replace(i, alteration.entity);
            result = result.combine(status);
        }

        let path = Alteration { entity: p, altered };

        if let Some(parent) = last {
            let (type_, s) = self.fetch_nested(
                Type::Unresolved(i, Path::default(), ty.gin()),
                parent
            );
            (type_.combine2(ty, path, |_, t, p| t.with_path(p)), result.combine(s))
        } else {
            (path.combine(ty, |ty, p| Type::Unresolved(i, p, ty.gin())), Status::Unfetched)
        }
    }

    fn fetch_unresolved_enum(&self, ty: Type, e: EnumProto, p: Path)
        -> Result
    {
        self.core.registry
            .lookup_enum(e.name)
            .map(|e| (
                Alteration::update(Type::Enum(e, p, ty.gin())),
                Status::Fetched,
            ))
            .unwrap_or((Alteration::forward(ty), Status::Unfetched))
    }

    fn fetch_unresolved_record(&self, ty: Type, r: RecordProto, p: Path)
        -> Result
    {
        self.core.registry
            .lookup_record(r.name)
            .map(|r| (
                Alteration::update(Type::Rec(r, p, ty.gin())),
                Status::Fetched,
            ))
            .unwrap_or((Alteration::forward(ty), Status::Unfetched))
    }

    fn fetch_enum_variant(&self, v: ItemIdentifier, e: ItemIdentifier, gin: Gin)
        -> Result
    {
        if let Some(e) = self.core.registry.lookup_enum(e) {
            for rec in e.variants {
                if v.id() == rec.prototype.name.id() {
                    let r = Type::UnresolvedRec(rec.prototype, Path::default(), gin);
                    return (Alteration::update(r), Status::Fetched);
                }
            }
        }

        (Alteration::forward(Type::Unresolved(v, Path::default(), gin)),
            Status::Unfetched)
    }

    fn fetch_nested(&self, t: Type, parent: Type) -> Result {
        use self::Type::*;

        let unfetched = (Alteration::forward(t.clone()), Status::Unfetched);
        let gin = t.gin();

        match (t, parent) {
            (Unresolved(id, ..), UnresolvedEnum(e, ..))
                => self.fetch_enum_variant(id, e.name, gin),
            (Unresolved(..), Unresolved(..)) => unfetched,
            (Unresolved(..), parent) => panic!("{:?} cannot be a parent!", parent),
            (UnresolvedEnum(..), _) | (UnresolvedRec(..), _) => unfetched,
            (t, _) => panic!("{:?} cannot be nested!", t),
        }
    }

    fn fetch_array<T, F>(&self, array: DynArray<T>, mut fun: F)
        -> (Alteration<DynArray<T>>, Status)
        where
            T: Default,
            F: FnMut(T) -> (Alteration<T>, Status),
    {
        let mut result = Status::Fetched;
        let mut altered = 0;

        for i in 0..array.len() {
            let element = array.replace_with_default(i);

            let (alteration, status) = fun(element);

            altered += alteration.altered;
            array.replace(i, alteration.entity);
            result = result.combine(status);
        }

        (Alteration { entity: array, altered }, result)
    }
}
