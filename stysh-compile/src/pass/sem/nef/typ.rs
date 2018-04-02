//! Type Fetcher

use basic::mem;

use model::hir::*;

use super::Alteration;
use super::com::*;
use super::flat::ValueHandle;

/// Type Fetcher.
#[derive(Clone, Debug)]
pub struct TypeFetcher<'a, 'g: 'a> {
    core: CoreFetcher<'a, 'g>,
    handle: ValueHandle<'a, 'g>,
}

//
//  Public interface of TypeFetcher
//

impl<'a, 'g: 'a> TypeFetcher<'a, 'g> {
    /// Creates a new instance.
    pub fn new(core: CoreFetcher<'a, 'g>, handle: ValueHandle<'a, 'g>)
        -> Self
    {
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

type Result<'g> = (Alteration<Type<'g>>, Status);

impl<'a, 'g: 'a> TypeFetcher<'a, 'g> {
    fn fetch_tuple(&self, t: Tuple<'g, Type<'g>>, gin: Gin) -> Result<'g> {
        let (fields, status) =
            self.fetch_slice(t.fields, |t| self.fetch_type(t));
        let fields = fields.combine(t, |f| Tuple { fields: f, names: t.names });

        (fields.map(|f| Type::Tuple(f, gin)), status)
    }

    fn fetch_type(&self, ty: Type<'g>) -> Result<'g> {
        use self::Type::*;

        match self.handle.type_() {
            Tuple(t, g) => self.fetch_tuple(t, g),
            Unresolved(u, p, _) => self.fetch_unresolved(ty, u, p),
            UnresolvedEnum(e, p, _) => self.fetch_unresolved_enum(ty, e, p),
            UnresolvedRec(r, p, _) => self.fetch_unresolved_record(ty, r, p),
            _ => (Alteration::forward(ty), Status::Fetched),
        }
    }

    fn fetch_unresolved(&self, ty: Type<'g>, i: ItemIdentifier, p: Path<'g>)
        -> Result<'g>
    {
        //  Cannot fetch anything without a root.
        if let Some(&Type::Unresolved(..)) = p.components.first() {
            return (Alteration::forward(ty), Status::Unfetched);
        }

        let mut last = None;
        let (path, status) = 
            self.fetch_slice(p.components, |c| {
                    let (result, status) = if let Some(parent) = last {
                        self.fetch_nested(c, parent)
                    } else {
                        (Alteration::forward(c), Status::Fetched)
                    };
                    last = Some(result.entity);
                    (result, status)
                });
        let path = path.map(|components| Path { components });

        if let Some(parent) = last {
            let (type_, s) = self.fetch_nested(
                Type::Unresolved(i, Path::default(), ty.gin()),
                parent
            );
            (type_.combine2(ty, path, |t, p| t.with_path(p)), status.combine(s))
        } else {
            (path.combine(ty, |p| Type::Unresolved(i, p, ty.gin())), Status::Unfetched)
        }
    }

    fn fetch_unresolved_enum(&self, ty: Type<'g>, e: EnumProto, p: Path<'g>)
        -> Result<'g>
    {
        self.core.registry
            .lookup_enum(e.name)
            .map(|e| (
                Alteration::update(Type::Enum(self.core.insert(e), p, ty.gin())),
                Status::Fetched,
            ))
            .unwrap_or((Alteration::forward(ty), Status::Unfetched))
    }

    fn fetch_unresolved_record(&self, ty: Type<'g>, r: RecordProto, p: Path<'g>)
        -> Result<'g>
    {
        self.core.registry
            .lookup_record(r.name)
            .map(|r| (
                Alteration::update(Type::Rec(self.core.insert(r), p, ty.gin())),
                Status::Fetched,
            ))
            .unwrap_or((Alteration::forward(ty), Status::Unfetched))
    }

    fn fetch_enum_variant(&self, v: ItemIdentifier, e: ItemIdentifier, gin: Gin)
        -> Result<'g>
    {
        if let Some(e) = self.core.registry.lookup_enum(e) {
            for rec in e.variants {
                if v.id() == rec.prototype.name.id() {
                    let r = Type::UnresolvedRec(*rec.prototype, Path::default(), gin);
                    return (Alteration::update(r), Status::Fetched);
                }
            }
        }

        (Alteration::forward(Type::Unresolved(v, Path::default(), gin)),
            Status::Unfetched)
    }

    fn fetch_nested(&self, t: Type<'g>, parent: Type<'g>)
        -> Result<'g>
    {
        use self::Type::*;

        let unfetched = (Alteration::forward(t), Status::Unfetched);

        match (t, parent) {
            (Unresolved(id, ..), UnresolvedEnum(e, ..))
                => self.fetch_enum_variant(id, e.name, t.gin()),
            (Unresolved(..), Unresolved(..)) => unfetched,
            (Unresolved(..), _) => panic!("{:?} cannot be a parent!", parent),
            (UnresolvedEnum(..), _) | (UnresolvedRec(..), _) => unfetched,
            _ => panic!("{:?} cannot be nested!", t),
        }
    }

    fn fetch_slice<T, F>(&self, slice: &'g [T], mut f: F)
        -> (Alteration<&'g [T]>, Status)
        where
            T: Copy + 'g,
            F: FnMut(T) -> (Alteration<T>, Status),
    {
        let mut found = None;

        for (i, e) in slice.iter().enumerate() {
            let (a, s) = f(*e);

            if a.altered > 0 {
                found = Some((i, a, s));
                break;
            }
        }

        if found.is_none() {
            return (Alteration::forward(slice), Status::Unfetched);
        }

        let (index, alteration, mut status) = found.unwrap();

        let length = slice.len();
        let mut result = mem::Array::with_capacity(length, self.core.global_arena);

        for e in &slice[..(index as usize)] {
            result.push(*e);
        }

        let mut altered = alteration.altered;
        result.push(alteration.entity);

        if index as usize + 1 < length {
            for e in &slice[(index as usize + 1)..] {
                let (a, s) = f(*e);

                altered += a.altered;
                status = status.combine(s);

                result.push(a.entity);
            }
        }

        let entity = result.into_slice();
        (Alteration { entity, altered }, status)
    }
}
