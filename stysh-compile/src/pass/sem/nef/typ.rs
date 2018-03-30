//! Type Fetcher

use model::hir::*;
use super::{common, Resolution};

/// Type Fetcher.
#[derive(Clone, Debug)]
pub struct TypeFetcher<'a, 'g>
    where 'g: 'a
{
    core: common::CoreFetcher<'a, 'g>,
}

//
//  Public interface of TypeFetcher
//

impl<'a, 'g> TypeFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreFetcher<'a, 'g>) -> Self {
        TypeFetcher { core }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self, ty: Type<'g>) -> Resolution<Type<'g>> {
        use self::Type::*;

        match ty {
            Tuple(t) => self.fetch_tuple(t).combine(ty, |t| Tuple(t)),
            Unresolved(u, p) => self.fetch_unresolved(ty, u, p),
            _ => Resolution::forward(ty),
        }
    }
}

//
//  Implementation Details
//

impl<'a, 'g> TypeFetcher<'a, 'g>
    where 'g: 'a
{
    fn fetch_tuple(&self, t: Tuple<'g, Type<'g>>)
        -> Resolution<Tuple<'g, Type<'g>>>
    {
        self.core.fetch_tuple(t, |t| self.fetch(t))
    }

    fn fetch_unresolved(&self, ty: Type<'g>, i: ItemIdentifier, p: Path<'g>)
        -> Resolution<Type<'g>>
    {
        //  Cannot fetch anything without a root.
        if let Some(&Type::Unresolved(..)) = p.components.first() {
            return Resolution::forward(ty);
        }

        let mut last = None;
        let path =
            self.core
                .fetch_slice(p.components, |c| {
                    let result = if let Some(parent) = last {
                        self.fetch_nested(c, parent)
                    } else {
                        Resolution::forward(c)
                    };
                    last = Some(result.entity);
                    result
                })
                .map(|components| Path { components });

        if let Some(parent) = last {
            let type_ = self.fetch_nested(
                Type::Unresolved(i, Path::default()),
                parent
            );
            type_.combine2(ty, path, |t, p| t.with_path(p))
        } else {
            path.combine(ty, |p| Type::Unresolved(i, p))
        }
    }

    fn fetch_nested(&self, t: Type<'g>, parent: Type<'g>)
        -> Resolution<Type<'g>>
    {
        use self::Type::*;

        match (t, parent) {
            (Unresolved(id, _), Enum(e, _))
                => self.fetch_enum_variant(id, e.name),
            (Unresolved(..), Unresolved(..)) => Resolution::forward(t),
            (Unresolved(..), _) => panic!("{:?} cannot be a parent!", parent),
            (Enum(..), _) | (Rec(..), _) => Resolution::forward(t),
            _ => panic!("{:?} cannot be nested!", t),
        }
    }

    fn fetch_enum_variant(&self, v: ItemIdentifier, e: ItemIdentifier)
        -> Resolution<Type<'g>>
    {
        if let Some(e) = self.core.registry.lookup_enum(e) {
            for rec in e.variants {
                if v.id() == rec.prototype.name.id() {
                    self.fetched_item(v);
                    let r = Type::Rec(*rec.prototype, Path::default());
                    return Resolution::forward(r).with_altered(1);
                }
            }
        }

        Resolution::forward(Type::Unresolved(v, Path::default()))
    }

    fn fetched_item(&self, i: ItemIdentifier) {
        self.core.context.fetched_item(i);
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::TypeFetcher;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn record_nested() {
        let env = Env::default();
        let (i, _, pro, _, t, _) = env.factories();

        let mut local = env.local(b":enum E { A, B };    E::B");
        local.mark_unfetched_items(&[i.id(24, 1)]);

        let e =
            i.enum_(pro.enum_(i.id(6, 1)).build())
                .push(i.unit(10, 1))
                .push(i.unit(13, 1))
                .build();
        local.insert_enum(e);

        let parent = Type::Enum(*e.prototype, Default::default());

        let unresolved = t.unresolved(i.id(24, 1)).push(parent).build();
        let fetched = t.record(i.id(13, 1), 13).push(parent).build();

        assert_eq!(fetch(&local, 1, unresolved), fetched);
    }

    fn fetch<'g>(local: &LocalEnv<'g>, altered: u32, ty: Type<'g>) -> Type<'g> {
        let ty = local.resolver().resolve_type(ty);

        let resolution =
            TypeFetcher::new(local.core())
                .fetch(ty)
                .map(|t| local.scrubber().scrub_type(t));

        assert_eq!(resolution.altered, altered);
        assert_eq!(resolution.introduced, 0);

        resolution.entity
    }

}
