//! Pattern Fetcher.

use model::hir::*;
use super::{common, typ, Resolution};

/// Pattern Fetcher.
#[derive(Clone, Debug)]
pub struct PatternFetcher<'a, 'g>
    where 'g: 'a
{
    core: common::CoreFetcher<'a, 'g>,
}

//
//  Public interface of PatternFetcher
//

impl<'a, 'g> PatternFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreFetcher<'a, 'g>) -> Self {
        PatternFetcher { core }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self, p: Pattern<'g>) -> Resolution<Pattern<'g>> {
        use self::Pattern::*;

        match p {
            Constructor(c)
                => self.fetch_constructor(c).combine(p, |c| Constructor(c)),
            Tuple(t, r) => self.fetch_tuple(t).combine(p, |t| Tuple(t, r)),
            _ => Resolution::forward(p),
        }
    }
}

//
//  Implementation Details
//

impl<'a, 'g> PatternFetcher<'a, 'g>
    where 'g: 'a
{
    fn fetch_constructor(&self, c: Constructor<'g, Pattern<'g>>)
        -> Resolution<Constructor<'g, Pattern<'g>>>
    {
        let type_ = typ::TypeFetcher::new(self.core).fetch(c.type_);
        let arguments = self.fetch_tuple(c.arguments);
        let range = c.range;

        type_.combine2(c, arguments, |type_, arguments| {
            Constructor { type_, arguments, range }
        })
    }

    fn fetch_tuple(&self, t: Tuple<'g, Pattern<'g>>)
        -> Resolution<Tuple<'g, Pattern<'g>>>
    {
        self.core.fetch_tuple(t, |p| self.fetch(p))
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::PatternFetcher;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn constructor_nested() {
        let env = Env::default();
        let (i, pat, pro, _, t, _) = env.factories();

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

        assert_eq!(
            fetch(
                &local, 1,
                pat.constructor(unresolved, 21, 4).build_pattern(),
            ),
            pat.constructor(fetched, 21, 4).build_pattern()
        );
    }

    fn fetch<'g>(local: &LocalEnv<'g>, altered: u32, pat: Pattern<'g>)
        -> Pattern<'g>
    {
        let pat = local.resolver().resolve_pattern(pat);

        let resolution =
            PatternFetcher::new(local.core())
                .fetch(pat)
                .map(|p| local.scrubber().scrub_pattern(p));

        assert_eq!(resolution.altered, altered);
        assert_eq!(resolution.introduced, 0);

        resolution.entity
    }
}
