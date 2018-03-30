//! Statement Fetcher.

use model::hir::*;
use super::{common, pat, val, Resolution};

/// Statement Fetcher.
#[derive(Clone, Debug)]
pub struct StatementFetcher<'a, 'g>
    where 'g: 'a
{
    core: common::CoreFetcher<'a, 'g>,
}

//
//  Public interface of StatementFetcher
//

impl<'a, 'g> StatementFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreFetcher<'a, 'g>) -> Self {
        StatementFetcher { core }
    }

    /// Fetches the inner entities, recursively.
    pub fn fetch(&self, s: Stmt<'g>) -> Resolution<Stmt<'g>> {
        use self::Stmt::*;

        match s {
            Return(r) => self.fetch_return(r).combine(s, |r| Return(r)),
            Set(r) => self.fetch_rebinding(r).combine(s, |r| Set(r)),
            Var(b) => self.fetch_binding(b).combine(s, |b| Var(b)),
        }
    }
}

//
//  Implementation Details
//

impl<'a, 'g> StatementFetcher<'a, 'g>
    where 'g: 'a
{
    fn fetch_binding(&self, b: Binding<'g>) -> Resolution<Binding<'g>> {
        let left = self.fetch_pattern(b.left);
        let right = self.fetch_value(b.right);
        let range = b.range;

        left.combine2(b, right, |left, right| Binding { left, right, range })
    }

    fn fetch_rebinding(&self, r: ReBinding<'g>) -> Resolution<ReBinding<'g>> {
        let left = self.fetch_value(r.left);
        let right = self.fetch_value(r.right);
        let range = r.range;

        left.combine2(r, right, |left, right| ReBinding { left, right, range })
    }

    fn fetch_return(&self, r: Return<'g>) -> Resolution<Return<'g>> {
        let value = self.fetch_value(r.value);
        let range = r.range;

        value.combine(r, |value| Return { value, range })
    }

    fn fetch_pattern(&self, p: Pattern<'g>) -> Resolution<Pattern<'g>> {
        pat::PatternFetcher::new(self.core).fetch(p)
    }

    fn fetch_value(&self, v: Value<'g>) -> Resolution<Value<'g>> {
        val::ValueFetcher::new(self.core).fetch(v)
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::StatementFetcher;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn var_constructed_nested() {
        let env = Env::default();
        let (i, _, p, s, t, v) = env.factories();

        let mut local = env.local(b":enum E { A, B };    :var b := E::B;");
        local.mark_unfetched_items(&[i.id(34, 1)]);

        let e =
            i.enum_(p.enum_(i.id(6, 1)).build())
                .push(i.unit(10, 1))
                .push(i.unit(13, 1))
                .build();
        local.insert_enum(e);

        let parent = Type::Enum(*e.prototype, Default::default());

        let unresolved = t.unresolved(i.id(34, 1)).push(parent).build();
        let fetched = t.record(i.id(13, 1), 13).push(parent).build();

        assert_eq!(
            fetch(
                &local,
                1,
                s.var_id(
                    v.id(26, 1),
                    v.constructor(unresolved, 31, 4).build_value().without_type(),
                )
            ),
            s.var_id(
                v.id(26, 1),
                v.constructor(fetched, 31, 4).build_value().without_type(),
            )
        );
    }

    fn fetch<'g>(local: &LocalEnv<'g>, altered: u32, s: Stmt<'g>) -> Stmt<'g> {
        let s = local.resolver().resolve_statement(s);

        let resolution =
            StatementFetcher::new(local.core())
                .fetch(s)
                .map(|s| local.scrubber().scrub_statement(s));

        assert_eq!(resolution.altered, altered);
        assert_eq!(resolution.introduced, 0);

        resolution.entity
    }
}
