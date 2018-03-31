//! Pattern Unifier & Propagator.

use model::hir::*;
use super::{common, typ, Alteration};

/// Pattern Unifier.
#[derive(Clone, Debug)]
pub struct PatternUnifier<'a, 'g>
    where 'g: 'a
{
    core: common::CoreUnifier<'a, 'g>,
}

//
//  Public interface of PatternUnifier
//

impl<'a, 'g> PatternUnifier<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a, 'g>) -> Self {
        PatternUnifier { core }
    }

    /// Unifies the inner entities (of p), recursively.
    pub fn unify(&self, p: Pattern<'g>, ty: Type<'g>)
        -> Alteration<Pattern<'g>>
    {
        use self::Pattern::*;

        match p {
            Ignored(_) => Alteration::forward(p),
            Constructor(c, g)
                => self.unify_constructor(c, ty).combine(p, |c| Constructor(c, g)),
            Tuple(t, r, g)
                => self.unify_tuple(t, ty).combine(p, |t| Tuple(t, r, g)),
            Var(_, g) => {
                self.unify_variable(g, ty);
                Alteration::forward(p)
            },
        }
    }
}

//
//  Implementation Details
//

impl<'a, 'g> PatternUnifier<'a, 'g>
    where 'g: 'a
{
    fn unify_constructor(&self, c: Constructor<'g, Pattern<'g>>, ty: Type<'g>)
        -> Alteration<Constructor<'g, Pattern<'g>>>
    {
        let type_ = self.select(c.type_, ty);
        let arguments = self.unify_tuple(c.arguments, type_);
        let range = c.range;

        arguments.combine(c, |arguments| {
            Constructor { type_, arguments, range }
        })
    }

    fn unify_tuple(&self, t: Tuple<'g, Pattern<'g>>, ty: Type<'g>)
        -> Alteration<Tuple<'g, Pattern<'g>>>
    {
        self.core.unify_tuple(t, ty, |p, ty| self.unify(p, ty))
    }

    fn unify_variable(&self, gvn: Gvn, ty: Type<'g>) {
        let known = self.core.type_of(gvn);
        let ty = self.select(known, ty);
        self.core.context.set_type_of(gvn, ty);
    }

    fn select(&self, t0: Type<'g>, t1: Type<'g>) -> Type<'g> {
        typ::TypeUnifier::new(self.core).select(t0, t1)
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::PatternUnifier;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn var() {
        let env = Env::default();
        let (_, p, _, _, t, v) = env.factories();

        let local = env.local(b"a");
        let a = v.id(0, 1);
        local.core().context.insert_binding(a, Type::unresolved());

        assert_eq!(
            unify(&local, 0, p.var(a), t.int()),
            p.var(a)
        );
    }

    fn unify<'g>(
        local: &LocalEnv<'g>,
        altered: u32,
        pat: Pattern<'g>,
        ty: Type<'g>,
    )
        -> Pattern<'g>
    {
        let pat = local.resolver().resolve_pattern(pat);
        let ty = local.resolver().resolve_type(ty);

        let resolution =
            PatternUnifier::new(local.core())
                .unify(pat, ty)
                .map(|p| local.scrubber().scrub_pattern(p));

        assert_eq!(resolution.altered, altered);

        resolution.entity
    }
}
