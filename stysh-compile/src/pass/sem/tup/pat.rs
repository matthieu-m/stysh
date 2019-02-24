//! Pattern Unifier & Propagator.

use model::hir::*;
use super::{common, typ, Alteration};

/// Pattern Unifier.
#[derive(Clone, Debug)]
pub struct PatternUnifier<'a> {
    core: common::CoreUnifier<'a>,
}

//
//  Public interface of PatternUnifier
//

impl<'a> PatternUnifier<'a> {
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a>) -> Self {
        PatternUnifier { core }
    }

    /// Unifies the inner entities (of p), recursively.
    pub fn unify(&self, p: Pattern, ty: Type)
        -> Alteration<Pattern>
    {
        use self::Pattern::*;

        match p.clone() {
            Ignored(_) => Alteration::forward(p),
            Constructor(c, g)
                => self.unify_constructor(c, ty).combine(p, |_, c| Constructor(c, g)),
            Tuple(t, r, g)
                => self.unify_tuple(t, ty).combine(p, |_, t| Tuple(t, r, g)),
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

impl<'a> PatternUnifier<'a> {
    fn unify_constructor(&self, c: Constructor<Pattern>, ty: Type)
        -> Alteration<Constructor<Pattern>>
    {
        let type_ = self.select(c.type_, ty);
        let arguments = self.unify_tuple(c.arguments, type_.clone());
        let range = c.range;

        Alteration {
            entity: Constructor { type_, arguments: arguments.entity, range },
            altered: arguments.altered,
        }
    }

    fn unify_tuple(&self, t: Tuple<Pattern>, ty: Type)
        -> Alteration<Tuple<Pattern>>
    {
        self.core.unify_tuple(t, ty, |p, ty| self.unify(p, ty))
    }

    fn unify_variable(&self, gvn: Gvn, ty: Type) {
        let known = self.core.type_of(gvn);
        let ty = self.select(known, ty);
        self.core.context.value(gvn).set_type(ty);
    }

    fn select(&self, t0: Type, t1: Type) -> Type {
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
        local.core().context.insert_value(a, Type::unresolved());

        assert_eq!(
            unify(&local, 0, p.var(a), t.int()),
            p.var(a)
        );
    }

    fn unify<'g>(
        local: &LocalEnv<'g>,
        altered: u32,
        pat: Pattern,
        ty: Type,
    )
        -> Pattern
    {
        let pat = local.resolver().resolve_pattern(pat);
        let pat = local.numberer().number_pattern(pat);
        let ty = local.resolver().resolve_type(ty);

        let resolution =
            PatternUnifier::new(local.core())
                .unify(pat, ty)
                .map(|p| local.scrubber().scrub_pattern(p))
                .map(|p| local.numberer().unnumber_pattern(p));

        assert_eq!(resolution.altered, altered);

        resolution.entity
    }
}
