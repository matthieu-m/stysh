//! Pattern Unifier & Propagator.

use crate::model::hir::*;
use super::common::{self, Status};
use super::typ::{self, Action};

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

    /// Unifies the type of the pattern.
    pub fn unify(&self, p: PatternId) -> Status {
        let ty = self.core.tree().get_pattern_type_id(p);

        match self.unify_type(ty) {
            Some(Action::Update(target)) =>
                self.update(p, target),
            Some(Action::Unified) =>
                Status::Unified,
            Some(Action::Cast(_)) | None =>
                Status::Diverging,
        }
    }
}

//
//  Implementation Details
//

impl<'a> PatternUnifier<'a> {
    /// Determine the necessary action to unify the type.
    fn unify_type(&self, ty: TypeId) -> Option<Action> {
        typ::TypeUnifier::new(self.core).unify(ty)
    }

    /// Update the type of the pattern to unify it.
    fn update(&self, p: PatternId, target: Type) -> Status {
        self.core.tree.borrow_mut().set_pattern_type(p, target);
        Status::Unified
    }
}

#[cfg(test)]
mod tests {
    use crate::model::hir::*;

    use super::{common, PatternUnifier, Status};
    use super::super::Relation;
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn name() {
        let env = Env::default();
        let local = env.local(b":var a := 1;");
        let a = local.value_id(5, 1);

        let pat = {
            let (_, p, s, _, _, v) = env.source_factories();
            let pat = p.var(a);
            let val= v.int(1, 10);
            s.var(pat, val);

            local.link_types_of(pat.into(), Relation::Identical(val.into()));

            pat
        };

        {
            let (_, p, s, _, _, v) = env.target_factories();
            let pat = p.var_typed(a, Type::int());
            s.var(pat, v.int(1, 10));
        }

        assert_eq!(unify(&local, pat), Status::Unified);
    }

    fn unify(local: &LocalEnv, p: PatternId) -> Status {
        println!("source before: {:?}", local.source());
        println!();

        let module = local.module().borrow();
        let core = common::CoreUnifier::new(
            local.context(),
            &*module,
            &*local.source()
        );
        let status = PatternUnifier::new(core).unify(p);

        println!("source after: {:?}", local.source());
        println!();
        println!("target: {:?}", local.target());
        println!();

        assert_eq!(local.source(), local.target());

        status
    }
}
