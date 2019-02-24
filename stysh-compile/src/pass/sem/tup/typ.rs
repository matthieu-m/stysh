//! Type Unifier & Propagator.

use basic::mem::DynArray;

use model::hir::*;
use super::common;

/// Action.
///
/// The result of determining the unified type for a number of types is that
/// a transformation may have to be applied to reach the desired type; such as
/// an implicit cast to an enum or a trait.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Action {
    /// The type needs to be updated, and this will require a cast.
    Cast(Type),
    /// No action needs to be taken, the value is a copy of the original.
    Keep(Type),
    /// The type needs to be updated, possibly in place.
    Update(Type),
}

/// Type Unifier.
#[derive(Clone, Debug)]
pub struct TypeUnifier<'a> {
    core: common::CoreUnifier<'a>,
}

//
//  Public interface of Action
//

impl Action {
    /// Return the type.
    pub fn type_(&self) -> Type {
        use self::Action::*;

        match self {
            Cast(t) | Keep(t) | Update(t) => t.clone(),
        }
    }

    /// Return the type.
    pub fn into_type(self) -> Type {
        use self::Action::*;

        match self {
            Cast(t) | Keep(t) | Update(t) => t,
        }
    }
}

//
//  Public interface of TypeUnifier
//

impl<'a> TypeUnifier<'a> {
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a>) -> Self {
        TypeUnifier { core }
    }

    /// Merges 2 types together.
    pub fn merge(&self, t0: Type, t1: Type) -> (Action, Action)
    {
        use self::Type::*;
        use self::BuiltinType::*;

        let keep = (Action::Keep(t0.clone()), Action::Keep(t1.clone()));

        if t0 == t1 { return keep; }

        match (t0.clone(), t1.clone()) {
            //  Special case: Void is the bottom type.
            (Builtin(Void), _) => keep,
            (_, Builtin(Void)) => keep,

            //  If one is unresolved, but not the other, then update!
            (Unresolved(..), Unresolved(..)) => keep,
            (Unresolved(..), _) => (Action::Update(t1), keep.1),
            (_, Unresolved(..)) => (keep.0, Action::Update(t0)),

            //  From then on, deal with concrete types.
            //  Update is NOT a valid action with concrete types; Cast is.

            (Enum(e, ..), Rec(r, ..)) => if r.prototype.enum_ == e.prototype.name {
                (keep.0, Action::Cast(t0))
            } else {
                keep
            },
            (Rec(r, ..), Enum(e, ..)) => if r.prototype.enum_ == e.prototype.name {
                (Action::Cast(t1), keep.1)
            } else {
                keep
            },

            (Tuple(t0, g0), Tuple(t1, g1)) => self.merge_tuple(t0, g0, t1, g1),

            //  In doubt, keep.
            _ => keep,
        }
    }

    /// Select the best out of 2.
    ///
    /// If there is no clear best, returns Unresolved.
    pub fn select(&self, t0: Type, t1: Type) -> Type {
        use self::Type::*;
        use self::BuiltinType::*;

        if t0 == t1 { return t0; }

        match (t0, t1) {
            (Builtin(Void), t1) => t1,
            (t0, Builtin(Void)) => t0,
            _ => Type::unresolved(),
        }
    }
}

//
//  Implementation Details
//

struct TupleMerger {
    tuple: Tuple<Type>,
    gin: Gin,
    update: bool,
    cast: bool,
}

impl TupleMerger {
    fn new(t: Tuple<Type>, gin: Gin) -> Self {
        //  Defensive copy.
        let fields = DynArray::new(t.fields.get_array());

        TupleMerger {
            tuple: Tuple { fields, names: t.names },
            gin: gin,
            update: false,
            cast: false,
        }
    }

    fn into_action(self) -> Action {
        let result = Type::Tuple(self.tuple, self.gin);

        match (self.cast, self.update) {
            (false, false) => Action::Keep(result),
            (true, _) => Action::Cast(result),
            _ => Action::Update(result)
        }
    }

    fn set(&mut self, index: usize, a: Action) {
        self.update_or_cast(&a);

        self.tuple.fields.replace(index, a.into_type());
    }

    fn update_or_cast(&mut self, a: &Action) {
        match a {
            Action::Cast(_) => self.cast = true,
            Action::Keep(_) => (),
            Action::Update(_) => self.update = true,
        }
    }
}

impl<'a> TypeUnifier<'a> {
    fn merge_tuple(
        &self,
        t0: Tuple<Type>,
        g0: Gin,
        t1: Tuple<Type>,
        g1: Gin,
    )
        -> (Action, Action)
    {
        let len = t0.fields.len();

        if len != t1.fields.len() {
            return (Action::Keep(Type::Tuple(t0, g0)), Action::Keep(Type::Tuple(t1, g1)));
        }

        let mut r0 = TupleMerger::new(t0, g0);
        let mut r1 = TupleMerger::new(t1, g1);

        for i in 0..len {
            let (f0, f1) = (r0.tuple.fields.at(i), r1.tuple.fields.at(i));

            let (a0, a1) = self.merge(f0, f1);

            r0.set(i, a0);
            r1.set(i, a1);
        }

        (r0.into_action(), r1.into_action())
    }
}
