//! Type Unifier & Propagator.

use basic::mem;

use model::hir::*;
use super::common;

/// Action.
///
/// The result of determining the unified type for a number of types is that
/// a transformation may have to be applied to reach the desired type; such as
/// an implicit cast to an enum or a trait.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Action<'a> {
    /// The type needs to be updated, and this will require a cast.
    Cast(Type<'a>),
    /// No action needs to be taken, the value is a copy of the original.
    Keep(Type<'a>),
    /// The type needs to be updated, possibly in place.
    Update(Type<'a>),
}

/// Type Unifier.
#[derive(Clone, Debug)]
pub struct TypeUnifier<'a, 'g>
    where 'g: 'a
{
    core: common::CoreUnifier<'a, 'g>,
}

//
//  Public interface of Action
//

impl<'a> Action<'a> {
    /// Return the type.
    pub fn type_(&self) -> Type<'a> {
        use self::Action::*;

        match *self {
            Cast(t) | Keep(t) | Update(t) => t,
        }
    }
}

//
//  Public interface of TypeUnifier
//

impl<'a, 'g> TypeUnifier<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a, 'g>) -> Self {
        TypeUnifier { core }
    }

    /// Merges 2 types together.
    pub fn merge(&self, t0: Type<'g>, t1: Type<'g>) -> (Action<'g>, Action<'g>)
    {
        use self::Type::*;
        use self::BuiltinType::*;

        let keep = (Action::Keep(t0), Action::Keep(t1));

        if t0 == t1 { return keep; }

        match (t0, t1) {
            //  Special case: Void is the bottom type.
            (Builtin(Void), _) => keep,
            (_, Builtin(Void)) => keep,

            //  If one is unresolved, but not the other, then update!
            (Unresolved(..), Unresolved(..)) => keep,
            (Unresolved(..), _) => (Action::Update(t1), Action::Keep(t1)),
            (_, Unresolved(..)) => (Action::Keep(t0), Action::Update(t0)),

            //  From then on, deal with concrete types.
            //  Update is NOT a valid action with concrete types; Cast is.

            (Enum(e, _), Rec(r, _)) if r.enum_ == e.name
                => (Action::Keep(t0), Action::Cast(t0)),
            (Rec(r, _), Enum(e, _)) if r.enum_ == e.name
                => (Action::Cast(t1), Action::Keep(t1)),

            (Tuple(t0), Tuple(t1)) => self.merge_tuple(t0, t1),

            //  In doubt, keep.
            _ => keep,
        }
    }

    /// Select the best out of 2.
    ///
    /// If there is no clear best, returns Unresolved.
    pub fn select(&self, t0: Type<'g>, t1: Type<'g>) -> Type<'g> {
        use self::Type::*;
        use self::BuiltinType::*;

        if t0 == t1 { return t0; }

        match (t0, t1) {
            (Builtin(Void), _) => t1,
            (_, Builtin(Void)) => t0,
            _ => Type::unresolved(),
        }
    }
}

//
//  Implementation Details
//

struct TupleMerger<'g> {
    original: Tuple<'g, Type<'g>>,
    altered: mem::Array<'g, Type<'g>>,
    length: usize,
    update: bool,
    cast: bool,
}

impl<'g> TupleMerger<'g> {
    fn new(t: Tuple<'g, Type<'g>>, arena: &'g mem::Arena) -> Self {
        TupleMerger {
            original: t,
            altered: mem::Array::new(arena),
            length: 0,
            update: false,
            cast: false,
        }
    }

    fn into_action(self) -> Action<'g> {
        if !self.cast && !self.update {
            debug_assert!(self.altered.is_empty());
            return Action::Keep(Type::Tuple(self.original));
        }

        debug_assert!(self.altered.len() == self.original.len());

        let t = Type::Tuple(Tuple {
            fields: self.altered.into_slice(),
            names: self.original.names,
        });

        if self.cast { Action::Cast(t) } else { Action::Update(t) }
    }

    fn push(&mut self, a: Action<'g>) {
        self.update_or_cast(a);

        if !self.altered.is_empty() {
            self.altered.push(a.type_());
            return;
        }

        match a {
            Action::Cast(t) | Action::Update(t) => {
                self.init_modified();
                self.altered.push(t);
            },
            Action::Keep(_) => self.length += 1,
        }
    }

    fn init_modified(&mut self) {
        debug_assert!(self.altered.is_empty());

        self.altered.reserve(self.original.fields.len());

        for t in &self.original.fields[0..self.length] {
            self.altered.push(*t);
        }
    }

    fn update_or_cast(&mut self, a: Action<'g>) {
        match a {
            Action::Cast(_) => self.cast = true,
            Action::Keep(_) => (),
            Action::Update(_) => self.update = true,
        }
    }
}

impl<'a, 'g> TypeUnifier<'a, 'g>
    where 'g: 'a
{
    fn merge_tuple(&self, t0: Tuple<'g, Type<'g>>, t1: Tuple<'g, Type<'g>>)
        -> (Action<'g>, Action<'g>)
    {
        if t0.fields.len() != t1.fields.len() {
            return (Action::Keep(Type::Tuple(t0)), Action::Keep(Type::Tuple(t1)));
        }

        let mut r0 = TupleMerger::new(t0, self.core.global_arena);
        let mut r1 = TupleMerger::new(t1, self.core.global_arena);

        for (f0, f1) in t0.fields.iter().zip(t1.fields.iter()) {
            let (a0, a1) = self.merge(*f0, *f1);
            r0.push(a0);
            r1.push(a1);
        }

        (r0.into_action(), r1.into_action())
    }
}
