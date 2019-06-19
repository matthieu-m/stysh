//! Type Unifier & Propagator.

use std::cell;

use model::hir::*;
use super::{common, Relation};

/// Action.
///
/// The result of determining the unified type for a number of types is that
/// a transformation may have to be applied to reach the desired type; such as
/// an implicit cast to an enum or a trait.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Action {
    /// The type needs to be updated, and this will require a cast.
    Cast(Type),
    /// The type needs to be updated, possibly in place.
    Update(Type),
    /// The type relationship needs to be evaluated.
    Unified,
}

/// Type Unifier.
#[derive(Clone, Debug)]
pub struct TypeUnifier<'a> {
    core: common::CoreUnifier<'a>,
}

//
//  Public interface of TypeUnifier
//

impl<'a> TypeUnifier<'a> {
    /// Creates a new instance.
    pub fn new(core: common::CoreUnifier<'a>) -> Self {
        TypeUnifier { core }
    }

    /// Attempts to unify ty with related types.
    pub fn unify(&self, ty: TypeId) -> Option<Action> {
        if self.is_determined(ty) {
            return Some(Action::Unified);
        }

        let relations = self.core.context.get_type_links(ty);

        //  Attempts one-to-one unification.
        for &rel in &relations {
            if let Some(action) = self.unify_with(ty, rel) {
                return Some(action);
            }
        }

        //  FIXME: Attempts to find SubTypeOf(T) + SuperTypeOf(T)

        None
    }
}

//
//  Implementation Details
//

impl<'a> TypeUnifier<'a> {
    /// Checks whether the type is already determined.
    fn is_determined(&self, ty: TypeId) -> bool {
        use self::Type::*;

        match self.tree().get_type(ty) {
            Builtin(_) | Enum(..) | Rec(..) => true,
            Tuple(tup) =>
                self.tree()
                    .get_type_ids(tup.fields)
                    .iter()
                    .all(|id| self.is_determined(*id)),
            Unresolved(..) => false,
        }
    }

    /// Attempts to unify a type based on its relation.
    fn unify_with(&self, ty: TypeId, rel: Relation<TypeId>) -> Option<Action> {
        use self::Relation::*;

        match rel {
            Identical(other) => self.unify_with_identical(ty, other),
            SubTypeOf(other) => self.unify_as_sub_type_of(ty, other),
            SuperTypeOf(other) => self.unify_as_super_type_of(ty, other),
        }
    }

    /// Attempts to unify a type with an identical type.
    fn unify_with_identical(&self, ty: TypeId, other: TypeId) -> Option<Action> {
        use self::Type::*;

        match self.tree().get_type(other) {
            Tuple(tup) =>
                self.unify_with_tuple(ty, tup, Relation::Identical),
            Builtin(_) | Enum(..) | Rec(..) | Unresolved(..) =>
                self.resolve(ty, other),
        }
    }

    /// Attempts to unify a type with a super type.
    fn unify_as_sub_type_of(&self, ty: TypeId, other: TypeId) -> Option<Action> {
        use self::Type::*;

        match self.tree().get_type(other) {
            Builtin(_) =>
                //  FIXME(matthieum): need to cater for Bool::True and Bool::False.
                self.resolve(ty, other),
            Enum(name, path, id) =>
                self.unify_as_sub_type_of_enum(ty, name, path, id),
            Tuple(tup) =>
                self.unify_with_tuple(ty, tup, Relation::SubTypeOf),
            Rec(..) | Unresolved(..) =>
                self.resolve(ty, other),
        }
    }

    /// Attempts to unify a type with a sub type.
    fn unify_as_super_type_of(&self, ty: TypeId, other: TypeId) -> Option<Action> {
        use self::Type::*;

        match self.tree().get_type(other) {
            Builtin(_) =>
                //  FIXME(matthieum): need to cater for Bool::True and Bool::False.
                self.resolve(ty, other),
            Tuple(tup) =>
                self.unify_with_tuple(ty, tup, Relation::SuperTypeOf),
            Enum(..) | Rec(..) | Unresolved(..) =>
                self.resolve(ty, other),
        }
    }

    /// Attempts to unify a type with a super enum.
    fn unify_as_sub_type_of_enum(
        &self,
        ty: TypeId,
        name: ItemIdentifier,
        path: PathId,
        variants: Id<[TypeId]>
    )
        -> Option<Action>
    {
        if let Type::Rec(rec, ..) = self.tree().get_type(ty) {
            let matches = self.tree()
                .get_type_ids(variants)
                .iter()
                .map(|&id| self.tree().get_type(id))
                .any(|variant| variant.name() == rec);

            if matches {
                return Some(Action::Cast(Type::Enum(name, path, variants)));
            }
        }

        None
    }

    /// Attempts to unify a type with a tuple.
    fn unify_with_tuple<F>(&self, ty: TypeId, other: Tuple<TypeId>, relate: F)
        -> Option<Action>
        where
            F: Fn(TypeId) -> Relation<TypeId>,
    {
        use self::Type::*;

        match self.tree().get_type(ty) {
            Tuple(current) =>
                self.unify_tuples(current, other, relate),
            Unresolved(name, ..) =>
                self.resolve_unresolved(name, Type::Tuple(other)),
            Builtin(..) | Enum(..) | Rec(..) =>
                None,
        }
    }

    /// Attempts to unify a tuple with another tuple.
    fn unify_tuples<F>(
        &self,
        current: Tuple<TypeId>,
        other: Tuple<TypeId>,
        relate: F
    )
        -> Option<Action>
        where
            F: Fn(TypeId) -> Relation<TypeId>,
    {
        let tree = self.tree();

        let current_fields = tree.get_type_ids(current.fields);
        let other_fields = tree.get_type_ids(other.fields);

        //  Cannot unify tuples of different lengths.
        if current_fields.len() != other_fields.len() {
            return None;
        }

        //  Cannot unify named tuple with anonymous tuple.
        if current.names.is_empty() != other.names.is_empty() {
            return None;
        }

        //  In case of named tuples, reorder fields to match by name.
        let mut current_reordered = vec!();

        let current_fields = if !current.names.is_empty() {
            let current_names = tree.get_names(current.names);
            let other_names = tree.get_names(other.names);

            for other_name in other_names {
                for (position, current_name) in current_names.iter().enumerate() {
                    if other_name.0 == current_name.0 {
                        current_reordered.push(current_fields[position]);
                        break;
                    }
                }
            }

            //  Cannot unify named tuples with different names.
            if current_reordered.len() != other_names.len() {
                return None;
            }

            &current_reordered
        } else {
            current_fields
        };

        //  Link fields.
        for (&current, &other) in current_fields.iter().zip(other_fields.iter()) {
            self.core.context.link_types(current, relate(other));
            self.core.context.link_types(other, relate(other).reciprocate(current));
        }

        Some(Action::Unified)
    }

    /// Resolves a type.
    fn resolve(&self, ty: TypeId, other: TypeId) -> Option<Action> {
        let other_type = self.tree().get_type(other);

        if let Type::Unresolved(..) = other_type {
            return None;
        }

        //  Always returns Unified, even if the unification does not happen, as
        //  the only way for unification to fail is for a conflict to occur,
        //  and such conflicts indicate a source code error.
        if let Type::Unresolved(name, ..) = self.tree().get_type(ty) {
            self.resolve_unresolved(name, other_type)
        } else {
            None
        }
    }

    /// Resolves an unresolved type.
    fn resolve_unresolved(&self, name: ItemIdentifier, other: Type)
        -> Option<Action>
    {
        if name == ItemIdentifier::unresolved() {
            Some(Action::Update(other))
        } else {
            None
        }
    }

    /// Returns a reference to the Tree.
    fn tree<'b>(&'b self) -> cell::Ref<'b, Tree> {
        self.core.tree()
    }
}

#[cfg(test)]
mod tests {
    use model::hir::*;

    use super::{Action, Relation, TypeUnifier};
    use super::super::tests::{Env, LocalEnv};

    #[test]
    fn unresolved_identical_to_unresolved() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let ty = t.unresolved();
        let linked = t.unresolved();

        assert_eq!(unify(&local, ty, Relation::Identical(linked)), None);
    }

    #[test]
    fn unresolved_identical_to_bool() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let ty = t.unresolved();

        assert_eq!(
            unify(&local, ty, Relation::Identical(t.bool_())),
            Some(Action::Update(Type::bool_()))
        );
    }

    #[test]
    fn unresolved_sub_type_of_int() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let ty = t.unresolved();

        assert_eq!(
            unify(&local, ty, Relation::SubTypeOf(t.int())),
            Some(Action::Update(Type::int()))
        );
    }

    #[test]
    fn unresolved_super_type_of_string() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let ty = t.unresolved();

        assert_eq!(
            unify(&local, ty, Relation::SuperTypeOf(t.string())),
            Some(Action::Update(Type::string()))
        );
    }

    #[test]
    fn unresolved_identical_to_record() {
        let env = Env::default();
        let local = env.local(b":rec Hello;");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let name = local.item_id(5, 5);

        let ty = t.unresolved();
        let rel = Relation::Identical(t.record(name).build());
        let rec = Type::Rec(name, Id::empty(), Tuple::unit());

        assert_eq!(
            unify(&local, ty, rel),
            Some(Action::Update(rec))
        );
    }

    #[test]
    fn unresolved_identical_to_tuple() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let i = Type::int();

        let ty = t.unresolved();
        let rel = Relation::Identical(t.tuple().push(i).push(i).build());
        let tup = local.source().borrow().get_type(*rel.get());

        assert_eq!(
            unify(&local, ty, rel),
            Some(Action::Update(tup))
        );
    }

    #[test]
    fn tuple_of_unresolved_identical_to_tuple_of_int() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let (i, u) = (Type::int(), Type::unresolved());

        let ty = t.tuple().push(u).push(u).build();
        let rel = Relation::Identical(t.tuple().push(i).push(i).build());

        assert_eq!(unify(&local, ty, rel), Some(Action::Unified));

        let fields = fields(&local, ty);
        assert_eq!(fields.len(), 2);

        for ty in fields {
            assert_eq!(
                relations(&local, ty),
                &[Relation::Identical(i)],
                "Relations of {:?}", ty
            );
        }
    }

    #[test]
    fn tuple_of_unresolved_identical_to_tuple_of_named() {
        let env = Env::default();
        let local = env.local(b"(.first: Int, .second: Int)");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let (b, i, u) = (Type::bool_(), Type::int(), Type::unresolved());
        let (first, second) = (local.value_id(1, 6), local.value_id(14, 7));

        let ty = t.tuple().push(u).name(first).push(u).name(second).build();
        let rel = Relation::Identical(
            t.tuple().push(i).name(second).push(b).name(first).build()
        );

        assert_eq!(unify(&local, ty, rel), Some(Action::Unified));

        let fields = fields(&local, ty);
        assert_eq!(fields.len(), 2);

        assert_eq!(relations(&local, fields[0]), &[Relation::Identical(b)]);
        assert_eq!(relations(&local, fields[1]), &[Relation::Identical(i)]);
    }

    #[test]
    fn tuple_of_unresolved_super_type_of_tuple_of_int() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let (i, u) = (Type::int(), Type::unresolved());

        let ty = t.tuple().push(u).push(u).build();
        let rel = Relation::SuperTypeOf(t.tuple().push(i).push(i).build());

        assert_eq!(unify(&local, ty, rel), Some(Action::Unified));

        let fields = fields(&local, ty);
        assert_eq!(fields.len(), 2);

        for ty in fields {
            assert_eq!(
                relations(&local, ty),
                &[Relation::SuperTypeOf(i)],
                "Relations of {:?}", ty
            );
        }
    }

    #[test]
    fn tuple_of_different_cardinality() {
        let env = Env::default();
        let local = env.local(b"");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let (i, u) = (Type::int(), Type::unresolved());

        let ty = t.tuple().push(u).push(u).build();
        let rel = Relation::Identical(t.tuple().push(i).push(i).push(i).build());

        assert_eq!(unify(&local, ty, rel), None);
    }

    #[test]
    fn tuple_of_different_anonymity() {
        let env = Env::default();
        let local = env.local(b"(.first: Int, .second: Int)");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let (i, u) = (Type::int(), Type::unresolved());
        let (first, second) = (local.value_id(1, 6), local.value_id(14, 7));

        let ty = t.tuple().push(u).push(u).build();
        let rel = Relation::Identical(
            t.tuple().push(i).name(first).push(i).name(second).build()
        );

        assert_eq!(unify(&local, ty, rel), None);
    }

    #[test]
    fn tuple_of_different_names() {
        let env = Env::default();
        let local = env.local(b"(.first: Int, .second: Int, .third: Int)");

        let (_, _, _, _, _, t, _) = env.source_factories();
        let (i, u) = (Type::int(), Type::unresolved());
        let (first, second, third) =
            (local.value_id(1, 6), local.value_id(14, 7), local.value_id(28, 6));

        let ty = t.tuple().push(u).name(first).push(u).name(third).build();
        let rel = Relation::Identical(
            t.tuple().push(i).name(first).push(i).name(second).build()
        );

        assert_eq!(unify(&local, ty, rel), None);
    }

    fn fields<'g>(local: &LocalEnv<'g>, ty: TypeId) -> Vec<TypeId> {
        let typ = local.source().borrow().get_type(ty);

        let tup = match typ {
            Type::Rec(_, _, tup) | Type::Tuple(tup) => tup,
            _ => panic!("No fields in {:?}", typ),
        };

        local.source().borrow().get_type_ids(tup.fields).iter().cloned().collect()
    }

    fn relations<'g>(local: &LocalEnv<'g>, ty: TypeId) -> Vec<Relation<Type>> {
        local.core().context
            .get_type_links(ty)
            .iter()
            .map(|rel| rel.map(|ty| local.source().borrow().get_type(ty)))
            .collect()
    }

    fn unify<'g>(local: &LocalEnv<'g>, ty: TypeId, rel: Relation<TypeId>)
        -> Option<Action>
    {
        local.link_types(ty, rel);

        println!("source before: {:?}", local.source());
        println!();

        let action = TypeUnifier::new(local.core()).unify(ty);

        println!("source after: {:?}", local.source());
        println!();

        action
    }
}
