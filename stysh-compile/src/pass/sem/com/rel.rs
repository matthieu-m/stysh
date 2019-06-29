//! Relation of two types.

//
//  Public Types
//

/// Relation between the types of two entities.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Relation<T> {
    /// The type of A must be identical to the type of T.
    Identical(T),
    /// The type of A must be a sub type of the type of T.
    SubTypeOf(T),
    /// The type of A must be a super type of the type of T.
    SuperTypeOf(T),
}

//
//  Public Implementation of Relation
//

impl<T> Relation<T> {
    /// Extracts the inner element.
    pub fn get(&self) -> &T {
        use self::Relation::*;

        match self {
            Identical(t) | SubTypeOf(t) | SuperTypeOf(t) => t,
        }
    }

    /// Creates the reciprocal Relation.
    pub fn reciprocate<U>(&self, other: U) -> Relation<U> {
        use self::Relation::*;

        match self {
            Identical(_) => Identical(other),
            SubTypeOf(_) => SuperTypeOf(other),
            SuperTypeOf(_) => SubTypeOf(other),
        }
    }

    /// Maps a relation to another.
    pub fn map<U, F: FnOnce(T) -> U>(self, mapper: F) -> Relation<U> {
        use self::Relation::*;

        match self {
            Identical(t) => Identical(mapper(t)),
            SubTypeOf(t) => SubTypeOf(mapper(t)),
            SuperTypeOf(t) => SuperTypeOf(mapper(t)),
        }
    }
}

impl<T: Clone + Eq> Relation<T> {
    /// Merges the two relationships in one.
    ///
    /// In case of opposite relations (Sub vs Super), yields Identical.
    ///
    /// #   Panics
    ///
    /// Panics if the two inner elements do not compare equal.
    pub fn merge(&mut self, other: &Relation<T>) {
        assert!(self.get() == other.get());

        if self == other { return; }

        *self = Relation::Identical(other.get().clone());
    }
}
