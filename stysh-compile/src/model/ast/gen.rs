//! Generic Packs

use crate::basic::com::{Range, Span, Store, MultiStore};

use crate::model::ast::*;

/// A Pack of Generic Paramters, introducing types and constants.
///
/// Used in declarations of types, extensions, implementations, and functions.
pub type GenericParameterPack = GenericPack<Identifier>;

/// A Pack of Generic Variables.
///
/// Used in instantiations of types, extensions, implementations, and functions
/// as well as within expressions.
pub type GenericVariablePack = GenericPack<GenericVariable>;

/// A Generic Pack.
///
/// Used to represent any appearance of `[...]`, with the appropriate `T`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct GenericPack<T> {
    /// Elements of the pack.
    pub elements: Id<[T]>,
    /// Offsets of the commas separating the elements, an absent comma is placed
    /// at the offset of the last character of the element it would have followed.
    pub commas: Id<[u32]>,
    /// Offset of the opening bracket.
    pub open: u32,
    /// Offset of the closing bracket, an absent bracket is placed at the offset
    /// of the last character of the field it would have followed.
    pub close: u32,
}

impl<T> GenericPack<T> {
    /// Replicates from the Source to the Target store.
    pub fn replicate<Source, Target>(
        &self,
        source: &Source,
        target: &mut Target
    )
        -> Self
        where
            Source: MultiStore<T> + MultiStore<u32>,
            Target: MultiStore<T> + MultiStore<u32>,
    {
        let elements = target.push_slice(source.get_slice(self.elements));
        let commas = target.push_slice(source.get_slice(self.commas));
        GenericPack { elements, commas, open: self.open, close: self.close, }
    }
}

impl<T> Span for GenericPack<T> {
    /// Returns the range spanned by the pack.
    fn span(&self) -> Range {
        Range::half_open(self.open, self.close + 1)
    }
}

/// A Generic Variable
///
/// Used to represent either a type or value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum GenericVariable {
    /// A literal value variable.
    Literal(Literal, Range),
    /// A type variable.
    Type(TypeId),
    /// A value variable.
    Value(VariableIdentifier),
}

impl GenericVariable {
    /// Returns the range of the pattern.
    pub fn range<S>(&self, store: &S) -> Range
        where
            S: Store<GenericVariablePack> + Store<Type>,
    {
        use GenericVariable::*;

        match *self {
            Literal(_, range) => range,
            Type(typ) => store.get_range(typ),
            Value(name) => name.span(),
        }
    }
}
