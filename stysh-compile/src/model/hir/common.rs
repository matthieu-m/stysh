//! Common types.

use std::fmt;

use basic::com::{self, Span};
use basic::mem::DynArray;

use model::hir::*;

/// A constructor.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<T> {
    /// The type.
    pub type_: Type,
    /// The arguments.
    pub arguments: Tuple<T>,
    /// The range.
    pub range: com::Range,
}

/// A field.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Field {
    /// Index of the field.
    Index(u16, com::Range),
    /// Unresolved name of the field.
    Unresolved(ValueIdentifier),
}

/// A tuple.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<T> {
    /// The tuple fields.
    pub fields: DynArray<T>,
    /// The name of the fields, empty if unnamed, otherwise of equal length.
    pub names: DynArray<ValueIdentifier>,
}

//
//  Public interface
//

impl Field {
    /// Returns the index.
    ///
    /// Panics: If the field is Unresolved.
    pub fn index(&self) -> u16 {
        use self::Field::*;

        match *self {
            Index(i, ..) => i,
            Unresolved(name) => panic!("Unresolved {:?}", name),
        }
    }
}

impl<T> Tuple<T> {
    /// Returns a unit tuple.
    pub fn unit() -> Self {
        Tuple { fields: Default::default(), names: Default::default() }
    }

    /// Returns the number of fields.
    pub fn len(&self) -> usize { self.fields.len() }
}

impl<T: Clone> Tuple<T> {
    /// Returns the field of the tuple by index.
    ///
    /// Returns None if the field is Unresolved or the index too large.
    pub fn field(&self, field: Field) -> Option<T> {
        if let Field::Index(i, ..) = field {
            self.fields.get(i as usize)
        } else {
            None
        }
    }
}

//
//  Span Implementations
//

impl<T> Span for Constructor<T> {
    /// Returns the range spanned by the constructor.
    fn span(&self) -> com::Range { self.range }
}

impl Span for Field {
    /// Returns the range spanned by the field.
    fn span(&self) -> com::Range {
        use self::Field::*;

        match *self {
            Index(_, r) => r,
            Unresolved(n) => n.span(),
        }
    }
}

//
//  Implementation Details
//

impl Default for Field {
    fn default() -> Self { Field::Index(0, Default::default()) }
}

impl<T> Default for Tuple<T> {
    fn default() -> Self {
        Tuple { fields: Default::default(), names: Default::default() }
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Field::*;

        match *self {
            Index(i, ..) => write!(f, "{}", i),
            Unresolved(name) => write!(f, "{:?}", name),
        }
    }
}

impl<T> fmt::Display for Tuple<T>
    where
        T: Clone + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "(")?;
        for (i, e) in self.fields.iter().enumerate() {
            if i != 0 { write!(f, ", ")? }
            write!(f, "{}", e)?;
        }
        write!(f, ")")
    }
}
