//! Common types.

use std::fmt;

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::hir::*;

/// A constructor.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<'a, T: 'a> {
    /// The type.
    pub type_: Type<'a>,
    /// The arguments.
    pub arguments: Tuple<'a, T>,
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
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<'a, T: 'a> {
    /// The tuple fields.
    pub fields: &'a [T],
    /// The name of the fields, empty if unnamed, otherwise of equal length.
    pub names: &'a [ValueIdentifier],
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

impl<T: 'static> Tuple<'static, T> {
    /// Returns a unit tuple.
    pub fn unit() -> Self { Tuple { fields: &[], names: &[] } }
}

impl<'a, T: 'a> Tuple<'a, T> {
    /// Returns the number of fields.
    pub fn len(&self) -> usize { self.fields.len() }
}

impl<'a, T: Clone + 'a> Tuple<'a, T> {
    /// Returns the field of the tuple by index.
    ///
    /// Returns None if the field is Unresolved or the index too large.
    pub fn field(&self, field: Field) -> Option<T> {
        if let Field::Index(i, ..) = field {
            self.fields.get(i as usize).cloned()
        } else {
            None
        }
    }
}

//
//  CloneInto implementations
//

impl<'a, 'target, T> CloneInto<'target> for Constructor<'a, T>
    where T: CloneInto<'target> + Copy + 'a
{
    type Output = Constructor<'target, <T as CloneInto<'target>>::Output>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Constructor {
            type_: arena.intern(&self.type_),
            arguments: CloneInto::clone_into(&self.arguments, arena),
            range: self.range,
        }
    }
}

impl<'a, 'target, T> CloneInto<'target> for Tuple<'a, T>
    where T: CloneInto<'target> + Copy + 'a
{
    type Output = Tuple<'target, <T as CloneInto<'target>>::Output>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Tuple {
            fields: CloneInto::clone_into(self.fields, arena),
            names: CloneInto::clone_into(self.names, arena),
        }
    }
}

//
//  Span Implementations
//

impl<'a, T: 'a> Span for Constructor<'a, T> {
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

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Field::*;

        match *self {
            Index(i, ..) => write!(f, "{}", i),
            Unresolved(name) => write!(f, "{:?}", name),
        }
    }
}

impl<'a, T> fmt::Display for Tuple<'a, T>
    where
        T: fmt::Display
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
