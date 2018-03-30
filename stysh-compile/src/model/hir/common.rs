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

impl<T: 'static> Tuple<'static, T> {
    /// Returns a unit tuple.
    pub fn unit() -> Self { Tuple { fields: &[], names: &[] } }
}

impl<'a, T: 'a> Tuple<'a, T> {
    /// Returns the number of fields.
    pub fn len(&self) -> usize { self.fields.len() }
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

//
//  Implementation Details
//

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
