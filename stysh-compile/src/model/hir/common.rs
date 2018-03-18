//! Common types.

use std::{convert, fmt};

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::hir::*;

/// A binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Binding<'a> {
    /// A function argument.
    Argument(ValueIdentifier, Gvn, Type<'a>, com::Range),
    /// A variable declaration.
    Variable(Pattern<'a>, Value<'a>, com::Range),
}

/// A constructor.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<'a, T: 'a> {
    /// The type.
    pub type_: RecordProto,
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

impl<'a> Binding<'a> {
    /// Sets the gvn of the binding.
    pub fn with_gvn<G: convert::Into<Gvn>>(self, gvn: G) -> Binding<'a> {
        use self::Binding::*;

        match self {
            Argument(id, _, t, r) => Argument(id, gvn.into(), t, r),
            Variable(..) => panic!("Cannot specify GVN in {:?}", self),
        }
    }
}

impl<T: 'static> Tuple<'static, T> {
    /// Returns a unit tuple.
    pub fn unit() -> Self { Tuple { fields: &[], names: &[] } }
}

//
//  CloneInto implementations
//

impl<'a, 'target> CloneInto<'target> for Binding<'a> {
    type Output = Binding<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Binding::*;

        match *self {
            Argument(id, gvn, type_, range)
                => Argument(id, gvn, arena.intern(&type_), range),
            Variable(pat, value, range)
                => Variable(arena.intern(&pat), arena.intern(&value), range),
        }
    }
}

impl<'a, 'target, T> CloneInto<'target> for Constructor<'a, T>
    where T: CloneInto<'target> + Copy + 'a
{
    type Output = Constructor<'target, <T as CloneInto<'target>>::Output>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Constructor {
            type_: self.type_,
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

impl<'a> Span for Binding<'a> {
    /// Range spanned by the binding.
    fn span(&self) -> com::Range {
        use self::Binding::*;

        match *self {
            Argument(_, _, _, r) | Variable(_, _, r) => r,
        }
    }
}

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
