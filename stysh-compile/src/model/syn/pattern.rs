//! Patterns

use std::convert;

use basic::com::{self, Span};

use model::syn::*;

/// A Pattern.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern<'a> {
    /// A constructor.
    Constructor(Constructor<'a, Pattern<'a>>),
    /// An ignored binding, always '_'.
    Ignored(VariableIdentifier),
    /// A tuple.
    Tuple(Tuple<'a, Pattern<'a>>),
    /// A variable identifier.
    Var(VariableIdentifier),
}

//
//  Implementations of Span
//
impl<'a> Span for Pattern<'a> {
    /// Returns the range spanned by the item.
    fn span(&self) -> com::Range {
        use self::Pattern::*;

        match *self {
            Constructor(c) => c.span(),
            Ignored(i) => i.span(),
            Tuple(t) => t.span(),
            Var(v) => v.span(),
        }
    }
}

//
//  Implementations of From
//
impl<'a> convert::From<Constructor<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(c: Constructor<'a, Pattern<'a>>) -> Pattern<'a> {
        Pattern::Constructor(c)
    }
}

impl<'a> convert::From<Tuple<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(t: Tuple<'a, Pattern<'a>>) -> Pattern<'a> {
        Pattern::Tuple(t)
    }
}
