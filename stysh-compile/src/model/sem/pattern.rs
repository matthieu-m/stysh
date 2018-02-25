//! Patterns

use std::convert;

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::sem::*;

/// A Pattern
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern<'a> {
    /// A constructor.
    Constructor(Constructor<'a, Pattern<'a>>),
    /// An ignored binding, '_'.
    Ignored(com::Range),
    /// A tuple.
    Tuple(Tuple<'a, Pattern<'a>>, com::Range),
    /// A variable.
    Var(ValueIdentifier, Gvn),
}

//
//  Public interface
//

impl<'a> Pattern<'a> {
    /// Sets the gvn of the pattern.
    ///
    /// Panics: if the pattern is not a Var.
    pub fn with_gvn<G: convert::Into<Gvn>>(self, gvn: G) -> Pattern<'a> {
        use self::Pattern::*;

        match self {
            Var(v, _) => Var(v, gvn.into()),
            _ => panic!("Cannot specify GVN in {:?}", self),
        }
    }
}

//
//  CloneInto implementations
//

impl<'a, 'target> CloneInto<'target> for Pattern<'a> {
    type Output = Pattern<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Pattern::*;

        match *self {
            Constructor(c) => Constructor(arena.intern(&c)),
            Ignored(r) => Ignored(r),
            Tuple(t, r) => Tuple(arena.intern(&t), r),
            Var(v, gvn) => Var(v, gvn),
        }
    }
}

//
//  Span Implementations
//

impl<'a> Span for Pattern<'a> {
    /// Returns the range spanned by the pattern.
    fn span(&self) -> com::Range {
        use self::Pattern::*;

        match *self {
            Constructor(c) => c.span(),
            Ignored(r) => r,
            Tuple(_, r) => r,
            Var(v, _) => v.0,
        }
    }
}

//
//  From Implementations
//

impl<'a> convert::From<Constructor<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(c: Constructor<'a, Pattern<'a>>) -> Self {
        Pattern::Constructor(c)
    }
}

impl<'a> convert::From<Tuple<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(t: Tuple<'a, Pattern<'a>>) -> Self {
        let f = &t.fields;
        let off = f.first().map(|p| p.span().offset() - 1).unwrap_or(0);
        let end = f.last().map(|p| p.span().end_offset() + 1).unwrap_or(0);

        Pattern::Tuple(t, com::Range::new(off, end - off))
    }
}
