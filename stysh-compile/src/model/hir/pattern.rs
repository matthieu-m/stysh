//! Patterns

use std::convert;

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::hir::*;

/// A Pattern
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern<'a> {
    /// A constructor.
    Constructor(Constructor<'a, Pattern<'a>>, Gvn),
    /// An ignored binding, '_'.
    Ignored(com::Range),
    /// A tuple.
    Tuple(Tuple<'a, Pattern<'a>>, com::Range, Gvn),
    /// A variable.
    Var(ValueIdentifier, Gvn),
}

//
//  Public interface
//

impl<'a> Pattern<'a> {
    /// Sets the gvn of the pattern.
    ///
    /// Panics: if the pattern is Ignored.
    pub fn with_gvn<G: convert::Into<Gvn>>(self, gvn: G) -> Pattern<'a> {
        use self::Pattern::*;

        match self {
            Constructor(c, _) => Constructor(c, gvn.into()),
            Ignored(_) => panic!("Cannot specify GVN in {:?}", self),
            Tuple(t, r, _) => Tuple(t, r, gvn.into()),
            Var(v, _) => Var(v, gvn.into()),
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
            Constructor(c, g) => Constructor(arena.intern(&c), g),
            Ignored(r) => Ignored(r),
            Tuple(t, r, g) => Tuple(arena.intern(&t), r, g),
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
            Constructor(c, _) => c.span(),
            Ignored(r) => r,
            Tuple(_, r, _) => r,
            Var(v, _) => v.span(),
        }
    }
}

//
//  From Implementations
//

impl<'a> convert::From<Constructor<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(c: Constructor<'a, Pattern<'a>>) -> Self {
        Pattern::Constructor(c, Default::default())
    }
}

impl<'a> convert::From<Tuple<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(t: Tuple<'a, Pattern<'a>>) -> Self {
        let f = &t.fields;
        let off = f.first().map(|p| p.span().offset() - 1).unwrap_or(0);
        let end = f.last().map(|p| p.span().end_offset() + 1).unwrap_or(0);

        Pattern::Tuple(t, com::Range::new(off, end - off), Default::default())
    }
}
