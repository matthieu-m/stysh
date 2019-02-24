//! Patterns

use std::convert;

use basic::com::{self, Span};

use model::hir::*;

/// A Pattern
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern {
    /// A constructor.
    Constructor(Constructor<Pattern>, Gvn),
    /// An ignored binding, '_'.
    Ignored(com::Range),
    /// A tuple.
    Tuple(Tuple<Pattern>, com::Range, Gvn),
    /// A variable.
    Var(ValueIdentifier, Gvn),
}

//
//  Public interface
//

impl Pattern {
    /// Sets the gvn of the pattern.
    ///
    /// Panics: if the pattern is Ignored.
    pub fn with_gvn<G: convert::Into<Gvn>>(self, gvn: G) -> Pattern {
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
//  Span Implementations
//

impl Span for Pattern {
    /// Returns the range spanned by the pattern.
    fn span(&self) -> com::Range {
        use self::Pattern::*;

        match self {
            Constructor(c, _) => c.span(),
            Ignored(r) => *r,
            Tuple(_, r, _) => *r,
            Var(v, _) => v.span(),
        }
    }
}

//
//  Default Implementations
//

impl Default for Pattern {
    fn default() -> Self { Pattern::Ignored(Default::default()) }
}

//
//  From Implementations
//

impl convert::From<Constructor<Pattern>> for Pattern {
    fn from(c: Constructor<Pattern>) -> Self {
        Pattern::Constructor(c, Default::default())
    }
}

impl convert::From<Tuple<Pattern>> for Pattern {
    fn from(t: Tuple<Pattern>) -> Self {
        let off = t.fields.first().map(|p| p.span().offset() - 1).unwrap_or(0);
        let end = t.fields.last().map(|p| p.span().end_offset() + 1).unwrap_or(0);

        Pattern::Tuple(t, com::Range::new(off, end - off), Default::default())
    }
}
