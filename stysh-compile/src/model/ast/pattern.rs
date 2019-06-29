//! Patterns

use std::convert;

use basic::com;

use model::ast::*;

/// A PatternId.
pub type PatternId = Id<Pattern>;

/// A Pattern.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern {
    /// A constructor.
    Constructor(Constructor<Pattern>),
    /// An ignored binding, always '_'.
    Ignored(com::Range),
    /// A tuple.
    Tuple(Tuple<Pattern>),
    /// A variable identifier.
    Var(VariableIdentifier),
}

//
//  Implementations of From
//
impl convert::From<Constructor<Pattern>> for Pattern {
    fn from(c: Constructor<Pattern>) -> Pattern {
        Pattern::Constructor(c)
    }
}

impl convert::From<Tuple<Pattern>> for Pattern {
    fn from(t: Tuple<Pattern>) -> Pattern {
        Pattern::Tuple(t)
    }
}
