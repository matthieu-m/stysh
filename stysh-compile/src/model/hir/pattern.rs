//! Patterns

use std::convert;

use basic::com;

use model::hir::*;

/// A Pattern
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern {
    /// A constructor.
    Constructor(Tuple<PatternId>),
    /// An ignored binding, '_'.
    Ignored(com::Range),
    /// A tuple.
    Tuple(Tuple<PatternId>),
    /// A variable.
    Var(ValueIdentifier),
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

impl convert::From<Tuple<PatternId>> for Pattern {
    fn from(t: Tuple<PatternId>) -> Pattern { Pattern::Tuple(t) }
}
