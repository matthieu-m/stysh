//! Statements

use std::convert;

use basic::com;

use model::hir::*;

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Stmt {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A return statement.
    Return(Return),
    /// A variable re-binding.
    Set(ReBinding),
    /// A variable binding.
    Var(Binding),
}

/// A binding.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Binding {
    /// The left-hand side pattern.
    pub left: PatternId,
    /// The right-hand side value.
    pub right: ExpressionId,
    /// The range of the statement.
    pub range: com::Range,
}

/// A re-binding.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ReBinding {
    /// The left-hand side value.
    pub left: ExpressionId,
    /// The right-hand side value.
    pub right: ExpressionId,
    /// The range of the re-binding statement.
    pub range: com::Range,
}

/// A return statement.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Return {
    /// The returned value.
    pub value: ExpressionId,
    /// The range of the return statement.
    pub range: com::Range,
}

//
//  Public interface
//

impl Stmt {
    /// Result type of the statement.
    ///
    /// Not to be mistaken for the type of the value assigned to or returned.
    pub fn result_type(&self) -> Type {
        use self::Stmt::*;

        match *self {
            Return(_) => Type::void(),
            Set(_) | Var(_) => Type::unit(),
        }
    }
}

//
//  Span Implementations
//

impl com::Span for Binding {
    /// Range spanned by the re-binding.
    fn span(&self) -> com::Range { self.range }
}

impl com::Span for ReBinding {
    /// Range spanned by the re-binding.
    fn span(&self) -> com::Range { self.range }
}

impl com::Span for Return {
    /// Range spanned by the return statement.
    fn span(&self) -> com::Range { self.range }
}

impl com::Span for Stmt {
    /// Range spanned by the statement.
    fn span(&self) -> com::Range {
        use self::Stmt::*;

        match self {
            Return(r) => r.span(),
            Set(r) => r.span(),
            Var(b) => b.span(),
        }
    }
}

//
//  Default Implementations
//

impl Default for Stmt {
    fn default() -> Self { Stmt::Return(Default::default()) }
}

//
//  From Implementations
//

impl convert::From<Binding> for Stmt {
    fn from(b: Binding) -> Self { Stmt::Var(b) }
}

impl convert::From<ReBinding> for Stmt {
    fn from(r: ReBinding) -> Self { Stmt::Set(r) }
}
