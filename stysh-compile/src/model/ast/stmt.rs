//! Statements

use std::convert;

use basic::com::{self, Span};

use model::ast::*;

/// A Statement ID.
pub type StatementId = Id<Statement>;

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Statement {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A variable re-binding.
    Set(VariableReBinding),
    /// A return statement.
    Return(Return),
    /// A variable definition.
    Var(VariableBinding),
}

/// A return statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Return {
    /// Expression being returned.
    pub expr: Option<ExpressionId>,
    /// Offset of the :return keyword.
    pub ret: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

/// A variable binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableBinding {
    /// Name of the binding.
    pub pattern: PatternId,
    /// Type of the binding, if specified.
    pub type_: Option<TypeId>,
    /// Expression being bound.
    pub expr: ExpressionId,
    /// Offset of the :var keyword.
    pub var: u32,
    /// Offset of the : sign, or 0 if none.
    pub colon: u32,
    /// Offset of the := sign.
    pub bind: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

/// A variable re-binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableReBinding {
    /// Left-hand side identifying a binding.
    pub left: ExpressionId,
    /// Expression being bound.
    pub expr: ExpressionId,
    /// Offset of the :set keyword.
    pub set: u32,
    /// Offset of the := sign.
    pub bind: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

//
//  Implementations of Span
//

impl Span for Statement {
    /// Returns the range spanned by the statement.
    fn span(&self) -> com::Range {
        use self::Statement::*;

        match *self {
            Set(s) => s.span(),
            Return(r) => r.span(),
            Var(v) => v.span(),
        }
    }
}

impl Span for Return {
    /// Returns the range spanned by the return statement.
    fn span(&self) -> com::Range {
        let len = self.semi + 1 - self.ret;
        com::Range::new(self.ret as usize, len as usize)
    }
}

impl Span for VariableBinding {
    /// Returns the range spanned by the binding.
    fn span(&self) -> com::Range {
        com::Range::new(
            self.var as usize,
            (self.semi + 1 - self.var) as usize
        )
    }
}

impl Span for VariableReBinding {
    /// Returns the range spanned by the binding.
    fn span(&self) -> com::Range {
        com::Range::new(
            self.set as usize,
            (self.semi + 1 - self.set) as usize
        )
    }
}


//
//  Implementations of From
//

impl convert::From<Return> for Statement {
    fn from(r: Return) -> Statement { Statement::Return(r) }
}

impl convert::From<VariableBinding> for Statement {
    fn from(v: VariableBinding) -> Statement { Statement::Var(v) }
}

impl convert::From<VariableReBinding> for Statement {
    fn from(v: VariableReBinding) -> Statement { Statement::Set(v) }
}
