//! Statements

use std::convert;

use basic::com::{self, Span};

use model::ast::*;

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Statement<'a> {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A variable re-binding.
    Set(VariableReBinding<'a>),
    /// A return statement.
    Return(Return<'a>),
    /// A variable definition.
    Var(VariableBinding<'a>),
}

/// A return statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Return<'a> {
    /// Expression being returned.
    pub expr: Option<Expression<'a>>,
    /// Offset of the :return keyword.
    pub ret: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

/// A variable binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableBinding<'a> {
    /// Name of the binding.
    pub pattern: Pattern<'a>,
    /// Type of the binding, if specified.
    pub type_: Option<Type<'a>>,
    /// Expression being bound.
    pub expr: Expression<'a>,
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
pub struct VariableReBinding<'a> {
    /// Left-hand side identifying a binding.
    pub left: Expression<'a>,
    /// Expression being bound.
    pub expr: Expression<'a>,
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
impl<'a> Span for Return<'a> {
    /// Returns the range spanned by the return statement.
    fn span(&self) -> com::Range {
        let len = self.semi + 1 - self.ret;
        com::Range::new(self.ret as usize, len as usize)
    }
}

impl<'a> Span for VariableBinding<'a> {
    /// Returns the range spanned by the binding.
    fn span(&self) -> com::Range {
        debug_assert!(
            self.semi as usize >= self.expr.span().end_offset() - 1,
            "{} should occur after {}", self.semi, self.expr.span()
        );
        com::Range::new(
            self.var as usize,
            (self.semi + 1 - self.var) as usize
        )
    }
}

impl<'a> Span for VariableReBinding<'a> {
    /// Returns the range spanned by the binding.
    fn span(&self) -> com::Range {
        debug_assert!(
            self.semi as usize >= self.expr.span().end_offset() - 1,
            "{} should occur after {}", self.semi, self.expr.span()
        );
        com::Range::new(
            self.set as usize,
            (self.semi + 1 - self.set) as usize
        )
    } 
}

//
//  Implementations of From
//
impl<'a> convert::From<Return<'a>> for Statement<'a> {
    fn from(r: Return<'a>) -> Statement<'a> { Statement::Return(r) }
}

impl<'a> convert::From<VariableBinding<'a>> for Statement<'a> {
    fn from(v: VariableBinding<'a>) -> Statement<'a> { Statement::Var(v) }
}

impl<'a> convert::From<VariableReBinding<'a>> for Statement<'a> {
    fn from(v: VariableReBinding<'a>) -> Statement<'a> { Statement::Set(v) }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use super::*;
    use model::ast::builder::Factory;

    #[test]
    fn range_stmt_variable_binding() {
        let global_arena = mem::Arena::new();
        let f = Factory::new(&global_arena);
        let (e, p, s) = (f.expr(), f.pat(), f.stmt());

        //  "     :var fool := 1234;"
        let mut var = s.var(p.var(10, 4), e.int(18, 4));

        let with_semi: Statement = var.build();
        assert_eq!(with_semi.span(), range(5, 18));

        let without_semi: Statement = var.semi_colon(21).build();
        assert_eq!(without_semi.span(), range(5, 17));
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
