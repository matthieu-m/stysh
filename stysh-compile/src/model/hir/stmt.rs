//! Statements

use std::convert;

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::hir::*;

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Stmt<'a> {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A return statement.
    Return(Return<'a>),
    /// A variable re-binding.
    Set(ReBinding<'a>),
    /// A variable binding.
    Var(Binding<'a>),
}

/// A binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Binding<'a> {
    /// The left-hand side pattern.
    pub left: Pattern<'a>,
    /// The right-hand side value.
    pub right: Value<'a>,
    /// The range of the statement.
    pub range: com::Range,
}

/// A re-binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ReBinding<'a> {
    /// The left-hand side value.
    pub left: Value<'a>,
    /// The right-hand side value.
    pub right: Value<'a>,
    /// The range of the re-binding statement.
    pub range: com::Range,
}

/// A return statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Return<'a> {
    /// The returned value.
    pub value: Value<'a>,
    /// The range of the return statement.
    pub range: com::Range,
}

//
//  Public interface
//

impl<'a> Stmt<'a> {
    /// Result type of the statement.
    ///
    /// Not to be mistaken for the type of the value assigned to or returned.
    pub fn result_type(&self) -> Type<'static> {
        use self::Stmt::*;

        match *self {
            Return(_) => Type::void(),
            Set(_) | Var(_) => Type::unit(),
        }
    }
}
//
//  CloneInto implementations
//

impl<'a, 'target> CloneInto<'target> for Stmt<'a> {
    type Output = Stmt<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Stmt::*;

        match *self {
            Return(r) => Return(arena.intern(&r)),
            Set(b) => Set(arena.intern(&b)),
            Var(b) => Var(arena.intern(&b)),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Binding<'a> {
    type Output = Binding<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Binding {
            left: arena.intern(&self.left),
            right: arena.intern(&self.right),
            range: self.range,
        }
    }
}

impl<'a, 'target> CloneInto<'target> for ReBinding<'a> {
    type Output = ReBinding<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        ReBinding {
            left: arena.intern(&self.left),
            right: arena.intern(&self.right),
            range: self.range,
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Return<'a> {
    type Output = Return<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Return {
            value: arena.intern(&self.value),
            range: self.range,
        }
    }
}

//
//  Span Implementations
//

impl<'a> Span for Binding<'a> {
    /// Range spanned by the re-binding.
    fn span(&self) -> com::Range { self.range }
}

impl<'a> Span for ReBinding<'a> {
    /// Range spanned by the re-binding.
    fn span(&self) -> com::Range { self.range }
}

impl<'a> Span for Return<'a> {
    /// Range spanned by the return statement.
    fn span(&self) -> com::Range { self.range }
}

impl<'a> Span for Stmt<'a> {
    /// Range spanned by the statement.
    fn span(&self) -> com::Range {
        use self::Stmt::*;

        match *self {
            Return(r) => r.span(),
            Set(r) => r.span(),
            Var(b) => b.span(),
        }
    }
}

//
//  From Implementations
//

impl<'a> convert::From<Binding<'a>> for Stmt<'a> {
    fn from(b: Binding<'a>) -> Self { Stmt::Var(b) }
}

impl<'a> convert::From<ReBinding<'a>> for Stmt<'a> {
    fn from(r: ReBinding<'a>) -> Self { Stmt::Set(r) }
}
