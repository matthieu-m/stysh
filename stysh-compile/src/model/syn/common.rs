//! Common types.

use basic::com::{self, Span};

use model::syn::*;
use model::tt;

/// A Constructor.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<'a, T: 'a> {
    /// Type of the constructor.
    pub type_: Type<'a>,
    /// Arguments of the constructor.
    pub arguments: Tuple<'a, T>,
}

/// A Tuple, either type or value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<'a, T: 'a> {
    /// Fields of the tuple.
    pub fields: &'a [T],
    /// Offsets of the commas separating the fields, an absent comma is placed
    /// at the offset of the last character of the field it would have followed.
    pub commas: &'a [u32],
    /// Offset of the opening parenthesis.
    pub open: u32,
    /// Offset of the closing parenthesis, an absent parenthesis is placed at
    /// at the offset of the last character of the field it would have followed.
    pub close: u32,
}

//
//  Implementations
//
impl<'a, T: 'a> Tuple<'a, T> {
    /// Returns whether the tuple is empty.
    pub fn is_empty(&self) -> bool { self.fields.is_empty() }

    /// Returns the number of fields of the tuple.
    pub fn len(&self) -> usize { self.fields.len() }

    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        self.commas
            .get(i)
            .map(|&o| tt::Token::new(tt::Kind::SignComma, o as usize, 1))
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisOpen, self.open as usize, 1)
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisClose, self.close as usize, 1)
    }
}

impl<'a, T: 'a + Clone> Tuple<'a, T> {
    /// Returns the field at index i.
    pub fn field(&self, i: usize) -> Option<T> {
        self.fields.get(i).cloned()
    }
}

//
//  Implementations of Span
//
impl<'a, T: 'a> Span for Constructor<'a, T> {
    /// Returns the range spanned by the constructor.
    fn span(&self) -> com::Range {
        if self.arguments.close == 0 {
            self.type_.span()
        } else {
            self.type_.span().extend(self.arguments.span())
        }
    }
}

impl<'a, T: 'a> Span for Tuple<'a, T> {
    /// Returns the range spanned by the tuple.
    fn span(&self) -> com::Range {
        self.parenthesis_open().span().extend(self.parenthesis_close().span())
    }
}

//
//  Implementations of Default
//
impl<'a, T: 'a> Default for Tuple<'a, T> {
    fn default() -> Tuple<'a, T> {
        Tuple { fields: &[], commas: &[], open: 0, close: 0 }
    }
}

