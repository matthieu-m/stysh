//! Common types.

use basic::com::{self, Span};
use basic::mem;

use model::ast::*;
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
    /// Ranges of the names of the fields:
    /// -   if the tuple is unnanmed, the slice is empty,
    /// -   otherwise, the slice has the same size as the fields.
    /// An absent name is placed at the offset of the first character of the
    /// field it would have preceeded, with a length of 0.
    pub names: &'a [(mem::InternId, com::Range)],
    /// Offsets of the separators between names and fields, an absent separator
    /// is placed at the offset of the first field it would have preceeded.
    /// Note: The separator is ':' for types and patterns and ':=' for values.
    pub separators: &'a [u32],
    /// Offset of the opening parenthesis.
    pub open: u32,
    /// Offset of the closing parenthesis, an absent parenthesis is placed at
    /// at the offset of the last character of the field it would have followed.
    pub close: u32,
}

//
//  Implementations
//
impl<T> Tuple<'static, T> {
    /// Returns a unit tuple.
    pub fn unit() -> Tuple<'static, T> { Tuple::default() }
}

impl<'a, T: 'a> Tuple<'a, T> {
    /// Returns whether the tuple is empty.
    pub fn is_empty(&self) -> bool { self.fields.is_empty() }

    /// Returns the number of fields of the tuple.
    pub fn len(&self) -> usize { self.fields.len() }

    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        Self::token_from_offset(self.commas.get(i), tt::Kind::SignComma, 1)
    }

    /// Returns the name of the field at index i, if any.
    pub fn name(&self, i: usize) -> Option<tt::Token> {
        self.names
            .get(i)
            .map(|&r| Self::token(tt::Kind::NameField, r.1.offset(), r.1.length()))
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        Self::token(tt::Kind::ParenthesisOpen, self.open as usize, 1)
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        Self::token(tt::Kind::ParenthesisClose, self.close as usize, 1)
    }
}

impl<'a> Tuple<'a, Expression<'a>> {
    /// Returns the token of the bind following the i-th field, if there is no
    /// such bind the position it would have been at is faked.
    pub fn bind(&self, i: usize) -> Option<tt::Token> {
        Self::token_from_offset(self.separators.get(i), tt::Kind::SignBind, 2)
    }
}

impl<'a> Tuple<'a, Pattern<'a>> {
    /// Returns the token of the colon following the i-th field, if there is no
    /// such colon the position it would have been at is faked.
    pub fn colon(&self, i: usize) -> Option<tt::Token> {
        Self::token_from_offset(self.separators.get(i), tt::Kind::SignColon, 1)
    }
}

impl<'a> Tuple<'a, Type<'a>> {
    /// Returns the token of the colon following the i-th field, if there is no
    /// such colon the position it would have been at is faked.
    pub fn colon(&self, i: usize) -> Option<tt::Token> {
        Self::token_from_offset(self.separators.get(i), tt::Kind::SignColon, 1)
    }
}

impl<'a, T: 'a + Clone> Tuple<'a, T> {
    /// Returns the field at index i.
    pub fn field(&self, i: usize) -> Option<T> {
        self.fields.get(i).cloned()
    }
}

//
//  Implementation Details
//
impl<'a, T: 'a> Tuple<'a, T> {
    fn token_from_offset(offset: Option<&u32>, kind: tt::Kind, length: usize)
        -> Option<tt::Token>
    {
        offset.map(|&o| Self::token(kind, o as usize, length))
    }

    fn token(kind: tt::Kind, offset: usize, length: usize) -> tt::Token {
        tt::Token::new(kind, offset, length)
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
        Tuple {
            fields: &[],
            commas: &[],
            names: &[],
            separators: &[],
            open: 0,
            close: 0
        }
    }
}

