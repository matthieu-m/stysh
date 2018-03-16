//! Types

use std::convert;

use basic::com::{self, Span};
use basic::mem::InternId;

use model::ast::*;
use model::tt;

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type<'a> {
    /// A missing type.
    Missing(com::Range),
    /// A nested nominal type.
    Nested(TypeIdentifier, Path<'a>),
    /// A simple nominal type.
    Simple(TypeIdentifier),
    /// A tuple.
    Tuple(Tuple<'a, Type<'a>>),
}

/// A Path.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Path<'a> {
    /// Components of the path.
    pub components: &'a [TypeIdentifier],
    /// Offsets of the double colons separating the arguments, an absent double
    /// colon is placed at the offset of the last character of the field it
    /// would have followed.
    pub colons: &'a [u32],
}

/// A Type Identifier.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeIdentifier(pub InternId, pub com::Range);

//
//  Implementations
//
impl<'a> Type<'a> {
    /// Returns the name of the type.
    pub fn name(&self) -> Option<TypeIdentifier> {
        use self::Type::*;

        match *self {
            Nested(t, _) | Simple(t) => Some(t),
            Missing(_) | Tuple(_) => None,
        }
    }
}

impl TypeIdentifier {
    /// Returns the InternId.
    pub fn id(&self) -> InternId { self.0 }

    /// Sets the InternId of the TypeIdentifier.
    pub fn with_id(self, id: InternId) -> Self {
        TypeIdentifier(id, self.1)
    }
}

impl<'a> Path<'a> {
    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn double_colon(&self, i: usize) -> Option<tt::Token> {
        self.colons
            .get(i)
            .map(|&o| tt::Token::new(tt::Kind::SignDoubleColon, o as usize, 2))
    }
}

//
//  Implementations of Span
//
impl<'a> Span for Type<'a> {
    /// Returns the range spanned by the type.
    fn span(&self) -> com::Range {
        use self::Type::*;

        match *self {
            Missing(r) => r,
            Nested(t, p) => p.span().extend(t.span()),
            Simple(t) => t.span(),
            Tuple(t) => t.span(),
        }
    }
}

impl<'a> Span for Path<'a> {
    /// Returns the range spanned by the path.
    fn span(&self) -> com::Range {
        self.components[0]
            .span()
            .extend(self.double_colon(self.colons.len() - 1).unwrap().span())
    }
}

impl Span for TypeIdentifier {
    /// Returns the range spanned by the type identifier.
    fn span(&self) -> com::Range { self.1 }
}

//
//  Implementations of From
//
impl<'a> convert::From<Tuple<'a, Type<'a>>> for Type<'a> {
    fn from(t: Tuple<'a, Type<'a>>) -> Type<'a> { Type::Tuple(t) }
}
