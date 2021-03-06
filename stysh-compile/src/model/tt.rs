//! Token Tree
//!
//! This is the most barebone model of a Stysh program; it mostly consists of
//! raw tokens and simply groups them according to braces and quotes.

use std;
use crate::basic::com::{self, Span};
use crate::basic::mem::{CloneInto, Arena};

/// A List of Token Trees.
pub type List<'a> = &'a [Node<'a>];

/// A node of the token tree.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Node<'a> {
    /// An uninterrupted run of simple tokens.
    Run(&'a [Token]),
    /// A brace-enclosed group.
    ///
    /// Note: in case the closing brace was inferred, its length is 0.
    Braced(Token, &'a [Node<'a>], Token),
    /// A raw bytes literal.
    ///
    /// Note: in case the end quote was inferred, its length is 0.
    Bytes(Token, &'a [StringFragment], Token),
    /// A raw string literal.
    ///
    /// Note: in case the end quote was inferred, its length is 0.
    String(Token, &'a [StringFragment], Token),
    /// An unexpected closing brace.
    UnexpectedBrace(Token),
}

/// A language token.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Token {
    kind_length: u32,
    offset: u32,
}

/// Kind of the token, the primary information used in forming the AST.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Kind {
    /// A '}' brace.
    BraceClose,
    /// A '{' brace.
    BraceOpen,
    /// A ']' brace.
    BracketClose,
    /// A '[' brace.
    BracketOpen,
    /// The :and keyword.
    KeywordAnd,
    /// The :else keyword.
    KeywordElse,
    /// The :enum keyword.
    KeywordEnum,
    /// The :ext keyword.
    KeywordExt,
    /// The :for keyword.
    KeywordFor,
    /// The :fun keyword.
    KeywordFun,
    /// The :if keyword.
    KeywordIf,
    /// The :imp keyword.
    KeywordImp,
    /// The :int keyword.
    KeywordInt,
    /// The :loop keyword.
    KeywordLoop,
    /// The :not keyword.
    KeywordNot,
    /// The :or keyword.
    KeywordOr,
    /// The :rec keyword.
    KeywordRec,
    /// The :set keyword.
    KeywordSet,
    /// The :return keyword.
    KeywordReturn,
    /// The :var keyword.
    KeywordVar,
    /// The :xor keyword.
    KeywordXor,
    /// A 'false' literal.
    LitBoolFalse,
    /// A 'true' literal.
    LitBoolTrue,
    /// An integral.
    LitIntegral,
    /// The name of a field.
    NameField,
    /// The name of a module.
    NameModule,
    /// The name of a type.
    NameType,
    /// The name of a value.
    NameValue,
    /// A ')' brace,
    ParenthesisClose,
    /// A '(' brace,
    ParenthesisOpen,
    /// A double quote.
    QuoteDouble,
    /// A single quote.
    QuoteSingle,
    /// A '->' sign.
    SignArrowSingle,
    /// A '!=' sign.
    SignBangEqual,
    /// A ':=' sign.
    SignBind,
    /// A ':' sign.
    SignColon,
    /// A ',' sign.
    SignComma,
    /// A '-' sign.
    SignDash,
    /// A '::' sign.
    SignDoubleColon,
    /// A '==' sign.
    SignDoubleEqual,
    /// A '//' sign.
    SignDoubleSlash,
    /// A '<' sign.
    SignLeft,
    /// A '<=' sign.
    SignLeftEqual,
    /// A '+' sign.
    SignPlus,
    /// A '>' sign.
    SignRight,
    /// A '>=' sign.
    SignRightEqual,
    /// A ';' sign.
    SignSemiColon,
    /// A '*' sign.
    SignStar,
    /// A '_' sign.
    SignUnderscore,
    /// A format for an argument or interpolated identifier.
    StringFormat,
    /// An identifier in scope, to be interpolated.
    StringIdentifier,
    /// An index to an argument passed to formatting ('0', '1', ...).
    StringIndexedArgument,
    /// A name to an argument passed to formatting ('.x', '.y', ...).
    StringNamedArgument,
    /// A position to an argument passed to formatting (always 0-length).
    StringPositionalArgument,
    /// A special character ('N', 'LF', 'U+0A', ...).
    StringSpecialCharacter,
    /// A text portion.
    StringText,
}

/// Fragment of String.
///
/// Multiple fragments are used to represent a single string literal because:
/// -   some characters may be escaped,
/// -   some special character may be present,
/// -   some whitespace must be ignored in multi-line literals,
/// -   some parts are interpolated.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum StringFragment {
    /// A text fragment.
    Text(Token),
    /// A special character (single token for "{...}").
    SpecialCharacter(Token),
    /// An interpolated section: "{", [argument][, ":", format,] "}".
    Interpolated(Token, Token, Token),
    /// An unexpectedly formatted interpolated section: "{", ..., "}".
    Unexpected(com::Range),
}

impl<'g> Node<'g> {
    /// Returns the first token.
    pub fn front(&self) -> Token {
        match *self {
            Node::Run(slice) => slice[0],
            Node::Braced(o, _, _) => o,
            Node::Bytes(o, _, _) => o,
            Node::String(o, _, _) => o,
            Node::UnexpectedBrace(t) => t,
        }
    }

    /// Returns the range spanned by the node.
    pub fn range(&self) -> com::Range {
        match *self {
            Node::Run(slice) => {
                let offset = slice.first().map_or(0, |n| n.span().offset());
                let end = slice.last().map_or(0, |n| n.span().end_offset());
                com::Range::new(offset, end - offset)
            },
            Node::Bytes(o, _, c) =>
                com::Range::new(o.offset() - 1, 1).extend(c.span()),
            Node::Braced(o, _, c) => o.span().extend(c.span()),
            Node::String(o, _, c) => o.span().extend(c.span()),
            Node::UnexpectedBrace(t) => t.span(),
        }
    }
}

impl StringFragment {
    /// Returns the length of the fragment.
    pub fn length(&self) -> usize { self.span().length() }

    /// Returns the offset of the fragment.
    pub fn offset(&self) -> usize { self.span().offset() }

    /// Returns the range spanned by the fragment.
    pub fn range(&self) -> com::Range {
        match *self {
            StringFragment::Text(tok) => tok.span(),
            StringFragment::SpecialCharacter(tok) => 
                com::Range::new(
                    tok.offset() - 1,
                    tok.length() + 2
                ),
            StringFragment::Interpolated(id, colon, format) => 
                com::Range::new(
                    id.offset() - 1,
                    id.length() + colon.length() + format.length() + 2
                ),
            StringFragment::Unexpected(r) => r,
        }
    }
}

impl Token {
    /// Creates a new token of the specified kind, spanning the specified range.
    ///
    /// Note:   on top of the usual restrictions on the range, no token can span
    ///         more than 16MB.
    pub fn new(kind: Kind, offset: usize, length: usize) -> Token {
        debug_assert!(offset <= std::u32::MAX as usize);
        debug_assert!(length < (1usize << 24));
        debug_assert!((kind as u32) < (1u32 << 8));

        Token {
            kind_length: ((kind as u32) << 24) + (length as u32),
            offset: offset as u32,
        }
    }

    /// Returns the kind of the token.
    pub fn kind(&self) -> Kind {
        unsafe { std::mem::transmute((self.kind_length >> 24) as u8) }
    }

    /// Returns the length of the token.
    pub fn length(&self) -> usize { (self.kind_length & 0x00FFFFFF) as usize }

    /// Returns the start position of the token.
    ///
    /// Equivalent to calling `self.span().offset()`.
    pub fn offset(&self) -> usize { self.offset as usize }
}

//
//  Implementation of Span
//

impl<'g> Span for Node<'g> {
    /// Returns the range spanned by the node.
    fn span(&self) -> com::Range {
        match *self {
            Node::Run(slice) => {
                let offset = slice.first().map_or(0, |n| n.span().offset());
                let end = slice.last().map_or(0, |n| n.span().end_offset());
                com::Range::new(offset, end - offset)
            },
            Node::Bytes(o, _, c) =>
                com::Range::new(o.offset() - 1, 1).extend(c.span()),
            Node::Braced(o, _, c) => o.span().extend(c.span()),
            Node::String(o, _, c) => o.span().extend(c.span()),
            Node::UnexpectedBrace(t) => t.span(),
        }
    }
}

impl Span for StringFragment {
    /// Returns the range spanned by the fragment.
    fn span(&self) -> com::Range {
        match *self {
            StringFragment::Text(tok) => tok.span(),
            StringFragment::SpecialCharacter(tok) => 
                com::Range::new(
                    tok.offset() - 1,
                    tok.length() + 2
                ),
            StringFragment::Interpolated(id, colon, format) => 
                com::Range::new(
                    id.offset() - 1,
                    id.length() + colon.length() + format.length() + 2
                ),
            StringFragment::Unexpected(r) => r,
        }
    }
}

impl Span for Token {
    /// Returns the range spanned by the token.
    fn span(&self) -> com::Range {
        com::Range::new(self.offset(), self.length())
    }
}

//
//  Implementations of CloneInto
//
impl<'target> CloneInto<'target> for StringFragment {
    type Output = Self;

    fn clone_into(&self, _: &'target Arena) -> Self { *self }
} 

//
//  Implementation Details
//
impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{:?}@{}-{}",
            self.kind(),
            self.offset(),
            self.offset() + self.length(),
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{{ {} {} }}", self.kind(), self.span())
    }
}
