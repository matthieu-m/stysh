//! Token Tree
//!
//! This is the most barebone model of a Stysh program; it mostly consists of
//! raw tokens and simply groups them according to braces and quotes.

use std;
use basic::com;

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
    /// A ':' sign.
    Colon,
    /// An integral.
    Integral,
    /// A '+' sign.
    OperatorPlus,
    /// A double quote.
    QuoteDouble,
    /// A single quote.
    QuoteSingle,
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

impl StringFragment {
    /// Returns the length of the fragment.
    pub fn length(&self) -> usize { self.range().length() }

    /// Returns the offset of the fragment.
    pub fn offset(&self) -> usize { self.range().offset() }

    /// Returns the range spanned by the fragment.
    pub fn range(&self) -> com::Range {
        match *self {
            StringFragment::Text(tok) => tok.range(),
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
            offset: offset as u32
        }
    }

    /// Returns the kind of the token.
    pub fn kind(&self) -> Kind {
        unsafe { std::mem::transmute((self.kind_length >> 24) as u8) }
    }

    /// Returns the length of the token.
    pub fn length(&self) -> usize {
        (self.kind_length & 0x00FFFFFF) as usize
    }

    /// Returns the start position of the token.
    ///
    /// Equivalent to calling `self.range().offset()`.
    pub fn offset(&self) -> usize {
        self.offset as usize
    }

    /// Returns the range spanned by the token.
    pub fn range(&self) -> com::Range {
        com::Range::new(self.offset(), self.length())
    }
}

//
//  Implementation Details
//
impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "Token {{ kind: {:?}, offset: {}, length: {} }}",
            self.kind(),
            self.offset(),
            self.length()
        )
    }
}
