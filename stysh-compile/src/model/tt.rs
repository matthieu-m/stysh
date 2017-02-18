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
    BalancedGroup(Token, &'a Node<'a>, Token),
    /// A single string literal.
    BalancedString(Token, &'a [StringFragment], Token),
    /// A brace-enclosed group missing its closing brace.
    UnclosedGroup(Token, &'a Node<'a>, Token),
    /// A single string literal missing its closing quote.
    UnclosedString(Token, &'a [StringFragment], Token),
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
    /// An integral.
    Integral,
    /// The '+' operator.
    OperatorPlus,
}

/// Fragment of String.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum StringFragment {
    /// A text fragment.
    Text(Token),
    /// An identifier fragment: "${name}".
    Identifier(Token),
    /// A special character: "${N}", "${CR}" or "${U+A}".
    SpecialCharacter(Token),
    /// A formatting specifier.
    FormatSpecifier(Token),
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
            "Token {{ kind: {:?}, length: {}, offset: {}",
            self.kind(),
            self.length(),
            self.offset()
        )
    }
}
