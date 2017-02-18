//! Raw Lexing pass.
//!
//! This sub-pass of lexing is concerned in tokenizing the stream, but does not
//! concern itself in identifying the tokens, except for a few special cases.
//!
//! This allows the lexer implementation to focus on:
//! -   correctly identifying the tokens (numbers, identifiers, keywords, ...),
//! -   correctly balancing the braces (and recovering from the incorrectly,
//!     balanced ones).
//!
//! The one exception to the rule is string literals; multi-line string literals
//! always require more effort. They are useful enough to be worth it.

use std::iter;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RawStream<'a> {
    raw: &'a [u8],
    offset: usize,
    line_index: usize,
    line_offset: usize,
    line_indent: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RawToken<'a> {
    pub raw: &'a [u8],
    pub offset: usize,
    pub line_index: usize,
    pub line_offset: usize,
}

impl<'a> RawStream<'a> {
    pub fn new(raw: &'a [u8]) -> RawStream<'a> {
        let mut result = RawStream {
            raw: raw,
            offset: 0,
            line_index: 0,
            line_offset: 0,
            line_indent: 0,
        };
        result.skip_whitespace();
        result
    }
}

impl<'a> iter::Iterator for RawStream<'a> {
    type Item = RawToken<'a>;

    fn next(&mut self) -> Option<RawToken<'a>> {
        if self.raw.is_empty() {
            return None;
        }

        let next = self.raw.get(1).cloned();
        let (o, li, lo) = (self.offset, self.line_index, self.line_offset);

        let tok = match self.raw[0] {
            c if ASCII_SINGLETONS.contains(c) => self.lex_singleton(),
            c if ASCII_QUOTES.contains(c) => self.lex_string(),
            b'b' if ASCII_QUOTES.contains_opt(next) => self.lex_string(),
            b'#' if next == Some(b'[') => self.lex_attribute(),
            b'#' => self.lex_comment(),
            _ => self.lex_generic(),
        };

        self.skip_whitespace();

        Some(RawToken { raw: tok, offset: o, line_index: li, line_offset: lo })
    }
}

//
//  Implementation Details
//
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
struct AsciiSet(u64, u64);

//  ()[]{};,
const ASCII_SINGLETONS: AsciiSet =
    AsciiSet(0x800130000000000, 0x2800000028000000);

//  "'
const ASCII_QUOTES: AsciiSet =  AsciiSet(0x8400000000, 0);

impl<'a> RawStream<'a> {
    fn skip(&mut self, length: usize) {
        debug_assert!(length <= self.raw.len());
        self.raw = &self.raw[length..];
        self.offset += length;
        self.line_offset += length;
    }

    fn new_line(&mut self) {
        self.line_index += 1;
        self.line_offset = 0;
    }

    fn skip_whitespace(&mut self) {
        while !self.raw.is_empty() {
            match self.raw[0] {
                b' ' => {
                    self.skip(1);
                    self.line_indent += 1;
                }
                b'\n' => {
                    self.skip(1);
                    self.new_line();
                },
                b'\r' if self.raw.get(1) == Some(&b'\n') => {
                    self.skip(2);
                    self.new_line();
                },
                _ => break,
            };
        }
    }

    fn lex_attribute(&mut self) -> &'a [u8] {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] == b'[');

        unimplemented!()
    }

    fn lex_comment(&mut self) -> &'a [u8] {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] != b'[');

        let index = self.first(|c| c == b'\n').unwrap_or(self.raw.len());
        self.pop(index)
    }

    fn lex_generic(&mut self) -> &'a [u8] {
        let is_special = |c| {
            c <= b' ' ||
            c >= 0x7f ||
            ASCII_SINGLETONS.contains(c)
        };

        let index = self.first(is_special).unwrap_or(self.raw.len());
        self.pop(index)
    }

    fn lex_singleton(&mut self) -> &'a [u8] {
        debug_assert!(
            self.raw[0] == b'(' || self.raw[0] == b')' ||
            self.raw[0] == b'{' || self.raw[0] == b'}' ||
            self.raw[0] == b'[' || self.raw[0] == b']' ||
            self.raw[0] == b';' || self.raw[0] == b','
        );
        self.pop(1)
    }

    fn lex_string(&mut self) -> &'a [u8] {
        let start = if self.raw[0] == b'b' { 1 } else { 0 };
        debug_assert!(self.raw[start] == 0x20 || self.raw[start] == b'\'');

        let quote = self.raw[start];

        let matcher = |c| -> bool { c == b'\n' || c == quote };

        //  Simple case: single-line and no escaped quote.
        if let Some(i) = self.first(matcher) {
            if self.raw[i] == quote && self.raw.get(i+1) != Some(&quote) {
                return self.pop(i+1);
            }
        }

        //  Complex case: multi-line and/or escaped quotes.
        unimplemented!()
    }

    fn pop(&mut self, length: usize) -> &'a [u8] {
        let result = &self.raw[..length];
        self.skip(length);
        result
    }

    fn first<F: Fn(u8) -> bool>(&self, f: F) -> Option<usize> {
        self.raw.iter().position(|&c| f(c))
    }
}

impl AsciiSet {
    fn contains(&self, byte: u8) -> bool {
        if byte >= 128 {
            false
        } else if byte >= 64 {
            self.1 & (1u64 << (byte - 64)) != 0
        } else {
            self.0 & (1u64 << byte) != 0
        }
    }

    fn contains_opt(&self, byte: Option<u8>) -> bool {
        match byte {
            Some(b) => self.contains(b),
            None => false,
        }
    }
}
