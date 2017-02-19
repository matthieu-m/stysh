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

use std::{iter, ops};

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

        self.skip(tok.len());
        self.skip_whitespace();

        Some(RawToken::new(tok, o, li, lo))
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

    fn lex_attribute(&self) -> &'a [u8] {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] == b'[');

        unimplemented!()
    }

    fn lex_comment(&self) -> &'a [u8] {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] != b'[');

        let index = self.first(|c| c == b'\n').unwrap_or(self.raw.len());
        &self.raw[..index]
    }

    fn lex_generic(&self) -> &'a [u8] {
        let is_special = |c| {
            c <= b' ' ||
            c >= 0x7f ||
            ASCII_SINGLETONS.contains(c)
        };

        let index = self.first(is_special).unwrap_or(self.raw.len());
        &self.raw[..index]
    }

    fn lex_singleton(&self) -> &'a [u8] {
        debug_assert!(
            self.raw[0] == b'(' || self.raw[0] == b')' ||
            self.raw[0] == b'{' || self.raw[0] == b'}' ||
            self.raw[0] == b'[' || self.raw[0] == b']' ||
            self.raw[0] == b';' || self.raw[0] == b','
        );
        &self.raw[..1]
    }

    fn lex_string(&self) -> &'a [u8] {
        let start = if self.raw[0] == b'b' { 1 } else { 0 };
        debug_assert!(self.raw[start] == 0x22 || self.raw[start] == b'\'');

        let quote = self.raw[start];

        let advance_until_end_or_eol = |mut index| -> Option<usize> {
            let matcher = |c| -> bool { c == b'\n' || c == quote };

            while let Some(i) = self.first_from(index + 1, &matcher) {
                //  If we reached EOL or an unescaped quote
                if self.raw[i] == b'\n' || self.raw.get(i + 1) != Some(&quote) {
                    return Some(i);
                }

                //  Move past the escaped quote
                index = i + 1;
            }

            //  Unterminated string!
            None
        };

        let is_whitespace = |range: ops::Range<usize>| {
            self.raw[range].iter().all(|&c| c == b' ')
        };

        //  Simple case: single-line.
        let index = advance_until_end_or_eol(start + 1);

        match index {
            Some(i) if self.raw[i] != b'\n' => return &self.raw[..i+1],
            Some(i) if !is_whitespace(0..i) => return &self.raw[..i],
            Some(_) => (),
            None => return &self.raw[..],
        }

        //  Complex case: multi-line.
        unimplemented!()
    }

    fn first<F: Fn(u8) -> bool>(&self, f: F) -> Option<usize> {
        self.raw.iter().position(|&c| f(c))
    }

    fn first_from<F: Fn(u8) -> bool>(&self, start: usize, f: F)
        -> Option<usize>
    {
        self.raw[start..].iter().position(|&c| f(c)).map(|i| i + start)
    }
}

impl<'a> RawToken<'a> {
    fn new(raw: &'a [u8], o: usize, li: usize, lo: usize)
        -> RawToken<'a>
    {
        RawToken { raw: raw, offset: o, line_index: li, line_offset: lo }
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

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::{RawStream, RawToken};

    #[test]
    fn lex_integer() {
        assert_eq!(
            lexit(b"1"),
            vec![
                RawToken::new(b"1", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_add_expression() {
        assert_eq!(
            lexit(b"1 +  2"),
            vec![
                RawToken::new(b"1", 0, 0, 0),
                RawToken::new(b"+", 2, 0, 2),
                RawToken::new(b"2", 5, 0, 5),
            ]
        );
    }

    #[test]
    fn lex_simple_string() {
        assert_eq!(
            lexit(b"'Hello, Arnold'"),
            vec![
                RawToken::new(b"'Hello, Arnold'", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_mixed_quotes_string() {
        assert_eq!(
            lexit(b"'Hello, \"Arnold\"'"),
            vec![
                RawToken::new(b"'Hello, \"Arnold\"'", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_string_with_escaped_quotes() {
        assert_eq!(
            lexit(b"'Hello, ''Arnold'''"),
            vec![
                RawToken::new(b"'Hello, ''Arnold'''", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_string_with_too_many_escaped_quotes() {
        assert_eq!(
            lexit(b"'Hello, ''Arnold'''''"),
            vec![
                RawToken::new(b"'Hello, ''Arnold'''''", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_string_with_missing_end_quote() {
        assert_eq!(
            lexit(b"'Hello, ''Arnold''''"),
            vec![
                RawToken::new(b"'Hello, ''Arnold''''", 0, 0, 0),
            ]
        );
        assert_eq!(
            lexit(b"'Hello, ''Arnold''''\n"),
            vec![
                RawToken::new(b"'Hello, ''Arnold''''", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_simple_string_bind() {
        assert_eq!(
            lexit(b":let x := 'Hello, Arnold';"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'Hello, Arnold'", 10, 0, 10),
                RawToken::new(b";", 25, 0, 25),
            ]
        );
    }

    fn lexit<'a>(raw: &'a [u8]) -> Vec<RawToken<'a>> {
        RawStream::new(&raw).collect()
    }
}

