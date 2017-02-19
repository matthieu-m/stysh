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
        self.line_indent = 0;
    }

    fn skip_whitespace(&mut self) {
        let mut seen_new_line = false;
        let line_indent = self.line_indent;

        while !self.raw.is_empty() {
            match self.raw[0] {
                b' ' => {
                    self.skip(1);
                    self.line_indent += 1;
                }
                b'\n' => {
                    self.skip(1);
                    self.new_line();
                    seen_new_line = true;
                },
                b'\r' if self.raw.get(1) == Some(&b'\n') => {
                    self.skip(2);
                    self.new_line();
                    seen_new_line = true;
                },
                _ => break,
            };
        }

        if !seen_new_line {
            self.line_indent = line_indent;
        }
    }

    fn lex_attribute(&mut self) -> &'a [u8] {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] == b'[');

        unimplemented!()
    }

    fn lex_comment(&mut self) -> &'a [u8] {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] != b'[');

        let length = self.first(|c| c == b'\n').unwrap_or(self.raw.len());
        self.pop(length)
    }

    fn lex_generic(&mut self) -> &'a [u8] {
        let is_special = |c| {
            c <= b' ' ||
            c >= 0x7f ||
            ASCII_SINGLETONS.contains(c)
        };

        let length = self.first(is_special).unwrap_or(self.raw.len());
        self.pop(length)
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
        use std::str;

        fn first_from<F: Fn(u8) -> bool>(raw: &[u8], start: usize, f: F)
            -> Option<usize>
        {
            if start >= raw.len() {
                None
            } else {
                raw[start..].iter().position(|&c| f(c)).map(|i| i + start)
            }
        }

        let raw = self.raw;

        let start = if raw[0] == b'b' { 1 } else { 0 };
        debug_assert!(raw[start] == 0x22 || raw[start] == b'\'');

        let quote = raw[start];

        let advance_until_end_or_eol = |mut index| -> Option<usize> {
            let matcher = |c| -> bool { c == b'\n' || c == quote };

            while let Some(i) = first_from(raw, index, &matcher) {
                //  If we reached EOL or an unescaped quote
                if raw[i] == b'\n' || raw.get(i + 1) != Some(&quote) {
                    return Some(i);
                }

                //  Move past the escaped quote
                index = i + 2;
            }

            //  Unterminated string!
            None
        };

        let is_whitespace = |range: ops::Range<usize>| {
            raw[range].iter().all(|&c| c == b' ')
        };

        let line_index = self.line_index;
        let line_indent = self.line_indent;

        let mut index = start + 1;
        let mut line_start_offset = self.offset - self.line_offset;

        loop {
            //  Look for end of string or line
            index = match advance_until_end_or_eol(index) {
                None => return self.pop_all(),

                Some(i) if raw[i] == quote =>
                    return self.pop_string(i + 1, line_start_offset),

                Some(i) if line_index == self.line_index &&
                            !is_whitespace(start + 1..i) =>
                    return self.pop_string(i, line_start_offset),

                Some(i) => i + 1,
            };

            self.new_line();
            line_start_offset = self.offset + index;

            //  Look for end of string, line or start of "next" raw token with
            //  no quote to terminate the string.
            index = match first_from(raw, index, |c| c != b' ') {
                None => return self.pop_all(),

                Some(i) if raw[i] == quote =>
                    return self.pop_string(i + 1, line_start_offset),

                Some(i) if raw[i] == b'\n' => i + 1,

                Some(i) if i - index > line_indent => i,

                Some(_) => return self.pop_string(index, line_start_offset),
            };
        }
    }

    fn first<F: Fn(u8) -> bool>(&self, f: F) -> Option<usize> {
        self.raw.iter().position(|&c| f(c))
    }

    fn pop(&mut self, length: usize) -> &'a [u8] {
        let result = &self.raw[..length];
        self.skip(length);
        result
    }

    fn pop_string(&mut self, length: usize, line_start_offset: usize) ->
        &'a [u8]
    {
        let result = &self.raw[..length];
        self.raw = &self.raw[length..];
        self.offset += length;
        self.line_offset = self.offset - line_start_offset;
        result
    }

    fn pop_all(&mut self) -> &'a [u8] {
        let result = &self.raw[..];
        self.skip(result.len());
        result
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
    fn lex_multiline_string() {
        assert_eq!(
            lexit(b"'\n    Hello, Arnold\n'"),
            vec![
                RawToken::new(b"'\n    Hello, Arnold\n'", 0, 0, 0),
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

    #[test]
    fn lex_multiline_string_bind() {
        assert_eq!(
            lexit(b":let x := '\n    Hello, Arnold\n';"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'\n    Hello, Arnold\n'", 10, 0, 10),
                RawToken::new(b";", 31, 2, 1),
            ]
        );

        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n    ';"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'\n        Hello, Arnold\n    '", 15, 1, 4),
                RawToken::new(b";", 44, 3, 5),
            ]
        );
    }

    #[test]
    fn lex_multiline_string_bind_misindented_end_quote() {
        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n';"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'\n        Hello, Arnold\n'", 15, 1, 4),
                RawToken::new(b";", 40, 3, 1),
            ]
        );
    }

    #[test]
    fn lex_multiline_string_bind_missing_end_quote() {
        assert_eq!(
            lexit(b":let x := '\n    Hello, Arnold\n:let"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'\n    Hello, Arnold\n", 10, 0, 10),
                RawToken::new(b":let", 30, 2, 0),
            ]
        );

        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n:let"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'\n        Hello, Arnold\n", 15, 1, 4),
                RawToken::new(b":let", 39, 3, 0),
            ]
        );

        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n    :let"),
            vec![
                RawToken::new(b":let", 0, 0, 0),
                RawToken::new(b"x", 5, 0, 5),
                RawToken::new(b":=", 7, 0, 7),
                RawToken::new(b"'\n        Hello, Arnold\n", 15, 1, 4),
                RawToken::new(b":let", 43, 3, 4),
            ]
        );
    }

    fn lexit<'a>(raw: &'a [u8]) -> Vec<RawToken<'a>> {
        RawStream::new(&raw).collect()
    }
}

