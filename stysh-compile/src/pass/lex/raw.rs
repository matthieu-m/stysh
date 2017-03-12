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

use std::{fmt, iter, ops};

use basic::com;

pub trait PeekableTokenStream<'a>: iter::Iterator<Item = RawToken<'a>> {
    fn underlying(&mut self) -> &mut iter::Peekable<RawStream<'a>>;
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RawStream<'a> {
    raw: &'a [u8],
    offset: usize,
    line_offset: usize,
    line_indent: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RawToken<'a> {
    pub kind: RawKind,
    pub raw: &'a [u8],
    pub offset: usize,
    pub line_offset: usize,
    pub line_indent: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum RawKind {
    Attribute,
    BraceOpen,
    BraceClose,
    Bytes,
    BytesMultiLines,
    Comment,
    Generic,
    String,
    StringMultiLines,
}

impl<'a> RawStream<'a> {
    pub fn new(raw: &'a [u8]) -> RawStream<'a> {
        let mut result = RawStream {
            raw: raw,
            offset: 0,
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
        let (o, lo, id) = (self.offset, self.line_offset, self.line_indent);

        let (kind, tok) = match self.raw[0] {
            c if ASCII_SINGLETONS.contains(c) => self.lex_singleton(),
            c if ASCII_QUOTES.contains(c) => self.lex_string(),
            b'b' if ASCII_QUOTES.contains_opt(next) => self.lex_string(),
            b'#' if next == Some(b'[') => self.lex_attribute(),
            b'#' => self.lex_comment(),
            b':' => self.lex_generic(1),
            _ => self.lex_generic(0),
        };

        self.skip_whitespace();

        Some(RawToken::new(kind, tok, o, lo, id))
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

    fn lex_attribute(&mut self) -> (RawKind, &'a [u8]) {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] == b'[');

        unimplemented!()
    }

    fn lex_comment(&mut self) -> (RawKind, &'a [u8]) {
        debug_assert!(self.raw[0] == b'#' && self.raw[1] != b'[');

        let length = self.first(|c| c == b'\n').unwrap_or(self.raw.len());
        (RawKind::Comment, self.pop(length))
    }

    fn lex_generic(&mut self, offset: usize) -> (RawKind, &'a [u8]) {
        let is_special = |&c| {
            c <= b' ' ||
            c >= 0x7f ||
            c == b':' ||
            ASCII_SINGLETONS.contains(c)
        };

        let length =
            self.raw[offset..]
                .iter()
                .position(is_special)
                .map(|i| i + offset)
                .unwrap_or(self.raw.len());

        (RawKind::Generic, self.pop(length))
    }

    fn lex_singleton(&mut self) -> (RawKind, &'a [u8]) {
        debug_assert!(
            self.raw[0] == b'(' || self.raw[0] == b')' ||
            self.raw[0] == b'{' || self.raw[0] == b'}' ||
            self.raw[0] == b'[' || self.raw[0] == b']' ||
            self.raw[0] == b';' || self.raw[0] == b','
        );

        match self.raw[0] {
            b'(' | b'{' | b'[' => (RawKind::BraceOpen, self.pop(1)),
            b')' | b'}' | b']' => (RawKind::BraceClose, self.pop(1)),
            _ => (RawKind::Generic, self.pop(1)),
        }
    }

    fn lex_string(&mut self) -> (RawKind, &'a [u8]) {
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
        let (mut kind, kind_multi) = if start == 0 {
            (RawKind::String, RawKind::StringMultiLines)
        } else {
            (RawKind::Bytes, RawKind::BytesMultiLines)
        };
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

        let line_indent = self.line_indent;

        let mut first_line = true;
        let mut index = start + 1;
        let mut line_start_offset = self.offset - self.line_offset;

        loop {
            //  Look for end of string or line
            index = match advance_until_end_or_eol(index) {
                None => return (kind, self.pop_all()),

                Some(i) if raw[i] == quote =>
                    return (kind, self.pop_string(i + 1, line_start_offset)),

                Some(i) if first_line && !is_whitespace(start + 1..i) =>
                    return (kind, self.pop_string(i, line_start_offset)),

                Some(i) => i + 1,
            };

            self.new_line();
            first_line = false;
            kind = kind_multi;
            line_start_offset = self.offset + index;

            //  Look for end of string, line or start of "next" raw token with
            //  no quote to terminate the string.
            let i = match first_from(raw, index, |c| c != b' ') {
                None => return (kind, self.pop_all()),
                Some(i) => i,
            };

            self.line_indent = i - index;

            index = if raw[i] == quote {
                    return (kind, self.pop_string(i + 1, line_start_offset));
            } else if raw[i] == b'\n' || self.line_indent > line_indent {
                i
            } else {
                return (kind, self.pop_string(index, line_start_offset));
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

    fn pop_string(&mut self, length: usize, line_start_offset: usize)
        -> &'a [u8]
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
    pub fn new(
        kind: RawKind,
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken {
            kind: kind,
            raw: raw,
            offset: offset,
            line_offset: line_offset,
            line_indent: line_indent
        }
    }

    #[allow(dead_code)]
    fn new_attribute(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(
            RawKind::Attribute,
            raw,
            offset,
            line_offset,
            line_indent
        )
    }

    #[allow(dead_code)]
    fn new_bytes(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(RawKind::Bytes, raw, offset, line_offset, line_indent)
    }

    #[allow(dead_code)]
    fn new_bytes_multi(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(
            RawKind::BytesMultiLines,
            raw,
            offset,
            line_offset,
            line_indent
        )
    }

    #[allow(dead_code)]
    fn new_comment(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(RawKind::Comment, raw, offset, line_offset, line_indent)
    }

    #[cfg(test)]
    fn new_generic(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(RawKind::Generic, raw, offset, line_offset, line_indent)
    }

    #[cfg(test)]
    fn new_string(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(RawKind::String, raw, offset, line_offset, line_indent)
    }

    #[cfg(test)]
    fn new_string_multi(
        raw: &'a [u8],
        offset: usize,
        line_offset: usize,
        line_indent: usize
    )
        -> RawToken<'a>
    {
        RawToken::new(
            RawKind::StringMultiLines, raw, offset, line_offset, line_indent
        )
    }
}

impl<'a> fmt::Display for RawToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "RawToken {{ kind: {:?}, raw: {}, offset: {}, line_offset: {}, line_indent: {} }}",
            self.kind,
            com::Slice(self.raw),
            self.offset,
            self.line_offset,
            self.line_indent
        )
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
                RawToken::new_generic(b"1", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_add_expression() {
        assert_eq!(
            lexit(b"1 +  2"),
            vec![
                RawToken::new_generic(b"1", 0, 0, 0),
                RawToken::new_generic(b"+", 2, 2, 0),
                RawToken::new_generic(b"2", 5, 5, 0),
            ]
        );
    }

    #[test]
    fn lex_simple_string() {
        assert_eq!(
            lexit(b"'Hello, Arnold'"),
            vec![
                RawToken::new_string(b"'Hello, Arnold'", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_mixed_quotes_string() {
        assert_eq!(
            lexit(b"'Hello, \"Arnold\"'"),
            vec![
                RawToken::new_string(b"'Hello, \"Arnold\"'", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_string_with_escaped_quotes() {
        assert_eq!(
            lexit(b"'Hello, ''Arnold'''"),
            vec![
                RawToken::new_string(b"'Hello, ''Arnold'''", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_string_with_too_many_escaped_quotes() {
        assert_eq!(
            lexit(b"'Hello, ''Arnold'''''"),
            vec![
                RawToken::new_string(b"'Hello, ''Arnold'''''", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_string_with_missing_end_quote() {
        assert_eq!(
            lexit(b"'Hello, ''Arnold''''"),
            vec![
                RawToken::new_string(b"'Hello, ''Arnold''''", 0, 0, 0),
            ]
        );
        assert_eq!(
            lexit(b"'Hello, ''Arnold''''\n"),
            vec![
                RawToken::new_string(b"'Hello, ''Arnold''''", 0, 0, 0),
            ]
        );
    }

    #[test]
    fn lex_multiline_string() {
        assert_eq!(
            lexit(b"'\n    Hello, Arnold\n'"),
            vec![
                RawToken::new_string_multi(
                    b"'\n    Hello, Arnold\n'", 0, 0, 0
                ),
            ]
        );
    }

    #[test]
    fn lex_simple_string_bind() {
        assert_eq!(
            lexit(b":let x := 'Hello, Arnold';"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string(b"'Hello, Arnold'", 10, 10, 0),
                RawToken::new_generic(b";", 25, 25, 0),
            ]
        );
    }

    #[test]
    fn lex_multiline_string_bind() {
        assert_eq!(
            lexit(b":let x := '\n    Hello, Arnold\n';"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string_multi(
                    b"'\n    Hello, Arnold\n'", 10, 10, 0
                ),
                RawToken::new_generic(b";", 31, 1, 0),
            ]
        );

        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n    ';"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string_multi(
                    b"'\n        Hello, Arnold\n    '", 15, 4, 4
                ),
                RawToken::new_generic(b";", 44, 5, 4),
            ]
        );
    }

    #[test]
    fn lex_multiline_string_bind_misindented_end_quote() {
        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n';"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string_multi(
                    b"'\n        Hello, Arnold\n'", 15, 4, 4
                ),
                RawToken::new_generic(b";", 40, 1, 0),
            ]
        );
    }

    #[test]
    fn lex_multiline_string_bind_missing_end_quote() {
        assert_eq!(
            lexit(b":let x := '\n    Hello, Arnold\n:let"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string_multi(
                    b"'\n    Hello, Arnold\n", 10, 10, 0
                ),
                RawToken::new_generic(b":let", 30, 0, 0),
            ]
        );

        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n:let"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string_multi(
                    b"'\n        Hello, Arnold\n", 15, 4, 4
                ),
                RawToken::new_generic(b":let", 39, 0, 0),
            ]
        );

        assert_eq!(
            lexit(b":let x := \n    '\n        Hello, Arnold\n    :let"),
            vec![
                RawToken::new_generic(b":let", 0, 0, 0),
                RawToken::new_generic(b"x", 5, 5, 0),
                RawToken::new_generic(b":=", 7, 7, 0),
                RawToken::new_string_multi(
                    b"'\n        Hello, Arnold\n", 15, 4, 4
                ),
                RawToken::new_generic(b":let", 43, 4, 4),
            ]
        );
    }

    fn lexit<'a>(raw: &'a [u8]) -> Vec<RawToken<'a>> {
        RawStream::new(&raw).collect()
    }
}

