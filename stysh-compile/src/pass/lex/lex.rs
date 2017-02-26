//! Lexing pass, aka parsing.
//!
//! Turns raw text into tokens.
//!
//! This is implemented as a stream, to avoid unnecessary memory allocation.

use std::iter;
use basic::mem;

use model::tt::*;
use super::str;
use super::raw::{RawStream, RawToken};

/// The Stysh lexer.
///
/// The responsibility of the parser is to turn a raw slice of bytes into a
/// Token Tree.
///
/// No client should have to parse the raw slice by themselves.
pub struct Lexer<'g, 'local> {
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> Lexer<'g, 'local> {
    /// Creates a new instance of the parser.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(global: &'g mem::Arena, local: &'local mem::Arena)
        -> Lexer<'g, 'local>
    {
        Lexer { global_arena: global, local_arena: local }
    }

    /// Parses a raw slice of bytes into a Token Tree.
    pub fn parse(&mut self, raw: &[u8]) -> List<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        let imp = LexerImpl::new(raw, self.global_arena, self.local_arena);

        for node in imp {
            buffer.push(node);
        }

        self.global_arena.insert_slice(buffer.into_slice())
    }
}

//
//  Implementation Details
//
struct LexerImpl<'a, 'g, 'local> {
    stream: iter::Peekable<RawStream<'a>>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> iter::Iterator for LexerImpl<'a, 'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        use super::raw::RawKind::*;

        self.stream.peek().cloned().map(|tok| {
            match tok.kind {
                Attribute => self.parse_attribute(),
                Bytes => self.parse_bytes(),
                BytesMultiLines => self.parse_bytes_multi_lines(),
                Comment => self.parse_comment(),
                Generic => self.parse_generic(),
                String => self.parse_string(),
                StringMultiLines => self.parse_string_multi_lines(),
            }
        })
    }
}

impl<'a, 'g, 'local> LexerImpl<'a, 'g, 'local> {
    fn new(raw: &'a [u8], g: &'g mem::Arena, a: &'local mem::Arena)
        -> LexerImpl<'a, 'g, 'local>
    {
        LexerImpl {
            stream: RawStream::new(raw).peekable(),
            global_arena: g,
            local_arena: a
        }
    }

    fn parse_attribute(&mut self) -> Node<'g> {
        unimplemented!()
    }

    fn parse_bytes(&mut self) -> Node<'g> {
        let (start, fragments, end) = self.parse_string_literal(1, false);
        Node::Bytes(start, fragments, end)
    }

    fn parse_bytes_multi_lines(&mut self) -> Node<'g> {
        let (start, fragments, end) = self.parse_string_literal(1, true);
        Node::Bytes(start, fragments, end)
    }

    fn parse_comment(&mut self) -> Node<'g> {
        unimplemented!()
    }

    fn parse_generic(&mut self) -> Node<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        while let Some(token) = self.parse_token() {
            buffer.push(token);
        }

        debug_assert!(!buffer.is_empty());

        Node::Run(self.global_arena.insert_slice(buffer.into_slice()))
    }

    fn parse_string(&mut self) -> Node<'g> {
        let (start, fragments, end) = self.parse_string_literal(0, false);
        Node::String(start, fragments, end)
    }

    fn parse_string_multi_lines(&mut self) -> Node<'g> {
        let (start, fragments, end) = self.parse_string_literal(0, true);
        Node::String(start, fragments, end)
    }

    fn parse_string_literal(&mut self, offset: usize, with_new_lines: bool)
        -> (Token, &'g [StringFragment], Token)
    {
        let tok = self.stream.next().expect("Only called if peek succeeds");

        str::parse(
            tok,
            offset,
            with_new_lines,
            &self.global_arena,
            &self.local_arena
        )
    }

    fn parse_token(&mut self) -> Option<Token> {
        self.stream.next().and_then(|tok| {
            match tok.raw[0] {
                b'0'...b'9' => self.parse_number(tok),
                b'+' => Some(Token::new(Kind::OperatorPlus, tok.offset, 1)),
                _ => unimplemented!(),
            }
        })
    }

    fn parse_number(&self, tok: RawToken) -> Option<Token> {
        assert!(tok.raw.iter().all(|&c| c >= b'0' && c <= b'9'));

        Some(Token::new(Kind::Integral, tok.offset, tok.raw.len()))
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::mem;
    use model::tt::*;
    use super::Lexer;

    #[test]
    fn lex_integral_single_digit() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"1"),
            &[
                Node::Run(&[
                    Token::new(Kind::Integral, 0, 1),
                ])
            ]
        );
    }

    #[test]
    fn lex_integral_multiple_digits() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"0123"),
            &[
                Node::Run(&[
                    Token::new(Kind::Integral, 0, 4),
                ])
            ]
        );
    }

    #[test]
    fn lex_expression_integral_addition() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b" 12 + 34 "),
            &[
                Node::Run(&[
                    Token::new(Kind::Integral, 1, 2),
                    Token::new(Kind::OperatorPlus, 4, 1),
                    Token::new(Kind::Integral, 6, 2),
                ])
            ]
        );
    }

    fn lexit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> List<'g> {
        let mut local_arena = mem::Arena::new();

        let result = Lexer::new(&global_arena, &local_arena).parse(raw);
        local_arena.recycle();

        result
    }
}
