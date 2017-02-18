//! Lexing pass, aka parsing.
//!
//! Turns raw text into tokens.
//!
//! This is implemented as a stream, to avoid unnecessary memory allocation.

use std::iter;
use basic::mem;

use model::tt::*;
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
    stream: RawStream<'a>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> iter::Iterator for LexerImpl<'a, 'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        let mut buffer = mem::Array::new(self.local_arena);

        while let Some(token) = self.parse_token() {
            buffer.push(token);
        }

        if buffer.is_empty() {
            None
        } else {
            let slice = self.global_arena.insert_slice(buffer.into_slice());
            Some(Node::Run(slice))
        }
    }
}

impl<'a, 'g, 'local> LexerImpl<'a, 'g, 'local> {
    fn new(raw: &'a [u8], g: &'g mem::Arena, a: &'local mem::Arena)
        -> LexerImpl<'a, 'g, 'local>
    {
        LexerImpl {
            stream: RawStream::new(raw),
            global_arena: g,
            local_arena: a
        }
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
