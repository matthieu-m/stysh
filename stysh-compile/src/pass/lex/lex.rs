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
        let mut raw = RawStream::new(raw).peekable();

        LexerImpl::new(
            RawPeekableStream::new(&mut raw, b"", -1),
            self.global_arena,
            self.local_arena
        ).parse_all()
    }
}

//
//  Implementation Details
//
type UnderlyingStream<'a> = iter::Peekable<RawStream<'a>>;

struct LexerImpl<'a, 'b, 'g, 'local>  where 'a: 'b {
    stream: RawPeekableStream<'a, 'b>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

pub struct RawPeekableStream<'a, 'b>  where 'a: 'b {
    raw: &'b mut UnderlyingStream<'a>,
    seek: &'static [u8],
    min_indent: isize,
}

impl<'a, 'b, 'g, 'local> iter::Iterator for LexerImpl<'a, 'b, 'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        use super::raw::RawKind::*;

        self.stream.peek().map(|tok| {
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

impl<'a, 'b, 'g, 'local> LexerImpl<'a, 'b, 'g, 'local> {
    fn new(
        stream: RawPeekableStream<'a, 'b>,
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena
    )
        -> LexerImpl<'a, 'b, 'g, 'local>
    {
        LexerImpl {
            stream: stream,
            global_arena: global_arena,
            local_arena: local_arena
        }
    }

    fn parse_all(&mut self) -> List<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        for node in &mut *self {
            buffer.push(node);
        }

        self.global_arena.insert_slice(buffer.into_slice())
    }

    fn parse_attribute(&mut self) -> Node<'g> {
        unimplemented!()
    }

    fn parse_braces(&mut self) -> Node<'g> {
        let raw_open = self.stream.next().expect("Only called when available");

        let (open_kind, close_kind, raw_close) = match raw_open.raw {
            b"{" => (Kind::BraceOpen, Kind::BraceClose, b"}"),
            b"[" => (Kind::BracketOpen, Kind::BracketClose, b"]"),
            b"(" => (Kind::ParenthesisOpen, Kind::ParenthesisClose, b")"),
            _ => panic!("Unexpected raw: '{:?}'", raw_open),
        };

        let open = Token::new(open_kind, raw_open.offset, raw_open.raw.len());

        let inner = {
            let inner_stream = RawPeekableStream::new(
                self.stream.underlying(),
                raw_close,
                raw_open.line_indent as isize
            );

            LexerImpl::new(inner_stream, self.global_arena, self.local_arena)
                .parse_all()
        };

        //  In case of nested braces such as "(())", self.stream will refuse to
        //  let one pull a ")", so we use the underlying directly to pull out
        //  the closing brace (if any).
        let underlying = self.stream.underlying();

        let pop = underlying.peek().map_or(false, |tok| tok.raw == raw_close);

        let close = if pop {
            let tok = underlying.next().expect("Only called when available");
            debug_assert!(tok.raw == raw_close);

            Token::new(close_kind, tok.offset, 1)
        } else {
            let offset = inner.first().map_or(
                open.range().end_offset(),
                |node| node.range().end_offset()
            );
        
            Token::new(close_kind, offset, 0)
        };

        Node::Braced(open, inner, close)
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

        if let Some(tok) = self.stream.peek() {
            if tok.raw == b"{" || tok.raw == b"(" || tok.raw == b"[" {
                return self.parse_braces();
            }
        }

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
                _ => { println!("parse_token - {:?}", tok); unimplemented!() },
            }
        })
    }

    fn parse_number(&self, tok: RawToken) -> Option<Token> {
        assert!(tok.raw.iter().all(|&c| c >= b'0' && c <= b'9'));

        Some(Token::new(Kind::Integral, tok.offset, tok.raw.len()))
    }
}

impl<'a, 'b> RawPeekableStream<'a, 'b> {
    pub fn new(
        raw: &'b mut iter::Peekable<RawStream<'a>>,
        seek: &'static [u8],
        min_indent: isize
    )
        -> RawPeekableStream<'a, 'b>
    {
        RawPeekableStream { raw: raw, seek: seek, min_indent: min_indent }
    }

    pub fn peek(&mut self) -> Option<RawToken<'a>> {
        if let Some(tok) = self.raw.peek().cloned() {
            if tok.line_offset as isize > self.min_indent &&
                tok.raw != self.seek
            {
                return Some(tok);
            }
        }

        None
    }

    pub fn underlying(&mut self) -> &mut UnderlyingStream<'a> {
        self.raw
    }
}

impl<'a, 'b> iter::Iterator for RawPeekableStream<'a, 'b> {
    type Item = RawToken<'a>;

    fn next(&mut self) -> Option<RawToken<'a>> {
        if self.peek().is_some() {
            self.raw.next()
        } else {
            None
        }
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
    fn lex_braces_empty() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"{}"),
            &[
                Node::Braced(
                    Token::new(Kind::BraceOpen, 0, 1),
                    &[],
                    Token::new(Kind::BraceClose, 1, 1),
                )
            ]
        );

        assert_eq!(
            lexit(&global_arena, b"[]"),
            &[
                Node::Braced(
                    Token::new(Kind::BracketOpen, 0, 1),
                    &[],
                    Token::new(Kind::BracketClose, 1, 1),
                )
            ]
        );

        assert_eq!(
            lexit(&global_arena, b"()"),
            &[
                Node::Braced(
                    Token::new(Kind::ParenthesisOpen, 0, 1),
                    &[],
                    Token::new(Kind::ParenthesisClose, 1, 1),
                )
            ]
        );
    }

    #[test]
    fn lex_braces_nested() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"{[[ ]]}"),
            &[
                Node::Braced(
                    Token::new(Kind::BraceOpen, 0, 1),
                    &[
                        Node::Braced(
                            Token::new(Kind::BracketOpen, 1, 1),
                            &[
                                Node::Braced(
                                    Token::new(Kind::BracketOpen, 2, 1),
                                    &[],
                                    Token::new(Kind::BracketClose, 4, 1),
                                )
                            ],
                            Token::new(Kind::BracketClose, 5, 1),
                        )
                    ],
                    Token::new(Kind::BraceClose, 6, 1),
                )
            ]
        );
    }

    #[test]
    fn lex_braces_expression() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"(1 + 2)"),
            &[
                Node::Braced(
                    Token::new(Kind::ParenthesisOpen, 0, 1),
                    &[
                        Node::Run(&[
                            Token::new(Kind::Integral, 1, 1),
                            Token::new(Kind::OperatorPlus, 3, 1),
                            Token::new(Kind::Integral, 5, 1),
                        ])
                    ],
                    Token::new(Kind::ParenthesisClose, 6, 1),
                )
            ]
        );
    }

    #[test]
    fn lex_braces_missing_close() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"(1 + 2"),
            &[
                Node::Braced(
                    Token::new(Kind::ParenthesisOpen, 0, 1),
                    &[
                        Node::Run(&[
                            Token::new(Kind::Integral, 1, 1),
                            Token::new(Kind::OperatorPlus, 3, 1),
                            Token::new(Kind::Integral, 5, 1),
                        ])
                    ],
                    Token::new(Kind::ParenthesisClose, 6, 0),
                )
            ]
        );

        assert_eq!(
            lexit(&global_arena, b"{\n    1 + 2\n3 + 4"),
            &[
                Node::Braced(
                    Token::new(Kind::BraceOpen, 0, 1),
                    &[
                        Node::Run(&[
                            Token::new(Kind::Integral, 6, 1),
                            Token::new(Kind::OperatorPlus, 8, 1),
                            Token::new(Kind::Integral, 10, 1),
                        ])
                    ],
                    Token::new(Kind::BraceClose, 11, 0),
                ),
                Node::Run(&[
                    Token::new(Kind::Integral, 12, 1),
                    Token::new(Kind::OperatorPlus, 14, 1),
                    Token::new(Kind::Integral, 16, 1),
                ])
            ]
        );
    }

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
