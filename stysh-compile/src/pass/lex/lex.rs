//! Lexing pass, aka parsing.
//!
//! Turns raw text into tokens.
//!
//! This is implemented as a stream, to avoid unnecessary memory allocation.

use std::iter;
use basic::mem;

use model::tt::*;
use super::str;
use super::raw::{AsciiSet, RawStream, RawToken};

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
            RawPeekableStream::new(&mut raw, 0, 0),
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
    close: u8,
    min_indent: usize,
}

impl<'a, 'b, 'g, 'local> iter::Iterator for LexerImpl<'a, 'b, 'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        use super::raw::RawKind::*;

        if let Some(node) = self.stream.peek().map(|tok| {
            match tok.kind {
                Attribute => self.parse_attribute(),
                Bytes => self.parse_bytes(),
                BytesMultiLines => self.parse_bytes_multi_lines(),
                Comment => self.parse_comment(),
                Generic => self.parse_generic(),
                String => self.parse_string(),
                StringMultiLines => self.parse_string_multi_lines(),
                BraceOpen | BraceClose => panic!("Unreachable: {:?}", tok),
            }
        }) {
            return Some(node);
        }

        self.stream.underlying().peek().cloned().and_then(|tok| {
            match tok.kind {
                BraceOpen => Some(self.parse_braces()),
                BraceClose if tok.raw[0] != self.stream.close =>
                    Some(self.parse_unexpected_brace()),
                _ => None,
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
        let underlying = self.stream.underlying();

        let raw_open = underlying.next().expect("Only called when available");

        let (open_kind, close_kind, raw_close) = match raw_open.raw {
            b"{" => (Kind::BraceOpen, Kind::BraceClose, b"}"),
            b"[" => (Kind::BracketOpen, Kind::BracketClose, b"]"),
            b"(" => (Kind::ParenthesisOpen, Kind::ParenthesisClose, b")"),
            _ => panic!("Unexpected raw: '{:?}'", raw_open),
        };

        let open = Token::new(open_kind, raw_open.offset, raw_open.raw.len());

        let inner = {
            let inner_stream = RawPeekableStream::new(
                underlying,
                raw_close[0],
                raw_open.line_indent + 1
            );

            LexerImpl::new(inner_stream, self.global_arena, self.local_arena)
                .parse_all()
        };

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

    fn parse_unexpected_brace(&mut self) -> Node<'g> {
        let tok =
            self.stream.underlying().next()
                .expect("Only called if peek succeeds");

        let kind = match tok.raw[0] {
            b'}' => Kind::BraceClose,
            b']' => Kind::BracketClose,
            b')' => Kind::ParenthesisClose,
            _ => panic!("Unreachable: {:?}", tok),
        };

        Node::UnexpectedBrace(Token::new(kind, tok.offset, 1))
    }

    fn parse_token(&mut self) -> Option<Token> {
        //  +-*/<>=!,;
        const SIMPLE_SIGNS: AsciiSet = AsciiSet(0x7800bc0200000000, 0x0);

        self.stream.next().and_then(|tok| {
            match tok.raw[0] {
                b'0'...b'9' => self.parse_number(tok),
                b'A'...b'Z' => self.parse_name(tok),
                b'a'...b'z' => self.parse_name(tok),
                b':' => self.parse_colon(tok),
                b if SIMPLE_SIGNS.contains(b) => self.parse_sign(tok),
                _ => panic!("parse_token not implemented for {}", tok),
            }
        })
    }

    fn parse_colon(&self, tok: RawToken) -> Option<Token> {
        let kind = match tok.raw {
            b":" => Kind::SignColon,
            b":=" => Kind::SignBind,
            b":and" => Kind::KeywordAnd,
            b":else" => Kind::KeywordElse,
            b":enum" => Kind::KeywordEnum,
            b":fun" => Kind::KeywordFun,
            b":if" => Kind::KeywordIf,
            b":not" => Kind::KeywordNot,
            b":or" => Kind::KeywordOr,
            b":var" => Kind::KeywordVar,
            b":xor" => Kind::KeywordXor,
            _ => panic!("parse_colon not implemented for {}", tok),
        };
        Some(Token::new(kind, tok.offset, tok.raw.len()))
    }

    fn parse_name(&self, tok: RawToken) -> Option<Token> {
        // TODO(matthieum): validate identifiers.

        let kind = match tok.raw[0] {
            b'A'...b'Z' => Kind::NameType,
            b'a'...b'z' => if tok.raw == b"false" {
                Kind::LitBoolFalse
            } else if tok.raw == b"true" {
                Kind::LitBoolTrue
            } else {
                Kind::NameValue
            },
            _ => panic!("parse_name not implemented {}", tok),
        };

        Some(Token::new(kind, tok.offset, tok.raw.len()))
    }

    fn parse_number(&self, tok: RawToken) -> Option<Token> {
        assert!(tok.raw.iter().all(|&c| c >= b'0' && c <= b'9'));

        Some(Token::new(Kind::LitIntegral, tok.offset, tok.raw.len()))
    }

    fn parse_sign(&self, tok: RawToken) -> Option<Token> {
        //  If starting with a colon, already dealt with.
        let kind = match tok.raw {
            b"+" => Kind::SignPlus,
            b"-" => Kind::SignDash,
            b"*" => Kind::SignStar,
            b"//" => Kind::SignDoubleSlash,
            b"<" => Kind::SignLeft,
            b"<=" => Kind::SignLeftEqual,
            b">" => Kind::SignRight,
            b">=" => Kind::SignRightEqual,
            b"==" => Kind::SignDoubleEqual,
            b"!=" => Kind::SignBangEqual,
            b"->" => Kind::SignArrowSingle,
            b"," => Kind::SignComma,
            b";" => Kind::SignSemiColon,
            _ => panic!("parse_sign not implemented for {}", tok),
        };

        Some(Token::new(kind, tok.offset, tok.raw.len()))
    }
}

impl<'a, 'b> RawPeekableStream<'a, 'b> {
    pub fn new(
        raw: &'b mut iter::Peekable<RawStream<'a>>,
        close: u8,
        min_indent: usize
    )
        -> RawPeekableStream<'a, 'b>
    {
        RawPeekableStream { raw: raw, close: close, min_indent: min_indent }
    }

    pub fn peek(&mut self) -> Option<RawToken<'a>> {
        use super::raw::RawKind;

        if let Some(tok) = self.raw.peek().cloned() {
            if tok.line_offset >= self.min_indent &&
                tok.kind != RawKind::BraceClose &&
                tok.kind != RawKind::BraceOpen
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
                            Token::new(Kind::LitIntegral, 1, 1),
                            Token::new(Kind::SignPlus, 3, 1),
                            Token::new(Kind::LitIntegral, 5, 1),
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
                            Token::new(Kind::LitIntegral, 1, 1),
                            Token::new(Kind::SignPlus, 3, 1),
                            Token::new(Kind::LitIntegral, 5, 1),
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
                            Token::new(Kind::LitIntegral, 6, 1),
                            Token::new(Kind::SignPlus, 8, 1),
                            Token::new(Kind::LitIntegral, 10, 1),
                        ])
                    ],
                    Token::new(Kind::BraceClose, 11, 0),
                ),
                Node::Run(&[
                    Token::new(Kind::LitIntegral, 12, 1),
                    Token::new(Kind::SignPlus, 14, 1),
                    Token::new(Kind::LitIntegral, 16, 1),
                ])
            ]
        );
    }

    #[test]
    fn lex_braces_tuples() {
        let global_arena = mem::Arena::new();

        fn int(offset: usize) -> Token {
            Token::new(Kind::NameType, offset, 3)
        }

        assert_eq!(
            lexit(&global_arena, b"((Int, Int), Int, )"),
            &[
                Node::Braced(
                    paren_open(0),
                    &[
                        Node::Braced(
                            paren_open(1),
                            &[
                                Node::Run(&[int(2), comma(5), int(7)])
                            ],
                            paren_close(10),
                        ),
                        Node::Run(&[comma(11), int(13), comma(16)]),
                    ],
                    paren_close(18),
                ),
            ]
        );
    }

    #[test]
    fn lex_function_simple() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b":fun add(x: Int, y: Int) -> Int { x + y }"),
            &[
                Node::Run(&[
                    Token::new(Kind::KeywordFun, 0, 4),
                    Token::new(Kind::NameValue, 5, 3),
                ]),
                Node::Braced(
                    Token::new(Kind::ParenthesisOpen, 8, 1),
                    &[
                        Node::Run(&[
                            Token::new(Kind::NameValue, 9, 1),
                            Token::new(Kind::SignColon, 10, 1),
                            Token::new(Kind::NameType, 12, 3),
                            Token::new(Kind::SignComma, 15, 1),
                            Token::new(Kind::NameValue, 17, 1),
                            Token::new(Kind::SignColon, 18, 1),
                            Token::new(Kind::NameType, 20, 3),
                        ]),
                    ],
                    Token::new(Kind::ParenthesisClose, 23, 1),
                ),
                Node::Run(&[
                    Token::new(Kind::SignArrowSingle, 25, 2),
                    Token::new(Kind::NameType, 28, 3),
                ]),
                Node::Braced(
                    Token::new(Kind::BraceOpen, 32, 1),
                    &[
                        Node::Run(&[
                            Token::new(Kind::NameValue, 34, 1),
                            Token::new(Kind::SignPlus, 36, 1),
                            Token::new(Kind::NameValue, 38, 1),
                        ]),
                    ],
                    Token::new(Kind::BraceClose, 40, 1),
                ),
            ]
        );
    }

    #[test]
    fn lex_boolean() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"false true"),
            &[
                Node::Run(&[
                    Token::new(Kind::LitBoolFalse, 0, 5),
                    Token::new(Kind::LitBoolTrue, 6, 4),
                ])
            ]
        );
    }

    #[test]
    fn lex_boolean_negative() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"False fals alse True tru rue"),
            &[
                Node::Run(&[
                    Token::new(Kind::NameType, 0, 5),
                    Token::new(Kind::NameValue, 6, 4),
                    Token::new(Kind::NameValue, 11, 4),
                    Token::new(Kind::NameType, 16, 4),
                    Token::new(Kind::NameValue, 21, 3),
                    Token::new(Kind::NameValue, 25, 3),
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
                    Token::new(Kind::LitIntegral, 0, 1),
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
                    Token::new(Kind::LitIntegral, 0, 4),
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
                    Token::new(Kind::LitIntegral, 1, 2),
                    Token::new(Kind::SignPlus, 4, 1),
                    Token::new(Kind::LitIntegral, 6, 2),
                ])
            ]
        );
    }

    #[test]
    fn lex_signs_farandole() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            lexit(&global_arena, b"-> != := : , - == // < <= + > >= ; *"),
            &[
                Node::Run(&[
                    Token::new(Kind::SignArrowSingle, 0, 2),
                    Token::new(Kind::SignBangEqual, 3, 2),
                    Token::new(Kind::SignBind, 6, 2),
                    Token::new(Kind::SignColon, 9, 1),
                    Token::new(Kind::SignComma, 11, 1),
                    Token::new(Kind::SignDash, 13, 1),
                    Token::new(Kind::SignDoubleEqual, 15, 2),
                    Token::new(Kind::SignDoubleSlash, 18, 2),
                    Token::new(Kind::SignLeft, 21, 1),
                    Token::new(Kind::SignLeftEqual, 23, 2),
                    Token::new(Kind::SignPlus, 26, 1),
                    Token::new(Kind::SignRight, 28, 1),
                    Token::new(Kind::SignRightEqual, 30, 2),
                    Token::new(Kind::SignSemiColon, 33, 1),
                    Token::new(Kind::SignStar, 35, 1),
                ]),
            ]
        )
    }

    fn lexit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> List<'g> {
        let mut local_arena = mem::Arena::new();

        let result = Lexer::new(&global_arena, &local_arena).parse(raw);
        local_arena.recycle();

        result
    }

    fn comma(offset: usize) -> Token {
        Token::new(Kind::SignComma, offset, 1)
    }

    fn paren_open(offset: usize) -> Token {
        Token::new(Kind::ParenthesisOpen, offset, 1)
    }

    fn paren_close(offset: usize) -> Token {
        Token::new(Kind::ParenthesisClose, offset, 1)
    }
}
