//! Lexing pass, aka parsing.
//!
//! Turns raw text into tokens.
//!
//! This is implemented as a stream, to avoid unnecessary memory allocation.

use std;
use basic::com;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Kind {
    Integral,
    OperatorPlus,
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Token {
    kind_length: u32,
    offset: u32,
}

#[derive(Clone, Debug)]
pub struct Stream<'a> {
    raw: &'a [u8],
    offset: usize,
}

impl Token {
    pub fn new(kind: Kind, offset: usize, length: usize) -> Token {
        debug_assert!(offset <= std::u32::MAX as usize);
        debug_assert!(length < (1usize << 24));
        debug_assert!((kind as u32) < (1u32 << 8));
        Token {
            kind_length: ((kind as u32) << 24) + (length as u32),
            offset: offset as u32
        }
    }

    pub fn kind(&self) -> Kind {
        unsafe { std::mem::transmute((self.kind_length >> 24) as u8) }
    }

    pub fn length(&self) -> usize {
        (self.kind_length & 0x00FFFFFF) as usize
    }

    pub fn offset(&self) -> usize {
        self.offset as usize
    }

    pub fn range(&self) -> com::Range {
        com::Range::new(self.offset(), self.length())
    }
}

impl<'a> Stream<'a> {
    pub fn new(raw: &'a [u8]) -> Stream<'a> {
        Stream { raw: raw, offset: 0 }
    }
}

impl<'a> std::iter::Iterator for Stream<'a> {
    type Item = Token;

    //  TODO(matthieum): use Result to be able to report issues (such as TABs).
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.raw.len() == 0 {
            return None;
        }

        let result = match self.raw[0] {
            b'0' ... b'9' => self.parse_number(),
            b'+' => Some(Token::new(Kind::OperatorPlus, self.offset, 1)),
            _ => unimplemented!(),
        };

        if let Some(token) = result {
            self.skip(token.length());
        }

        result
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

impl<'a> Stream<'a> {
    fn skip(&mut self, length: usize) {
        debug_assert!(length <= self.raw.len());
        self.raw = &self.raw[length..];
        self.offset += length;
    }

    fn skip_whitespace(&mut self) {
        //  Apart from strings and comments, source code only contains ASCII
        //  characters, making things much simpler here.
        while self.raw.len() > 0 {
            if self.raw[0] > 0x20 { break; }

            match self.raw[0] {
                b' ' | b'\n' | b'\r' => self.skip(1),
                byte => panic!("Unexpected byte {}", byte),
            }
        }
    }

    fn parse_number(&self) -> Option<Token> {
        let mut length = 1;
        while length < self.raw.len() {
            match self.raw[length] {
                b'0' ... b'9' => length += 1,
                _ => break,
            };
        }

        Some(Token::new(Kind::Integral, self.offset, length))
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::{Kind, Token, Stream};

    #[test]
    fn lex_integral_single_digit() {
        let result: Vec<Token> = Stream::new(b"1").collect();

        assert_eq!(result, vec![Token::new(Kind::Integral, 0, 1)]);
    }

    #[test]
    fn lex_integral_multiple_digits() {
        let result: Vec<Token> = Stream::new(b"0123").collect();

        assert_eq!(result, vec![Token::new(Kind::Integral, 0, 4)]);
    }

    #[test]
    fn lex_expression_integral_addition() {
        let result: Vec<Token> = Stream::new(b" 12 + 34 ").collect();

        assert_eq!(
            result,
            vec![
                Token::new(Kind::Integral, 1, 2),
                Token::new(Kind::OperatorPlus, 4, 1),
                Token::new(Kind::Integral, 6, 2),
            ]
        );
    }
}
