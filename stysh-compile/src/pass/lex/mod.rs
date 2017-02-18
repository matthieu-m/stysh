//! Lexing pass, aka parsing.
//!
//! This pass is in charge of:
//! -   lexing proper,
//! -   balancing parentheses and quotes.
//!
//! It produces the token tree.

mod lex;
mod raw;

pub use self::lex::Lexer;
