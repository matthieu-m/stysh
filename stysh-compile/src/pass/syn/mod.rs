//! Syntactic passes, aka parsing.
//!
//! This module is in charge of:
//! -   lexing,
//! -   balancing parentheses and quotes,
//! -   finally producing a Syn model (Abstract Syntax Tree).

mod lex;
mod syn;

pub use self::syn::Parser;
