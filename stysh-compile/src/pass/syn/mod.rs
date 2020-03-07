//! Syntactic passes, aka parsing.
//!
//! This module is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

mod body;
mod com;
mod expr;
mod ext;
mod int;
mod fun;
mod syn;
mod typ;

pub use self::syn::Parser;
