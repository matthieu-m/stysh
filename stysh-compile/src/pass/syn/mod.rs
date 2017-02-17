//! Syntactic passes, aka parsing.
//!
//! This module is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

mod syn;

pub use self::syn::Parser;
