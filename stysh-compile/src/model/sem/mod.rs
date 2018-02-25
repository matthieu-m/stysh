//! Semantic model, also confusingly named AST.
//!
//! This is the model describing the semantics of the language. It abstracts
//! over syntactic sugar ("0xa", "1_0" and "0b1010" are all an integral of value
//! "10") and resolves the names and types to their declaration.
//!
//! When produced by the sem pass, it is also expected that the model be
//! type-checked, that is that if an variable of type T is passed to a function
//! expecting an argument of type U, it has been checked that T be compatible to
//! U *and* that transformation has been made explicit.
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

#[cfg(test)]
pub mod builder;

mod common;
mod item;
mod pattern;
mod stmt;
mod value;

pub use self::common::*;
pub use self::item::*;
pub use self::pattern::*;
pub use self::stmt::*;
pub use self::value::*;
