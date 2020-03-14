//! Semantic model, also confusingly named AST.
//!
//! For differentiations purposes, it will be called HIR for High-level
//! Intermediate Representation.
//!
//! This is the model describing the semantics of the language. It abstracts
//! over syntactic sugar ("0xa", "1_0" and "0b1010" are all an integral of value
//! "10") and resolves the names and types to their declaration.
//!
//! When produced by the sem pass, it is also expected that the model be
//! type-checked, that is that if an variable of type T is passed to a function
//! expecting an argument of type U, it has been checked that T be compatible to
//! U *and* that transformation has been made explicit.

#[cfg(test)]
pub mod builder;
#[cfg(test)]
pub mod interning;

mod common;
mod expression;
mod item;
mod module;
mod pattern;
mod registry;
mod repository;
mod statement;
mod tree;

pub use self::common::*;
pub use self::expression::*;
pub use self::item::*;
pub use self::module::*;
pub use self::pattern::*;
pub use self::registry::*;
pub use self::repository::*;
pub use self::statement::*;
pub use self::tree::*;

#[cfg(test)]
pub use self::registry::mocks::MockRegistry;
