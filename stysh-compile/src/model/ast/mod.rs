//! Syntactic model, aka AST.
//!
//! This is the model describing the syntax of the language, with a 1-to-1
//! mapping to the actual textual representation (modulo whitespace).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

#[cfg(test)]
pub mod builder;
#[cfg(test)]
pub mod interning;

mod common;
mod expr;
mod item;
mod module;
mod pattern;
mod stmt;
mod store;
mod tree;
mod typ;

pub use self::common::*;
pub use self::expr::*;
pub use self::item::*;
pub use self::pattern::*;
pub use self::stmt::*;
pub use self::typ::*;
pub use self::module::Module;
pub use self::store::{Store, MultiStore};
pub use self::tree::{Root, Tree};

pub use model::tt::StringFragment;
