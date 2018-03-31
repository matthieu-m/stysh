//! Semantic pass: Type Unifying & Propagating.
//!
//! This specific pass is concerned with determining the type of expressions,
//! variables, and propagate this information to their references.
//!
//! It will insert implicit conversions (such as from variant to enum) as
//! necessary.

mod common;
mod tup;
mod pat;
mod stmt;
mod typ;
mod val;

use super::{Alteration, Context};
pub use self::tup::TypeUnifier;

#[cfg(test)]
use self::common::tests;
