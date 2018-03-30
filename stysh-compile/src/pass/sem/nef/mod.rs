//! Semantic pass: Nested Entity Fetching.
//!
//! This specific pass is concerned with looking up inner entities.
//!
//! For each entity `a` which is access through an entity `b`, it will lookup
//! the definition of `b` and fetch `a`.

mod common;
mod nef;
mod pat;
mod stmt;
mod typ;
mod val;

use super::{Context, Resolution};
pub use self::nef::NestedEntityFetcher;

#[cfg(test)]
use self::common::tests;
