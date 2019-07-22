//! Semantic pass: Nested Entity Fetching.
//!
//! This specific pass is concerned with looking up inner entities.
//!
//! For each entity `a` which is access through an entity `b`, it will lookup
//! the definition of `b` and fetch `a`.

mod com;
mod fld;
mod nef;
mod typ;

use super::com::*;
use super::{RegRef, Scope};

pub use self::nef::NestedEntityFetcher;
