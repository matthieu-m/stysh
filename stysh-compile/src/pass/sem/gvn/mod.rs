//! Semantic pass: global value numbering.
//!
//! This specific pass is concerned with global value numbering, each value
//! within a given context is assigned a unique identifier.

mod gvn;

pub use self::gvn::GlobalValueNumberer;
