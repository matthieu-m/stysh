//! Semantic pass: name resolution.
//!
//! This specific pass is concerned with name resolution:
//! -   it identifies which local variable is referred to,
//! -   it identifies the type that is referred to,
//!-    it identifies the set of function overloads that is referred to.

mod nmr;
pub mod scp;

pub use self::nmr::NameResolver;
