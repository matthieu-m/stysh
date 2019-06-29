//! Sea of items.
//!
//! An alternative graph representation geared toward incremental compilation.
//!
//! In the presence of type inference, name resolution and type resolution
//! occur incrementally: the nature of `foo` in `bar.foo` is unknown until the
//! type of `bar` is known, and once it is, then the type of `bar.foo` can
//! in turn help resolve the types of surrounding elements.
//!
//! Implementing this efficiently requires incrementally resolving, and thus
//! mutating, the HIR. Furthermore, for efficiency, it also requires direct
//! access to the HIR nodes, rather than having to scan it fully each time.
//!
//! The purpose of the `sea` module is to support building a graph of nodes
//! allowing:
//! -   direct addressing of individual nodes, through their ID.
//! -   in-place mutation of nodes, as more information trickles through.

mod table;

pub use self::table::*;
