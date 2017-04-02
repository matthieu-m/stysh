//! Semantic passes, aka name resolution, type checking, ...

mod nmr;
mod sem;

pub use self::sem::GraphBuilder;
