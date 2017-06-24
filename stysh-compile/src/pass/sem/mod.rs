//! Semantic passes, aka name resolution, type checking, ...

mod nmr;
mod sem;

pub use self::nmr::scp;
pub use self::sem::GraphBuilder;
