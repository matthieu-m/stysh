//! Semantic passes, aka name resolution, type checking, ...

mod gvn;
mod nmr;
mod sem;

pub use self::gvn::GlobalValueNumberer;
pub use self::nmr::scp;
pub use self::sem::GraphBuilder;
