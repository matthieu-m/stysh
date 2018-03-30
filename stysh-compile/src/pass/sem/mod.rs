//! Semantic passes, aka name resolution, type checking, ...

mod common;
mod gvn;
mod nef;
mod sem;
mod sym;
mod tup;

pub use self::common::{Context, Resolution};
pub use self::gvn::GlobalValueNumberer;
pub use self::nef::NestedEntityFetcher;
pub use self::sym::{scp, SymbolMapper};
pub use self::sem::GraphBuilder;
pub use self::tup::TypeUnifier;
