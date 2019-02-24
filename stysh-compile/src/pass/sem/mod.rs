//! Semantic passes, aka name resolution, type checking, ...

pub mod com;
mod nef;
mod sem;
mod sym;
mod tup;

pub use self::com::{Alteration, Context};
pub use self::nef::NestedEntityFetcher;
pub use self::sym::{scp, SymbolMapper};
pub use self::sem::GraphBuilder;
pub use self::tup::TypeUnifier;
