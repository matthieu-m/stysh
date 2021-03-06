//! Semantic passes, aka name resolution, type checking, ...

pub mod com;
mod nef;
mod sem;
mod sym;
mod tup;

pub use self::com::{Context, Reg, RegRef, Relation};
pub use self::nef::NestedEntityFetcher;
pub use self::sym::{scp, Scope, SymbolMapper, TypeMapper};
pub use self::sem::GraphBuilder;
pub use self::tup::TypeUnifier;
