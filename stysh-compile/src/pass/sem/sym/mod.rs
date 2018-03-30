//! Semantic pass: Symbol Mapping.
//!
//! This specific pass is concerned with mapping symbols.
//!
//! For each name which is not a definition, it finds the corresponding
//! definition (if in the file) or the import it refers to.
//!
//! Note:   it also doubles up as translated from AST to HIR.

mod sym;
pub mod scp;

use super::Context;
pub use self::sym::SymbolMapper;
