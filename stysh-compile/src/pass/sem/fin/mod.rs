//! Semantic pass: Graph Finalizing.
//!
//! Transform the first HIR draft into its final casting by applying the
//! changes described in the Context.

mod com;
mod fin;
mod pat;
mod stmt;
mod typ;
mod val;

use super::{com::flat, Alteration, Context};
pub use self::fin::GraphFinalizer;
