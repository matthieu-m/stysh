//! Memory handling
//!
//! For efficiency, the compiler uses arena allocation and types that do not
//! implement the `Drop` trait.
//!
//! This module contains an Arena type, as well as collection types built on
//! top of Arena.

mod arena;
mod array;
mod arraymap;

pub use self::arena::{Arena, CloneInto};
pub use self::array::Array;
pub use self::arraymap::ArrayMap;
