//! Memory handling
//!
//! For efficiency, the compiler uses arena allocation and types that do not
//! implement the `Drop` trait.
//!
//! This module contains an Arena type, as well as collection types built on
//! top of Arena.

mod arena;
mod array;
mod interner;
mod jaggedarray;
mod jaggedhashmap;

pub use self::arena::{Arena, CloneInto};
pub use self::array::Array;
pub use self::interner::{Interner, InternerSnapshot, InternId};
pub use self::jaggedarray::{JaggedArray, JaggedArrayIterator, JaggedArraySnapshot};
pub use self::jaggedhashmap::{JaggedHashMap, JaggedHashMapSnapshot};
