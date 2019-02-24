//! The interpreter.
//!
//! This module is in charge of evaluating SIR expressions, transforming them
//! into values.
//!
//! Its primary purpose is to perform compile-time function evaluation on Stysh
//! code:
//! -   to enable rich non-type generic parameters computation,
//! -   to enable rich compile-time checks and selections,
//! -   to optimize out computations that can be performed at compile-time.
//!
//! Additionally, it can be used as a full interpreter if supplemented by I/O,
//! however most I/O will have to be provided externally: only reading from a
//! restricted subset of the filesystem will be allowed within the compiler
//! proper to enforce reproducible builds. Further I/O and FFI will have to be
//! provided by a plugin system, later.

mod int;
mod reg;

pub use self::int::{Interpreter, Value};
pub use self::reg::{Registry, SimpleRegistry};
