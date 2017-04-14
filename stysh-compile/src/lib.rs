//! The Stysh compiler
//!
//! The guts of the Stysh compiler, exposed as a library to favor reuse.

#![deny(missing_docs)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

pub mod basic;
pub mod model;
pub mod pass;
