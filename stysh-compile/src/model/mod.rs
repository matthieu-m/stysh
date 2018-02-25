//! The model used to represent the code in the various stages of the pipeline.

pub mod tt;
pub mod syn;
pub mod sem;
pub mod sir;

#[cfg(test)]
pub mod sem_builder;
