//! Semantic Pass: Common Elements

use std::{convert, marker};

use model::hir::{Gin, Gvn};

/// GlobalNumber.
///
/// An index specific to the tables.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct GlobalNumber(u32);

/// Table.
///
/// A simplified, and typed, sequential access container.
#[derive(Clone, Debug)]
pub struct Table<G: convert::Into<GlobalNumber>, T> {
    data: Vec<T>,
    _marker: marker::PhantomData<*const G>,
}

//
//  Public interface of Table
//

impl<G: convert::Into<GlobalNumber>, T> Table<G, T> {
    pub fn len(&self) -> usize { self.data.len() }

    pub fn clear(&mut self) { self.data.clear(); }

    pub fn push(&mut self, t: T) { self.data.push(t); }

    pub fn set(&mut self, index: G, e: T) {
        let index = index.into().0 as usize;
        self.data[index] = e;
    }
}

impl<G: convert::Into<GlobalNumber>, T: Clone> Table<G, T> {
    pub fn get(&self, index: G) -> T {
        let index = index.into().0 as usize;
        self.data[index].clone()
    }
}

//
//  Implementation of traits for GlobalNumber
//

impl convert::From<Gin> for GlobalNumber {
    fn from(gin: Gin) -> GlobalNumber { GlobalNumber(gin.0) }
}

impl convert::From<Gvn> for GlobalNumber {
    fn from(gvn: Gvn) -> GlobalNumber { GlobalNumber(gvn.0) }
}

//
//  Implementation of traits for Table
//

impl<G: convert::Into<GlobalNumber>, T: Default> Default for Table<G, T> {
    fn default() -> Self {
        Table {
            data: vec!(Default::default()),
            _marker: marker::PhantomData,
        }
    }
}
