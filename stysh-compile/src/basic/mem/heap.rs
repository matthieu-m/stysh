//! Simple wrappers around `Rc`, `RefCell` and `Vec`.

use std::{hash, iter, mem};
use std::cell::RefCell;
use std::rc::Rc;

//
//  Public Types
//

/// Ptr
///
/// A simple `Rc<RefCell<T>>`, with convenient methods.
#[derive(Default, PartialEq, PartialOrd, Eq, Ord)]
pub struct Ptr<T>(Rc<RefCell<T>>);

/// DynArray
///
/// A simple `Rc<RefCell<Vec<T>>>`, with convenient methods.
#[derive(PartialEq, PartialOrd, Eq, Ord)]
pub struct DynArray<T>(Rc<RefCell<Vec<T>>>);

/// DynArrayIterator
///
/// An iterator over a `DynArray<T>`.
#[derive(Clone, Debug)]
pub struct DynArrayIterator<T>(DynArray<T>, usize);

//
//  Public Methods
//

impl<T> Ptr<T> {
    /// Creates a new instance.
    pub fn new(t: T) -> Ptr<T> { Ptr(Rc::new(RefCell::new(t))) }

    /// Replaces the previous value with a new one.
    ///
    /// Returns the previous one.
    pub fn replace(&self, t: T) -> T {
        mem::replace(&mut self.0.borrow_mut(), t)
    }
}

impl<T: Clone> Ptr<T> {
    /// Returns a clone of the value.
    pub fn get(&self) -> T { self.0.borrow().clone() }
}

impl<T: Default> Ptr<T> {
    /// Replaces the previous value with its default value.
    ///
    /// Returns the previous one.
    pub fn replace_with_default(&self) -> T {
        self.replace(Default::default())
    }

    /// Updates the value in place.
    ///
    /// In case of panic of the function argument, leaves a default value.
    pub fn update<F: FnOnce(T) -> T>(&self, fun: F) {
        let value = self.replace_with_default();
        self.replace(fun(value));
    }
}

impl<T> DynArray<T> {
    /// Creates a new instance.
    pub fn new(v: Vec<T>) -> DynArray<T> { DynArray(Rc::new(RefCell::new(v))) }

    /// Creates a new instance, with the specified capacity.
    pub fn with_capacity(capacity: usize) -> DynArray<T> {
        DynArray(Rc::new(RefCell::new(Vec::with_capacity(capacity))))
    }

    /// Returns whether the array is empty.
    pub fn is_empty(&self) -> bool { self.0.borrow().is_empty() }

    /// Returns the length of the array.
    pub fn len(&self) -> usize { self.0.borrow().len() }

    /// Clears the array.
    pub fn clear(&self) { self.0.borrow_mut().clear() }

    /// Finds first index matching predicate, if any.
    pub fn find<F: FnMut(&T) -> bool>(&self, mut fun: F) -> Option<usize> {
        let guard = self.0.borrow();
        guard.iter()
            .enumerate()
            .filter_map(|(i, e)| if fun(e) { Some(i) } else { None })
            .next()
    }

    /// Applies transformation, in place.
    pub fn for_each<F: FnMut(usize, &mut T)>(&self, mut fun: F) {
        let mut guard = self.0.borrow_mut();
        guard.iter_mut()
            .enumerate()
            .for_each(|(i, e)| fun(i, e))
    }

    /// Pushes a new element.
    pub fn push(&self, e: T) { self.0.borrow_mut().push(e) }

    /// Replaces the previous array with a new one.
    ///
    /// Returns the previous one.
    pub fn replace_array(&self, v: Vec<T>) -> Vec<T> {
        mem::replace(&mut self.0.borrow_mut(), v)
    }

    /// Replaces the element at the given index with a new one.
    ///
    /// Returns the previous one.
    ///
    /// Panics if there is no such element.
    pub fn replace(&self, index: usize, e: T) -> T {
        mem::replace(self.0.borrow_mut().get_mut(index).expect("Valid Index"), e)
    }
}

impl<T: Clone> DynArray<T> {
    /// Returns a clone of the element at a given index.
    ///
    /// Panics if no such element exists.
    pub fn at(&self, index: usize) -> T {
        self.get(index).expect("Invalid Index")
    }

    /// Extends array by cloning content of other array.
    pub fn extend(&self, other: DynArray<T>) {
        self.0.borrow_mut().extend_from_slice(&*other.0.borrow());
    }

    /// Returns a clone of the array.
    pub fn get_array(&self) -> Vec<T> { self.0.borrow().clone() }

    /// Returns a clone of the element at the given index, if any.
    pub fn get(&self, index: usize) -> Option<T> {
        self.0.borrow().get(index).cloned()
    }

    /// Returns an iterator.
    pub fn iter(&self) -> DynArrayIterator<T> { self.into_iter() }

    /// Returns a clone of the first element, if any.
    pub fn first(&self) -> Option<T> { self.get(0) }

    /// Returns a clone of the last element, if any.
    pub fn last(&self) -> Option<T> {
        let length = self.len();
        if length == 0 {
            None
        } else {
            self.get(length - 1)
        }
    }
}

impl<T: Default> DynArray<T> {
    /// Replaces the element at the given index with its default value.
    ///
    /// Returns the previous one.
    ///
    /// Panics if there is no such element.
    pub fn replace_with_default(&self, index: usize) -> T {
        self.replace(index, Default::default())
    }

    /// Updates the element at the given index in place.
    ///
    /// In case of panic of the function argument, leaves a default value.
    pub fn update<F: FnOnce(T) -> T>(&self, index: usize, fun: F) {
        let value = self.replace_with_default(index);
        self.replace(index, fun(value));
    }

    /// Updates all elements in place.
    ///
    /// In case of panic of the function argument, leaves a default value.
    pub fn update_all<F: FnMut(T) -> T>(&self, mut fun: F) {
        for i in 0..self.len() {
            let value = self.replace_with_default(i);
            self.replace(i, fun(value));
        }
    }
}

//
//  Trait Implementations
//

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self { Ptr(self.0.clone()) }
}

impl<T> Clone for DynArray<T> {
    fn clone(&self) -> Self { DynArray(self.0.clone()) }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Ptr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        f.debug_tuple("Ptr").field(&*self.0.borrow()).finish()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for DynArray<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.is_empty() {
            write!(f, "DynArray([])")
        } else {
            f.debug_tuple("DynArray").field(&*self.0.borrow()).finish()
        }
    }
}

impl<T> Default for DynArray<T> {
    fn default() -> Self { DynArray::new(vec!()) }
}

impl<T: hash::Hash> hash::Hash for Ptr<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

impl<T: hash::Hash> hash::Hash for DynArray<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

impl<T: Clone> iter::IntoIterator for DynArray<T> {
    type Item = T;
    type IntoIter = DynArrayIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        DynArrayIterator(self, 0)
    }
}

impl<'a, T: Clone> iter::IntoIterator for &'a DynArray<T> {
    type Item = T;
    type IntoIter = DynArrayIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        DynArrayIterator(self.clone(), 0)
    }
}

impl<T: Clone> iter::Iterator for DynArrayIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.1;
        self.1 += 1;
        self.0.get(index)
    }
}

impl<T: Clone> iter::FusedIterator for DynArrayIterator<T> {}
