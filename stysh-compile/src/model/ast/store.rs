//! A generic concept of Stores, to abstract over Tree and Directory.

use basic::com::{Id, Range};

/// A Store trait, to abstract over the actual storage of individual elements.
pub trait Store<T> {
    /// Returns the number of items.
    fn len(&self) -> usize;

    /// Returns a copy of the item.
    fn get(&self, id: Id<T>) -> T;

    /// Returns the range of the item.
    fn get_range(&self, id: Id<T>) -> Range;

    /// Pushes an item.
    fn push(&mut self, item: T, range: Range) -> Id<T>;
}

/// A MultiStore trait, to abstract over the actual storage of slices.
pub trait MultiStore<T> {
    /// Returns the slice of items.
    fn get_slice(&self, id: Id<[T]>) -> &[T];

    /// Pushes a slice of element.
    fn push_slice(&mut self, items: &[T]) -> Id<[T]>;
}
