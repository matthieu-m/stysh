//! ArrayMap
//!
//! An ordered map interface laid out in an Arena based Array.

use std::cmp;

use basic::mem::{Arena, Array};

/// The ArrayMap structure
///
/// An ordered map interface laid out in an Array.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ArrayMap<'a, K: 'a + cmp::Ord, V: 'a>(Array<'a, K>, Array<'a, V>);

//
//  Public interface
//
impl<'a, K: 'a + cmp::Ord, V: 'a> ArrayMap<'a, K, V> {
    /// Creates a new, empty, instance.
    pub fn new(arena: &'a Arena) -> ArrayMap<'a, K, V> {
        ArrayMap(Array::new(arena), Array::new(arena))
    }

    /// Creates a new, empty, instance with at least the specified capacity.
    pub fn with_capacity(cap: usize, arena: &'a Arena) -> ArrayMap<'a, K, V> {
        ArrayMap(
            Array::with_capacity(cap, arena),
            Array::with_capacity(cap, arena)
        )
    }

    /// Returns the arena in use by the array map.
    pub fn arena(&self) -> &'a Arena { self.0.arena() }

    /// Returns the current capacity of the array map.
    pub fn capacity(&self) -> usize { self.0.capacity() }

    /// Clears the array map, removing all elements.
    ///
    /// Note: does NOT call `Drop` on any element.
    pub fn clear(&mut self) {
        self.0.clear();
        self.1.clear();
    }

    /// Gets the value associated to a specific key.
    pub fn get(&self, key: &K) -> Option<&V> {
        match self.0.binary_search(&key) {
            Ok(index) => Some(&self.1[index]),
            Err(_) => None,
        }
    }

    /// Gets the value associated to a specific key.
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.0.binary_search(&key) {
            Ok(index) => Some(&mut self.1[index]),
            Err(_) => None,
        }
    }

    /// Inserts a key and value in the array-map.
    ///
    /// Note: in case the key is already present, no insertion occurs, and the
    ///       arguments are returned instead.
    pub fn insert(&mut self, key: K, value: V) -> Option<(K, V)> {
        match self.0.binary_search(&key) {
            Ok(_) => Some((key, value)),
            Err(index) => {
                self.0.insert(index, key);
                self.1.insert(index, value);
                None
            },
        }
    }

    /// Reserves the necessary spaces for `n` more items.
    ///
    /// Note: if the capacity is already sufficient, has no effect, otherwise
    /// increases the capacity to at least be enough for `n` more items. This
    /// reallocates the underlying storage.
    pub fn reserve(&mut self, n: usize) {
        self.0.reserve(n);
        self.1.reserve(n);
    }
}
