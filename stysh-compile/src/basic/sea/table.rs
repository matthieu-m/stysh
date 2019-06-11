//! Tables.
//!
//! The `Table` and `MultiTable` are collections geared toward tracking
//! one-to-one and one-to-many relationships with O(1) memory allocations.
//!
//! In exchange for the efficiency, they only offer a limited interface.

use std::{fmt, iter, marker};
use std::ops::Range;
use std::slice::from_raw_parts_mut;

//
//  Public Types
//

/// TableIndex.
///
/// The keys in `Table` and `MultiTable` must implement the `TableIndex` trait.
pub trait TableIndex {
    /// Creates an instance of the type from an index.
    fn from_index(index: usize) -> Self;

    /// Returns the index in a `Table` or `MultiTable` of this key.
    fn index(&self) -> usize;
}

/// Table.
///
/// An append-only container mapping a key to a single value.
#[derive(Clone, Debug)]
pub struct Table<K: TableIndex, V> {
    //  Pool of all values, in order of insertion.
    values: Vec<V>,
    //  Marker for ownership reasons.
    _marker: marker::PhantomData<*const K>,
}

/// MultiTable.
///
/// An append-only container mapping a key to multiple values.
#[derive(Clone, Debug)]
pub struct MultiTable<K: TableIndex, V> {
    //  Maps a key to the range of indices of its values.
    index: Vec<Range<u32>>,
    //  Pool of all values, in order of insertion.
    values: Vec<V>,
    //  Marker for ownership reasons.
    _marker: marker::PhantomData<*const K>,
}

/// TableIter
///
/// An iterator over a Table.
#[derive(Clone, Debug)]
pub struct TableIter<'a, K: TableIndex, V> {
    table: &'a Table<K, V>,
    index: usize,
}

/// TableIterMut
///
/// A mutable iterator over a Table.
#[derive(Debug)]
pub struct TableIterMut<'a, K: TableIndex, V> {
    table: &'a mut Table<K, V>,
    index: usize,
}

/// MultiTableIter
///
/// An iterator over a MultiTable.
#[derive(Clone, Debug)]
pub struct MultiTableIter<'a, K: TableIndex, V> {
    table: &'a MultiTable<K, V>,
    index: usize,
}

/// MultiTableIterMut
///
/// A mutable iterator over a MultiTable.
#[derive(Debug)]
pub struct MultiTableIterMut<'a, K: TableIndex, V> {
    table: &'a mut MultiTable<K, V>,
    index: usize,
}

//
//  Public Methods
//

impl<K: TableIndex, V> Table<K, V> {
    /// Creates an instance.
    pub fn new() -> Self { Default::default() }

    /// Returns true if and only if the map contains no element.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn is_empty(&self) -> bool { self.values.is_empty() }

    /// Returns the number of values in the map.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn len(&self) -> usize { self.values.len() }

    /// Clears the map.
    ///
    /// # Complexity
    ///
    /// O(self.len())
    pub fn clear(&mut self) { self.values.clear(); }

    /// Returns true if and only if the map contains the key.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn contains(&self, key: &K) -> bool { key.index() < self.len() }

    /// Returns the values associated to a key.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panics if the key does not exist.
    pub fn at<'a>(&'a self, key: &K) -> &'a V {
        self.get(key).expect("Invalid key")
    }

    /// Returns the value associated to a key.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panics if the key does not exist.
    pub fn at_mut<'a>(&'a mut self, key: &K) -> &'a mut V {
        self.get_mut(key).expect("Invalid key")
    }

    /// Returns the values associated to a key.
    ///
    /// If there is no such key, returns `None`.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn get<'a>(&'a self, key: &K) -> Option<&'a V> {
        self.values.get(key.index())
    }

    /// Returns the value associated to a key.
    ///
    /// If there is no such key, return `None`.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn get_mut<'a>(&'a mut self, key: &K) -> Option<&'a mut V> {
        self.values.get_mut(key.index())
    }

    /// Returns the values associated to the two keys.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panic if the keys map to the same index.
    pub fn get_duo_mut<'a>(&'a mut self, one: &K, two: &K)
        -> (Option<&'a mut V>, Option<&'a mut V>)
    {
        let (first, second) = (one.index(), two.index());

        assert!(first != second);

        let ptr = self.values.as_mut_ptr();
        let len = self.values.len();

        //  Safety checks:
        //  -   non-overlapping indices.
        //  -   lifetime constrained.
        unsafe { (
            element_of::<'a>(ptr, len, first),
            element_of::<'a>(ptr, len, second),
        ) }
    }

    /// Returns the values associated to the three keys.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panics if any two keys map to the same index.
    pub fn get_trio_mut<'a>(&'a mut self, one: &K, two: &K, three: &K)
        -> (Option<&'a mut V>, Option<&'a mut V>, Option<&'a mut V>)
    {
        let (first, second, third) = (one.index(), two.index(), three.index());

        assert!(first != second);
        assert!(first != third);
        assert!(second != third);

        let ptr = self.values.as_mut_ptr();
        let len = self.values.len();

        //  Safety checks:
        //  -   non-overlapping indices.
        //  -   lifetime constrained.
        unsafe { (
            element_of::<'a>(ptr, len, first),
            element_of::<'a>(ptr, len, second),
            element_of::<'a>(ptr, len, third),
        ) }
    }

    /// Returns an Iterator over the values of the Table.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn iter<'a>(&'a self) -> TableIter<'a, K, V> {
        iter::IntoIterator::into_iter(self)
    }

    /// Returns an Iterator over the mutable values of the Table.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn iter_mut<'a>(&'a mut self) -> TableIterMut<'a, K, V> {
        iter::IntoIterator::into_iter(self)
    }
}

impl<K: fmt::Debug + TableIndex, V> Table<K, V> {
    /// Inserts a new key and its value.
    ///
    /// # Complexity
    ///
    /// Amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the key's index differs from `self.len()`.
    pub fn push(&mut self, key: &K, value: V) {
        let index = key.index();

        assert!(
            index == self.len(),
            "Cannot insert {:?} mapping to {:?} != {:?}.", key, index, self.len()
        );

        self.values.push(value);
    }
}

impl<K: fmt::Debug + TableIndex, V: Clone + Default> Table<K, V> {
    /// Inserts a new key and its value.
    ///
    /// # Complexity
    ///
    /// Amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the key already exists.
    pub fn insert(&mut self, key: &K, value: V) {
        let index = key.index();

        assert!(
            index >= self.len(),
            "Cannot insert {:?} mapping to {:?} >= {:?}.", key, index, self.len()
        );

        self.values.resize(index, Default::default());
        self.values.push(value);
    }
}

impl<K: TableIndex, V> MultiTable<K, V> {
    /// Creates an instance.
    pub fn new() -> Self { Default::default() }

    /// Returns true if and only if the map contains no element.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn is_empty(&self) -> bool { self.index.is_empty() }

    /// Returns the number of keys in the map.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn len(&self) -> usize { self.index.len() }

    /// Clears the map.
    ///
    /// # Complexity
    ///
    /// O(keys.len() + values.len())
    pub fn clear(&mut self) {
        self.index.clear();
        self.values.clear();
    }

    /// Returns true if and only if the map contains the key.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn contains(&self, key: &K) -> bool { key.index() < self.len() }

    /// Returns the values associated to a key.
    ///
    /// If there is no such key, returns an empty slice.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn get<'a>(&'a self, key: &K) -> &'a [V] {
        let range = self.range(key);
        &self.values[range]
    }

    /// Returns the values associated to a key.
    ///
    /// If there is no such key, returns an empty slice.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn get_mut<'a>(&'a mut self, key: &K) -> &'a mut [V] {
        let range = self.range(key);
        &mut self.values[range]
    }

    /// Returns the values associated to the two keys.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panic if the keys are equal.
    pub fn get_duo_mut<'a>(&'a mut self, one: &K, two: &K)
        -> (&'a mut [V], &'a mut [V])
    {
        let first = self.range(one);
        let second = self.range(two);

        assert!(first != second || first.start == first.end);

        let ptr = self.values.as_mut_ptr();
        let len = self.values.len();

        //  Safety checks:
        //  -   non-overlapping slices.
        //  -   lifetime constrained.
        unsafe { (
            part_of::<'a>(ptr, len, first),
            part_of::<'a>(ptr, len, second),
        ) }
    }

    /// Returns the values associated to the three keys.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panics if any two keys are equal.
    pub fn get_trio_mut<'a>(&'a mut self, one: &K, two: &K, three: &K)
        -> (&'a mut [V], &'a mut [V], &'a mut [V])
    {
        let first = self.range(one);
        let second = self.range(two);
        let third = self.range(three);

        assert!(first != second || first.start == first.end);
        assert!(first != third || first.start == first.end);
        assert!(second != third || second.start == second.end);

        let ptr = self.values.as_mut_ptr();
        let len = self.values.len();

        //  Safety checks:
        //  -   non-overlapping slices.
        //  -   lifetime constrained.
        unsafe { (
            part_of::<'a>(ptr, len, first),
            part_of::<'a>(ptr, len, second),
            part_of::<'a>(ptr, len, third),
        ) }
    }

    /// Returns an Iterator over the values of the Table.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn iter<'a>(&'a self) -> MultiTableIter<'a, K, V> {
        iter::IntoIterator::into_iter(self)
    }

    /// Returns an Iterator over the mutable values of the Table.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn iter_mut<'a>(&'a mut self) -> MultiTableIterMut<'a, K, V> {
        iter::IntoIterator::into_iter(self)
    }
}

impl<K: fmt::Debug + TableIndex, V: Clone> MultiTable<K, V> {
    /// Inserts a new key and its value.
    ///
    /// # Complexity
    ///
    /// Amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the key's index differs from `self.len()`.
    pub fn push(&mut self, key: &K, values: &[V]) {
        let index = key.index();

        assert!(
            index == self.len(),
            "Cannot insert {:?} mapping to {:?} != {:?}.", key, index, self.len()
        );

        self.insert(key, values)
    }

    /// Inserts a new key and its values.
    ///
    /// # Complexity
    ///
    /// Amortized O(1 + values.len())
    ///
    /// # Panics
    ///
    /// Panics if the key already exists.
    pub fn insert(&mut self, key: &K, values: &[V]) {
        fn make_range(index: usize, length: usize) -> Range<u32> {
            Range { start: index as u32, end: (index + length) as u32 }
        }

        let index = key.index();
        let start = self.values.len();

        assert!(
            index >= self.index.len(),
            "Cannot insert {:?} mapping to {:?} >= {:?}.", key, index, self.index.len()
        );

        self.index.resize(index, 0..0);

        self.values.extend_from_slice(values);
        self.index.push(make_range(start, values.len()));
    }
}

//
//  Trait Implementations
//

impl<K: TableIndex, V> Default for Table<K, V> {
    fn default() -> Self {
       Table {
            values: Default::default(),
            _marker: marker::PhantomData,
        }
    }
}

impl<K: TableIndex, V> Default for MultiTable<K, V> {
    fn default() -> Self {
        MultiTable {
            index: Default::default(),
            values: Default::default(),
            _marker: marker::PhantomData,
        }
    }
}

impl<'a, K: TableIndex, V> iter::Iterator for TableIter<'a, K, V> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.table.len() {
            let index = K::from_index(self.index);
            debug_assert!(index.index() == self.index);

            self.index += 1;
            self.table.get(&index)
        } else {
            None
        }
    }
}

impl<'a, K: TableIndex, V> iter::Iterator for TableIterMut<'a, K, V> {
    type Item = &'a mut V;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.table.len() {
            let index = K::from_index(self.index);
            assert!(index.index() == self.index);

            //  Safety:
            //  -   non-overlapping indices.
            //  -   constrained lifetime.
            let table: &'a mut Table<K, V> =
                unsafe { &mut *(self.table as *mut _) };

            self.index += 1;
            table.get_mut(&index)
        } else {
            None
        }
    }
}

impl<'a, K: TableIndex, V> iter::Iterator for MultiTableIter<'a, K, V> {
    type Item = &'a [V];

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.table.len() {
            let index = K::from_index(self.index);
            debug_assert!(index.index() == self.index);

            self.index += 1;
            Some(self.table.get(&index))
        } else {
            None
        }
    }
}

impl<'a, K: TableIndex, V> iter::Iterator for MultiTableIterMut<'a, K, V> {
    type Item = &'a mut [V];

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.table.len() {
            let index = K::from_index(self.index);
            assert!(index.index() == self.index);

            //  Safety:
            //  -   non-overlapping indices.
            //  -   constrained lifetime.
            let table: &'a mut MultiTable<K, V> =
                unsafe { &mut *(self.table as *mut _) };

            self.index += 1;
            Some(table.get_mut(&index))
        } else {
            None
        }
    }
}

impl<'a, K: TableIndex, V> iter::IntoIterator for &'a Table<K, V> {
    type Item = &'a V;
    type IntoIter = TableIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        TableIter { table: self, index: 0 }
    }
}

impl<'a, K: TableIndex, V> iter::IntoIterator for &'a mut Table<K, V> {
    type Item = &'a mut V;
    type IntoIter = TableIterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        TableIterMut { table: self, index: 0 }
    }
}

impl<'a, K: TableIndex, V> iter::IntoIterator for &'a MultiTable<K, V> {
    type Item = &'a [V];
    type IntoIter = MultiTableIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        MultiTableIter { table: self, index: 0 }
    }
}

impl<'a, K: TableIndex, V> iter::IntoIterator for &'a mut MultiTable<K, V> {
    type Item = &'a mut [V];
    type IntoIter = MultiTableIterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        MultiTableIterMut { table: self, index: 0 }
    }
}

//
//  Private Methods
//

impl<K: TableIndex, V> MultiTable<K, V> {
    fn range(&self, key: &K) -> Range<usize> {
        self.index.get(key.index())
            .map(|r| Range { start: r.start as usize, end: r.end as usize })
            .unwrap_or(Range { start: 0, end: 0 })
    }
}

unsafe fn element_of<'a, T: 'a>(ptr: *mut T, len: usize, index: usize)
    -> Option<&'a mut T>
{
    let slice = from_raw_parts_mut(ptr, len);
    slice.get_mut(index)
}

unsafe fn part_of<'a, T: 'a>(ptr: *mut T, len: usize, range: Range<usize>)
    -> &'a mut [T]
{
    let slice = from_raw_parts_mut(ptr, len);
    &mut slice[range]
}
