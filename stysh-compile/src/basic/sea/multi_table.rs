//! MultiTable.
//!
//! The pendant to `Table` for one-to-many relationships.

use std::{cmp, fmt, hash, iter, marker};
use std::iter::IntoIterator;

use super::TableIndex;

/// MultiTable.
///
/// A container mapping a key to multiple values.
#[derive(Clone)]
pub struct MultiTable<K: TableIndex, V: Copy> {
    //  Maps a key to the range of indices of its values.
    index: Vec<details::SequenceIndex>,
    //  Pool of all values.
    values: details::SequenceTable<V>,
    //  Marker for ownership reasons.
    _marker: marker::PhantomData<*const K>,
}

impl<K: TableIndex, V: Copy> MultiTable<K, V> {
    /// Creates an instance.
    pub fn new() -> Self {
        MultiTable {
            index: vec!(),
            values: details::SequenceTable::new(),
            _marker: marker::PhantomData,
        }
    }

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
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panic if the key is out of range.
    pub fn get<'a>(&'a self, key: &K) -> &'a [V] {
        let sequence = self.sequence(key);
        self.values.get(sequence)
    }

    /// Returns the values associated to a key.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panic if the key is out of range.
    pub fn get_mut<'a>(&'a mut self, key: &K) -> &'a mut [V] {
        let sequence = self.sequence(key);
        self.values.get_mut(sequence)
    }

    /// Returns the values associated to the two keys.
    ///
    /// # Complexity
    ///
    /// O(1)
    ///
    /// # Panics
    ///
    /// Panic if either key is out of range or if the keys are equal.
    pub fn get_duo_mut<'a>(&'a mut self, one: &K, two: &K)
        -> (&'a mut [V], &'a mut [V])
    {
        let one = self.sequence(one);
        let two = self.sequence(two);

        self.values.get_duo_mut(one, two)
    }

    /// Returns an Iterator over the values of the Table.
    ///
    /// # Complexity
    ///
    /// O(1)
    pub fn iter<'a>(&'a self) -> MultiTableIter<'a, K, V> {
        iter::IntoIterator::into_iter(self)
    }

    /// Creates a new entry in the table.
    ///
    /// Returns the generated key, if any element was inserted.
    ///
    /// # Complexity
    ///
    /// Amortized O(len(iterator))
    pub fn create<I>(&mut self, into_iterator: I) -> Option<K>
        where
            I: IntoIterator<Item = V>,
    {
        let iterator = into_iterator.into_iter();

        let length = {
            let (min, max) = iterator.size_hint();
            assert!(max == Some(min));
            min
        };

        if length == 0 {
            return None;
        }

        let reverse_index = self.index.len() as u32;
        let sequence = self.values.create(reverse_index, length);

        let (sequence, other) = self.values.extend(reverse_index, sequence, iterator);
        debug_assert!(other.is_none());

        self.index.push(sequence);

        Some(K::from_index(reverse_index as usize))
    }

    /// Erases a sequence from the table.
    ///
    /// The key remains valid until the table is cleared, and is now associated
    /// to an empty sequence which can be extended like any other.
    ///
    /// # Complexity
    ///
    /// O(len(sequence))
    pub fn erase(&mut self, key: &K) {
        let sequence = self.sequence(key);
        self.index[key.index()] = details::SequenceIndex::default();

        if let Some((index, sequence)) = self.values.erase(sequence) {
            self.index[index] = sequence;
        }
    }

    /// Extends an existing sequence.
    ///
    /// # Complexity
    ///
    /// Amortized O(len(iterator))
    ///
    /// # Panics
    ///
    /// Panic if the sequence does not exist.
    pub fn extend<I>(&mut self, key: &K, into_iterator: I)
        where
            I: iter::IntoIterator<Item = V>
    {
        let reverse_index = key.index();
        let sequence = self.sequence(key);

        let iterator = into_iterator.into_iter();
        let (sequence, other) = self.values.extend(reverse_index as u32, sequence, iterator);

        self.index[reverse_index] = sequence;

        if let Some((other_index, other_sequence)) = other {
            self.index[other_index] = other_sequence;
        }
    }

    /// Removes an element from an existing sequence.
    ///
    /// Returns the removed element.
    ///
    /// # Complexity
    ///
    /// O(N) where N is the length of the sequence.
    ///
    /// # Panics
    ///
    /// Panic if the sequence does not exist or the index is out of bounds.
    pub fn remove(&mut self, key: &K, index: usize) -> V {
        let sequence = self.sequence(key);
        self.values.remove(sequence, index)
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

impl<K: TableIndex, V: Copy> MultiTable<K, V> {
    fn sequence(&self, key: &K) -> details::SequenceIndex {
        self.index[key.index()]
    }
}

impl<K: TableIndex, V: Copy> Default for MultiTable<K, V> {
    fn default() -> Self { MultiTable::new() }
}

impl<K: TableIndex + fmt::Debug, V: Copy + fmt::Debug> fmt::Debug for MultiTable<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("MultiTable")
            .field("index", &self.index)
            .field("values", &self.values)
            .finish()
    }
}

impl<K: TableIndex, V: Copy + cmp::PartialEq> cmp::PartialEq for MultiTable<K, V> {
    fn eq(&self, other: &Self) -> bool {
        for (left, right) in self.iter().zip(other) {
            if left != right {
                return false;
            }
        }
        true
    }
}

impl<K: TableIndex, V: Copy + cmp::Eq> cmp::Eq for MultiTable<K, V> {}

impl<K: TableIndex, V: Copy + hash::Hash> hash::Hash for MultiTable<K, V> {
    fn hash<H>(&self, state: &mut H)
        where
            H: hash::Hasher
    {
        for slice in self {
            slice.hash(state);
        }
    }
}


/// MultiTableIter
///
/// An iterator over a MultiTable.
#[derive(Clone, Debug)]
pub struct MultiTableIter<'a, K: TableIndex, V: Copy> {
    table: &'a MultiTable<K, V>,
    index: usize,
}

impl<'a, K: TableIndex, V: Copy> iter::Iterator for MultiTableIter<'a, K, V> {
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

impl<'a, K: TableIndex, V: Copy> iter::IntoIterator for &'a MultiTable<K, V> {
    type Item = &'a [V];
    type IntoIter = MultiTableIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        MultiTableIter { table: self, index: 0 }
    }
}


/// MultiTableIterMut
///
/// A mutable iterator over a MultiTable.
#[derive(Debug)]
pub struct MultiTableIterMut<'a, K: TableIndex, V: Copy> {
    table: &'a mut MultiTable<K, V>,
    index: usize,
}

impl<'a, K: TableIndex, V: Copy> iter::Iterator for MultiTableIterMut<'a, K, V> {
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

impl<'a, K: TableIndex, V: Copy> iter::IntoIterator for &'a mut MultiTable<K, V> {
    type Item = &'a mut [V];
    type IntoIter = MultiTableIterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        MultiTableIterMut { table: self, index: 0 }
    }
}

mod details {

use std::{fmt, mem, ops};

//  An index into the SequenceTable.
//
//  It is composed of two numbers:
//  -   5 bits sub-table index.
//  -   27 bits local index.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct SequenceIndex(u32);

impl SequenceIndex {
    const TABLE_SHIFT: u32 = 27;
    const LOCAL_MASK: u32 = (1u32 << Self::TABLE_SHIFT) - 1;

    //  Creates a new instance.
    pub fn new(table_index: usize, local_index: usize) -> SequenceIndex {
        debug_assert!(table_index < 32);
        debug_assert!((local_index as u32) < (1u32 << Self::TABLE_SHIFT));
        let table_index = (table_index as u32) << Self::TABLE_SHIFT;
        let local_index = local_index as u32;
        SequenceIndex(table_index + local_index)
    }

    //  Returns the table index.
    pub fn table_index(&self) -> usize { (self.0 >> Self::TABLE_SHIFT) as usize }

    //  Returns the local index.
    pub fn local_index(&self) -> usize { (self.0 & Self::LOCAL_MASK) as usize }
}

impl SequenceIndex {
    //  Returns the range of local indices.
    fn local_range(&self) -> ops::Range<usize> {
        let table_index = self.table_index();
        let local_index = self.local_index() << table_index;
        local_index..(local_index + (1 << table_index))
    }
}

impl fmt::Debug for SequenceIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "SequenceIndex({}, {})", self.table_index(), self.local_index())
    }
}


//  Magic behind the magic.
union MaybeElement<T: Copy> {
    reverse_index: u32,
    size: u32,
    element: T,
}

impl<T: Copy> Clone for MaybeElement<T> {
    fn clone(&self) -> Self { *self }
}

impl<T: Copy> Copy for MaybeElement<T> {}


//  SequenceTable.
//
//  Core unsafe abstraction of MultiTable.
//
//  Layout:
//  -   tables[0]:  empty.
//  -   tables[1]:  empty.
//  -   tables[2]:  sequences of 4 MaybeElement, [reverse_index, size, T...].
//  -   tables[3]:  sequences of 8 MaybeElement, [reverse_index, size, T...].
//  -   ...
//
//  A SequenceIndex::table_index of 0 or 1 indicates an empty slice, otherwise:
//  -   table_index is the index in self.tables.
//  -   1 << table_index is the capacity, aka number of MaybeElements.
//  -   capacity - 2 is the maximum number of Ts.
#[derive(Clone)]
pub struct SequenceTable<T: Copy> {
    tables: Vec<Vec<MaybeElement<T>>>,
}

impl<T: Copy> SequenceTable<T> {
    //  Creates a new instance.
    pub fn new() -> Self {
        assert!(mem::size_of::<T>() == mem::size_of::<MaybeElement<T>>());
        SequenceTable { tables: vec!() }
    }

    //  Clears the table, preserving the allocated memory.
    pub fn clear(&mut self) {
        for table in &mut self.tables {
            table.clear();
        }
    }

    //  Returns the slice of elements of a particular sequence.
    pub fn get<'a>(&'a self, sequence: SequenceIndex) -> &'a [T] {
        let slice = self.get_maybe(sequence);

        if slice.is_empty() {
            return &[];
        }

        debug_assert!(slice.len() >= 2);

        unsafe {
            //  Safety: see layout.
            let size = slice[1].size as usize;
            debug_assert!(2 + size <= slice.len());

            //  Safety:
            //  -   Layout ensures that 2..2+size is only Ts.
            //  -   size ensures size elements are initialized.
            //  -   new ensures that sizeof T == sizeof MaybeElement<T>.
            mem::transmute::<_, &'a [T]>(&slice[2..][..size])
        }
    }

    //  Returns the slice of elements of a particular sequence.
    pub fn get_mut<'a>(&'a mut self, sequence: SequenceIndex) -> &'a mut [T] {
        let slice = self.get_maybe_mut(sequence);
        Self::elements_of(slice)
    }

    //  Returns the slice of elements of the particular sequences.
    pub fn get_duo_mut<'a>(&'a mut self, one: SequenceIndex, two: SequenceIndex)
        -> (&'a mut [T], &'a mut [T])
    {
        let (one, two) = self.get_duo_maybe_mut(one, two);
        (Self::elements_of(one), Self::elements_of(two))
    }

    //  Creates an empty new sequence of the required capacity.
    //
    //  It is a waste of space to create such a sequence without pushing at
    //  least one element later.
    pub fn create(&mut self, reverse_index: u32, capacity: usize) -> SequenceIndex {
        let table_index = Self::table_index_of(capacity);

        self.create_sequence(reverse_index, table_index)
    }

    //  Erases an existing sequence.
    //
    //  If this sequence is not the last of its sub-table, the last sequence of
    //  the sub-table is moved in its stead, and its SequenceIndex must be updated.
    //
    //  Returns the reverse_index of the displaced sequence, if any, and its new
    //  SequenceIndex.
    //
    //  # Panics
    //
    //  If the SequenceIndex is out-of-bounds.
    pub fn erase(&mut self, sequence: SequenceIndex)
        -> Option<(usize, SequenceIndex)>
    {
        let table_index = sequence.table_index();
        let local_index = sequence.local_index() << table_index;

        if table_index < 2 {
            return None;
        }

        let capacity = 1 << table_index;

        let table = &mut self.tables[table_index];
        let slice = &mut table[local_index..];

        if slice.len() == local_index + capacity {
            table.resize(local_index, MaybeElement { size: 0 });
            return None;
        }

        let (slice, tail) = slice.split_at_mut(capacity);
        let (_, tail) = tail.split_at_mut(tail.len() - capacity);
        debug_assert!(slice.len() == capacity);
        debug_assert!(tail.len() == capacity);

        let reverse_index = unsafe {
            //  Safety:
            //  -   Layout.
            tail[0].reverse_index
        };

        slice.swap_with_slice(tail);

        table.resize(table.len() - capacity, MaybeElement { size: 0 });

        Some((reverse_index as usize, sequence))
    }

    //  Extends an existing sequence with new elements.
    //
    //  Returns:
    //  -   The new SequenceIndex of the sequence, which may have moved.
    //  -   The reverse_index of the displaced sequence, if any, and its new
    //      SequenceIndex.
    pub fn extend<I>(&mut self, reverse_index: u32, sequence: SequenceIndex, mut iterator: I)
        -> (SequenceIndex, Option<(usize, SequenceIndex)>)
        where
            I: Iterator<Item = T>,
    {
        let (sequence, other) = {
            //  For now, does not implement variable sized iterators.
            let (minimum, maximum) = iterator.size_hint();
            assert!(maximum == Some(minimum));

            self.reserve_sequence(reverse_index, sequence, minimum)
        };

        let slice = self.get_maybe_mut(sequence);
        unsafe {
            //  Safety:
            //  -   Layout.
            let mut size = slice[1].size as usize;

            while let Some(element) = iterator.next() {
                slice[2 + size] = MaybeElement { element };
                size += 1;
            }

            slice[1].size = size as u32;
        }

        (sequence, other)
    }

    //  Removes an element from an existing sequence.
    //
    //  # Panics
    //
    //  If index is out of bounds for the sequence.
    pub fn remove(&mut self, sequence: SequenceIndex, index: usize) -> T {
        let slice = self.get_maybe_mut(sequence);

        assert!(!slice.is_empty());

        unsafe {
            //  Safety:
            //  -   Layout.
            let size = slice[1].size as usize;
            assert!(index < size);

            let elements = &mut slice[2+index..2+size];
            elements.rotate_left(1);

            slice[1].size = (size as u32) - 1;
            let maybe = mem::replace(&mut slice[1 + size], MaybeElement { size: 0 });
            maybe.element
        }
    }
}

impl<T: Copy> SequenceTable<T> {
    //  Returns the table_index required for a given capacity.
    fn table_index_of(capacity: usize) -> usize {
        if capacity == 0 {
            return 0;
        }

        (mem::size_of::<usize>() * 8) - ((capacity + 1).leading_zeros() as usize)
    }

    //  Returns the elements of a slice.
    fn elements_of<'a>(slice: &'a mut [MaybeElement<T>]) -> &'a mut [T] {
        if slice.is_empty() {
            return &mut [];
        }

        debug_assert!(slice.len() >= 2);

        unsafe {
            //  Safety: see layout.
            let size = slice[1].size as usize;
            debug_assert!(2 + size <= slice.len());

            //  Safety:
            //  -   Layout ensures that 2..capacity is only Ts.
            //  -   size ensures size elements are initialized.
            //  -   new ensures that sizeof T == sizeof MaybeElement<T>.
            mem::transmute::<_, &'a mut [T]>(&mut slice[2..][..size])
        }
    }

    //  Returns the slice of MaybeElement<T> for a given sequence.
    //
    //  # Panics
    //
    //  If the sequence is out of bounds.
    fn get_maybe<'a>(&'a self, sequence: SequenceIndex) -> &'a [MaybeElement<T>] {
        let table_index = sequence.table_index();

        if table_index < 2 {
            return &[];
        }

        &self.tables[table_index][sequence.local_range()]
    }

    //  Returns the slice of MaybeElement<T> for a given sequence.
    fn get_maybe_mut<'a>(&'a mut self, sequence: SequenceIndex)
        -> &'a mut [MaybeElement<T>]
    {
        let table_index = sequence.table_index();

        if table_index < 2 {
            return &mut [];
        }

        &mut self.tables[table_index][sequence.local_range()]
    }

    //  Returns the two slices of MaybeElement<T> for the given indexes.
    //
    //  # Panics
    //
    //  If the indexes are equal and point to non-empty slices.
    fn get_duo_maybe_mut<'a>(&'a mut self, one: SequenceIndex, two: SequenceIndex)
        -> (&'a mut [MaybeElement<T>], &'a mut [MaybeElement<T>])
    {
        if one.0 > two.0 {
            let (two, one) = self.get_duo_maybe_mut(two, one);
            return (one, two);
        }

        let one_table_index = one.table_index();
        let two_table_index = two.table_index();

        debug_assert!(one_table_index <= two_table_index);

        if one_table_index < 2 {
            return (&mut [], self.get_maybe_mut(two));
        }

        assert!(one != two);

        if one_table_index == two_table_index {
            let table = &mut self.tables[one_table_index];
            return split_slice_mut(&mut *table, one.local_range(), two.local_range());
        }

        let (one_table, two_table) = {
            let one = one_table_index..one_table_index + 1;
            let two = two_table_index..two_table_index + 1;
            let (one, two) = split_slice_mut(&mut *self.tables, one, two);
            (&mut one[0], &mut two[0])
        };

        (&mut one_table[one.local_range()], &mut two_table[two.local_range()])
    }

    //  Creates a sequence in a given table.
    fn create_sequence(&mut self, reverse_index: u32, table_index: usize) -> SequenceIndex {
        assert!(table_index < 32);

        if table_index < 2 {
            return SequenceIndex::new(0, 0);
        }

        while self.tables.len() <= table_index {
            self.tables.push(vec!());
        }

        let table = &mut self.tables[table_index];
        let capacity = 1 << table_index;
        debug_assert!(table.len() % capacity == 0);

        let local_index = table.len() >> table_index;

        //  Safety:
        //  -   Establish Layout invariant.
        let index = table.len();
        table.resize(index + capacity, MaybeElement { size: 0 });
        table[index] = MaybeElement { reverse_index };

        SequenceIndex::new(table_index, local_index)
    }

    //  Reserves extra space in given sequence, may require moving it.
    fn reserve_sequence(&mut self, reverse_index: u32, sequence: SequenceIndex, extra: usize)
        -> (SequenceIndex, Option<(usize, SequenceIndex)>)
    {
        let table_index = sequence.table_index();

        //  Was previously empty, only need to create a new one.
        if table_index < 2 {
            return (self.create(reverse_index, extra), None);
        }

        let total_capacity = unsafe {
            let local_index = sequence.local_index() << table_index;

            //  Safety:
            //  -   Layout.
            let size = self.tables[table_index][local_index + 1].size as usize;
            size + extra
        };

        //  Current capacity is sufficient, no need to do anything.
        if total_capacity < 1 << table_index {
            return (sequence, None);
        }

        //  Capacity did change:
        //  -   Copy sequence elements to new place.
        //  -   Remove sequence from old place, potentially moving another.
        let new_index = self.create(reverse_index, total_capacity);

        unsafe {
            let (old_slice, new_slice) = self.get_duo_maybe_mut(sequence, new_index);
            debug_assert!(old_slice[0].reverse_index == reverse_index);
            debug_assert!(new_slice[0].reverse_index == reverse_index);

            let size = old_slice[1].size as usize;
            new_slice[2..][..size].copy_from_slice(&old_slice[2..][..size]);
            new_slice[1].size = size as u32;
        }

        let displaced_sequence = self.erase(sequence);
        (new_index, displaced_sequence)
    }
}

impl<T: Copy> Default for SequenceTable<T> {
    fn default() -> Self { SequenceTable::new() }
}

impl<T: Copy + fmt::Debug> fmt::Debug for SequenceTable<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use std::fmt::Write;

        let mut printer = f.debug_struct("");

        for (index, table) in self.tables.iter().enumerate() {
            if table.is_empty() { continue; }

            let mut buffer = String::new();
            write!(buffer, "[")?;

            for chunk in table.chunks_exact(1 << index) {
                if buffer.len() > 1 {
                    write!(buffer, ", ")?;
                }

                unsafe {
                    //  Safety:
                    //  -   Layout.
                    let size = chunk[1].size as usize;

                    //  Safety:
                    //  -   Layout.
                    //  -   size guarantees that the size first T elements are initialized.
                    //  -   new guarantees that sizeof T == sizeof MaybeElement<T>.
                    let slice = mem::transmute::<_, &[T]>(&chunk[2..][..size]);

                    write!(buffer, "{:?}", slice)?;
                }
            }

            write!(buffer, "]")?;

            printer.field(&format!("[{}]", index), &buffer);
        }
        printer.finish()
    }
}

fn split_slice_mut<T>(slice: &mut [T], one: ops::Range<usize>, two: ops::Range<usize>)
    -> (&mut [T], &mut [T])
{
    if one.start > two.start {
        let (two, one) = split_slice_mut(slice, two, one);
        return (one, two);
    }

    debug_assert!(one.start <= one.end);
    debug_assert!(two.start <= two.end);

    if one.start == one.end {
        return (&mut [], &mut slice[two]);
    }

    assert!(one != two);

    let slice = &mut slice[one.start..];
    let (one_slice, tail) = slice.split_at_mut(one.end - one.start);
    let two_slice = &mut tail[two.start - one.end .. two.end - one.end];
    return (one_slice, two_slice);
}

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let empty = empty_table();
        assert!(empty.is_empty());
        assert_eq!(0, empty.len());
        assert_eq!("MultiTable { index: [], values:  }", format!("{:?}", empty));
    }

    #[test]
    fn len() {
        assert_eq!(0, empty_table().len());
        assert_eq!(1, single_table().len());
    }

    #[test]
    fn contains() {
        assert!(!empty_table().contains(&TestId(0)));
        assert!(single_table().contains(&TestId(0)));
        assert!(!single_table().contains(&TestId(1)));
    }

    #[test]
    fn get_simple() {
        assert_eq!(&[33], single_table().get(&TestId(0)));
    }

    #[test]
    fn get_mut_simple() {
        assert_eq!(&[33], single_table().get_mut(&TestId(0)));
    }

    #[test]
    fn get_duo_mut_simple() {
        let mut table = duo_table();
        let (one, two) = table.get_duo_mut(&TestId(0), &TestId(1));
        assert_eq!(&[33], one);
        assert_eq!(&[22, 32, 42, 52], two);
    }

    #[test]
    fn iter_empty() {
        let empty = empty_table();
        let collection = collect(&empty);
        assert!(collection.is_empty());
    }

    #[test]
    fn iter_single() {
        let single = single_table();
        let collection = collect(&single);
        assert_eq!(vec!(vec!(33_u32)), collection);
    }

    #[test]
    fn iter_duo() {
        let duo = duo_table();
        let collection = collect(&duo);
        assert_eq!(vec!(vec!(33_u32), vec!(22_u32, 32, 42, 52)), collection);
    }

    #[test]
    fn create() {
        let mut table = duo_table();

        let key = table.create([77].iter().cloned()).unwrap();
        assert_eq!(2, key.index());

        assert_eq!(&[77], table.get(&key));
    }

    #[test]
    fn create_repetitive() {
        let mut table = empty_table();

        for i in 0..42 {
            let key = table.create([i].iter().cloned()).unwrap();
            assert_eq!(TestId(i), key);

            assert_eq!(&[i], table.get(&key));
        }
    }

    #[test]
    fn erase() {
        let key = TestId(0);
        let mut table = single_table();
        table.erase(&key);

        assert_eq!(1, table.len());
        assert_eq!(&[] as &[u32], table.get(&key));
    }

    #[test]
    fn erase_first() {
        let key = TestId(0);
        let mut table = duo_table();
        table.erase(&key);

        assert_eq!(2, table.len());
        assert_eq!(&[] as &[u32], table.get(&key));
        assert_eq!(&[22, 32, 42, 52], table.get(&TestId(1)));
    }

    #[test]
    fn extend_in_place() {
        let key = TestId(0);
        let mut table = single_table();

        table.extend(&key, [43].iter().cloned());

        assert_eq!(&[33, 43], table.get(&key));
    }

    #[test]
    fn extend_move_last() {
        let key = TestId(0);
        let mut table = single_table();

        table.extend(&key, [43, 53, 63].iter().cloned());

        assert_eq!(&[33, 43, 53, 63], table.get(&key));
    }

    #[test]
    fn extend_move_shuffle() {
        let key = TestId(0);
        let mut table = single_table();
        let other_key = table.create([77].iter().cloned()).unwrap();

        table.extend(&key, [43, 53, 63].iter().cloned());

        assert_eq!(&[33, 43, 53, 63], table.get(&key));
        assert_eq!(&[77], table.get(&other_key));
    }

    #[test]
    fn remove_last() {
        let key = TestId(1);
        let mut table = duo_table();

        table.remove(&key, 3);

        assert_eq!(&[33], table.get(&TestId(0)));
        assert_eq!(&[22, 32, 42], table.get(&key));
    }

    #[test]
    fn remove_shuffle() {
        let key = TestId(1);
        let mut table = duo_table();

        table.remove(&key, 1);

        assert_eq!(&[33], table.get(&TestId(0)));
        assert_eq!(&[22, 42, 52], table.get(&key));
    }

    #[test]
    #[should_panic]
    fn remove_invalid() {
        let key = TestId(1);
        let mut table = duo_table();

        table.remove(&key, 4);
    }

    #[test]
    #[should_panic]
    fn invalid_key_get() { empty_table().get(&TestId(0)); }

    #[test]
    #[should_panic]
    fn invalid_key_get_mut() { empty_table().get_mut(&TestId(0)); }

    #[test]
    #[should_panic]
    fn invalid_key_get_duo_mut_first() {
        single_table().get_duo_mut(&TestId(1), &TestId(0));
    }

    #[test]
    #[should_panic]
    fn invalid_key_get_duo_mut_second() {
        single_table().get_duo_mut(&TestId(0), &TestId(1));
    }

    #[test]
    #[should_panic]
    fn invalid_key_get_duo_mut_both() {
        single_table().get_duo_mut(&TestId(1), &TestId(2));
    }

    #[test]
    #[should_panic]
    fn invalid_key_extend() {
        empty_table().extend(&TestId(0), [33].iter().cloned());
    }

    #[test]
    #[should_panic]
    fn invalid_key_remove() {
        empty_table().remove(&TestId(0), 0);
    }

    fn empty_table() -> MultiTable<TestId, u32> { MultiTable::new() }

    fn single_table() -> MultiTable<TestId, u32> {
        let mut table = empty_table();
        table.create([33].iter().cloned());
        table
    }

    fn duo_table() -> MultiTable<TestId, u32> {
        let mut table = single_table();
        table.create([22, 32, 42, 52].iter().cloned());
        table
    }

    fn collect(table: &MultiTable<TestId, u32>) -> Vec<Vec<u32>> {
        table.iter()
            .map(|slice| slice.iter().cloned().collect::<Vec<_>>())
            .collect()
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct TestId(u32);

    impl TableIndex for TestId {
        fn index(&self) -> usize { self.0 as usize }

        fn from_index(index: usize) -> TestId { TestId(index as u32) }
    }
}
