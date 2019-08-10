//! Jagged Hash Map.
//!
//! A Jagged Hash Map is a Hash Map built on top of a Jagged Array.
//!
//! The main advantage of the data-structure is that it allows pushing new
//! elements without displacing the existing ones, so that it is possible
//! to retain a reference to them externally across pushes.
//!
//! Additionally, if one extracts a *view* of the current elements, this view
//! can be accessed from other threads concurrently with modifications of the
//! underlying array from a single thread.
//!
//! Thread Safety:  JaggedArray is not Sync, yet JaggedSlice is Send.

use std::{fmt, iter, mem, sync};
use std::borrow::Borrow;
use std::collections::hash_map;
use std::hash::{self, BuildHasher, Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::basic::mem::{JaggedArray, JaggedArrayIterator, JaggedArraySnapshot};

/// JaggedHashMap
#[derive(Clone, Debug)]
pub struct JaggedHashMap<K, V, H = DefaultState>
    where
        K: Eq + Hash,
        H: BuildHasher,
{
    /// BuildHasher.
    hash_builder: sync::Arc<H>,
    /// Hash residual -> index in data.
    index: JaggedArray<Index>,
    /// Elements.
    data: JaggedArray<(K, V)>,
}

/// JaggedHashMapSnapshot
#[derive(Debug)]
pub struct JaggedHashMapSnapshot<K, V, H = DefaultState>
    where
        K: Eq + Hash,
        H: BuildHasher,
{
    hash_builder: sync::Arc<H>,
    index: JaggedArraySnapshot<Index>,
    data: JaggedArraySnapshot<(K, V)>,
}

/// JaggedHashMapIterator
pub struct JaggedHashMapIterator<'a, K:'a , V: 'a> {
    inner: JaggedArrayIterator<'a, (K, V)>,
}

/// Default HashBuilder for JaggedHashMap.
pub type DefaultState = hash::BuildHasherDefault<hash_map::DefaultHasher>;

//
//  Public Interface of JaggedHashMap
//

impl<K, V, H> JaggedHashMap<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher,
{
    /// Creates an instance with a specific hasher.
    pub fn with_hasher(log2_initial: usize, hash_builder: H) -> Self {
        JaggedHashMap {
            hash_builder: sync::Arc::new(hash_builder),
            index: JaggedArray::new(log2_initial + LOG2_BUCKET_SIZE),
            data: JaggedArray::new(log2_initial),
        }
    }

    /// Returns a reference to the map's BuilderHasher.
    pub fn hasher(&self) -> &H { &self.hash_builder }

    /// Returns the current capacity of the map.
    pub fn capacity(&self) -> usize { self.data.capacity() }

    /// Returns the maximum capacity of the map.
    pub fn max_capacity(&self) -> usize { self.data.max_capacity() }

    /// Returns whether the map is empty or not.
    pub fn is_empty(&self) -> bool { self.data.is_empty() }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize { self.data.len() }

    /// Returns a snapshot.
    pub fn snapshot(&self) -> JaggedHashMapSnapshot<K, V, H> {
        JaggedHashMapSnapshot::new(self)
    }

    /// Clears the map, keep the allocated memory for reuse.
    pub fn clear(&mut self) {
        self.index.clear();
        self.data.clear();
    }

    /// Shrinks the map, removing unused allocated memory.
    pub fn shrink(&mut self) {
        self.index.shrink();
        self.data.shrink();
    }

    /// Returns a reference to the value corresponding to the key.
    ///
    /// Complexity: O(log(self.capacity()))
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
    {
        //  Safety:
        //  -   Ties the lifetime of the reference to self, which is also correct.
        unsafe { self.snapshot().with_lifetime(self).get(key) }
    }

    /// Inserts the key and its value.
    ///
    /// Returns None if the insertion succeeds, the key and value otherwise.
    ///
    /// Complexity: O(log(self.capacity()))
    pub fn insert(&self, key: K, value: V) -> Option<(K, V)> {
        let (hash, position) = match self.snapshot().lookup(&key) {
            Lookup::Found(_) => return Some((key, value)),
            Lookup::Vacant(hash, position) => (hash, position),
            Lookup::Absent(hash) => (hash, self.grow_index(hash)),
        };

        let index = self.data.len();
        self.data.push((key, value));

        //  Safety:
        //  -   `position` either returned or made valid.
        unsafe { self.write_index(position, hash, index); }
        None
    }
}

impl<K, V> JaggedHashMap<K, V, DefaultState>
    where
        K: Eq + Hash,
{
    /// Creates an instance, with a default hash builder.
    pub fn new(log2_initial: usize) -> Self {
        JaggedHashMap::with_hasher(log2_initial, Default::default())
    }
}

//
//  Public Interface of JaggedHashMapSnapshot
//

impl<K, V, H> JaggedHashMapSnapshot<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher,
{
    /// Creates an instance.
    pub fn new(map: &JaggedHashMap<K, V, H>) -> Self {
        JaggedHashMapSnapshot {
            hash_builder: map.hash_builder.clone(),
            index: JaggedArraySnapshot::new(&map.index),
            data: JaggedArraySnapshot::new(&map.data),
        }
    }

    /// Returns a reference to the map's BuilderHasher.
    pub fn hasher(&self) -> &H { &self.hash_builder }

    /// Returns whether the map is empty or not.
    pub fn is_empty(&self) -> bool { self.data.is_empty() }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize { self.data.len() }

    /// Returns a reference to the value corresponding to the key.
    ///
    /// Complexity: O(log(self.capacity()))
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
    {
        match self.lookup(key) {
            Lookup::Absent(..) | Lookup::Vacant(..) => None,
            Lookup::Found(index)
                //  Safety:
                //  -   Index just found.
                => Some(unsafe { &self.data.get_unchecked(index).1 }),
        }
    }
}

//
//  Public Interface of JaggedHashMapIterator
//

impl<'a, K: 'a, V: 'a> JaggedHashMapIterator<'a, K, V> {
    /// Creates an instance.
    pub fn new<H>(snapshot: &'a JaggedHashMapSnapshot<K, V, H>) -> Self
        where
            K: Eq + Hash,
            H: BuildHasher + Clone,
    {
        JaggedHashMapIterator { inner: snapshot.data.into_iter() }
    }
}

//
//  Implementation Details
//

//  The hashmap implementation uses a quadratic probing scheme, this is great
//  to avoid clustering but is not cache friendly. To recover some cache-
//  friendliness, it also a bucket concept, so that instead of probing one
//  index at a time, it probes a full bucket of them.
//
//  The bucket size is best if small-ish. 4 or 8 seem like a good choice.
const LOG2_BUCKET_SIZE: usize = 2;

//  TODO:   Should be AtomicU32 (for compactness), but it is nightly-only.
#[derive(Default)]
struct RelaxedAtomicU32(AtomicUsize);

#[derive(Clone, Debug, Default)]
struct Index {
    hash_residual: RelaxedAtomicU32,
    index: RelaxedAtomicU32,
}

enum Lookup {
    /// Not Found.
    Absent(u32),
    /// Found at self.data[i].
    Found(usize),
    /// Not found, self.index[i] can be used to store it.
    Vacant(u32, usize),
}

impl<K, V, H> JaggedHashMap<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher,
{
    /// Grows the index.
    ///
    /// Returns the position at which hash should be inserted.
    fn grow_index(&self, hash: u32) -> usize {
        let before = self.index.capacity();

        let added = if before == 0 {
            self.index.reserve(1);
            self.index.capacity()
        } else {
            let capacity = self.index.capacity();
            self.index.reserve(capacity);
            capacity
        };

        for _ in 0..added {
            self.index.push(Default::default());
        }

        before + (bucket_position(hash, added) << LOG2_BUCKET_SIZE)
    }

    /// Writes the index.
    ///
    /// Requires caller to specify a valid `position`.
    unsafe fn write_index(&self, position: usize, hash: u32, value: usize) {
        let e = self.index.get_unchecked(position);
        debug_assert!(!e.is_set(), "{:?} at {}", e, position);

        e.set(hash, value);
    }
}

impl<K, V, H> JaggedHashMapSnapshot<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher,
{
    /// Aligns lifetime of the instance on lifetime of the caller.
    ///
    /// With great power comes great responsibility.
    unsafe fn with_lifetime<'a, C>(&self, _: &'a C) -> &'a Self {
        mem::transmute(self)
    }

    /// Hash the key.
    fn hash<Q: ?Sized>(&self, key: &Q) -> u32
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
    {
        let mut hasher = self.hash_builder.build_hasher();
        key.hash(&mut hasher);
        hasher.finish() as u32
    }

    /// Lookup the key.
    fn lookup<Q: ?Sized>(&self, key: &Q) -> Lookup
        where
            K: Borrow<Q>,
            Q: Eq + Hash,
    {
        let hash = self.hash(key);

        let mut seen = 0;
        for slice in &self.index {
            debug_assert!(
                slice.len().count_ones() == 1,
                "Expected power of 2, got {}", slice.len()
            );

            let start = bucket_position(hash, slice.len()) << LOG2_BUCKET_SIZE;

            for position in start..(start + (1 << LOG2_BUCKET_SIZE)) {
                //  Safety:
                //  -   `position` guaranted valid by interval checked.
                let e = unsafe { &slice.get_unchecked(position) };

                if let Some(index) = self.read_index(e, hash) {
                    //  Safety:
                    //  -   `index` guaranteed valid by successful read.
                    let k = unsafe { &self.data.get_unchecked(index).0 };
                    if key == k.borrow() {
                        return Lookup::Found(index);
                    }
                } else if !e.is_set() {
                    return Lookup::Vacant(hash, seen + position);
                }
            }

            seen += slice.len();
        }

        Lookup::Absent(hash)
    }

    /// Read the index, accounting for:
    /// -   mismatching hashes,
    /// -   references to elements not captured in the snapshot.
    fn read_index(&self, e: &Index, hash: u32) -> Option<usize> {
        if e.hash() != hash {
            return None;
        }

        let index = e.index();
        if index < self.data.len() { Some(index) } else { None }
    }
}

impl RelaxedAtomicU32 {
    /// Creates an instance.
    fn new(value: u32) -> RelaxedAtomicU32 {
        RelaxedAtomicU32(AtomicUsize::new(value as usize))
    }

    /// Gets the value.
    fn load(&self) -> u32 { self.0.load(Ordering::Relaxed) as u32 }

    /// Sets the value.
    fn store(&self, value: u32) {
        self.0.store(value as usize, Ordering::Relaxed)
    }
}

impl Index {
    /// Returns whether the Index is set or not.
    fn is_set(&self) -> bool { self.index.load() > 0 }

    /// Gets hash residual.
    fn hash(&self) -> u32 { self.hash_residual.load() }

    /// Gets index.
    fn index(&self) -> usize { self.index.load() as usize - 1 }

    /// Sets the hash residual and index.
    fn set(&self, hash: u32, index: usize) {
        debug_assert!(!self.is_set(), "{:?}", self);
        self.hash_residual.store(hash);
        self.index.store(index as u32 + 1);
    }
}

fn bucket_position(hash: u32, slice_capacity: usize) -> usize {
    hash as usize & ((slice_capacity >> LOG2_BUCKET_SIZE) - 1)
}

//
//  Implementation of traits for JaggedHashMap
//

impl<K, V> Default for JaggedHashMap<K, V, DefaultState>
    where
        K: Eq + Hash,
{
    fn default() -> Self { JaggedHashMap::new(0) }
}

impl<'a, K, V, H> iter::IntoIterator for &'a JaggedHashMap<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher + Clone,
{
    type Item = &'a [(K, V)];
    type IntoIter = JaggedHashMapIterator<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        //  Safety:
        //  -   Ties the lifetime of the reference to self, which is also correct.
        unsafe { self.snapshot().with_lifetime(self).into_iter() }
    }
}

//
//  Implementation of traits for JaggedHashMapSnapshot
//

impl<K, V, H> Clone for JaggedHashMapSnapshot<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher + Clone,
{
    fn clone(&self) -> Self {
        JaggedHashMapSnapshot {
            hash_builder: self.hash_builder.clone(),
            index: self.index.clone(),
            data: self.data.clone(),
        }
    }
}

impl<K, V, H> Default for JaggedHashMapSnapshot<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher + Default,
{
    fn default() -> Self {
        JaggedHashMapSnapshot {
            hash_builder: Default::default(),
            index: Default::default(),
            data: Default::default(),
        }
    }
}

impl<'a, K, V, H> iter::IntoIterator for &'a JaggedHashMapSnapshot<K, V, H>
    where
        K: Eq + Hash,
        H: BuildHasher + Clone,
{
    type Item = &'a [(K, V)];
    type IntoIter = JaggedHashMapIterator<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter { JaggedHashMapIterator::new(self) }
}

//
//  Implementation of traits for JaggedHashMapIterator
//

impl<'a, K: 'a, V: 'a> Clone for JaggedHashMapIterator<'a, K, V> {
    fn clone(&self) -> Self {
        JaggedHashMapIterator { inner: self.inner.clone() }
    }
}

impl<'a, K: 'a, V: 'a> iter::Iterator for JaggedHashMapIterator<'a, K, V> {
    type Item = &'a [(K, V)];

    fn next(&mut self) -> Option<Self::Item> { self.inner.next() }
}

//
//  Implementation of traits for RelaxedAtomicU32
//

impl Clone for RelaxedAtomicU32 {
    fn clone(&self) -> Self { RelaxedAtomicU32::new(self.load()) }
}

impl fmt::Debug for RelaxedAtomicU32 {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.load())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ensure_send() {
        fn check_send<T: Send>() {}

        check_send::<JaggedHashMap<i32, i32>>();
        check_send::<JaggedHashMapSnapshot<i32, i32>>();
    }

    #[test]
    fn map() {
        let map = JaggedHashMap::default();
        assert!(map.is_empty());

        assert_eq!(map.insert(1, 32), None);
        assert_eq!(map.insert(1, 42), Some((1, 42)));
        assert_eq!(map.insert(20, 2), None);

        assert_eq!(map.len(), 2);
        assert_eq!(map.capacity(), 2);

        assert_eq!(map.get(&0), None);
        assert_eq!(map.get(&1), Some(&32));
        assert_eq!(map.get(&20), Some(&2));
    }

    #[test]
    fn iterator() {
        let map = JaggedHashMap::default();
        map.insert(1, 42);
        map.insert(3, 32);
        map.insert(5, 22);
        map.insert(7, 12);

        let mut iterator = map.into_iter();
        assert_eq!(iterator.next(), Some(&[(1, 42)][..]));
        assert_eq!(iterator.next(), Some(&[(3, 32)][..]));
        assert_eq!(iterator.next(), Some(&[(5, 22), (7, 12)][..]));

        assert_eq!(iterator.next(), None);

        map.insert(9, 2);

        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn snapshot() {
        let map = JaggedHashMap::default();
        map.insert(1, 42);
        map.insert(3, 32);
        map.insert(5, 22);
        map.insert(7, 12);

        let snapshot = map.snapshot();
        assert_eq!(snapshot.get(&1), Some(&42));
        assert_eq!(snapshot.get(&3), Some(&32));
        assert_eq!(snapshot.get(&5), Some(&22));
        assert_eq!(snapshot.get(&7), Some(&12));
        assert_eq!(snapshot.get(&9), None);

        map.insert(9, 2);
        assert_eq!(snapshot.get(&9), None);
    }
}
