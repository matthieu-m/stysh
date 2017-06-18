//! Memory handling
//!
//! For efficiency, the compiler uses arena allocation and types that do not
//! implement the `Drop` trait.
//!
//! The same type of arena is used for both long-lived and transient
//! allocations, the only difference being that the memory in transient arenas
//! is often recycled.
//!
//! Memory once owned by an arena is never deallocated unless explicitly asked
//! for; recycling is simply about wiping it out (in Debug mode) and reusing it.

use std;
use std::{cell, cmp, fmt, hash, iter, mem, ops, ptr, slice};

/// The Arena structure
///
/// Handles type-less allocation; types so allocated should not implement `Drop`
/// as it will never be called.
///
/// The Arena cannot be recycled or shrunk as long as a single reference lives.
///
/// On the other hand, further values may be handed to the Arena without
/// invalidating the current ones.
pub struct Arena(cell::UnsafeCell<ArenaImpl>);

/// The Array structure
///
/// Similar to the standard Vec, but grows its memory from an arena instead.
pub struct Array<'a, T: 'a> {
    arena: &'a Arena,
    capacity: usize,
    length: usize,
    ptr: *mut T
}

/// The ArrayMap structure
///
/// An ordered map interface laid out in an Array.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ArrayMap<'a, K: 'a + cmp::Ord, V: 'a>(Array<'a, K>, Array<'a, V>);

/// The CloneInto trait.
///
/// Clones a type into a target Arena.
pub trait CloneInto<'a> {
    /// Result of the clone.
    ///
    /// Generally equals to `Self`, with new extended lifetime specifiers.
    type Output: 'a;

    /// Clones self into the target Arena.
    fn clone_into(&self, arena: &'a Arena) -> Self::Output;
}

impl Arena {
    /// Creates a fresh `Arena`.
    pub fn new() -> Arena { Arena(cell::UnsafeCell::new(ArenaImpl::new())) }

    /// Returns the current capacity, in bytes.
    pub fn capacity(&self) -> usize {
        self.get().capacity()
    }

    /// Returns the currently used number of bytes.
    pub fn used(&self) -> usize {
        self.get().used()
    }

    /// Returns whether the arena contains a given object, or not.
    pub fn contains<T>(&self, t: &T) -> bool {
        self.get().contains(t)
    }

    /// Inserts a new value into the `Arena`, and returns a reference to the new
    /// value.
    pub fn insert<'a, T: 'a>(&'a self, t: T) -> &'a mut T {
        self.get_mut().insert(t)
    }

    /// Inserts a slice into the `Arena`, and returns a reference to the new
    /// slice.
    pub fn insert_slice<'a, T: 'a>(&'a self, ts: &[T]) -> &'a mut [T] {
        self.get_mut().insert_slice(ts)
    }

    /// Returns a value semantically equivalent to the input and whose internal
    /// lifetimes are extended to that of the `Arena`.
    ///
    /// Note: if the value is already contained in the arena, then is merely
    ///       copied.
    pub fn intern<'a, T: CloneInto<'a> + Copy>(&'a self, t: &T)
        -> <T as CloneInto<'a>>::Output
    {
        debug_assert!(
            mem::size_of::<T>() ==
            mem::size_of::<<T as CloneInto<'a>>::Output>()
        );
        if self.contains(t) {
            unsafe { mem::transmute_copy(t) }
        } else {
            CloneInto::clone_into(t, self)
        }
    }

    /// Returns a reference semantically equivalent to the input, and whose
    /// lifetime is extended to that of the `Arena`.
    ///
    /// Note: if the value is already contained in the arena, then the reference
    ///       is merely transmuted.
    pub fn intern_ref<'a, T: CloneInto<'a>>(&'a self, t: &T)
        -> &'a <T as CloneInto<'a>>::Output
    {
        if self.contains(t) {
            unsafe { mem::transmute(t) }
        } else {
            self.insert(CloneInto::clone_into(t, self))
        }
    }

    /// Reserves an area of memory for `nb` items of type `T` laid contiguously.
    ///
    /// # Warning
    ///
    /// The memory is uninitialized, and no lifetime is carried by the pointer.
    pub fn reserve<'a, T: 'a>(&'a self, nb: usize) -> *mut T {
        self.get_mut().reserve::<T>(nb)
    }

    /// Shrinks the `Arena` to fit its current load.
    ///
    /// This still reserves more memory than strictly necessary.
    pub fn shrink_to_fit(&self) {
        self.get_mut().shrink_to_fit()
    }

    /// Recyles the memory of the `Arena`.
    ///
    /// In Debug mode, this overrides the content with `0x55` bytes
    /// (`0b01010101`).
    pub fn recycle(&mut self) {
        self.get_mut().recycle();
    }

    /// Resets the `Arena` to the state it had at creation.
    pub fn reset(&mut self) {
        self.recycle();
        self.shrink_to_fit();
    }

    fn get(&self) -> &ArenaImpl {
        unsafe { &*self.0.get() }
    }

    fn get_mut(&self) -> &mut ArenaImpl {
        unsafe { &mut *self.0.get() }
    }
}

impl<'a, T: 'a> Array<'a, T> {
    /// Creates a new, empty, instance.
    pub fn new(arena: &'a Arena) -> Array<'a, T> {
        Array { arena: arena, capacity: 0, length: 0, ptr: ptr::null_mut() }
    }

    /// Creates a new, empty, instance with at least the specified capacity.
    pub fn with_capacity(cap: usize, arena: &'a Arena) -> Array<'a, T> {
        let mut result = Array::new(arena);
        result.reserve(cap);
        result
    }

    /// Creates a new instance from a slice.
    pub fn from_slice(slice: &[T], arena: &'a Arena) -> Array<'a, T>
        where T: Clone
    {
        let mut result = Array::with_capacity(slice.len(), arena);
        result.extend(slice);
        result
    }

    /// Returns the arena in use by the array.
    pub fn arena(&self) -> &'a Arena { self.arena }

    /// Returns the current capacity of the array.
    pub fn capacity(&self) -> usize { self.capacity }

    /// Clears the array, removing all elements.
    ///
    /// Note: does NOT call `Drop` on any element.
    pub fn clear(&mut self) { self.length = 0; }

    /// Inserts a new item at the designated index.
    ///
    /// Note: in the case where the array does not have enough capacity, it
    /// reallocates the underlying storage.
    ///
    /// Panics: if the index is invalid, which is not within [0..len()] in this
    /// specific case.
    pub fn insert(&mut self, index: usize, t: T) {
        assert!(index <= self.length);

        self.reserve(1);

        unsafe {
            if index < self.length {
                let src = self.ptr.offset(index as isize);
                ptr::copy(src, src.offset(1), self.length - index);
            }

            ptr::write(self.ptr.offset(index as isize), t);
        }
        self.length += 1;
    }

    /// Returns a reference to the item at the back of the array, if any.
    pub fn peek(&self) -> Option<&T> {
        if self.length > 0 {
            Some(unsafe { &*self.ptr.offset((self.length - 1) as isize) })
        } else {
            None
        }
    }

    /// Pop an item off the back of the array, if any.
    pub fn pop(&mut self) -> Option<T> {
        if self.length > 0 {
            self.length -= 1;
            Some(unsafe { ptr::read(self.ptr.offset(self.length as isize)) })
        } else {
            None
        }
    }

    /// Pushes a new item at the back of the array.
    ///
    /// Note: in the case where the array does not have enough capacity, it
    /// reallocates the underlying storage.
    pub fn push(&mut self, t: T) {
        self.reserve(1);
        unsafe {
            ptr::write(self.ptr.offset(self.length as isize), t);
        }
        self.length += 1;
    }

    /// Extends the array by appending the slice.
    ///
    /// Note: in the case where the array does not have enough capacity, it
    /// reallocates the underlying storage.
    pub fn extend(&mut self, slice: &[T]) where T: Clone {
        self.reserve(slice.len());

        for (offset, item) in slice.iter().enumerate() {
            unsafe {
                let dst = self.ptr.offset((self.length + offset) as isize);
                ptr::write(dst, item.clone());
            }
        }
        self.length += slice.len();
    }

    /// Reserves the necessary spaces for `n` more items.
    ///
    /// Note: if the capacity is already sufficient, has no effect, otherwise
    /// increases the capacity to at least be enough for `n` more items. This
    /// reallocates the underlying storage.
    pub fn reserve(&mut self, n: usize) {
        if self.length + n <= self.capacity { return; }

        let new_capacity = Array::<T>::next_power_of_two(self.length + n);
        assert!(new_capacity <= std::usize::MAX / 2 + 1);

        let new_ptr = self.arena.reserve::<T>(new_capacity);
        unsafe {
            ptr::copy_nonoverlapping(self.ptr, new_ptr, self.length);
        }

        self.capacity = new_capacity;
        self.ptr = new_ptr;
    }

    /// Extracts a slice containing the entire array.
    pub fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr, self.length) }
    }

    /// Extracts a mutable slice containing the entire array.
    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.length) }
    }

    /// Extracts the mutable slice containing the array elements.
    pub fn into_slice(self) -> &'a mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.length) }
    }
}

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

    /// Returns the current capacity of the array.
    pub fn capacity(&self) -> usize { self.0.capacity() }

    /// Clears the array, removing all elements.
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

impl<'target, T> CloneInto<'target> for [T]
    where
        T: CloneInto<'target> + Copy
{
    type Output = &'target [<T as CloneInto<'target>>::Output];

    fn clone_into(&self, arena: &'target Arena) -> Self::Output {
        let mut array = Array::with_capacity(self.len(), arena);

        for e in self {
            array.push(arena.intern(e));
        }

        array.into_slice()
    }
}

impl<'a, T: 'a + Clone> Clone for Array<'a, T> {
    fn clone(&self) -> Self {
        let mut new = Array::with_capacity(self.length, self.arena);
        new.extend(self.as_slice());
        new
    }
}

impl<'a, T: 'a + cmp::PartialEq> cmp::PartialEq for Array<'a, T> {
    fn eq(&self, other: &Self) -> bool { self.as_slice().eq(other.as_slice()) }
    fn ne(&self, other: &Self) -> bool { self.as_slice().ne(other.as_slice()) }
}

impl<'a, T: 'a + cmp::Eq> cmp::Eq for Array<'a, T> {}

impl<'a, T: 'a + cmp::PartialOrd> cmp::PartialOrd for Array<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }

    fn lt(&self, other: &Self) -> bool { self.as_slice().lt(other.as_slice()) }
    fn le(&self, other: &Self) -> bool { self.as_slice().le(other.as_slice()) }
    fn gt(&self, other: &Self) -> bool { self.as_slice().gt(other.as_slice()) }
    fn ge(&self, other: &Self) -> bool { self.as_slice().ge(other.as_slice()) }
}

impl<'a, T: 'a + cmp::Ord> cmp::Ord for Array<'a, T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl<'a, T: 'a + fmt::Debug> fmt::Debug for Array<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.as_slice())
    }
}

impl<'a, T: 'a + hash::Hash> hash::Hash for Array<'a, T> {
    fn hash<H>(&self, state: &mut H) where H: hash::Hasher {
        self.as_slice().hash(state)
    }
}

impl<'a, 'b, T: 'a> iter::IntoIterator for &'b Array<'a, T> {
    type Item = &'b T;
    type IntoIter = std::slice::Iter<'b, T>;

    fn into_iter(self) -> std::slice::Iter<'b, T> {
        self.as_slice().into_iter()
    }
}

impl<'a, T: 'a> ops::Deref for Array<'a, T> {
    type Target = [T];

    fn deref(&self) -> &[T] { self.as_slice() }
}

impl<'a, T: 'a> ops::DerefMut for Array<'a, T> {
    fn deref_mut(&mut self) -> &mut [T] { self.as_slice_mut() }
}

//
//  Implementation Details
//
struct ArenaImpl {
    pools: Vec<Vec<u8>>,
    pool_index: usize,
    next_index: usize
}

impl ArenaImpl {
    fn new() -> ArenaImpl {
        ArenaImpl {
            pools: vec![ArenaImpl::create_pool(4096); 2],
            pool_index: 0,
            next_index: 0
        }
    }

    fn capacity(&self) -> usize {
        self.pools.iter().map(|v| v.len()).sum()
    }

    fn used(&self) -> usize {
        let prev_pools_size: usize =
            self.pools[0..self.pool_index].iter().map(|v| v.len()).sum();
        prev_pools_size + self.next_index
    }

    fn contains<T>(&self, t: &T) -> bool {
        let address = t as *const _ as usize;
        for p in &self.pools {
            let start = p.as_ptr() as usize;
            let end = unsafe { p.as_ptr().offset(p.len() as isize) } as usize;
            
            if start <= address && address < end {
                return true;
            }
        }
        return false;
    }

    fn insert<'a, T: 'a>(&'a mut self, t: T) -> &'a mut T {
        unsafe {
            let spot = self.reserve::<T>(1);
            ptr::write(spot, t);
            &mut *spot
        }
    }

    fn insert_slice<'a, T: 'a>(&'a mut self, ts: &[T]) -> &'a mut [T] {
        if ts.is_empty() {
            return &mut [];
        }

        unsafe {
            let spot = self.reserve::<T>(ts.len());
            ptr::copy_nonoverlapping(ts.as_ptr(), spot, ts.len());
            slice::from_raw_parts_mut(spot, ts.len())
        }
    }

    fn reserve<'a, T: 'a>(&'a mut self, nb: usize) -> *mut T {
        if nb == 0 { return ptr::null_mut() }

        debug_assert!(mem::align_of::<T>() <= 8);
        assert!(mem::size_of::<T>() <= std::usize::MAX / nb);

        // 1. Ensure 8-bytes alignment for next type.
        let size = mem::size_of::<T>() * nb;
        let size = if size % 8 == 0 { size } else { size + (8 - size % 8) };

        // 2. Prepare memory
        while self.pools[self.pool_index].len() < self.next_index + size {
            if self.pool_index + 1 == self.pools.len() {
                let new_size = self.pools[self.pool_index].len() * 2;
                let new_pool = ArenaImpl::create_pool(new_size);
                self.pools.push(new_pool);
            }
            self.pool_index += 1;
            self.next_index = 0;
        }

        // 3. Place in memory
        let spot: &mut u8 = &mut self.pools[self.pool_index][self.next_index];
        self.next_index += size;
        spot as *mut u8 as *mut T
    }

    fn shrink_to_fit(&mut self) {
        self.pools.resize(self.pool_index + 1, vec!())
    }

    #[cfg(debug_assertions)]
    fn recycle(&mut self) {
        for v in &mut self.pools {
            let size = v.len();
            v.resize(size, 0x55);
        }
        self.pool_index = 0;
        self.next_index = 0;
    }

    #[cfg(not(debug_assertions))]
    fn recycle(&mut self) {
        self.pool_index = 0;
        self.next_index = 0;
    }

    fn create_pool(size: usize) -> Vec<u8> {
        let mut pool = Vec::with_capacity(size);
        pool.resize(size, 0);
        pool
    }
}

impl<'a, T: 'a> Array<'a, T> {
    fn next_power_of_two(n: usize) -> usize {
        //  http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
        let mut n = n;
        n -= 1;
        n |= n >> 1;
        n |= n >> 2;
        n |= n >> 4;
        n |= n >> 8;
        n |= n >> 16;
        if mem::size_of::<usize>() == 64 {
            n |= n >> 32;
        }
        n + 1
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::fmt;
    use super::{Arena, Array};

    #[test]
    fn arena_overview() {
        //  A simple brush test exercising a mixture of features:
        //  -   a mixed load of varied size and alignment, interspersed,
        //  -   the new/capacity/used/insert methods,
        //  -   the growth in insert,
        //  -   the fact that the arena does not overwrite the memory it holds.
        fn check<T>(left: &[&mut T], right: &[T])
            where T: fmt::Debug + PartialEq<T>
        {
            assert_eq!(left.len(), right.len());
            for i in 0..left.len() {
                assert_eq!(*left[i], right[i]);
            }
        }

        let arena = Arena::new();
        let mut v8 = vec!();
        let mut c8 = vec!();
        let mut v16 = vec!();
        let mut c16 = vec!();
        let mut v32 = vec!();
        let mut c32 = vec!();
        let mut v64 = vec!();
        let mut c64 = vec!();

        for i in 0..256 {
            v8.push(arena.insert(i as u8));
            c8.push(i as u8);
            v16.push(arena.insert(i as u16));
            c16.push(i as u16);
            v32.push(arena.insert(i as u32));
            c32.push(i as u32);
            v64.push(arena.insert(i as u64));
            c64.push(i as u64);
        }

        assert_eq!(8192, arena.capacity());
        assert_eq!(8192, arena.used());

        check(&v8, &c8);
        check(&v16, &c16);
        check(&v32, &c32);
        check(&v64, &c64);

        for i in 0..128 {
            v8.push(arena.insert(i as u8));
            c8.push(i as u8);
            v16.push(arena.insert(i as u16));
            c16.push(i as u16);
            v32.push(arena.insert(i as u32));
            c32.push(i as u32);
            v64.push(arena.insert(i as u64));
            c64.push(i as u64);
        }

        assert_eq!(16384, arena.capacity());
        assert_eq!(12288, arena.used());

        check(&v8, &c8);
        check(&v16, &c16);
        check(&v32, &c32);
        check(&v64, &c64);
    }

    #[test]
    fn array_overview() {
        let arena = Arena::new();
        let mut array = Array::new(&arena);
        let mut vec = Vec::new();

        for i in 0..256usize {
            array.push(i);
            vec.push(i);
        }

        assert_eq!(array.as_slice(), vec.as_slice());

        let used = arena.used();

        array.clear();
        vec.clear();

        for i in 0..256usize {
            array.push(i);
            vec.push(i);
        }

        assert_eq!(arena.used(), used);
        assert_eq!(array.as_slice(), vec.as_slice());
        assert_eq!(array.into_slice(), vec.as_slice());
    }
}
