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
use std::{cell, mem, ops, ptr, slice};

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

    /// Returns the current capacity of the array.
    pub fn capacity(&self) -> usize { self.capacity }

    /// Clears the array, removing all elements.
    ///
    /// Note: does NOT call `Drop` on any element.
    pub fn clear(&mut self) { self.length = 0; }

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
    pub fn extend(&mut self, t: &[T]) {
        self.reserve(t.len());
        unsafe {
            let dst = self.ptr.offset(self.length as isize);
            ptr::copy(t.as_ptr(), dst, t.len());
        }
        self.length += t.len();
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
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.length) }
    }

    /// Extracts the mutable slice containing the array elements.
    pub fn into_slice(self) -> &'a mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.length) }
    }
}

impl<'a, T: 'a> ops::Deref for Array<'a, T> {
    type Target = [T];

    fn deref(&self) -> &[T] { self.as_slice() }
}

impl<'a, T: 'a> ops::DerefMut for Array<'a, T> {
    fn deref_mut(&mut self) -> &mut [T] { self.as_mut_slice() }
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
