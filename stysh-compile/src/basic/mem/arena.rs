//! Arena
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

use std::{cell, fmt, mem, ptr, slice, usize};

use crate::basic::com::Range;
use crate::basic::mem::Array;

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

//
//  Public interface
//
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

//
//  Implementations of CloneInto
//
impl<'target> CloneInto<'target> for u32 {
    type Output = u32;

    fn clone_into(&self, _: &'target Arena) -> Self::Output { *self }
}

impl<'target> CloneInto<'target> for Range {
    type Output = Range;

    fn clone_into(&self, _: &'target Arena) -> Self::Output { *self }
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

//
//  Implementation of traits
//
impl fmt::Debug for Arena {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{}}")
    }
}

impl Default for Arena {
    fn default() -> Self { Arena::new() }
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
        assert!(mem::size_of::<T>() <= usize::MAX / nb);

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

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::fmt;
    use super::Arena;

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
}
