//! Array
//!
//! A growable array built on top of an Arena.

use std::{cmp, fmt, hash, iter, mem, ops, ptr, slice, usize};

use basic::mem::Arena;

/// The Array structure
///
/// Similar to the standard Vec, but grows its memory from an arena instead.
pub struct Array<'a, T: 'a> {
    arena: &'a Arena,
    capacity: usize,
    length: usize,
    ptr: *mut T
}

//
//  Public interface
//
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
        assert!(new_capacity <= usize::MAX / 2 + 1);

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

//
//  Implementation of traits for Array
//
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
    type IntoIter = slice::Iter<'b, T>;

    fn into_iter(self) -> slice::Iter<'b, T> {
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
    use super::{Arena, Array};

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
