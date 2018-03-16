//! Jagged Array.
//!
//! A Jagged Array is a fixed-size array of exponentially bigger arrays.
//!
//! The main advantage of the data-structure is that it allows pushing new
//! elements without displacing the existing ones, so that it is possible
//! to retain a reference to them externally across pushes.
//!
//! Additionally, if one extracts a *snapshot* of the current elements, this
//! snapshot can be accessed from other threads concurrently with modifications
//! of the underlying array from a single thread.
//!
//! Thread Safety:  JaggedArray is not Sync, yet JaggedArraySnapshot is Send.

use std::{cell, cmp, fmt, iter, marker, mem, ops, ptr, slice};

const NB_SLABS: usize = 21;

/// JaggedArray.
pub struct JaggedArray<T> {
    //  Number of initialized elements.
    length: cell::Cell<usize>,
    //  Number of allocated slabs.
    allocated: cell::Cell<usize>,
    //  Core
    core: cell::UnsafeCell<Core<T>>,
}

/// JaggedArraySnapshot
pub struct JaggedArraySnapshot<'a, T: 'a> {
    length: usize,
    core: &'a Core<T>,
}

/// JaggedArrayIterator
pub struct JaggedArrayIterator<'a, T: 'a> {
    current: usize,
    snapshot: JaggedArraySnapshot<'a, T>,
}

//
//  Public interface of JaggedArray
//
impl<T> JaggedArray<T> {
    /// Creates a new instance.
    ///
    /// The user specifices the log2 of the capacity of the first and second
    /// underlying arrays. Since each subsequent array is twice the size of its
    /// predecessor, this indirectly defines the maximum capacity.
    pub fn new(log2_initial: usize) -> JaggedArray<T> {
        JaggedArray {
            length: Default::default(),
            allocated: Default::default(),
            core: cell::UnsafeCell::new(Core::new(log2_initial)),
        }
    }

    /// Returns whether the array is empty, or not.
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    /// Returns the length of the array.
    pub fn len(&self) -> usize { self.length.get() }

    /// Returns the current capacity of the array.
    pub fn capacity(&self) -> usize {
        self.inner().slab_capacity_before(self.allocated.get())
    }

    /// Returns the maximum capacity of the array.
    pub fn max_capacity(&self) -> usize { self.inner().max_capacity() }

    /// Returns a snapshot.
    pub fn snapshot<'a>(&'a self) -> JaggedArraySnapshot<'a, T> {
        JaggedArraySnapshot::new(self)
    }

    /// Returns the element at index `i` if `i < self.len()` or None.
    pub fn get(&self, i: usize) -> Option<&T> {
        self.inner().get(i, self.len())
    }

    /// Returns the element at index `i` if `i < self.len()` or None.
    pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
        let length = self.len();
        self.inner_mut().get_mut(i, length)
    }

    /// Returns the longest contiguous slice starting at index `i`.
    pub fn get_slice(&self, i: usize) -> &[T] {
        self.inner().get_slice(i, self.len())
    }

    /// Returns the longest contiguous slice starting at index `i`.
    pub fn get_slice_mut(&mut self, i: usize) -> &mut [T] {
        let length = self.len();
        self.inner_mut().get_slice_mut(i, length)
    }

    /// Returns the element at index `i` if `i < self.len()`, otherwise the
    /// behavior is undefined.
    pub unsafe fn get_unchecked(&self, i: usize) -> &T {
        debug_assert!(i < self.len(), "{} < {}", i, self.len());
        self.inner().get_unchecked(i)
    }

    /// Returns the element at index `i` if `i < self.len()`, otherwise the
    /// behavior is undefined.
    pub unsafe fn get_unchecked_mut(&mut self, i: usize) -> &mut T {
        debug_assert!(i < self.len(), "{} < {}", i, self.len());
        self.inner_mut().get_unchecked_mut(i)
    }

    /// Clears the array, removing all values.
    ///
    /// The array retains its capacity; see shrink to deallocate unused memory.
    pub fn clear(&mut self) {
        self.inner_mut().clear(self.len());
        self.length.set(0);
    }

    /// Shrinks the array, deallocating unused memory.
    pub fn shrink(&mut self) {
        self.allocated.set(self.inner_mut().shrink(self.len()));
    }

    /// Pushes a new element at the back of the array.
    pub fn push(&self, e: T) {
        self.reserve(1);
        self.slab_for_push().push(e);
        self.length.set(self.len() + 1);
    }

    /// Reserves capacity for n new elements.
    pub fn reserve(&self, n: usize) {
        let target_capacity = self.len() + n;

        if target_capacity <= self.capacity() {
            return;
        }

        while target_capacity > self.capacity() {
            self.inner_mut().allocate_slab(self.allocated.get());
            self.allocated.set(self.allocated.get() + 1);
        }
    }
}

impl<T: Clone> JaggedArray<T> {
    /// Clones and appends all elements in the slice to the array.
    pub fn extend_clone(&self, mut other: &[T]) {
        self.reserve(other.len());

        while !other.is_empty() {
            let length = other.len();
            other = self.slab_for_push().extend_clone(other);
            self.length.set(self.len() + length - other.len());
        }
    }

    /// Clones and appends all elements in the slice to the array, contiguously.
    /// Pads the intervening elements, if necessary, by invoking "padder".
    pub fn extend_contiguous_clone_with<F>(&self, other: &[T], padder: F)
        where F: Fn() -> T,
    {
        let mut slab = self.prepare_extend_contiguous_with(other.len(), padder);
        slab.extend_clone(other);
        self.length.set(self.len() + other.len());
    }
}

impl<T: Clone + Default> JaggedArray<T> {
    /// Clones and appends all elements in the slice to the array, contiguously.
    /// Pads the intervening elements, if necessary, by invoking "padder".
    pub fn extend_contiguous_clone(&self, other: &[T]) {
        self.extend_contiguous_clone_with(other, Default::default);
    }
}

impl<T: Copy> JaggedArray<T> {
    /// Copies and appends all elements in the slice to the array.
    pub fn extend_copy(&self, mut other: &[T]) {
        self.reserve(other.len());

        while !other.is_empty() {
            let length = other.len();
            other = self.slab_for_push().extend_copy(other);
            self.length.set(self.len() + length - other.len());
        }
    }

    /// Copies and appends all elements in the slice to the array, contiguously.
    /// Pads the intervening elements, if necessary, by invoking "padder".
    pub fn extend_contiguous_copy_with<F>(&self, other: &[T], padder: F)
        where F: Fn() -> T,
    {
        let mut slab = self.prepare_extend_contiguous_with(other.len(), padder);
        slab.extend_copy(other);
        self.length.set(self.len() + other.len());
    }
}

impl<T: Copy + Default> JaggedArray<T> {
    /// Copies and appends all elements in the slice to the array, contiguously.
    /// Pads the intervening elements, if necessary, by invoking "padder".
    pub fn extend_contiguous_copy(&self, other: &[T]) {
        self.extend_contiguous_clone_with(other, Default::default);
    }
}

//
//  Public interface of JaggedArraySnapshot
//
impl<'a, T: 'a> JaggedArraySnapshot<'a, T> {
    /// Creates a new instance.
    pub fn new(array: &'a JaggedArray<T>) -> JaggedArraySnapshot<'a, T> {
        JaggedArraySnapshot {
            length: array.len(),
            core: array.inner(),
        }
    }

    /// Returns whether the array is empty, or not.
    pub fn is_empty(&self) -> bool { self.length == 0 }

    /// Returns the length of the array.
    pub fn len(&self) -> usize { self.length }

    /// Returns the element at index `i` if `i < self.len()` or None.
    pub fn get(&self, i: usize) -> Option<&'a T> {
        self.core.get(i, self.length)
    }

    /// Returns the longest contiguous slice starting at index `i`.
    pub fn get_slice(&self, i: usize) -> &'a [T] {
        self.core.get_slice(i, self.length)
    }

    /// Returns the element at index `i` if `i < self.len()`, otherwise the
    /// behavior is undefined.
    pub unsafe fn get_unchecked(&self, i: usize) -> &'a T {
        debug_assert!(i < self.length);
        self.core.get_unchecked(i)
    }
}

//
//  Public interface of JaggedArrayIterator
//

impl<'a, T: 'a> JaggedArrayIterator<'a, T> {
    /// Creates an instance.
    pub fn new(snapshot: JaggedArraySnapshot<'a, T>) -> Self {
        JaggedArrayIterator {
            current: 0,
            snapshot: snapshot,
        }
    }
}

//
//  Implementation Details
//

struct Slab<'a, T: 'a> {
    ptr: *const T,
    length: usize,
    _marker: marker::PhantomData<&'a T>,
}

struct SlabMut<'a, T: 'a> {
    ptr: *mut T,
    length: usize,
    capacity: usize,
    _marker: marker::PhantomData<&'a mut T>,
}

struct Core<T> {
    //  Slabs.
    slabs: [*mut T; NB_SLABS],
    //  Capacity of the first slab; any subsequent slab has a capacity equal
    //  to the sum of all previous slabs (doubling the overall capacity).
    log2_initial: usize,
    //  Owner.
    _marker: marker::PhantomData<T>,
}

impl<T> JaggedArray<T> {
    /// Returns a reference to the Core.
    fn inner(&self) -> &Core<T> {
        unsafe { &*(self.core.get() as *const Core<T>) }
    }

    /// Returns a mutable reference to the Core.
    fn inner_mut(&self) -> &mut Core<T> {
        unsafe { &mut *(self.core.get()) }
    }

    /// Prepare the array for contiguous insertion of specified length, padding
    /// with elements generated by "padder".
    fn prepare_extend_contiguous_with<F>(&self, length: usize, padder: F)
        -> SlabMut<T>
        where
            F: Fn() -> T,
    {
        if length == 0 { return SlabMut::default(); }

        loop {
            self.reserve(length);

            let mut slab = self.slab_for_push();
            let remaining = slab.remaining();

            if remaining >= length {
                return slab;
            }

            for _ in 0..remaining {
                slab.push(padder());
            }

            self.length.set(self.len() + remaining);
        }
    }

    /// Returns the last slab with room to spare, or an empty slab if none.
    fn slab_for_push(&self) -> SlabMut<T> {
        debug_assert!(self.allocated.get() > 0);
        debug_assert!(self.capacity() > self.len());

        let inner = self.inner_mut();
        let index = inner.slab_index(self.len());
        unsafe { inner.slab_mut(index, self.len()) }
    }
}

impl<'a, T: 'a> Slab<'a, T> {
    /// Creates a new instance.
    fn new(ptr: *const T, length: usize) -> Self {
        Slab { ptr: ptr, length: length, _marker: marker::PhantomData }
    }

    /// Get a slice of the elements.
    fn slice(&self) -> &'a [T] {
        unsafe { slice::from_raw_parts(self.ptr, self.length) }
    }
}

impl<'a, T: 'a> SlabMut<'a, T> {
    /// Creates a new instance.
    fn new(ptr: *mut T, length: usize, capacity: usize) -> Self {
        let _marker = marker::PhantomData;
        SlabMut { ptr, length, capacity, _marker }
    }

    /// Returns the remaining capacity of the Slab.
    fn remaining(&self) -> usize { self.capacity - self.length }

    /// Get a slice of the elements.
    fn slice(&self) -> &'a mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.length) }
    }

    /// Clear elements.
    fn clear(&mut self) {
        for i in 0..self.length {
            unsafe { ptr::drop_in_place(self.ptr.offset(i as isize)) }
        }
    }

    /// Push element.
    fn push(&mut self, e: T) {
        debug_assert!(
            self.length < self.capacity,
            "{} < {}", self.length, self.capacity
        );

        unsafe { ptr::write(self.ptr.offset(self.length as isize), e); }
        self.length += 1;
    }

    /// Extends the contents of the slab, cloning from the slice.
    ///
    /// Returns the remaining, non-cloned, elements from the slice.
    fn extend_clone<'b>(&mut self, slice: &'b [T]) -> &'b [T]
        where T: Clone
    {
        let (dst, source, result) = self.prepare_extend(slice);

        for (index, e) in source.iter().enumerate() {
            unsafe { ptr::write(dst.offset(index as isize), e.clone()); }
        }

        self.length += source.len();

        result
    }

    /// Extends the contents of the slab, copying from the slice.
    ///
    /// Returns the remaining, non-copied, elements from the slice.
    fn extend_copy<'b>(&mut self, slice: &'b [T]) -> &'b [T]
        where T: Copy
    {
        let (dst, source, result) = self.prepare_extend(slice);

        unsafe {
            ptr::copy_nonoverlapping(source.as_ptr(), dst, source.len());
        }

        self.length += source.len();

        result
    }

    /// Returns a triplet:
    /// -   pointer to the first element to copy into,
    /// -   slice of elements to copy,
    /// -   slice of left-over elements.
    fn prepare_extend<'b>(&mut self, slice: &'b [T])
        -> (*mut T, &'b [T], &'b [T])
    {
        let ptr = unsafe { self.ptr.offset(self.length as isize) };
        let free = self.capacity - self.length;

        let (source, leftover) = if free < slice.len() {
            slice.split_at(free)
        } else {
            (slice, &[] as &[T])
        };

        (ptr, source, leftover)
    }

}

impl<T> Core<T> {
    /// Creates a new instance.
    fn new(log2_initial: usize) -> Core<T> {
        Core {
            slabs: [ptr::null_mut(); NB_SLABS],
            log2_initial: log2_initial,
            _marker: marker::PhantomData,
        }
    }

    //
    //  Global Properties
    //

    /// Returns the maximum capacity of the array.
    #[inline(always)]
    fn max_capacity(&self) -> usize {
        1 << (self.log2_initial as usize + NB_SLABS - 1)
    }

    //
    //  Element Handling
    //
    /// Returns the element at index `i` if `i < self.len()` or None.
    fn get(&self, i: usize, length: usize) -> Option<&T> {
        if i >= length {
            None
        } else {
            unsafe { Some(self.get_unchecked(i)) }
        }
    }

    /// Returns the element at index `i` if `i < self.len()` or None.
    fn get_mut(&mut self, i: usize, length: usize) -> Option<&mut T> {
        if i >= length {
            None
        } else {
            unsafe { Some(self.get_unchecked_mut(i)) }
        }
    }

    /// Returns the longest contiguous slice starting at index `i`.
    pub fn get_slice(&self, i: usize, length: usize) -> &[T] {
        let (outer, inner) = self.split(i);
        unsafe { self.slab(outer, length) }.slice().split_at(inner).1
    }

    /// Returns the longest contiguous slice starting at index `i`.
    pub fn get_slice_mut(&mut self, i: usize, length: usize) -> &mut [T] {
        let (outer, inner) = self.split(i);
        unsafe { self.slab_mut(outer, length) }.slice().split_at_mut(inner).1
    }

    /// Returns the element at index `i` if `i < self.len()`, otherwise the
    /// behavior is undefined.
    unsafe fn get_unchecked(&self, i: usize) -> &T {
        let (outer, inner) = self.split(i);
        &*self.slabs.get_unchecked(outer).offset(inner as isize)
    }

    /// Returns the element at index `i` if `i < self.len()`, otherwise the
    /// behavior is undefined.
    unsafe fn get_unchecked_mut(&mut self, i: usize) -> &mut T {
        let (outer, inner) = self.split(i);
        &mut *self.slabs.get_unchecked(outer).offset(inner as isize)
    }

    /// Clear the elements.
    fn clear(&mut self, length: usize) {
        if length == 0 {
            return;
        }

        for index in self.slabs_used(length).rev() {
            unsafe { self.slab_mut(index, length) }.clear();
        }
    }

    /// Shrinks the array, deallocating unused memory.
    ///
    /// Returns the number of slabs still in use.
    fn shrink(&mut self, length: usize) -> usize {
        let first_free = self.split(length).0;

        for i in first_free..NB_SLABS {
            if self.slabs[i] == ptr::null_mut() {
                break;
            }
            self.deallocate_slab(i);
        }

        first_free
    }

    /// Splits an index into slab index + index in slab.
    ///
    /// The number of elements of slab `i` is:
    /// -   if `i == 0`, `initial`,
    /// -   otherwise, `initial * 2**(i - 1)`.
    ///
    /// That is: `initial * [1, 1, 2, 4, 8, 16, ...]`.
    fn split(&self, i: usize) -> (usize, usize) {
        let outer = self.slab_index(i);
        (outer, i - self.slab_capacity_before(outer))
    }

    //
    //  Slab handling
    //

    /// Returns the capacity before the slab at index i.
    #[inline(always)]
    fn slab_capacity_before(&self, i: usize) -> usize {
        if i == 0 { 0 }
        else { self.slab_capacity(i) }
    }

    /// Returns the capacity of the slab at index i.
    #[inline(always)]
    fn slab_capacity(&self, i: usize) -> usize {
        let slide = if i == 0 { 0 } else { i - 1 };
        1 << (self.log2_initial as usize + slide)
    }

    /// Returns the length of the slab at index i.
    #[inline(always)]
    fn slab_length(&self, i: usize, length: usize) -> usize {
        let capacity = self.slab_capacity(i);
        if length < capacity { 0 }
        else { cmp::min(length - capacity, capacity) }
    }

    /// Returns the index of the slab containing the element.
    #[inline(always)]
    fn slab_index(&self, i: usize) -> usize {
        let u = i >> self.log2_initial;
        mem::size_of::<usize>() * 8 - u.leading_zeros() as usize
    }

    /// Returns a range of indices of the used slabs.
    #[inline(always)]
    fn slabs_used(&self, length: usize) -> ops::Range<usize> {
        let last_used = self.split(length - 1).0;

        ops::Range { start: 0, end: last_used + 1 }
    }

    /// Allocates a new slab at index i.
    ///
    /// Panics: if the maximum capacity is already reached.
    fn allocate_slab(&mut self, i: usize) {
        assert!(i < NB_SLABS);
        debug_assert!(self.slabs[i] == ptr::null_mut());

        let capacity = self.slab_capacity(i);
        let mut v = Vec::with_capacity(capacity);
        let ptr = v.as_mut_ptr();
        mem::forget(v);

        self.slabs[i] = ptr;
    }

    /// Deallocates the slab at index i.
    ///
    /// Note:   Assumes that no element exist in the slab any longer.
    fn deallocate_slab(&mut self, i: usize) {
        debug_assert!(i < NB_SLABS);
        debug_assert!(self.slabs[i] != ptr::null_mut());

        let ptr = self.slabs[i];
        self.slabs[i] = ptr::null_mut();

        let capacity = self.slab_capacity(i);
        unsafe { Vec::from_raw_parts(ptr, 0, capacity) };
    }

    /// Returns the slab at index i.
    unsafe fn slab(&self, i: usize, length: usize) -> Slab<T> {
        let length = self.slab_length(i, length);
        Slab::new(*self.slabs.get_unchecked(i), length)
    }

    /// Returns the slab at index i.
    unsafe fn slab_mut(&mut self, i: usize, length: usize) -> SlabMut<T> {
        SlabMut::new(
            *self.slabs.get_unchecked(i),
            self.slab_length(i, length),
            self.slab_capacity(i)
        )
    }
}

impl<T: Clone> Core<T> {
    /// Clone the elements of other.
    fn clone(&mut self, other: &Core<T>, length: usize)  {
        for i in other.slabs_used(length) {
            debug_assert!(self.slabs[i] != ptr::null_mut());
            unsafe {
                self.slab_mut(i, 0)
                    .extend_clone(other.slab(i, length).slice());
            }
        }
    }
}

//
//  Implementation of traits for JaggedArray
//

impl<T: Clone> Clone for JaggedArray<T> {
    fn clone(&self) -> Self {
        let result = JaggedArray {
            length: Default::default(),
            allocated: Default::default(),
            core: cell::UnsafeCell::new(
                Core::new(self.inner().log2_initial as usize)
            ),
        };

        result.reserve(self.len());
        result.inner_mut().clone(self.inner(), self.len());

        result
    }
}

impl<T> Default for JaggedArray<T> {
    fn default() -> Self { JaggedArray::new(0) }
}

impl<T: fmt::Debug> fmt::Debug for JaggedArray<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", JaggedArraySnapshot::new(self))
    }
}

impl<'a, T: 'a> iter::IntoIterator for &'a JaggedArray<T> {
    type Item = &'a [T];
    type IntoIter = JaggedArrayIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        JaggedArraySnapshot::new(self).into_iter()
    }
}

impl<T> ops::Drop for JaggedArray<T> {
    fn drop(&mut self) {
        self.clear();
        self.shrink();
    }
}

unsafe impl<T: marker::Send> marker::Send for JaggedArray<T> {}

//
//  Implementation of traits for JaggedArraySnapshot
//

impl<'a, T: 'a> Clone for JaggedArraySnapshot<'a, T> {
    fn clone(&self) -> Self { *self }
}

impl<'a, T: 'a> Copy for JaggedArraySnapshot<'a, T> {}

impl<'a, T: fmt::Debug + 'a> fmt::Debug for JaggedArraySnapshot<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_empty() {
            return write!(f, "[]");
        }

        write!(f, "[ ")?;

        let mut separator = "";
        for slice in self {
            debug_assert!(slice.len() > 0);
            write!(f, "{}{:?}", separator, slice)?;
            separator = ", ";
        }

        write!(f, " ]")
    }
}

impl<'a, T: 'a> iter::IntoIterator for JaggedArraySnapshot<'a, T> {
    type Item = &'a [T];
    type IntoIter = JaggedArrayIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter { JaggedArrayIterator::new(self) }
}

impl<'a, 'b, T: 'a> iter::IntoIterator for &'b JaggedArraySnapshot<'a, T> {
    type Item = &'a [T];
    type IntoIter = JaggedArrayIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        JaggedArrayIterator::new(*self)
    }
}

unsafe impl<'a, T> marker::Send for JaggedArraySnapshot<'a, T>
    where
        T: marker::Sync + 'a
{}

//
//  Implementation of traits for JaggedArrayIterator
//

impl<'a, T: 'a> Clone for JaggedArrayIterator<'a, T> {
    fn clone(&self) -> Self { *self }
}

impl<'a, T: 'a> Copy for JaggedArrayIterator<'a, T> {}

impl<'a, T: 'a> iter::Iterator for JaggedArrayIterator<'a, T> {
    type Item = &'a [T];

    fn next(&mut self) -> Option<Self::Item> {
        let slice = self.snapshot.get_slice(self.current);

        if slice.is_empty() {
            None
        } else {
            self.current += slice.len();
            Some(slice)
        }
    }
}

//
//  Implementation of traits for internal structures
//

impl<'a, T: 'a> Default for Slab<'a, T> {
    fn default() -> Self { Slab::new(ptr::null(), 0) }
}

impl<'a, T: 'a> Default for SlabMut<'a, T> {
    fn default() -> Self { SlabMut::new(ptr::null_mut(), 0, 0) }
}

#[cfg(test)]
mod tests {
    use super::{JaggedArray, JaggedArraySnapshot};

    #[test]
    fn ensure_send() {
        fn check_send<T: Send>() {}

        check_send::<JaggedArray<i32>>();
        check_send::<JaggedArraySnapshot<'static, i32>>();
    }

    #[test]
    fn ensure_size() {
        use std::mem::size_of;

        const POINTER_SIZE: usize = size_of::<usize>();

        assert_eq!(size_of::<JaggedArray<i32>>(), 24 * POINTER_SIZE);
        assert_eq!(size_of::<JaggedArraySnapshot<'static, i32>>(), 2 * POINTER_SIZE);
    }

    #[test]
    fn brush() {
        let array = JaggedArray::new(0);
        assert_eq!(array.len(), 0);
        assert_eq!(array.capacity(), 0);

        array.push(1);
        assert_eq!(array.len(), 1);
        assert_eq!(array.capacity(), 1);

        array.push(2);
        assert_eq!(array.len(), 2);
        assert_eq!(array.capacity(), 2);

        array.push(3);
        assert_eq!(array.len(), 3);
        assert_eq!(array.capacity(), 4);

        assert_eq!(array.get(0), Some(&1));
        assert_eq!(array.get(1), Some(&2));
        assert_eq!(array.get(2), Some(&3));
        assert_eq!(array.get(3), None);

        let mut array = array;
        array.clear();

        assert_eq!(array.len(), 0);
        assert_eq!(array.capacity(), 4);
    }

    #[test]
    fn extend_contiguous() {
        let array = JaggedArray::new(0);
        assert_eq!(array.len(), 0);
        assert_eq!(array.capacity(), 0);

        array.extend_contiguous_copy(b"Hello, World!");
        assert_eq!(array.len(), 29);
        assert_eq!(array.capacity(), 32);

        assert_eq!(array.get(0), Some(&0));
        assert_eq!(array.get(15), Some(&0));
        assert_eq!(array.get_slice(16), b"Hello, World!");
    }

    #[test]
    fn iterator() {
        let array = JaggedArray::new(0);
        array.extend_copy(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);

        let mut iterator = array.into_iter();
        assert_eq!(iterator.next(), Some(&[0][..]));
        assert_eq!(iterator.next(), Some(&[1][..]));
        assert_eq!(iterator.next(), Some(&[2, 3][..]));
        assert_eq!(iterator.next(), Some(&[4, 5, 6, 7][..]));
        assert_eq!(iterator.next(), Some(&[8, 9][..]));

        assert_eq!(iterator.next(), None);

        array.push(10);

        assert_eq!(iterator.next(), None);
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn snapshot() {
        let array = JaggedArray::new(0);
        array.extend_copy(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);

        assert_eq!(array.get_slice(0), &[0]);
        assert_eq!(array.get_slice(1), &[1]);
        assert_eq!(array.get_slice(2), &[2, 3]);
        assert_eq!(array.get_slice(3), &[3]);
        assert_eq!(array.get_slice(4), &[4, 5, 6, 7]);
        assert_eq!(array.get_slice(5), &[5, 6, 7]);
        assert_eq!(array.get_slice(6), &[6, 7]);
        assert_eq!(array.get_slice(7), &[7]);
        assert_eq!(array.get_slice(8), &[8, 9]);
        assert_eq!(array.get_slice(9), &[9]);
        assert_eq!(array.get_slice(10), &[]);

        let snapshot = array.snapshot();
        array.push(10);

        assert_eq!(snapshot.get_slice(8), &[8, 9]);
        assert_eq!(array.get_slice(8), &[8, 9, 10]);

        assert_eq!(snapshot.get_slice(9), &[9]);
        assert_eq!(array.get_slice(9), &[9, 10]);

        assert_eq!(snapshot.get_slice(10), &[]);
        assert_eq!(array.get_slice(10), &[10]);
    }

    #[test]
    fn snapshot_debug() {
        let array = JaggedArray::new(0);
        array.extend_copy(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);

        let snapshot = JaggedArraySnapshot::new(&array);
        assert_eq!(format!("{:?}", snapshot), "[ [0], [1], [2, 3], [4, 5, 6, 7], [8, 9] ]");
    }
}
