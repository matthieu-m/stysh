//! Common utilities
//!
//! A standard vocabulary used throughout the code.

use std::{self, cmp, convert, fmt, hash, marker, num, ops, sync};

use basic::sea::TableIndex;

/// A fragment of source code.
#[derive(Clone)]
pub struct CodeFragment(sync::Arc<Vec<u8>>);

impl CodeFragment {
    /// Creates a new `CodeFragment`.
    pub fn new(code: Vec<u8>) -> CodeFragment {
        CodeFragment(sync::Arc::new(code))
    }
}

impl ops::Deref for CodeFragment {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &*self.0
    }
}

/// The core implementation of a u32-based ID.
///
/// The ID can be any number in the `[0, u32::MAX - 2]` range:
/// -   `u32::MAX` is reserved to enable size optimizations (Option).
/// -   `u32::MAX - 1` is reserved to denote Default constructed IDs.
///
/// IDs built on top of `CoreId` may reserve further numbers for their own ends.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CoreId(num::NonZeroU32);

impl CoreId {
    /// Creates a new instance.
    ///
    /// # Panics
    ///
    /// Panics if the integer provided is `u32::MAX`.
    pub fn new(id: u32) -> CoreId {
        if id == std::u32::MAX {
            panic!("Unsuitable ID: {}", id);
        }
        unsafe { CoreId(num::NonZeroU32::new_unchecked(id + 1)) }
    }

    /// Get the raw ID.
    pub fn raw(&self) -> u32 { self.0.get() - 1 }
}

impl fmt::Debug for CoreId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.raw())
    }
}

impl Default for CoreId {
    fn default() -> CoreId {
        unsafe { CoreId(num::NonZeroU32::new_unchecked(std::u32::MAX)) }
    }
}

impl fmt::Display for CoreId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.raw())
    }
}

impl convert::From<CoreId> for u32 {
    fn from(core_id: CoreId) -> u32 { core_id.raw() }
}

/// An Id implementation based on CoreId.
///
/// It contains a default empty state, to represent empty streams.
//  #[manual(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Id<T: ?Sized>(CoreId, marker::PhantomData<*const T>);

impl<T: ?Sized> Id<T> {
    /// Creates a new instance.
    pub fn new(id: u32) -> Self { Id(CoreId::new(id), marker::PhantomData) }

    /// Creates an empty instance.
    pub fn empty() -> Self { Self::new(std::u32::MAX - 2) }

    /// Returns whether the corresponding list is empty.
    pub fn is_empty(&self) -> bool { *self == Self::empty() }
}

impl<T: ?Sized> Clone for Id<T> {
    fn clone(&self) -> Self { *self }
}

impl<T: ?Sized> Copy for Id<T> {}

impl<T: ?Sized> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        //
        //  FIXME(matthieum): consider adding `std::intrinsics::type_name<T>()`
        //  once it stabilizes.
        if *self == Default::default() {
            write!(f, "Id(default)")
        } else if *self == Self::empty() {
            write!(f, "Id(empty)")
        } else {
            write!(f, "Id({})", self.index())
        }
    }
}

impl<T: ?Sized> Default for Id<T> {
    fn default() -> Self { Id(Default::default(), marker::PhantomData) }
}

impl<T: ?Sized> cmp::Eq for Id<T> {}

impl<T: ?Sized> hash::Hash for Id<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: ?Sized> cmp::Ord for Id<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering { self.0.cmp(&other.0) }
}

impl<T: ?Sized> cmp::PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool { self.0.eq(&other.0) }
}

impl<T: ?Sized> cmp::PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T: ?Sized> TableIndex for Id<T> {
    fn from_index(index: usize) -> Self { Id::new(index as u32) }

    fn index(&self) -> usize { self.0.raw() as usize }
}

/// A Range represents a start and end position in a buffer.
///
/// Note:   the `Range` does not know which buffer it indexes in.
///
/// Note:   a `Range` cannot index past 4GB.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Range {
    offset: u32,
    length: u32,
}

impl Range {
    /// Creates a new `Range` from a start position and length.
    ///
    /// In Debug, it is checked that the end position will not exceed 4GB.
    pub fn new(offset: usize, length: usize) -> Range {
        debug_assert!(offset <= std::u32::MAX as usize);
        debug_assert!(length <= std::u32::MAX as usize);
        debug_assert!(offset <= (std::u32::MAX as usize - length));
        Range { offset: offset as u32, length: length as u32 }
    }

    /// Returns the start position of the range.
    pub fn offset(self) -> usize { self.offset as usize }

    /// Returns the end position of the range (excluded).
    pub fn end_offset(self) -> usize { self.offset() + self.length() }

    /// Returns the length of the range.
    pub fn length(self) -> usize { self.length as usize }

    /// Shifts range to the left.
    pub fn shift_left(self, n: usize) -> Range {
        self.shift_to(self.offset() - n)
    }

    /// Shifts range to the right.
    pub fn shift_right(self, n: usize) -> Range {
        self.shift_to(self.offset() + n)
    }

    /// Shifts range to specified offset.
    pub fn shift_to(self, offset: usize) -> Range {
        Range { offset: offset as u32, ..self }
    }

    /// Skips n from the left.
    pub fn skip_left(self, n: usize) -> Range {
        Range {
            offset: self.offset + (n as u32),
            length: self.length - (n as u32),
        }
    }

    /// Skips n from the right.
    pub fn skip_right(self, n: usize) -> Range {
        Range {
            offset: self.offset,
            length: self.length - (n as u32),
        }
    }

    /// Extend one range with another, the resulting range spans both ranges,
    /// and in the case they were discontiguous also spans the interval.
    pub fn extend(self, other: Range) -> Range {
        if self.offset > other.offset {
            other.extend(self)
        } else if self.end_offset() >= other.end_offset() {
            self
        } else {
            Range {
                offset: self.offset,
                length: (other.end_offset() - self.offset()) as u32
            }
        }
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}@{}", self.length, self.offset)
    }
}

impl Default for Range {
    fn default() -> Range { Range::new(0, 0) }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}@{}", self.length, self.offset)
    }
}

impl ops::Index<Range> for [u8] {
    type Output = [u8];

    fn index(&self, index: Range) -> &[u8] {
        &self[index.offset()..index.end_offset()]
    }
}

/// A Slice of bytes, printed more pleasantly
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Slice<'a>(pub &'a [u8]);

impl<'a> Slice<'a> {
    /// Returns true if empty, false otherwise.
    pub fn is_empty(&self) -> bool { self.0.is_empty() }

    /// Returns the length of the slice.
    pub fn len(&self) -> usize { self.0.len() }

    /// Returns the byte at the indicated position, or None if it is invalid.
    pub fn get(&self, pos: usize) -> Option<&u8> { self.0.get(pos) }
}

impl<'a> fmt::Debug for Slice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self)
    }
}

impl<'a> fmt::Display for Slice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut start = 0;
        while start < self.0.len() {
            let end =
                self.0[start..].iter().position(|&b| b < 32 || b > 126)
                    .unwrap_or(self.len());

            f.write_str(
                std::str::from_utf8(&self.0[start..end]).expect("Valid UTF-8")
            )?;

            start = end;

            let end =
                self.0[start..].iter().position(|&b| b >= 32 && b <= 126)
                    .unwrap_or(self.len());

            for &byte in &self.0[start..end] {
                write!(f, "{{0x{:X}}}", byte)?;
            }

            start = end;
        }

        Ok(())
    }
}

/// Span
pub trait Span {
    /// Returns the Range spanned by the element.
    fn span(&self) -> Range;
}

/// A Store trait, to abstract over the actual storage of individual elements.
pub trait Store<T, I = Id<T>> {
    /// Returns the number of items.
    fn len(&self) -> usize;

    /// Returns a copy of the item.
    fn get(&self, id: I) -> T;

    /// Returns the range of the item.
    fn get_range(&self, id: I) -> Range;

    /// Pushes an item.
    fn push(&mut self, item: T, range: Range) -> I;
}

/// A MultiStore trait, to abstract over the actual storage of slices.
pub trait MultiStore<T, I = Id<[T]>> {
    /// Returns the slice of items.
    fn get_slice(&self, id: I) -> &[T];

    /// Pushes a slice of element.
    fn push_slice(&mut self, items: &[T]) -> I;
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::{CoreId, Range};

    #[test]
    fn core_id_roundtrip() {
        for i in 0..10 {
            assert_eq!(i, CoreId::new(i).raw());
        }
    }

    #[test]
    fn core_id_default() {
        let core: CoreId = Default::default();
        assert_eq!(std::u32::MAX - 1, core.raw());
    }

    #[test]
    #[should_panic]
    fn core_id_reserved_size_optimization() { CoreId::new(std::u32::MAX); }

    #[test]
    fn range_extend_contiguous() {
        let result = Range::new(3, 4).extend(Range::new(7, 2));
        assert_eq!(result, Range::new(3, 6));
    }

    #[test]
    fn range_extend_separated() {
        let result = Range::new(3, 4).extend(Range::new(11, 3));
        assert_eq!(result, Range::new(3, 11));
    }

    #[test]
    fn range_extend_partially_overlapping() {
        let result = Range::new(3, 4).extend(Range::new(5, 3));
        assert_eq!(result, Range::new(3, 5));
    }

    #[test]
    fn range_extend_totally_overlapping() {
        let result = Range::new(3, 4).extend(Range::new(5, 2));
        assert_eq!(result, Range::new(3, 4));
    }

    #[test]
    fn range_extend_reversed() {
        let result = Range::new(5, 3).extend(Range::new(3, 4));
        assert_eq!(result, Range::new(3, 5));
    }
}
