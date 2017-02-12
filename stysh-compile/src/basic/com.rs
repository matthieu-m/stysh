//! Common utilities
//!
//! A standard vocabulary used throughout the code.

use std;

/// A fragment of source code.
#[derive(Clone)]
pub struct CodeFragment(std::sync::Arc<Vec<u8>>);

impl CodeFragment {
    /// Creates a new `CodeFragment`.
    pub fn new(code: Vec<u8>) -> CodeFragment {
        CodeFragment(std::sync::Arc::new(code))
    }
}

impl std::ops::Index<Range> for CodeFragment {
    type Output = [u8];

    fn index(&self, index: Range) -> &[u8] {
        &(*self.0)[index.offset()..index.end_offset()]
    }
}

impl std::ops::Index<std::ops::Range<usize>> for CodeFragment {
    type Output = [u8];

    fn index(&self, index: std::ops::Range<usize>) -> &[u8] {
        &(*self.0)[index]
    }
}

impl std::ops::Index<std::ops::RangeFrom<usize>> for CodeFragment {
    type Output = [u8];

    fn index(&self, index: std::ops::RangeFrom<usize>) -> &[u8] {
        &(*self.0)[index]
    }
}

impl std::ops::Index<std::ops::RangeFull> for CodeFragment {
    type Output = [u8];

    fn index(&self, index: std::ops::RangeFull) -> &[u8] {
        &(*self.0)[index]
    }
}

impl std::ops::Index<std::ops::RangeTo<usize>> for CodeFragment {
    type Output = [u8];

    fn index(&self, index: std::ops::RangeTo<usize>) -> &[u8] {
        &(*self.0)[index]
    }
}

/// A library identifier.
///
/// Note:   The `LibraryId` is unique within a compilation, but unstable.
///
/// Note:   0 is reserved for the built-in types.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct LibraryId(u32);

impl LibraryId {
    /// Creates a new `LibraryId`, it MUST be unique within a compilation.
    pub fn new(id: u32) -> LibraryId { LibraryId(id) }
}

/// A module identifier.
///
/// Note:   the `ModuleId` is unique within a compilation, but unstable.
///
/// Note:   (0, 0) is reserved for the built-in types.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ModuleId {
    library: LibraryId,
    index: u32,
}

impl ModuleId {
    /// Creates a new `ModuleId`, it MUST be unique within a compilation.
    pub fn new(library: LibraryId, index: u32) -> ModuleId {
        ModuleId { library: library, index: index }
    }

    /// Returns the LibraryId of the library to which this module belongs.
    pub fn library(self) -> LibraryId { self.library }
}

/// A Range represents a start and end position in a buffer.
///
/// Note:   the `Range` does not know which buffer it indexes in.
///
/// Note:   a `Range` cannot index past 4GB.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
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

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::Range;

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
