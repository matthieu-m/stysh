//! Common utilities
//!
//! A standard vocabulary used throughout the code.

use std;

/// A Range represents a start and end position in a buffer.
///
/// Note:   the `Range` does not know which buffer it indexes in.
///
/// Note:   a `Range` cannot index past 4GB.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Range {
    offset: u32,
    length: u32
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
