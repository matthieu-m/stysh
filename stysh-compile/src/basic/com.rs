//! Common utilities
//!
//! A standard vocabulary used throughout the code.

use std;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Range {
    offset: u32,
    length: u32
}

impl Range {
    pub fn new(offset: usize, length: usize) -> Range {
        debug_assert!(offset <= std::u32::MAX as usize);
        debug_assert!(length <= std::u32::MAX as usize);
        debug_assert!(offset <= (std::u32::MAX as usize - length));
        Range { offset: offset as u32, length: length as u32 }
    }

    pub fn offset(self) -> usize { self.offset as usize }

    pub fn end_offset(self) -> usize { self.offset() + self.length() }

    pub fn length(self) -> usize { self.length as usize }

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
