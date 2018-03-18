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

impl std::ops::Deref for CodeFragment {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &*self.0
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

    /// Shifts range to the left.
    pub fn shift_left(self, n: usize) -> Range {
        Range { offset: self.offset - (n as u32), ..self }
    }

    /// Shifts range to the right.
    pub fn shift_right(self, n: usize) -> Range {
        Range { offset: self.offset + (n as u32), ..self }
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

impl std::default::Default for Range {
    fn default() -> Range { Range::new(0, 0) }
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}@{}", self.length, self.offset)
    }
}

impl std::ops::Index<Range> for [u8] {
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

impl<'a> std::fmt::Debug for Slice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

impl<'a> std::fmt::Display for Slice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
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
