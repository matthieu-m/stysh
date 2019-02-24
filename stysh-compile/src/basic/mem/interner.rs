//! Interner
//!
//! Maps each identifier or literal (excluding strings) to a unique ID which
//! can be used for quick comparison or for retrieving the original spelling.

use std::mem;

use basic::com::Range;
use super::{
    JaggedArray, JaggedArraySnapshot, JaggedHashMap, JaggedHashMapSnapshot
};

/// InternId
///
/// The unique ID associated with an identifier or literal.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct InternId(u32);

/// Interner
///
/// A bi-directional mapping from ID to spelling and vice-versa.
#[derive(Clone, Debug)]
pub struct Interner {
    //  A mapping InternId -> Range into strings.
    ranges: JaggedArray<Range>,
    //  The raw strings of the identifiers, laid out contiguously.
    strings: JaggedArray<u8>,
    //  A reverse mapping from strings to InternId.
    reverse: JaggedHashMap<&'static [u8], InternId>,
}

/// InternerSnapshot
///
/// A read-only reference to the Interner, capturing a snapshot of its state
/// at the moment the reference was obtained.
#[derive(Clone, Copy, Debug)]
pub struct InternerSnapshot<'a> {
    ranges: JaggedArraySnapshot<'a, Range>,
    strings: JaggedArraySnapshot<'a, u8>,
    reverse: JaggedHashMapSnapshot<'a, &'static [u8], InternId>,
}

//
//  Implementation of publc interface
//

impl InternId {
    /// Bool
    pub fn bool_() -> Self { InternId::from_builtin(0) }

    /// True
    pub fn true_() -> Self { InternId::from_builtin(1) }

    /// False
    pub fn false_() -> Self { InternId::from_builtin(2) }

    /// Int
    pub fn int() -> Self { InternId::from_builtin(3) }

    /// String
    pub fn string() -> Self { InternId::from_builtin(4) }
}

impl Interner {
    /// Creates a new instance.
    pub fn new() -> Self {
        Interner {
            ranges: JaggedArray::default(),
            strings: JaggedArray::new(5),
            reverse: JaggedHashMap::default(),
        }
    }

    /// Creates a snapshot.
    pub fn snapshot(&self) -> InternerSnapshot { InternerSnapshot::new(self) }

    /// Returns the number of interned strings.
    pub fn len(&self) -> usize { self.snapshot().len() }

    /// Returns the string associated to the InternId.
    pub fn get(&self, id: InternId) -> Option<&[u8]> { self.snapshot().get(id) }

    /// Returns the InternId associated to the string, if any.
    pub fn lookup(&self, string: &[u8]) -> Option<InternId> {
        self.snapshot().lookup(string)
    }

    /// Inserts a strings, returns its InternId.
    pub fn insert(&self, string: &[u8]) -> InternId {
        if let Some(id) = self.lookup(string) {
            return id;
        }

        let id = InternId::from_index(self.ranges.len());

        self.strings.extend_contiguous_copy(string);

        let range =
            Range::new(self.strings.len() - string.len(), string.len());
        self.ranges.push(range);

        //  Self-referential, the dirty way.
        {
            let slice = self.snapshot().from_range(range);
            let slice: &'static [u8] = unsafe { mem::transmute(slice) };
            self.reverse.insert(slice, id);
        }

        id
    }
}

impl<'a> InternerSnapshot<'a> {
    /// Creates a new instance.
    pub fn new(interner: &'a Interner) -> Self {
        InternerSnapshot {
            ranges: interner.ranges.snapshot(),
            strings: interner.strings.snapshot(),
            reverse: interner.reverse.snapshot(),
        }
    }

    /// Returns the number of interned strings.
    pub fn len(&self) -> usize {
        1 + ASCII.len() + BUILTIN.len() + self.reverse.len()
    }

    /// Returns the string associated to the InternId.
    pub fn get(&self, id: InternId) -> Option<&'a [u8]> {
        if id == Default::default() {
            return None;
        }

        magic_string_of(id)
            .or_else(|| builtin_string_of(id))
            .or_else(|| self.from_index(id.index().expect("Wasn't index!")))
    }

    /// Returns the InternId associated to the string, if any.
    pub fn lookup(&self, string: &[u8]) -> Option<InternId> {
        magic_id_of(string)
            .or_else(|| builtin_id_of(string))
            .or_else(|| self.reverse.get(string).cloned())
    }
}

//
//  Implementation details
//
const ASCII: &'static [u8] =
    b"................................\
      .!.#$%&'()*+,-./0123456789:;<=>?\
      @ABCDEFGHIJKLMNOPQRSTUVWXYZ[.]^_\
      `abcdefghijklmnopqrstuvwxyz{|}~.";

const BUILTIN: [&[u8]; 5] = [b"Bool", b"True", b"False", b"Int", b"String"];

impl InternId {
    fn from_magic(magic: usize) -> Self {
        InternId(magic as u32 + Self::magic_offset())
    }

    fn from_builtin(builtin: usize) -> Self {
        InternId(builtin as u32 + Self::builtin_offset())
    }

    fn from_index(index: usize) -> Self {
        InternId(index as u32 + Self::index_offset())
    }

    fn magic(&self) -> Option<usize> {
        if self.is_magic() {
            Some((self.0 - Self::magic_offset()) as usize)
        } else {
            None
        }
    }

    fn builtin(&self) -> Option<usize> {
        if self.is_builtin() {
            Some((self.0 - Self::builtin_offset()) as usize)
        } else {
            None
        }
    }

    fn index(&self) -> Option<usize> {
        if self.is_index() {
            Some((self.0 - Self::index_offset()) as usize)
        } else {
            None
        }
    }

    fn is_magic(&self) -> bool {
        self.0 < Self::builtin_offset() && self.0 >= Self::magic_offset()
    }

    fn is_builtin(&self) -> bool {
        self.0 < Self::index_offset() && self.0 >= Self::builtin_offset()
    }

    fn is_index(&self) -> bool { self.0 >= Self::index_offset() }

    fn magic_offset() -> u32 { 1 }

    fn builtin_offset() -> u32 { 200 }

    fn index_offset() -> u32 { 300 }
}

impl<'a> InternerSnapshot<'a> {
    fn from_index(&self, index: usize) -> Option<&'a [u8]> {
        self.ranges.get(index).map(|r| self.from_range(*r))
    }

    fn from_range(&self, range: Range) -> &'a [u8] {
        let offset = range.offset();
        let length = range.length();

        let slice = self.strings.get_slice(offset);
        assert!(
            slice.len() >= length,
            "{} >= {} (at {}), {:?}", slice.len(), length, offset, self.strings
        );

        &slice[..length]
    }
}

fn builtin_id_of(string: &[u8]) -> Option<InternId> {
    BUILTIN.iter().position(|s| s == &string).map(|i| InternId::from_builtin(i))
}

fn builtin_string_of(id: InternId) -> Option<&'static [u8]> {
    id.builtin().map(|i| BUILTIN[i])
}

fn magic_id_of(string: &[u8]) -> Option<InternId> {
    fn from_byte(c: usize) -> Option<InternId> {
        if c < ASCII.len() { Some(InternId::from_magic(c)) } else { None }
    }

    match string.len() {
        0 => Some(InternId::from_magic(0)),
        1 => from_byte(string[0] as usize + 1),
        _ => None,
    }
}

fn magic_string_of(id: InternId) -> Option<&'static [u8]> {
    id.magic()
        .map(|i| if i == 0 { b"" } else { &ASCII[(i - 1)..i] })
}

//
//  Implementation of traits
//

impl std::fmt::Debug for InternId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl Default for Interner {
    fn default() -> Self { Interner::new() }
}

impl Default for InternId {
    fn default() -> Self { InternId(0) }
}

impl Drop for Interner {
    fn drop(&mut self) {
        //  Because we implement self-referential the dirty way.
        self.reverse.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ensure_send() {
        fn check_send<T: Send>() {}

        check_send::<Interner>();
        check_send::<InternerSnapshot<'static>>();
    }

    #[test]
    fn offsets() {
        let ascii = ASCII.len() as u32;
        let builtin = BUILTIN.len() as u32;

        assert!(InternId::builtin_offset() >= ascii + InternId::magic_offset());
        assert!(InternId::index_offset() >= builtin + InternId::builtin_offset());
    }

    #[test]
    fn intern_magic() {
        let interner = Interner::default();

        assert_interned(&interner,  1, b"");
        assert_interned(&interner, 48, b".");
        assert_interned(&interner, 50, b"0");
        assert_interned(&interner, 67, b"A");
        assert_interned(&interner, 99, b"a");
    }

    #[test]
    fn intern_builtin() {
        let interner = Interner::default();

        assert_interned(&interner, InternId::bool_().0, b"Bool");
        assert_interned(&interner, InternId::true_().0, b"True");
        assert_interned(&interner, InternId::false_().0, b"False");
        assert_interned(&interner, InternId::int().0, b"Int");
        assert_interned(&interner, InternId::string().0, b"String");
    }

    #[test]
    fn intern_regular() {
        let interner = Interner::default();

        assert_eq!(interner.get(InternId(340)), None);

        let id = interner.insert(b"foo");
        assert_interned(&interner, id.0, b"foo");

        let other = interner.insert(b"bar");
        assert_ne!(id, other);

        assert_interned(&interner, id.0, b"foo");
        assert_interned(&interner, other.0, b"bar");
    }

    fn assert_interned(interner: &Interner, id: u32, expected: &[u8]) {
        let result = interner.get(InternId(id));
        assert_eq!(
            result, Some(expected),
            "Expected ID {} to map to {:?}, it mapped to {:?}",
            id, expected, result
        );

        let reverse = interner.lookup(expected);
        assert_eq!(reverse, Some(InternId(id)));

        let fresh = interner.insert(expected);
        assert_eq!(fresh.0, id);
    }
}
