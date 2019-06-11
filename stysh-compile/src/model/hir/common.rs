//! Common types.

use std::{cmp, convert, fmt, hash, marker, num};

use basic::{com, mem};
use basic::mem::DynArray;
use basic::sea::TableIndex;

use model::ast;

//  FIXME(matthieum): consider defining TypeId fully to avoid cyclic dependency.
use model::hir::Type;

//
//  Public Types (IDs)
//

/// Index of an Expr in the Tree.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ExpressionId(num::NonZeroU32);

/// Index of a T in the ValueTree.
//  #[manual(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Id<T: ?Sized>(num::NonZeroU32, marker::PhantomData<*const T>);

/// Index of a Path in the Tree.
pub type PathId = Id<[ItemIdentifier]>;

/// Index of a Pattern in the Tree.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct PatternId(num::NonZeroU32);

/// Index of a Type in the Tree.
pub type TypeId = Id<Type>;

/// A global value number.
///
/// Defaults to 0, which is considered an invalid value.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Gvn(pub u32);

/// An item identifier.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ItemIdentifier(pub mem::InternId, pub com::Range);

/// A value identifier.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ValueIdentifier(pub mem::InternId, pub com::Range);

//
//  Public Types
//

/// A built-in Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinType {
    /// A boolean.
    Bool,
    /// A 64-bits signed integer.
    Int,
    /// A String.
    String,
    /// An uninhabited type.
    Void,
}

/// A field.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Field {
    /// Index of the field.
    Index(u16, com::Range),
    /// Unresolved name of the field.
    Unresolved(ValueIdentifier),
}

/// A tuple with dynamic allocations.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct DynTuple<T> {
    /// The tuple fields.
    pub fields: DynArray<T>,
    /// The name of the fields, empty if unnamed, otherwise of equal length.
    pub names: DynArray<ValueIdentifier>,
}

/// A tuple.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<T> {
    /// The tuple fields.
    pub fields: Id<[T]>,
    /// The name of the fields, empty if unnamed, otherwise of equal length.
    pub names: Id<[ValueIdentifier]>,
}

//
//  Public interface (IDs)
//

impl ExpressionId {
    /// Creates a new instance.
    pub fn new(id: u32) -> Self { ExpressionId(non_zero(id)) }

    /// Creates an instance from an Expression Gvn.
    pub fn from_gvn(gvn: Gvn) -> Option<Self> {
        if gvn.0 >= Gvn::EXPRESSION_OFFSET && gvn.0 < Gvn::PATTERN_OFFSET {
            Some(ExpressionId::new(gvn.0 - Gvn::EXPRESSION_OFFSET))
        } else {
            None
        }
    }
}

impl PatternId {
    /// Creates a new instance.
    pub fn new(id: u32) -> Self { PatternId(non_zero(id)) }

    /// Creates an instance from an Pattern Gvn.
    pub fn from_gvn(gvn: Gvn) -> Option<Self> {
        if gvn.0 >= Gvn::PATTERN_OFFSET {
            Some(PatternId::new(gvn.0 - Gvn::PATTERN_OFFSET))
        } else {
            None
        }
    }
}

impl<T: ?Sized> Id<T> {
    /// Creates a new instance.
    pub fn new(id: u32) -> Self { Id(non_zero(id), marker::PhantomData) }

    /// Creates an empty instance.
    pub fn empty() -> Self { Id(empty_non_zero(), marker::PhantomData) }

    /// Returns whether the corresponding list is empty.
    pub fn is_empty(&self) -> bool { *self == Self::empty() }
}

impl Gvn {
    /// Converts Gvn into an ExpressionId, if possible.
    pub fn as_expression(self) -> Option<ExpressionId> {
        ExpressionId::from_gvn(self)
    }

    /// Converts Gvn into an PatternId, if possible.
    pub fn as_pattern(self) -> Option<PatternId> {
        PatternId::from_gvn(self)
    }
}

impl ItemIdentifier {
    /// Returns the InternId.
    pub fn id(&self) -> mem::InternId { self.0 }

    /// Sets the InternId.
    pub fn with_id(self, id: mem::InternId) -> Self {
        ItemIdentifier(id, self.1)
    }

    /// Sets the Range.
    pub fn with_range(self, range: com::Range) -> Self {
        ItemIdentifier(self.0, range)
    }

    /// Returns a sentinel instance of ItemIdentifier.
    pub fn unresolved() -> ItemIdentifier { Default::default() }
}

impl ValueIdentifier {
    /// Returns the InternId.
    pub fn id(&self) -> mem::InternId { self.0 }

    /// Sets the InternId.
    pub fn with_id(self, id: mem::InternId) -> Self {
        ValueIdentifier(id, self.1)
    }

    /// Sets the Range.
    pub fn with_range(self, range: com::Range) -> Self {
        ValueIdentifier(self.0, range)
    }
}

//
//  Public interface
//

impl Field {
    /// Returns the index.
    ///
    /// Panics: If the field is Unresolved.
    pub fn index(&self) -> u16 {
        use self::Field::*;

        match *self {
            Index(i, ..) => i,
            Unresolved(name) => panic!("Unresolved {:?}", name),
        }
    }
}

impl<T> DynTuple<T> {
    /// Returns a unit tuple.
    pub fn unit() -> Self {
        DynTuple { fields: Default::default(), names: Default::default() }
    }

    /// Returns the number of fields.
    pub fn len(&self) -> usize { self.fields.len() }
}

impl<T: Clone> DynTuple<T> {
    /// Returns the field of the tuple by index.
    ///
    /// Returns None if the field is Unresolved or the index too large.
    pub fn field(&self, field: Field) -> Option<T> {
        if let Field::Index(i, ..) = field {
            self.fields.get(i as usize)
        } else {
            None
        }
    }
}

impl<T> Tuple<T> {
    /// Creates a unit tuple.
    pub fn unit() -> Tuple<T> {
        Tuple { fields: Id::empty(), names: Id::empty() }
    }

    /// Creates a tuple with unnamed fields.
    pub fn unnamed(fields: Id<[T]>) -> Tuple<T> {
        Tuple { fields, names: Id::empty() }
    }
}

//
//  Private Interface
//

impl Gvn {
    const EXPRESSION_OFFSET: u32 = 1;
    const PATTERN_OFFSET: u32 = std::u32::MAX / 2;
}

fn default_non_zero() -> num::NonZeroU32 {
    //  Safety:
    //  -   u32::MAX is not 0.
    unsafe { num::NonZeroU32::new_unchecked(std::u32::MAX) }
}

fn empty_non_zero() -> num::NonZeroU32 {
    //  Safety:
    //  -   u32::MAX - 1 is not 0.
    unsafe { num::NonZeroU32::new_unchecked(std::u32::MAX - 1) }
}

fn non_zero(id: u32) -> num::NonZeroU32 {
    num::NonZeroU32::new(id + 1).expect("Non wrapping")
}

fn from_non_zero(id: num::NonZeroU32) -> u32 {
    id.get() - 1
}

//
//  Traits Implementations
//

impl<T: ?Sized> Clone for Id<T> {
    fn clone(&self) -> Self { Id(self.0, self.1) }
}

impl<T: ?Sized> Copy for Id<T> {}

impl fmt::Debug for ExpressionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        if self.0 == default_non_zero() {
            write!(f, "ExpressionId(default)")
        } else if self.0 == empty_non_zero() {
            write!(f, "ExpressionId(empty)")
        } else {
            write!(f, "ExpressionId({})", self.index())
        }
    }
}

impl fmt::Debug for PatternId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        if self.0 == default_non_zero() {
            write!(f, "PatternId(default)")
        } else if self.0 == empty_non_zero() {
            write!(f, "PatternId(empty)")
        } else {
            write!(f, "PatternId({})", self.index())
        }
    }
}

impl<T: ?Sized> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        //
        //  FIXME(matthieum): consider adding `std::intrinsics::type_name<T>()`
        //  once it stabilizes.
        if self.0 == default_non_zero() {
            write!(f, "Id(default)")
        } else if self.0 == empty_non_zero() {
            write!(f, "Id(empty)")
        } else {
            write!(f, "Id({})", self.index())
        }
    }
}

impl fmt::Debug for Gvn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match (self.as_expression(), self.as_pattern()) {
            (Some(e), None) => write!(f, "Gvn({:?})", e),
            (None, Some(p)) => write!(f, "Gvn({:?})", p),
            (..) => write!(f, "Gvn(-)"),
        }
    }
}

impl std::fmt::Debug for ItemIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "ItemIdentifier({:?}, {})", self.0, self.1)
    }
}

impl fmt::Debug for ValueIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "ValueIdentifier({:?}, {})", self.0, self.1)
    }
}

impl Default for ExpressionId {
    fn default() -> Self { ExpressionId(default_non_zero()) }
}

impl Default for PatternId {
    fn default() -> Self { PatternId(default_non_zero()) }
}

impl<T: ?Sized> Default for Id<T> {
    fn default() -> Self { Id(default_non_zero(), marker::PhantomData) }
}

impl Default for Field {
    fn default() -> Self { Field::Index(0, Default::default()) }
}

impl<T> Default for DynTuple<T> {
    fn default() -> Self {
        DynTuple { fields: Default::default(), names: Default::default() }
    }
}

impl<T> Default for Tuple<T> {
    fn default() -> Self {
        Tuple { fields: Default::default(), names: Default::default() }
    }
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Field::*;

        match *self {
            Index(i, ..) => write!(f, "{}", i),
            Unresolved(name) => write!(f, "{:?}", name),
        }
    }
}

impl fmt::Display for ItemIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "<{}>", self.1)
    }
}

impl<T> fmt::Display for DynTuple<T>
    where
        T: Clone + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "(")?;
        for (i, e) in self.fields.iter().enumerate() {
            if i != 0 { write!(f, ", ")? }
            write!(f, "{}", e)?;
        }
        write!(f, ")")
    }
}

impl<T: ?Sized> cmp::Eq for Id<T> {}

impl convert::From<ExpressionId> for Gvn {
    fn from(id: ExpressionId) -> Self {
        Gvn(from_non_zero(id.0).wrapping_add(Gvn::EXPRESSION_OFFSET))
    }
}

impl convert::From<PatternId> for Gvn {
    fn from(id: PatternId) -> Self {
        Gvn(from_non_zero(id.0).wrapping_add(Gvn::PATTERN_OFFSET))
    }
}

impl convert::From<ast::TypeIdentifier> for ItemIdentifier {
    fn from(value: ast::TypeIdentifier) -> Self {
        ItemIdentifier(value.0, value.1)
    }
}

impl convert::From<ast::VariableIdentifier> for ValueIdentifier {
    fn from(value: ast::VariableIdentifier) -> Self {
        ValueIdentifier(value.id(), com::Span::span(&value))
    }
}

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

impl com::Span for Field {
    /// Returns the range spanned by the field.
    fn span(&self) -> com::Range {
        use self::Field::*;

        match *self {
            Index(_, r) => r,
            Unresolved(n) => n.1,
        }
    }
}

impl com::Span for ItemIdentifier {
    /// Returns the range spanned by the ItemIdentifier.
    fn span(&self) -> com::Range { self.1 }
}

impl com::Span for ValueIdentifier {
    /// Returns the range spanned by the ValueIdentifier.
    fn span(&self) -> com::Range { self.1 }
}

impl TableIndex for ExpressionId {
    fn from_index(index: usize) -> Self { ExpressionId::new(index as u32) }

    fn index(&self) -> usize { from_non_zero(self.0) as usize }
}

impl TableIndex for PatternId {
    fn from_index(index: usize) -> Self { PatternId::new(index as u32) }

    fn index(&self) -> usize { from_non_zero(self.0) as usize }
}

impl<T: ?Sized> TableIndex for Id<T> {
    fn from_index(index: usize) -> Self { Id::new(index as u32) }

    fn index(&self) -> usize { (self.0.get() - 1) as usize }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_id() {
        assert_eq!(ExpressionId::new(0).index(), 0);
        assert_eq!(ExpressionId::new(5).index(), 5);

        assert_eq!(
            Gvn::from(ExpressionId::new(0)).as_expression(),
            Some(ExpressionId::new(0))
        );
    }

    #[test]
    fn pattern_id() {
        assert_eq!(PatternId::new(0).index(), 0);
        assert_eq!(PatternId::new(5).index(), 5);

        assert_eq!(
            Gvn::from(PatternId::new(0)).as_pattern(),
            Some(PatternId::new(0))
        );
    }
}
