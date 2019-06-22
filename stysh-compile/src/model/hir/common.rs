//! Common types.

use std::{convert, fmt};

use basic::{com, mem};
use basic::mem::DynArray;
use basic::sea::TableIndex;

use model::ast;

//
//  Public Types (IDs)
//
pub use self::com::Id;

/// Index of an Expr in the Tree.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ExpressionId(com::CoreId);

/// Index of a Path in the Tree.
pub type PathId = Id<[ItemIdentifier]>;

/// Index of a Pattern in the Tree.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct PatternId(com::CoreId);

/// Index of a Type in the Tree.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeId(com::CoreId);

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
    pub fn new(id: u32) -> Self { ExpressionId(com::CoreId::new(id)) }

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
    pub fn new(id: u32) -> Self { PatternId(com::CoreId::new(id)) }

    /// Creates an instance from an Pattern Gvn.
    pub fn from_gvn(gvn: Gvn) -> Option<Self> {
        if gvn.0 >= Gvn::PATTERN_OFFSET {
            Some(PatternId::new(gvn.0 - Gvn::PATTERN_OFFSET))
        } else {
            None
        }
    }
}

impl TypeId {
    /// Creates a new instance.
    pub fn new(id: u32) -> Self { TypeId(com::CoreId::new(id)) }

    /// Creates a new instance of a Bool TypeId.
    pub fn bool_() -> Self { TypeId::new(TypeId::BOOL_ID) }

    /// Creates a new instance of a Int TypeId.
    pub fn int() -> Self { TypeId::new(TypeId::INT_ID) }

    /// Creates a new instance of a String TypeId.
    pub fn string() -> Self { TypeId::new(TypeId::STRING_ID) }

    /// Creates a new instance of a Void TypeId.
    pub fn void() -> Self { TypeId::new(TypeId::VOID_ID) }

    /// Returns whether the corresponding Type is a built-in.
    pub fn is_builtin(&self) -> bool { self.builtin().is_some() }

    /// Converts to a BuiltinType, if possible.
    pub fn builtin(&self) -> Option<BuiltinType> {
        match self.0.raw() {
            t if t == TypeId::BOOL_ID => Some(BuiltinType::Bool),
            t if t == TypeId::INT_ID => Some(BuiltinType::Int),
            t if t == TypeId::STRING_ID => Some(BuiltinType::String),
            t if t == TypeId::VOID_ID => Some(BuiltinType::Void),
            _ => None
        }
    }
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

impl TypeId {
    const BOOL_ID: u32 = std::u32::MAX - 3;
    const INT_ID: u32 = std::u32::MAX - 4;
    const STRING_ID: u32 = std::u32::MAX - 5;
    const VOID_ID: u32 = std::u32::MAX - 6;
}

//
//  Traits Implementations
//

impl fmt::Debug for ExpressionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        if *self == Default::default() {
            write!(f, "ExpressionId(default)")
        } else {
            write!(f, "ExpressionId({})", self.index())
        }
    }
}

impl fmt::Debug for PatternId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        if *self == Default::default() {
            write!(f, "PatternId(default)")
        } else {
            write!(f, "PatternId({})", self.index())
        }
    }
}

impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //  More compact representation for `{:#?}`.
        if *self == Default::default() {
            write!(f, "TypeId(default)")
        } else if let Some(t) = self.builtin() {
            write!(f, "TypeId({:?})", t)
        } else {
            write!(f, "TypeId({})", self.index())
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

impl convert::From<BuiltinType> for TypeId {
    fn from(b: BuiltinType) -> Self {
        match b {
            BuiltinType::Bool => TypeId::bool_(),
            BuiltinType::Int => TypeId::int(),
            BuiltinType::String => TypeId::string(),
            BuiltinType::Void => TypeId::void(),
        }
    }
}

impl convert::From<ExpressionId> for Gvn {
    fn from(id: ExpressionId) -> Self {
        Gvn(id.0.raw().wrapping_add(Gvn::EXPRESSION_OFFSET))
    }
}

impl convert::From<PatternId> for Gvn {
    fn from(id: PatternId) -> Self {
        Gvn(id.0.raw().wrapping_add(Gvn::PATTERN_OFFSET))
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

    fn index(&self) -> usize { self.0.raw() as usize }
}

impl TableIndex for PatternId {
    fn from_index(index: usize) -> Self { PatternId::new(index as u32) }

    fn index(&self) -> usize { self.0.raw() as usize }
}

impl TableIndex for TypeId {
    fn from_index(index: usize) -> Self { TypeId::new(index as u32) }

    fn index(&self) -> usize { self.0.raw() as usize }
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

    #[test]
    fn type_id() {
        assert_eq!(TypeId::new(0).index(), 0);
        assert_eq!(TypeId::new(5).index(), 5);
    }

    #[test]
    fn type_id_builtin() {
        use self::BuiltinType::*;

        assert_eq!(Some(Bool), TypeId::bool_().builtin());
        assert_eq!(Some(Int), TypeId::int().builtin());
        assert_eq!(Some(String), TypeId::string().builtin());
        assert_eq!(Some(Void), TypeId::void().builtin());

        for &b in &[Bool, Int, String, Void] {
            assert_eq!(Some(b), TypeId::from(b).builtin());
        }

        assert_eq!(None, TypeId::default().builtin());
        assert_eq!(None, TypeId::new(0).builtin());
    }
}
