//! Items

use std::{convert, fmt};

use basic::com::{self, Span};
use basic::mem::{self, DynArray};

use model::ast;
use model::hir::*;

/// A registry of the definitions
pub trait Registry: fmt::Debug {
    /// Get the definition of the enum.
    fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum>;

    /// Get the definition of the record.
    fn lookup_record(&self, id: ItemIdentifier) -> Option<Record>;
}

/// A full-fledged item.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item {
    /// A full-fledged enum definition.
    Enum(Enum),
    /// A full-fledged function definition.
    Fun(Function),
    /// A full-fledged record definition.
    Rec(Record),
}

/// A function argument.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Argument {
    /// The name.
    pub name: ValueIdentifier,
    /// The type.
    pub type_: Type,
    /// The range.
    pub range: com::Range,
    /// The GVN.
    pub gvn: Gvn,
}

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

/// An enum.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum {
    /// The prototype.
    pub prototype: EnumProto,
    /// The variants.
    pub variants: DynArray<Record>,
}

/// An enum prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EnumProto {
    /// The enum identifier.
    pub name: ItemIdentifier,
    /// The enum range.
    pub range: com::Range,
}

/// A function.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function {
    /// The prototype.
    pub prototype: FunctionProto,
    /// The body.
    pub body: Value,
}

/// A function prototype.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionProto {
    /// The function identifier.
    pub name: ItemIdentifier,
    /// The function prototype range.
    pub range: com::Range,
    /// The function arguments (always arguments).
    pub arguments: DynArray<Argument>,
    /// The return type of the function.
    pub result: Type,
}

/// A global item number.
///
/// Defaults to 0, which is considered an invalid value.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Gin(pub u32);

/// A Path.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Path {
    /// The path components, in order; possibly empty.
    pub components: DynArray<Type>,
}

/// An annotated prototype.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Prototype {
    /// An enum prototype.
    Enum(EnumProto),
    /// A function prototype.
    Fun(FunctionProto),
    /// A record prototype.
    Rec(RecordProto),
}

/// A record.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record {
    /// The prototype.
    pub prototype: RecordProto,
    /// The definition.
    pub definition: Tuple<Type>,
}

/// A record prototype.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RecordProto {
    /// The record identifier.
    pub name: ItemIdentifier,
    /// The record range.
    pub range: com::Range,
    /// The enum this record is a part of, or undefined.
    pub enum_: ItemIdentifier,
}

/// A Type.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type, possibly nested.
    Enum(Enum, Path, Gin),
    /// A record type, possibly nested.
    Rec(Record, Path, Gin),
    /// A tuple type.
    Tuple(Tuple<Type>, Gin),
    /// An unresolved type, possibly nested.
    Unresolved(ItemIdentifier, Path, Gin),
    /// An unresolved enum type, possibly nested.
    UnresolvedEnum(EnumProto, Path, Gin),
    /// An unresolved record type, possibly nested.
    UnresolvedRec(RecordProto, Path, Gin),
}

/// An item identifier.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ItemIdentifier(pub mem::InternId, pub com::Range);

//
//  Public interface
//

impl Argument {
    /// Sets the gvn of the binding.
    pub fn with_gvn<G: convert::Into<Gvn>>(mut self, gvn: G) -> Argument {
        self.gvn = gvn.into();
        self
    }
}

impl BuiltinType {
    /// Returns the gin associated to the type.
    pub fn gin(&self) -> Gin {
        use self::BuiltinType::*;

        match *self {
            Bool => Gin(1),
            Int => Gin(2),
            String => Gin(3),
            Void => Gin(4),
        }
    }

    /// Returns the maximum gin associated to a built-in type.
    pub fn maximum_gin() -> Gin { Gin(4) }
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

impl Type {
    /// Returns a Bool type.
    pub fn bool_() -> Self { Type::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { Type::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { Type::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { Type::Builtin(BuiltinType::Void) }

    /// Returns a unit type.
    pub fn unit() -> Self { Type::Tuple(Tuple::unit(), Gin::default()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        Type::Unresolved(
            ItemIdentifier::unresolved(),
            Path::default(),
            Gin::default(),
        )
    }

    /// Returns the type of the field, or Unresolved if unknown.
    pub fn field(&self, field: Field) -> Type {
        self.fields().field(field).unwrap_or(Type::unresolved())
    }

    /// Returns the fields of the type, as a Tuple.
    ///
    /// Note:   unless the type is a Rec or Tuple, the Tuple will be empty.
    pub fn fields(&self) -> Tuple<Type> {
        match self {
            Type::Rec(r, ..) => r.definition.clone(),
            Type::Tuple(t, ..) => t.clone(),
            _ => Tuple::unit(),
        }
    }
    /// Returns the gin associated to the type.
    pub fn gin(&self) -> Gin {
        use self::Type::*;

        match self {
            Builtin(b) => b.gin(),
            Enum(_, _, gin) | Rec(_, _, gin) | Tuple(_, gin)
                | Unresolved(_, _, gin) | UnresolvedEnum(_, _, gin)
                | UnresolvedRec(_, _, gin)
                    => *gin,
        }
    }

    /// Switches the gin of the type.
    ///
    /// Panics: If the type is a built-in.
    pub fn with_gin(self, gin: Gin) -> Type {
        use self::Type::*;

        match self {
            Builtin(_) => panic!("Built-in have no GIN!"),
            Enum(e, p, _) => Enum(e, p, gin),
            Rec(r, p, _) => Rec(r, p, gin),
            Tuple(t, _) => Tuple(t, gin),
            Unresolved(i, p, _) => Unresolved(i, p, gin),
            UnresolvedEnum(e, p, _) => UnresolvedEnum(e, p, gin),
            UnresolvedRec(r, p, _) => UnresolvedRec(r, p, gin),
        }
    }


    /// Switches the path of the type.
    ///
    /// Panics: If this variant has no path.
    pub fn with_path(self, p: Path) -> Type {
        use self::Type::*;

        match self {
            Enum(e, _, gin) => Enum(e, p, gin),
            Rec(r, _, gin) => Rec(r, p, gin),
            Unresolved(i, _, gin) => Unresolved(i, p, gin),
            UnresolvedEnum(e, _, gin) => UnresolvedEnum(e, p, gin),
            UnresolvedRec(r, _, gin) => UnresolvedRec(r, p, gin),
            _ => panic!("{} has no path!", self),
        }
    }
}

//
//  Span Implementations
//

impl Span for Argument {
    /// Range spanned by the binding.
    fn span(&self) -> com::Range { self.range }
}

impl Span for ItemIdentifier {
    /// Returns the range spanned by the ItemIdentifier.
    fn span(&self) -> com::Range { self.1 }
}

impl Span for Prototype {
    /// Returns the range spanned by the prototype.
    fn span(&self) -> com::Range {
        use self::Prototype::*;

        match self {
            Enum(e) => e.range,
            Fun(fun) => fun.range,
            Rec(r) => r.range,
        }
    }
}

impl Span for Type {
    /// Returns the range this type would cover if it was anchored at 0.
    fn span(&self) -> com::Range {
        use self::Type::*;
        use self::BuiltinType::*;

        fn len(i: ItemIdentifier, p: &Path) -> usize {
            p.components.iter().map(|c| c.span().length() + 2).sum::<usize>()
                + i.span().length()
        }

        let len = match self {
            Builtin(Bool) => 4,
            Builtin(Int) => 3,
            Builtin(String) => 6,
            Builtin(Void) => 4,
            Enum(e, p, _) => len(e.prototype.name, p),
            Rec(r, p, _) => len(r.prototype.name, p),
            Tuple(t, _) => t.fields.iter().map(|t| t.span().length()).sum(),
            Unresolved(i, p, _) => len(*i, p),
            UnresolvedEnum(e, p, _) => len(e.name, p),
            UnresolvedRec(r, p, _) => len(r.name, p),
        };

        com::Range::new(0, len)
    }
}

//
//  Debug Implementations
//

impl std::fmt::Debug for Gin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Gin({})", self.0)
    }
}

impl std::fmt::Debug for ItemIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "ItemIdentifier({:?}, {})", self.0, self.1)
    }
}

//
//  From Implementations
//

impl convert::From<u32> for Gin {
    fn from(i: u32) -> Gin { Gin(i) }
}

impl convert::From<Enum> for Item {
    fn from(e: Enum) -> Self { Item::Enum(e) }
}

impl convert::From<Function> for Item {
    fn from(f: Function) -> Self { Item::Fun(f) }
}

impl convert::From<Record> for Item {
    fn from(r: Record) -> Self { Item::Rec(r) }
}

impl convert::From<ast::TypeIdentifier> for ItemIdentifier {
    fn from(value: ast::TypeIdentifier) -> Self {
        ItemIdentifier(value.id(), value.span())
    }
}

impl convert::From<EnumProto> for Prototype {
    fn from(e: EnumProto) -> Self { Prototype::Enum(e) }
}

impl convert::From<FunctionProto> for Prototype {
    fn from(f: FunctionProto) -> Self { Prototype::Fun(f) }
}

impl convert::From<RecordProto> for Prototype {
    fn from(r: RecordProto) -> Self { Prototype::Rec(r) }
}

impl convert::From<EnumProto> for Type {
    fn from(e: EnumProto) -> Self {
        Type::UnresolvedEnum(e, Path::default(), Gin::default())
    }
}

impl convert::From<RecordProto> for Type {
    fn from(r: RecordProto) -> Self {
        Type::UnresolvedRec(r, Path::default(), Gin::default())
    }
}

impl convert::From<Tuple<Type>> for Type {
    fn from(t: Tuple<Type>) -> Self { Type::Tuple(t, Gin::default()) }
}

//
//  Implementation Details
//

impl Default for Path {
    fn default() -> Self { Path { components: Default::default() } }
}

impl Default for Type {
    fn default() -> Self { Type::unresolved() }
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for ItemIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "<{}>", self.span())
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for c in &self.components {
            write!(f, "{}::", c)?;
        }

        Ok(())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Type::*;

        match self {
            Builtin(t) => write!(f, "{}", t),
            Enum(e, p, _) => write!(f, "{}{}", p, e.prototype.name),
            Rec(r, p, _) => write!(f, "{}{}", p, r.prototype.name),
            Tuple(t, _) => write!(f, "{}", t),
            Unresolved(i, p, _) => write!(f, "{}{}", p, i),
            UnresolvedEnum(e, p, _) => write!(f, "{}{}", p, e.name),
            UnresolvedRec(r, p, _) => write!(f, "{}{}", p, r.name),
        }
    }
}

/// Mocks for the traits.
pub mod mocks {
    use std::collections::HashMap;

    use basic::com::{self, Span};

    use super::{Enum, ItemIdentifier, Record, Registry};

    /// A mock for the Regitry trait.
    #[derive(Debug)]
    pub struct MockRegistry {
        /// Map of enums to be returned from lookup_enum.
        pub enums: HashMap<com::Range, Enum>,
        /// Map of records to be returned from lookup_record.
        pub records: HashMap<com::Range, Record>,
    }

    impl MockRegistry {
        /// Creates a new instance of MockRegistry.
        pub fn new() -> MockRegistry {
            MockRegistry { 
                enums: HashMap::new(),
                records: HashMap::new(),
            }
        }

        /// Inserts an enum, indexing it by the span of its identifier.
        ///
        /// Note:   Also inserts all records it contains.
        pub fn insert_enum(&mut self, e: Enum) {
            self.enums.insert(e.prototype.name.span(), e.clone());

            for r in e.variants {
                self.insert_record(r.clone());
            }
        }

        /// Inserts a record, indexing it by the span of its identifier.
        pub fn insert_record(&mut self, r: Record) {
            self.records.insert(r.prototype.name.span(), r);
        }
    }

    impl Registry for MockRegistry {
        fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum> {
            self.enums.get(&id.span()).cloned()
        }

        fn lookup_record(&self, id: ItemIdentifier) -> Option<Record> {
            self.records.get(&id.span()).cloned()
        }
    }
}
