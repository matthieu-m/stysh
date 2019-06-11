//! Items

use std::{convert, fmt};

use basic::com::{self, Span};
use basic::mem::DynArray;

use model::hir::*;

/// A registry of the definitions
pub trait Registry: fmt::Debug {
    /// Get the definition of the enum.
    fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum>;

    /// Get the definition of the record.
    fn lookup_record(&self, id: ItemIdentifier) -> Option<Record>;
}

/// A full-fledged item.
#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub type_: TypeDefinition,
    /// The range.
    pub range: com::Range,
}

/// An enum definition.
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    /// The prototype.
    pub prototype: FunctionProto,
    /// The body.
    pub body: Tree,
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
    pub result: TypeDefinition,
}

/// A Path.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Path {
    /// The path components, in order; possibly empty.
    pub components: DynArray<TypeDefinition>,
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
    pub definition: DynTuple<TypeDefinition>,
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
pub enum TypeDefinition {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type, possibly nested.
    Enum(Enum, Path),
    /// A record type, possibly nested.
    Rec(Record, Path),
    /// A tuple type.
    Tuple(DynTuple<TypeDefinition>),
    /// An unresolved type, possibly nested.
    Unresolved(ItemIdentifier, Path),
    /// An unresolved enum type, possibly nested.
    UnresolvedEnum(EnumProto, Path),
    /// An unresolved record type, possibly nested.
    UnresolvedRec(RecordProto, Path),
}

//
//  Public interface
//

impl TypeDefinition {
    /// Returns a Bool type.
    pub fn bool_() -> Self { TypeDefinition::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { TypeDefinition::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { TypeDefinition::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { TypeDefinition::Builtin(BuiltinType::Void) }

    /// Returns a unit type.
    pub fn unit() -> Self { TypeDefinition::Tuple(DynTuple::unit()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        TypeDefinition::Unresolved(
            ItemIdentifier::unresolved(),
            Path::default(),
        )
    }

    /// Returns the type of the field, or Unresolved if unknown.
    pub fn field(&self, field: Field) -> TypeDefinition {
        self.fields().field(field).unwrap_or(TypeDefinition::unresolved())
    }

    /// Returns the fields of the type, as a Tuple.
    ///
    /// Note:   unless the type is a Rec or Tuple, the Tuple will be empty.
    pub fn fields(&self) -> DynTuple<TypeDefinition> {
        match self {
            TypeDefinition::Rec(r, ..) => r.definition.clone(),
            TypeDefinition::Tuple(t, ..) => t.clone(),
            _ => DynTuple::unit(),
        }
    }

    /// Returns the name of the type.
    pub fn name(&self) -> ItemIdentifier {
        use self::TypeDefinition::*;

        match self {
            Builtin(_) => ItemIdentifier::unresolved(),
            Enum(e, _) => e.prototype.name,
            Rec(r, _) => r.prototype.name,
            Tuple(_) => ItemIdentifier::unresolved(),
            Unresolved(i, _) => *i,
            UnresolvedEnum(e, _) => e.name,
            UnresolvedRec(r, _) => r.name,
        }
    }

    /// Switches the path of the type.
    ///
    /// Panics: If this variant has no path.
    pub fn with_path(self, p: Path) -> TypeDefinition {
        use self::TypeDefinition::*;

        match self {
            Enum(e, _) => Enum(e, p),
            Rec(r, _) => Rec(r, p),
            Unresolved(i, _) => Unresolved(i, p),
            UnresolvedEnum(e, _) => UnresolvedEnum(e, p),
            UnresolvedRec(r, _) => UnresolvedRec(r, p),
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

impl Span for TypeDefinition {
    /// Returns the range this type would cover if it was anchored at 0.
    fn span(&self) -> com::Range {
        use self::TypeDefinition::*;
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
            Enum(e, p) => len(e.prototype.name, p),
            Rec(r, p) => len(r.prototype.name, p),
            Tuple(t) => t.fields.iter().map(|t| t.span().length()).sum(),
            Unresolved(i, p) => len(*i, p),
            UnresolvedEnum(e, p) => len(e.name, p),
            UnresolvedRec(r, p) => len(r.name, p),
        };

        com::Range::new(0, len)
    }
}

//
//  From Implementations
//

impl convert::From<Enum> for Item {
    fn from(e: Enum) -> Self { Item::Enum(e) }
}

impl convert::From<Function> for Item {
    fn from(f: Function) -> Self { Item::Fun(f) }
}

impl convert::From<Record> for Item {
    fn from(r: Record) -> Self { Item::Rec(r) }
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

impl convert::From<EnumProto> for TypeDefinition {
    fn from(e: EnumProto) -> Self {
        TypeDefinition::UnresolvedEnum(e, Path::default())
    }
}

impl convert::From<RecordProto> for TypeDefinition {
    fn from(r: RecordProto) -> Self {
        TypeDefinition::UnresolvedRec(r, Path::default())
    }
}

impl convert::From<DynTuple<TypeDefinition>> for TypeDefinition {
    fn from(t: DynTuple<TypeDefinition>) -> Self { TypeDefinition::Tuple(t) }
}

//
//  Implementation Details
//

impl Default for Path {
    fn default() -> Self { Path { components: Default::default() } }
}

impl Default for TypeDefinition {
    fn default() -> Self { TypeDefinition::unresolved() }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for c in &self.components {
            write!(f, "{}::", c)?;
        }

        Ok(())
    }
}

impl fmt::Display for TypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::TypeDefinition::*;

        match self {
            Builtin(t) => write!(f, "{}", t),
            Enum(e, p) => write!(f, "{}{}", p, e.prototype.name),
            Rec(r, p) => write!(f, "{}{}", p, r.prototype.name),
            Tuple(t) => write!(f, "{}", t),
            Unresolved(i, p) => write!(f, "{}{}", p, i),
            UnresolvedEnum(e, p) => write!(f, "{}{}", p, e.name),
            UnresolvedRec(r, p) => write!(f, "{}{}", p, r.name),
        }
    }
}

/// Mocks for the traits.
pub mod mocks {
    use std::collections::HashMap;

    use basic::com::{self, Span};

    use super::*;

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
