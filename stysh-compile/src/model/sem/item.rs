//! Items

use std::{convert, fmt};

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::syn;
use model::sem::*;

/// A registry of the definitions
pub trait Registry<'a> {
    /// Get the definition of the enum.
    fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'a>>;

    /// Get the definition of the record.
    fn lookup_record(&self, id: ItemIdentifier) -> Option<Record<'a>>;
}

/// A full-fledged item.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item<'a> {
    /// A full-fledged enum definition.
    Enum(Enum<'a>),
    /// A full-fledged function definition.
    Fun(Function<'a>),
    /// A full-fledged record definition.
    Rec(Record<'a>),
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
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum<'a> {
    /// The prototype.
    pub prototype: &'a EnumProto,
    /// The variants.
    pub variants: &'a [Record<'a>],
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
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function<'a> {
    /// The prototype.
    pub prototype: &'a FunctionProto<'a>,
    /// The body.
    pub body: Value<'a>,
}

/// A function prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionProto<'a> {
    /// The function identifier.
    pub name: ItemIdentifier,
    /// The function prototype range.
    pub range: com::Range,
    /// The function arguments (always arguments).
    pub arguments: &'a [Binding<'a>],
    /// The return type of the function.
    pub result: Type<'a>,
}

/// An annotated prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Prototype<'a> {
    /// An enum prototype.
    Enum(EnumProto),
    /// A function prototype.
    Fun(FunctionProto<'a>),
    /// A record prototype.
    Rec(RecordProto),
}

/// A record.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record<'a> {
    /// The prototype.
    pub prototype: &'a RecordProto,
    /// The fields.
    pub fields: &'a [Type<'a>],
}

/// A record prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RecordProto {
    /// The record identifier.
    pub name: ItemIdentifier,
    /// The record range.
    pub range: com::Range,
    /// The enum this record is a part of, or undefined.
    pub enum_: ItemIdentifier,
}

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type<'a> {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type.
    Enum(EnumProto),
    /// A record type.
    Rec(RecordProto),
    /// A tuple type.
    Tuple(Tuple<'a, Type<'a>>),
    /// An unresolved type.
    Unresolved(ItemIdentifier),
}

/// An item identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ItemIdentifier(pub com::Range);

//
//  Public interface
//

impl ItemIdentifier {
    /// Returns a sentinel instance of ItemIdentifier.
    pub fn unresolved() -> ItemIdentifier {
        ItemIdentifier(com::Range::new(0, 0))
    }
}

impl Type<'static> {
    /// Returns a Bool type.
    pub fn bool_() -> Self { Type::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { Type::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { Type::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { Type::Builtin(BuiltinType::Void) }

    /// Returns a unit type.
    pub fn unit() -> Self { Type::Tuple(Tuple::unit()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        Type::Unresolved(ItemIdentifier::unresolved())
    }
}

//
//  CloneInto implementations
//

impl<'target> CloneInto<'target> for EnumProto {
    type Output = EnumProto;

    fn clone_into(&self, _: &'target mem::Arena) -> Self::Output {
        *self
    }
}

impl<'a, 'target> CloneInto<'target> for Enum<'a> {
    type Output = Enum<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Enum {
            prototype: arena.intern_ref(self.prototype),
            variants: CloneInto::clone_into(self.variants, arena),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Function<'a> {
    type Output = Function<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Function {
            prototype: arena.intern_ref(self.prototype),
            body: arena.intern(&self.body),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for FunctionProto<'a> {
    type Output = FunctionProto<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        FunctionProto {
            name: self.name,
            range: self.range,
            arguments: CloneInto::clone_into(self.arguments, arena),
            result: arena.intern(&self.result),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Prototype<'a> {
    type Output = Prototype<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Prototype::*;

        match *self {
            Enum(e) => Enum(e),
            Rec(r) => Rec(r),
            Fun(fun) => Fun(arena.intern(&fun)),
        }
    }
}

impl<'target> CloneInto<'target> for RecordProto {
    type Output = RecordProto;

    fn clone_into(&self, _: &'target mem::Arena) -> Self::Output {
        *self
    }
}

impl<'a, 'target> CloneInto<'target> for Record<'a> {
    type Output = Record<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Record {
            prototype: arena.intern_ref(self.prototype),
            fields: CloneInto::clone_into(self.fields, arena),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Type<'a> {
    type Output = Type<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Type::*;

        match *self {
            Builtin(t) => Builtin(t),
            Enum(e) => Enum(e),
            Rec(r) => Rec(r),
            Tuple(t) => Tuple(arena.intern(&t)),
            Unresolved(n) => Unresolved(n),
        }
    }
}

//
//  Span Implementations
//

impl Span for ItemIdentifier {
    /// Returns the range spanned by the ItemIdentifier.
    fn span(&self) -> com::Range { self.0 }
}

impl<'a> Span for Prototype<'a> {
    /// Returns the range spanned by the prototype.
    fn span(&self) -> com::Range {
        use self::Prototype::*;

        match *self {
            Enum(e) => e.range,
            Fun(fun) => fun.range,
            Rec(r) => r.range,
        }
    }
}

impl<'a> Span for Type<'a> {
    /// Returns the range this type would cover if it was anchored at 0.
    fn span(&self) -> com::Range {
        use self::Type::*;
        use self::BuiltinType::*;

        fn len(i: ItemIdentifier) -> usize { i.span().length() }

        let len = match *self {
            Builtin(Bool) => 4,
            Builtin(Int) => 3,
            Builtin(String) => 6,
            Builtin(Void) => 4,
            Enum(p) => len(p.name),
            Rec(r) => len(r.name)
                + if len(r.enum_) > 0 { 2 + len(r.enum_) } else { 0 },
            Tuple(t) => t.fields.iter().map(|t| t.span().length()).sum(),
            Unresolved(i) => len(i),
        };

        com::Range::new(0, len)
    }
}

//
//  From Implementations
//

impl<'a> convert::From<Enum<'a>> for Item<'a> {
    fn from(e: Enum<'a>) -> Self { Item::Enum(e) }
}

impl<'a> convert::From<Function<'a>> for Item<'a> {
    fn from(f: Function<'a>) -> Self { Item::Fun(f) }
}

impl<'a> convert::From<Record<'a>> for Item<'a> {
    fn from(r: Record<'a>) -> Self { Item::Rec(r) }
}

impl convert::From<syn::TypeIdentifier> for ItemIdentifier {
    fn from(value: syn::TypeIdentifier) -> Self {
        ItemIdentifier(value.span())
    }
}

impl convert::From<EnumProto> for Prototype<'static> {
    fn from(e: EnumProto) -> Self { Prototype::Enum(e) }
}

impl<'a> convert::From<FunctionProto<'a>> for Prototype<'a> {
    fn from(f: FunctionProto<'a>) -> Self { Prototype::Fun(f) }
}

impl convert::From<RecordProto> for Prototype<'static> {
    fn from(r: RecordProto) -> Self { Prototype::Rec(r) }
}

impl convert::From<EnumProto> for Type<'static> {
    fn from(e: EnumProto) -> Self { Type::Enum(e) }
}

impl convert::From<RecordProto> for Type<'static> {
    fn from(r: RecordProto) -> Self { Type::Rec(r) }
}

impl<'a> convert::From<Tuple<'a, Type<'a>>> for Type<'a> {
    fn from(t: Tuple<'a, Type<'a>>) -> Self { Type::Tuple(t) }
}

//
//  Implementation Details
//

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl<'a> fmt::Display for ItemIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "<{}>", self.0)
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Type::Builtin(t) => write!(f, "{}", t),
            Type::Enum(e) => write!(f, "{}", e.name),
            Type::Rec(r) => write!(f, "{}", r.name),
            Type::Tuple(t) => write!(f, "{}", t),
            Type::Unresolved(i) => write!(f, "{}", i),
        }
    }
}

/// Mocks for the traits.
pub mod mocks {
    use basic::mem;
    use super::{Enum, ItemIdentifier, Record, Registry};

    /// A mock for the Regitry trait.
    #[derive(Debug)]
    pub struct MockRegistry<'g> {
        /// Map of enums to be returned from lookup_enum.
        pub enums: mem::ArrayMap<'g, ItemIdentifier, Enum<'g>>,
        /// Map of records to be returned from lookup_record.
        pub records: mem::ArrayMap<'g, ItemIdentifier, Record<'g>>,
    }

    impl<'g> MockRegistry<'g> {
        /// Creates a new instance of MockRegistry.
        pub fn new(arena: &'g mem::Arena) -> MockRegistry<'g> {
            MockRegistry { 
                enums: mem::ArrayMap::new(arena),
                records: mem::ArrayMap::new(arena),
            }
        }
    }

    impl<'g> Registry<'g> for MockRegistry<'g> {
        fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'g>> {
            self.enums.get(&id).cloned()
        }

        fn lookup_record(&self, id: ItemIdentifier) -> Option<Record<'g>> {
            self.records.get(&id).cloned()
        }
    }
}
