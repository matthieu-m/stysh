//! Items

use std::{convert, fmt};

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::ast;
use model::hir::*;

/// A registry of the definitions
pub trait Registry<'a>: fmt::Debug {
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

/// A function argument.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Argument<'a> {
    /// The name.
    pub name: ValueIdentifier,
    /// The type.
    pub type_: Type<'a>,
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
    pub arguments: &'a [Argument<'a>],
    /// The return type of the function.
    pub result: Type<'a>,
}

/// A Path.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Path<'a> {
    /// The path components, in order; possibly empty.
    pub components: &'a [Type<'a>],
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
    /// The definition.
    pub definition: Tuple<'a, Type<'a>>,
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
    /// An enum type, possibly nested.
    Enum(&'a Enum<'a>, Path<'a>),
    /// A record type, possibly nested.
    Rec(&'a Record<'a>, Path<'a>),
    /// A tuple type.
    Tuple(Tuple<'a, Type<'a>>),
    /// An unresolved type, possibly nested.
    Unresolved(ItemIdentifier, Path<'a>),
    /// An unresolved enum type, possibly nested.
    UnresolvedEnum(EnumProto, Path<'a>),
    /// An unresolved record type, possibly nested.
    UnresolvedRec(RecordProto, Path<'a>),
}

/// An item identifier.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ItemIdentifier(pub mem::InternId, pub com::Range);

//
//  Public interface
//

impl<'a> Argument<'a> {
    /// Sets the gvn of the binding.
    pub fn with_gvn<G: convert::Into<Gvn>>(mut self, gvn: G) -> Argument<'a> {
        self.gvn = gvn.into();
        self
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
        Type::Unresolved(ItemIdentifier::unresolved(), Default::default())
    }
}

impl<'a> Type<'a> {
    /// Switches the path of the type.
    ///
    /// Panics: If this variant has no path.
    pub fn with_path(self, p: Path<'a>) -> Type<'a> {
        use self::Type::*;

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
//  CloneInto implementations
//

impl<'a, 'target> CloneInto<'target> for Argument<'a> {
    type Output = Argument<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Argument {
            name: self.name,
            type_: arena.intern(&self.type_),
            range: self.range,
            gvn: self.gvn,
        }
    }
}

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

impl<'a, 'target> CloneInto<'target> for Path<'a> {
    type Output = Path<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Path {
            components: CloneInto::clone_into(self.components, arena),
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
            definition: arena.intern(&self.definition),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Type<'a> {
    type Output = Type<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Type::*;

        match *self {
            Builtin(t) => Builtin(t),
            Enum(e, p) => Enum(arena.intern_ref(e), arena.intern(&p)),
            Rec(r, p) => Rec(arena.intern_ref(r), arena.intern(&p)),
            Tuple(t) => Tuple(arena.intern(&t)),
            Unresolved(n, p) => Unresolved(n, arena.intern(&p)),
            UnresolvedEnum(e, p) => UnresolvedEnum(e, arena.intern(&p)),
            UnresolvedRec(r, p) => UnresolvedRec(r, arena.intern(&p)),
        }
    }
}

impl<'target> CloneInto<'target> for ItemIdentifier {
    type Output = ItemIdentifier;

    fn clone_into(&self, _: &'target mem::Arena) -> Self::Output { *self }
}

//
//  Span Implementations
//

impl<'a> Span for Argument<'a> {
    /// Range spanned by the binding.
    fn span(&self) -> com::Range { self.range }
}

impl Span for ItemIdentifier {
    /// Returns the range spanned by the ItemIdentifier.
    fn span(&self) -> com::Range { self.1 }
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

        fn len(i: ItemIdentifier, p: Path) -> usize {
            p.components.iter().map(|c| c.span().length() + 2).sum::<usize>()
                + i.span().length()
        }

        let len = match *self {
            Builtin(Bool) => 4,
            Builtin(Int) => 3,
            Builtin(String) => 6,
            Builtin(Void) => 4,
            Enum(e, p) => len(e.prototype.name, p),
            Rec(r, p) => len(r.prototype.name, p),
            Tuple(t) => t.fields.iter().map(|t| t.span().length()).sum(),
            Unresolved(i, p) => len(i, p),
            UnresolvedEnum(e, p) => len(e.name, p),
            UnresolvedRec(r, p) => len(r.name, p),
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

impl convert::From<ast::TypeIdentifier> for ItemIdentifier {
    fn from(value: ast::TypeIdentifier) -> Self {
        ItemIdentifier(value.id(), value.span())
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
    fn from(e: EnumProto) -> Self { Type::UnresolvedEnum(e, Path::default()) }
}

impl convert::From<RecordProto> for Type<'static> {
    fn from(r: RecordProto) -> Self { Type::UnresolvedRec(r, Path::default()) }
}

impl<'a> convert::From<Tuple<'a, Type<'a>>> for Type<'a> {
    fn from(t: Tuple<'a, Type<'a>>) -> Self { Type::Tuple(t) }
}

//
//  Implementation Details
//

impl Default for Path<'static> {
    fn default() -> Self { Path { components: &[] } }
}

impl Default for Type<'static> {
    fn default() -> Self { Type::unresolved() }
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl<'a> fmt::Display for ItemIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "<{}>", self.span())
    }
}

impl<'a> fmt::Display for Path<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for c in self.components {
            write!(f, "{}::", c)?;
        }

        Ok(())
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Type::*;

        match *self {
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
    use basic::mem;
    use basic::com::{self, Span};

    use super::{Enum, ItemIdentifier, Record, Registry};

    /// A mock for the Regitry trait.
    #[derive(Debug)]
    pub struct MockRegistry<'g> {
        /// Map of enums to be returned from lookup_enum.
        pub enums: mem::ArrayMap<'g, com::Range, Enum<'g>>,
        /// Map of records to be returned from lookup_record.
        pub records: mem::ArrayMap<'g, com::Range, Record<'g>>,
    }

    impl<'g> MockRegistry<'g> {
        /// Creates a new instance of MockRegistry.
        pub fn new(arena: &'g mem::Arena) -> MockRegistry<'g> {
            MockRegistry { 
                enums: mem::ArrayMap::new(arena),
                records: mem::ArrayMap::new(arena),
            }
        }

        /// Inserts an enum, indexing it by the span of its identifier.
        ///
        /// Note:   Also inserts all records it contains.
        pub fn insert_enum(&mut self, e: Enum<'g>) {
            self.enums.insert(e.prototype.name.span(), e);

            for r in e.variants {
                self.insert_record(*r);
            }
        }

        /// Inserts a record, indexing it by the span of its identifier.
        pub fn insert_record(&mut self, r: Record<'g>) {
            self.records.insert(r.prototype.name.span(), r);
        }
    }

    impl<'g> Registry<'g> for MockRegistry<'g> {
        fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'g>> {
            self.enums.get(&id.span()).cloned()
        }

        fn lookup_record(&self, id: ItemIdentifier) -> Option<Record<'g>> {
            self.records.get(&id.span()).cloned()
        }
    }
}
