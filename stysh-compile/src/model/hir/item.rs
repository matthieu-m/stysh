//! Items

use std::convert;

use basic::com::{Range, Span};

use model::hir::*;

/// The ID of an Enum.
pub type EnumId = Id<Enum>;
/// The ID of a Function.
pub type FunctionId = Id<Function>;
/// The ID of a Path.
pub type PathId = Id<[PathComponent]>;
/// The ID of a Record.
pub type RecordId = Id<Record>;

/// ItemId.
///
/// A trait for item IDs. The main characteristics of item IDs is that items
/// can be defined both locally while analysing a module and then globally to
/// refer to them.
pub trait ItemId {
    /// Creates a new tree item.
    fn new_tree(id: u32) -> Self;

    /// Creates a new module item.
    fn new_module(id: u32) -> Self;

    /// Creates a new repository item.
    fn new_repository(id: u32) -> Self;

    /// Returns the tree ID of the item, if created with `new_tree`.
    fn get_tree(&self) -> Option<u32>;

    /// Returns the module ID of the item, if created with `new_module`.
    fn get_module(&self) -> Option<u32>;

    /// Returns the repository ID of the item, if created with `new_repository`.
    fn get_repository(&self) -> Option<u32>;

    /// Returns whether the ID was created with `new_tree`.
    fn is_tree(&self) -> bool { self.get_tree().is_some() }

    /// Returns whether the ID was created with `new_module`.
    fn is_module(&self) -> bool { self.get_module().is_some() }

    /// Returns whether the ID was created with `new_repository`.
    fn is_repository(&self) -> bool { self.get_repository().is_some() }
}

/// An item
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item {
    /// An enum definition.
    Enum(EnumId),
    /// A function definition.
    Fun(FunctionId),
    /// A record definition.
    Rec(RecordId),
}

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type, possibly nested.
    Enum(EnumId, PathId),
    /// A record type, possibly nested.
    Rec(RecordId, PathId),
    /// A tuple type.
    Tuple(Tuple<TypeId>),
    /// An unresolved type, possibly nested.
    Unresolved(ItemIdentifier, PathId),
}

/// An enum definition.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum {
    /// The prototype.
    pub prototype: EnumPrototype,
    /// The variants.
    pub variants: Id<[RecordId]>,
}

/// An enum prototype.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EnumPrototype {
    /// The enum identifier.
    pub name: ItemIdentifier,
    /// The enum range.
    pub range: Range,
}

/// A function.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Function {
    /// The prototype.
    pub prototype: FunctionPrototype,
    /// The body.
    pub body: Tree,
}

/// A function prototype.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionPrototype {
    /// The function identifier.
    pub name: ItemIdentifier,
    /// The function arguments.
    pub arguments: Tuple<TypeId>,
    /// The return type of the function.
    pub result: TypeId,
    /// The function prototype range.
    pub range: Range,
}

/// A PathComponent.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum PathComponent {
    /// An Enum.
    Enum(EnumId, Range),
    /// A Record.
    Rec(RecordId, Range),
    /// An Unresolved component.
    Unresolved(ItemIdentifier),
    //  TODO: add module.
}

/// An annotated prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Prototype {
    /// An enum prototype.
    Enum(EnumPrototype),
    /// A function prototype.
    Fun(FunctionPrototype),
    /// A record prototype.
    Rec(RecordPrototype),
}

/// A record.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record {
    /// The prototype.
    pub prototype: RecordPrototype,
    /// The definition.
    pub definition: Tuple<TypeId>,
}

/// A record prototype.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RecordPrototype {
    /// The record identifier.
    pub name: ItemIdentifier,
    /// The record range.
    pub range: Range,
    /// The enum this record is a part of, if any, or undefined.
    pub enum_: Option<EnumId>,
}


//
//  Public Implementations
//

impl Type {
    /// Returns a Bool type.
    pub fn bool_() -> Self { Type::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { Type::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { Type::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { Type::Builtin(BuiltinType::Void) }

    /// Returns a Unit type.
    pub fn unit() -> Self { Type::Tuple(Tuple::unit()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        Type::Unresolved(ItemIdentifier::unresolved(), PathId::empty())
    }

    /// Replaces the Path.
    pub fn with_path(self, path: PathId) -> Self {
        use self::Type::*;

        match self {
            Enum(id, _) => Enum(id, path),
            Rec(id, _) => Rec(id, path),
            Unresolved(name, _) => Unresolved(name, path),
            _ => panic!("{:?} has no path!", self),
        }
    }
}


//
//  ItemId Implementations
//

const MODULE_OFFSET: u32 = 1u32 << 30;
const REPOSITORY_OFFSET: u32 = 1u32 << 31;
const SPECIAL_OFFSET: u32 = REPOSITORY_OFFSET + MODULE_OFFSET;

impl<T: ?Sized> ItemId for Id<T> {
    fn new_tree(id: u32) -> Self {
        debug_assert!(id < MODULE_OFFSET);

        Id::new(id)
    }

    fn new_module(id: u32) -> Self {
        debug_assert!(id < REPOSITORY_OFFSET - MODULE_OFFSET);

        Id::new(id + MODULE_OFFSET)
    }

    fn new_repository(id: u32) -> Self {
        debug_assert!(id < SPECIAL_OFFSET - REPOSITORY_OFFSET);

        Id::new(id + REPOSITORY_OFFSET)
    }

    fn get_tree(&self) -> Option<u32> {
        let value = self.value();
        if value < MODULE_OFFSET {
            Some(value)
        } else {
            None
        }
    }

    fn get_module(&self) -> Option<u32> {
        let value = self.value();
        if MODULE_OFFSET <= value && value < REPOSITORY_OFFSET {
            Some(value - MODULE_OFFSET)
        } else {
            None
        }
    }

    fn get_repository(&self) -> Option<u32> {
        let value = self.value();
        if REPOSITORY_OFFSET <= value && value < SPECIAL_OFFSET {
            Some(value - REPOSITORY_OFFSET)
        } else {
            None
        }
    }
}

impl ItemId for TypeId {
    fn new_tree(id: u32) -> Self {
        debug_assert!(id < MODULE_OFFSET);

        TypeId::new(id)
    }

    fn new_module(id: u32) -> Self {
        debug_assert!(id < REPOSITORY_OFFSET - MODULE_OFFSET);

        TypeId::new(id + MODULE_OFFSET)
    }

    fn new_repository(id: u32) -> Self {
        debug_assert!(id < SPECIAL_OFFSET - REPOSITORY_OFFSET);

        TypeId::new(id + REPOSITORY_OFFSET)
    }

    fn get_tree(&self) -> Option<u32> {
        let value = self.value();
        if value < MODULE_OFFSET {
            Some(value)
        } else {
            None
        }
    }

    fn get_module(&self) -> Option<u32> {
        let value = self.value();
        if MODULE_OFFSET <= value && value < REPOSITORY_OFFSET {
            Some(value - MODULE_OFFSET)
        } else {
            None
        }
    }

    fn get_repository(&self) -> Option<u32> {
        let value = self.value();
        if REPOSITORY_OFFSET <= value && value < SPECIAL_OFFSET {
            Some(value - REPOSITORY_OFFSET)
        } else {
            None
        }
    }
}


//
//  Span Implementations
//

impl Span for Prototype {
    /// Returns the range spanned by the prototype.
    fn span(&self) -> Range {
        use self::Prototype::*;

        match self {
            Enum(e) => e.range,
            Fun(fun) => fun.range,
            Rec(r) => r.range,
        }
    }
}


//
//  Default Implementations
//

impl Default for PathComponent {
    fn default() -> Self { PathComponent::Unresolved(Default::default()) }
}

//
//  From Implementations
//

impl convert::From<EnumId> for Item {
    fn from(e: EnumId) -> Self { Item::Enum(e) }
}

impl convert::From<FunctionId> for Item {
    fn from(f: FunctionId) -> Self { Item::Fun(f) }
}

impl convert::From<RecordId> for Item {
    fn from(r: RecordId) -> Self { Item::Rec(r) }
}

impl convert::From<EnumPrototype> for Prototype {
    fn from(e: EnumPrototype) -> Self { Prototype::Enum(e) }
}

impl convert::From<FunctionPrototype> for Prototype {
    fn from(f: FunctionPrototype) -> Self { Prototype::Fun(f) }
}

impl convert::From<RecordPrototype> for Prototype {
    fn from(r: RecordPrototype) -> Self { Prototype::Rec(r) }
}
