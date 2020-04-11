//! Items

use std::convert;

use crate::basic::com::Range;

use crate::model::hir::*;

/// The ID of an Enum.
pub type EnumId = Id<Enum>;
/// The ID of an Extension.
pub type ExtensionId = Id<Extension>;
/// The ID of a Function.
pub type FunctionId = Id<FunctionSignature>;
/// The ID of an Implementation.
pub type ImplementationId = Id<Implementation>;
/// The ID of an Interface.
pub type InterfaceId = Id<Interface>;
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
    /// An extension definition.
    Ext(ExtensionId),
    /// A function definition.
    Fun(FunctionId),
    /// An implementation definition.
    Imp(ImplementationId),
    /// An interface definition.
    Int(InterfaceId),
    /// A record definition.
    Rec(RecordId),
}

/// A Type.
///
/// The actual Type of an expression, field, pattern, ...
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type.
    Enum(EnumId),
    /// An interface type.
    Int(InterfaceId),
    /// A record type.
    Rec(RecordId),
    /// A tuple type.
    Tuple(Tuple<TypeId>),
    /// An unresolved type.
    Unresolved,
}

/// An Elaborate Type.
///
/// The explicitly specified type of an expression, field, pattern, ...
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ElaborateType {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type, possibly nested.
    Enum(EnumId, PathId),
    /// An interface type, possibly nested.
    Int(InterfaceId, PathId),
    /// A record type, possibly nested.
    Rec(RecordId, PathId),
    /// A tuple type.
    Tuple(Tuple<ElaborateTypeId>),
    /// An unresolved type, possibly nested.
    Unresolved(ItemIdentifier, PathId),
}

/// An enum definition.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum {
    /// The enum identifier.
    pub name: ItemIdentifier,
    /// The enum range.
    pub range: Range,
    /// The variants.
    pub variants: Id<[RecordId]>,
}

/// An extension definition.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Extension {
    /// The extension identifier.
    pub name: ItemIdentifier,
    /// The extension range.
    pub range: Range,
    /// The extended type.
    pub extended: TypeId,
    /// The specified extended type.
    pub elaborate_extended: ElaborateTypeId,
}

/// A function signature.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionSignature {
    /// The function identifier.
    pub name: ItemIdentifier,
    /// The function prototype range.
    pub range: Range,
    /// The scope.
    pub scope: Scope,
    /// The function arguments.
    pub arguments: Tuple<TypeId>,
    /// The return type of the function.
    pub result: TypeId,
    /// The specified function arguments.
    pub elaborate_arguments: Tuple<ElaborateTypeId>,
    /// The specified return type.
    pub elaborate_result: ElaborateTypeId,
}

/// An implementation definition.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Implementation {
    /// The implemented identifier.
    pub implemented_name: ItemIdentifier,
    /// The extended identifier.
    pub extended_name: ItemIdentifier,
    /// The implementation range.
    pub range: Range,
    /// The interface.
    pub implemented: InterfaceId,
    /// The extended type.
    pub extended: TypeId,
    /// The specified interface type.
    pub elaborate_implemented: ElaborateTypeId,
    /// The specified extended type.
    pub elaborate_extended: ElaborateTypeId,
}

/// An interface definition.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Interface {
    /// The interface identifier.
    pub name: ItemIdentifier,
    /// The interface range.
    pub range: Range,
}

/// A PathComponent.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum PathComponent {
    /// An Enum.
    Enum(EnumId, Range),
    /// An Interface.
    Int(InterfaceId, Range),
    /// A Record.
    Rec(RecordId, Range),
    /// An Unresolved component.
    Unresolved(ItemIdentifier),
    //  TODO: add module.
}

/// A record.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record {
    /// The record identifier.
    pub name: ItemIdentifier,
    /// The record range.
    pub range: Range,
    /// The enum this record is a part of, if any, or undefined.
    pub enum_: Option<EnumId>,
    /// The definition.
    pub definition: Tuple<TypeId>,
    /// The specified definition.
    pub elaborate_definition: Tuple<ElaborateTypeId>,
}

/// A scope.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Scope {
    /// Module scope.
    Module,
    /// Extension scope.
    Ext(ExtensionId),
    /// Implementation scope.
    Imp(ImplementationId),
    /// Interface scope.
    Int(InterfaceId),
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
    pub fn unresolved() -> Self { Type::Unresolved }

    /// Replaces the Path.
    pub fn elaborate(self, name: ItemIdentifier, path: PathId) -> ElaborateType
    {
        use self::Type::*;

        match self {
            Builtin(b) if path == PathId::empty() => ElaborateType::Builtin(b),
            Enum(id) => ElaborateType::Enum(id, path),
            Int(id) => ElaborateType::Int(id, path),
            Rec(id) => ElaborateType::Rec(id, path),
            Unresolved => ElaborateType::Unresolved(name, path),
            _ => panic!("{:?} has no path!", self),
        }
    }
}

impl ElaborateType {
    /// Returns a Bool type.
    pub fn bool_() -> Self { ElaborateType::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { ElaborateType::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { ElaborateType::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { ElaborateType::Builtin(BuiltinType::Void) }

    /// Returns a Unit type.
    pub fn unit() -> Self { ElaborateType::Tuple(Tuple::unit()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        ElaborateType::Unresolved(ItemIdentifier::unresolved(), PathId::empty())
    }

    /// Replaces the Path.
    pub fn with_path(self, path: PathId) -> Self {
        use self::ElaborateType::*;

        if path == PathId::empty() {
            return self;
        }

        match self {
            Enum(id, _) => Enum(id, path),
            Int(id, _) => Int(id, path),
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

impl ItemId for ElaborateTypeId {
    fn new_tree(id: u32) -> Self {
        debug_assert!(id < MODULE_OFFSET);

        ElaborateTypeId::new(id)
    }

    fn new_module(id: u32) -> Self {
        debug_assert!(id < REPOSITORY_OFFSET - MODULE_OFFSET);

        ElaborateTypeId::new(id + MODULE_OFFSET)
    }

    fn new_repository(id: u32) -> Self {
        debug_assert!(id < SPECIAL_OFFSET - REPOSITORY_OFFSET);

        ElaborateTypeId::new(id + REPOSITORY_OFFSET)
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
//  Default Implementations
//

impl Default for Type {
    fn default() -> Self { Type::Unresolved }
}

impl Default for ElaborateType {
    fn default() -> Self {
        ElaborateType::Unresolved(Default::default(), Default::default())
    }
}

impl Default for PathComponent {
    fn default() -> Self { PathComponent::Unresolved(Default::default()) }
}

impl Default for Scope {
    fn default() -> Self { Scope::Module }
}

//
//  From Implementations
//

impl convert::From<EnumId> for Item {
    fn from(e: EnumId) -> Self { Item::Enum(e) }
}

impl convert::From<ExtensionId> for Item {
    fn from(e: ExtensionId) -> Self { Item::Ext(e) }
}

impl convert::From<FunctionId> for Item {
    fn from(f: FunctionId) -> Self { Item::Fun(f) }
}

impl convert::From<ImplementationId> for Item {
    fn from(i: ImplementationId) -> Self { Item::Imp(i) }
}

impl convert::From<InterfaceId> for Item {
    fn from(i: InterfaceId) -> Self { Item::Int(i) }
}

impl convert::From<RecordId> for Item {
    fn from(r: RecordId) -> Self { Item::Rec(r) }
}

//
//  TryFrom Implementations
//

impl convert::TryFrom<Type> for ElaborateType {
    type Error = &'static str;

    fn try_from(t: Type) -> Result<Self, Self::Error> {
        use self::Type::*;

        match t {
            Builtin(b) => Ok(ElaborateType::Builtin(b)),
            Enum(e) => Ok(ElaborateType::Enum(e, PathId::empty())),
            Int(i) => Ok(ElaborateType::Int(i, PathId::empty())),
            Rec(r) => Ok(ElaborateType::Rec(r, PathId::empty())),
            Tuple(..) => Err("No conversion from Type::Tuple to ElaborateType"),
            Unresolved => Ok(ElaborateType::Unresolved(Default::default(), PathId::empty())),
        }
    }
}

impl convert::TryFrom<ElaborateType> for Type {
    type Error = &'static str;

    fn try_from(t: ElaborateType) -> Result<Self, Self::Error> {
        use self::ElaborateType::*;

        match t {
            Builtin(b) => Ok(Type::Builtin(b)),
            Enum(e, ..) => Ok(Type::Enum(e)),
            Int(i, ..) => Ok(Type::Int(i)),
            Rec(r, ..) => Ok(Type::Rec(r)),
            Tuple(..) => Err("No conversion from ElaborateType::Tuple to Type"),
            Unresolved(..) => Ok(Type::Unresolved),
        }
    }
}
