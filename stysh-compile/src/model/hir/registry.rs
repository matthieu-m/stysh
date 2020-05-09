//! Registry
//!
//! A read-only interface to obtain the definition of items.

use std::fmt;

use crate::model::hir::*;

/// Registry.
pub trait Registry: fmt::Debug {
    /// Returns the list of functions associated to the enum, sorted.
    fn get_builtin_functions(&self, ty: BuiltinType) -> &[(Identifier, FunctionId)];

    /// Returns the list of known enum IDs.
    fn enums(&self) -> Vec<EnumId>;

    /// Returns the definition of the enum associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_enum(&self, id: EnumId) -> Enum;

    /// Returns the list of functions associated to the enum, sorted.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_enum_functions(&self, id: EnumId) -> &[(Identifier, FunctionId)];

    /// Returns the list of known extension IDs.
    fn extensions(&self) -> Vec<ExtensionId>;

    /// Returns the definition of the extension associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_extension(&self, id: ExtensionId) -> Extension;

    /// Returns the list of known function IDs.
    fn functions(&self) -> Vec<FunctionId>;

    /// Returns the signature of the function associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_function(&self, id: FunctionId) -> FunctionSignature;

    /// Returns the list of known implementation IDs.
    fn implementations(&self) -> Vec<ImplementationId>;

    /// Returns the definition of the implementation associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_implementation(&self, id: ImplementationId) -> Implementation;

    /// Return the list of functions associated to the implementation, sorted.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_implementation_functions(&self, id: ImplementationId)
        -> &[(Identifier, FunctionId)];

    /// Returns the implemention IDs of the implementation matching the
    /// interface and type, if any.
    fn get_implementation_of(&self, int: InterfaceId, ty: Type)
        -> Option<ImplementationId>;

    /// Returns the list of known interface IDs.
    fn interfaces(&self) -> Vec<InterfaceId>;

    /// Returns the definition of the interface associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_interface(&self, id: InterfaceId) -> Interface;

    /// Returns the list of functions associated to the interface, sorted.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_interface_functions(&self, id: InterfaceId) -> &[(Identifier, FunctionId)];

    /// Returns the list of known record IDs.
    fn records(&self) -> Vec<RecordId>;

    /// Returns the definition of the record associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_record(&self, id: RecordId) -> Record;

    /// Returns the list of functions associated to the record, sorted.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_record_functions(&self, id: RecordId) -> &[(Identifier, FunctionId)];

    /// Returns the list of functions associated to tuples, sorted.
    fn get_tuple_functions(&self) -> &[(Identifier, FunctionId)];

    /// Returns the arguments associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_arguments(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier];

    /// Returns the Elaborate Type associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType;

    /// Returns the Elaborate Type IDs associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>) -> &[ElaborateTypeId];

    /// Returns the names associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_names(&self, id: Id<[Identifier]>) -> &[Identifier];

    /// Returns the path components associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent];

    /// Returns the record IDs associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId];

    /// Returns the Type associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_type(&self, id: TypeId) -> Type;

    /// Returns the type IDs associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId];
}

/// Mocks for the traits.
#[cfg(test)]
pub mod mocks {

use std::collections::BTreeMap;

use crate::model::hir::*;

use super::Registry;

/// A MockRegistry for unit-tests.
#[derive(Default, Debug)]
pub struct MockRegistry {
    /// Functions for builtins.
    pub builtin_functions: [Vec<(Identifier, FunctionId)>; BuiltinType::NUMBER],
    /// Enums.
    pub enums: Vec<Enum>,
    /// Functions for enums.
    pub enum_functions: Vec<Vec<(Identifier, FunctionId)>>,
    /// Extensions.
    pub extensions: Vec<Extension>,
    /// Functions.
    pub functions: Vec<FunctionSignature>,
    /// Implementations.
    pub implementations: Vec<Implementation>,
    /// Functions for implementations.
    pub implementation_functions: Vec<Vec<(Identifier, FunctionId)>>,
    /// Implementation by Interface/Type.
    pub implementation_of: BTreeMap<(InterfaceId, Type), ImplementationId>,
    /// Interfaces.
    pub interfaces: Vec<Interface>,
    /// Functions for interfaces.
    pub interface_functions: Vec<Vec<(Identifier, FunctionId)>>,
    /// Records.
    pub records: Vec<Record>,
    /// Functions for records.
    pub record_functions: Vec<Vec<(Identifier, FunctionId)>>,
    /// Functions for tuples.
    pub tuple_functions: Vec<(Identifier, FunctionId)>,
    /// Arguments.
    pub arguments: Vec<Vec<ValueIdentifier>>,
    /// Elaborate Types.
    pub elaborate_types: Vec<ElaborateType>,
    /// Elaborate Type IDs.
    pub elaborate_type_ids: Vec<Vec<ElaborateTypeId>>,
    /// Names.
    pub names: Vec<Vec<Identifier>>,
    /// Path Components.
    pub path_components: Vec<Vec<PathComponent>>,
    /// Record IDs.
    pub record_ids: Vec<Vec<RecordId>>,
    /// Types.
    pub types: Vec<Type>,
    /// Type IDs.
    pub type_ids: Vec<Vec<TypeId>>,
}

impl MockRegistry {
    pub fn new() -> Self { Default::default() }
}

impl Registry for MockRegistry {
    fn enums(&self) -> Vec<EnumId> { Self::enumerate(self.enums.len()) }

    fn get_builtin_functions(&self, ty: BuiltinType) -> &[(Identifier, FunctionId)] {
        &self.builtin_functions[ty.index()]
    }

    fn get_enum(&self, id: EnumId) -> Enum {
        self.enums[Self::index_of(id)]
    }

    fn get_enum_functions(&self, id: EnumId) -> &[(Identifier, FunctionId)] {
        &self.enum_functions[Self::index_of(id)]
    }

    fn extensions(&self) -> Vec<ExtensionId> {
        Self::enumerate(self.extensions.len())
    }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        self.extensions[Self::index_of(id)]
    }

    fn functions(&self) -> Vec<FunctionId> {
        Self::enumerate(self.functions.len())
    }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        self.functions[Self::index_of(id)]
    }

    fn implementations(&self) -> Vec<ImplementationId> {
        Self::enumerate(self.implementations.len())
    }

    fn get_implementation(&self, id: ImplementationId) -> Implementation {
        self.implementations[Self::index_of(id)]
    }

    fn get_implementation_functions(&self, id: ImplementationId)
        -> &[(Identifier, FunctionId)]
    {
        &self.implementation_functions[Self::index_of(id)]
    }

    fn get_implementation_of(&self, int: InterfaceId, ty: Type)
        -> Option<ImplementationId>
    {
        self.implementation_of.get(&(int, ty)).copied()
    }

    fn interfaces(&self) -> Vec<InterfaceId> {
        Self::enumerate(self.interfaces.len())
    }

    fn get_interface(&self, id: InterfaceId) -> Interface {
        self.interfaces[Self::index_of(id)]
    }

    fn get_interface_functions(&self, id: InterfaceId)
        -> &[(Identifier, FunctionId)]
    {
        &self.interface_functions[Self::index_of(id)]
    }

    fn records(&self) -> Vec<RecordId> {
        Self::enumerate(self.records.len())
    }

    fn get_record(&self, id: RecordId) -> Record {
        self.records[Self::index_of(id)]
    }

    fn get_record_functions(&self, id: RecordId)
        -> &[(Identifier, FunctionId)]
    {
        &self.record_functions[Self::index_of(id)]
    }

    fn get_tuple_functions(&self) -> &[(Identifier, FunctionId)] {
        &self.tuple_functions
    }

    fn get_arguments(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        &self.arguments[Self::index_of(id)]
    }

    fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType {
        self.elaborate_types[Self::index_of(id)]
    }

    fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>)
        -> &[ElaborateTypeId]
    {
        &self.elaborate_type_ids[Self::index_of(id)]
    }

    fn get_names(&self, id: Id<[Identifier]>) -> &[Identifier] {
        &self.names[Self::index_of(id)]
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        &self.path_components[Self::index_of(id)]
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        &self.record_ids[Self::index_of(id)]
    }

    fn get_type(&self, id: TypeId) -> Type {
        self.types[Self::index_of(id)]
    }

    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        &self.type_ids[Self::index_of(id)]
    }
}

impl MockRegistry {
    fn enumerate<T: ItemId>(to: usize) -> Vec<T> {
        (0..to as u32).into_iter().map(|i| T::new_tree(i)).collect()
    }

    fn index_of<T: ItemId>(id: T) -> usize { id.get_tree().unwrap() as usize }
}

}
