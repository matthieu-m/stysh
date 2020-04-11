//! A unified Registry, abstracting over the local Module and distant Repository.

use std::cell;

use crate::model::hir::*;

/// Reg
///
/// A Registry unifying over a local Module and a distant Repository.
#[derive(Clone, Copy, Debug)]
pub struct Reg<'a> {
    repository: &'a RepositorySnapshot,
    module: &'a Module,
}

impl<'a> Reg<'a> {
    /// Creates an instance.
    pub fn new(repository: &'a RepositorySnapshot, module: &'a Module) -> Self {
        Reg { repository, module }
    }
}

impl<'a> Registry for Reg<'a> {
    fn get_builtin_functions(&self, ty: BuiltinType) -> &[(Identifier, FunctionId)] {
        let module = self.module.get_builtin_functions(ty);

        //  Builtin extensions should only ever belong to a core module, so
        //  either this is the core module and it has them, or it is not
        //  and they are in the repository.
        if !module.is_empty() {
            module
        } else {
            self.repository.get_builtin_functions(ty)
        }
    }

    fn enums(&self) -> Vec<EnumId> {
        let mut result = self.module.enums();
        result.extend(&self.repository.enums());
        result
    }

    fn get_enum(&self, id: EnumId) -> Enum {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_enum(id)
        } else {
            self.repository.get_enum(id)
        }
    }

    fn get_enum_functions(&self, id: EnumId) -> &[(Identifier, FunctionId)] {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_enum_functions(id)
        } else {
            self.repository.get_enum_functions(id)
        }
    }

    fn extensions(&self) -> Vec<ExtensionId> {
        let mut result = self.module.extensions();
        result.extend(&self.repository.extensions());
        result
    }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_extension(id)
        } else {
            self.repository.get_extension(id)
        }
    }

    fn functions(&self) -> Vec<FunctionId> {
        let mut result = self.module.functions();
        result.extend(&self.repository.functions());
        result
    }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_function(id)
        } else {
            self.repository.get_function(id)
        }
    }

    fn implementations(&self) -> Vec<ImplementationId> {
        let mut result = self.module.implementations();
        result.extend(&self.repository.implementations());
        result
    }

    fn get_implementation(&self, id: ImplementationId) -> Implementation {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_implementation(id)
        } else {
            self.repository.get_implementation(id)
        }
    }

    fn get_implementation_functions(&self, id: ImplementationId)
        -> &[(Identifier, FunctionId)]
    {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_implementation_functions(id)
        } else {
            self.repository.get_implementation_functions(id)
        }
    }

    fn get_implementation_of(&self, int: InterfaceId, ty: Type)
        -> Option<ImplementationId>
    {
        debug_assert!(int.is_module() || int.is_repository(), "{:?}", int);

        if int.is_module() {
            self.module.get_implementation_of(int, ty)
        } else {
            self.repository.get_implementation_of(int, ty)
        }
    }

    fn interfaces(&self) -> Vec<InterfaceId> {
        let mut result = self.module.interfaces();
        result.extend(&self.repository.interfaces());
        result
    }

    fn get_interface(&self, id: InterfaceId) -> Interface {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_interface(id)
        } else {
            self.repository.get_interface(id)
        }
    }

    fn get_interface_functions(&self, id: InterfaceId) -> &[(Identifier, FunctionId)] {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_interface_functions(id)
        } else {
            self.repository.get_interface_functions(id)
        }
    }

    fn records(&self) -> Vec<RecordId> {
        let mut result = self.module.records();
        result.extend(&self.repository.records());
        result
    }

    fn get_record(&self, id: RecordId) -> Record {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_record(id)
        } else {
            self.repository.get_record(id)
        }
    }

    fn get_record_functions(&self, id: RecordId) -> &[(Identifier, FunctionId)] {
        debug_assert!(id.is_module() || id.is_repository(), "{:?}", id);

        if id.is_module() {
            self.module.get_record_functions(id)
        } else {
            self.repository.get_record_functions(id)
        }
    }

    fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType {
        debug_assert!(id.is_builtin() || id.is_module() || id.is_repository(), "{:?}", id);

        if let Some(b) = id.builtin() {
            ElaborateType::Builtin(b)
        } else if id.is_module() {
            self.module.get_elaborate_type(id)
        } else {
            self.repository.get_elaborate_type(id)
        }
    }
  
    fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>) -> &[ElaborateTypeId] {
        debug_assert!(id == Id::empty() || id.is_module() || id.is_repository(), "{:?}", id);

        if id == Id::empty() {
            &[]
        } else if id.is_module() {
            self.module.get_elaborate_type_ids(id)
        } else {
            self.repository.get_elaborate_type_ids(id)
        }
    }

    fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        debug_assert!(id == Id::empty() || id.is_module() || id.is_repository(), "{:?}", id);

        if id == Id::empty() {
            &[]
        } else if id.is_module() {
            self.module.get_names(id)
        } else {
            self.repository.get_names(id)
        }
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        debug_assert!(id == Id::empty() || id.is_module() || id.is_repository(), "{:?}", id);

        if id == Id::empty() {
            &[]
        } else if id.is_module() {
            self.module.get_path_components(id)
        } else {
            self.repository.get_path_components(id)
        }
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        debug_assert!(id == Id::empty() || id.is_module() || id.is_repository(), "{:?}", id);

        if id == Id::empty() {
            &[]
        } else if id.is_module() {
            self.module.get_record_ids(id)
        } else {
            self.repository.get_record_ids(id)
        }
    }

    fn get_type(&self, id: TypeId) -> Type {
        debug_assert!(id.is_builtin() || id.is_module() || id.is_repository(), "{:?}", id);

        if let Some(b) = id.builtin() {
            Type::Builtin(b)
        } else if id.is_module() {
            self.module.get_type(id)
        } else {
            self.repository.get_type(id)
        }
    }

    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        debug_assert!(id == Id::empty() || id.is_module() || id.is_repository(), "{:?}", id);

        if id == Id::empty() {
            &[]
        } else if id.is_module() {
            self.module.get_type_ids(id)
        } else {
            self.repository.get_type_ids(id)
        }
    }
}

/// RegRef
///
/// A Registry unifiying over a Tree and a Registry.
#[derive(Debug)]
pub struct RegRef<'a> {
    registry: &'a dyn Registry,
    tree: cell::Ref<'a, Tree>,
}

impl<'a> RegRef<'a> {
    /// Creates an instance.
    pub fn new(registry: &'a dyn Registry, tree: cell::Ref<'a, Tree>) -> Self {
        RegRef { registry, tree }
    }
}

impl<'a> Registry for RegRef<'a> {
    fn get_builtin_functions(&self, ty: BuiltinType) -> &[(Identifier, FunctionId)] {
        self.registry.get_builtin_functions(ty)
    }

    fn enums(&self) -> Vec<EnumId> { self.registry.enums() }

    fn get_enum(&self, id: EnumId) -> Enum { self.registry.get_enum(id) }

    fn get_enum_functions(&self, id: EnumId) -> &[(Identifier, FunctionId)] {
        self.registry.get_enum_functions(id)
    }

    fn extensions(&self) -> Vec<ExtensionId> { self.registry.extensions() }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        self.registry.get_extension(id)
    }

    fn functions(&self) -> Vec<FunctionId> { self.registry.functions() }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        self.registry.get_function(id)
    }

    fn implementations(&self) -> Vec<ImplementationId> { self.registry.implementations() }

    fn get_implementation(&self, id: ImplementationId) -> Implementation {
        self.registry.get_implementation(id)
    }

    fn get_implementation_functions(&self, id: ImplementationId)
        -> &[(Identifier, FunctionId)]
    {
        self.registry.get_implementation_functions(id)
    }

    fn get_implementation_of(&self, int: InterfaceId, ty: Type)
        -> Option<ImplementationId>
    {
        self.registry.get_implementation_of(int, ty)
    }

    fn interfaces(&self) -> Vec<InterfaceId> { self.registry.interfaces() }

    fn get_interface(&self, id: InterfaceId) -> Interface {
        self.registry.get_interface(id)
    }

    fn get_interface_functions(&self, id: InterfaceId) -> &[(Identifier, FunctionId)] {
        self.registry.get_interface_functions(id)
    }

    fn records(&self) -> Vec<RecordId> { self.registry.records() }

    fn get_record(&self, id: RecordId) -> Record { self.registry.get_record(id) }

    fn get_record_functions(&self, id: RecordId) -> &[(Identifier, FunctionId)] {
        self.registry.get_record_functions(id)
    }

    fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType {
        if let Some(b) = id.builtin() {
            ElaborateType::Builtin(b)
        } else if id.is_tree() {
            self.tree.get_elaborate_type(id)
        } else {
            self.registry.get_elaborate_type(id)
        }
    }
  
    fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>) -> &[ElaborateTypeId] {
        if id == Id::empty() {
            &[]
        } else if id.is_tree() {
            self.tree.get_elaborate_type_ids(id)
        } else {
            self.registry.get_elaborate_type_ids(id)
        }
    }

    fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        if id == Id::empty() {
            &[]
        } else if id.is_tree() {
            self.tree.get_names(id)
        } else {
            self.registry.get_names(id)
        }
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        if id == Id::empty() {
            &[]
        } else if id.is_tree() {
            self.tree.get_path(id)
        } else {
            self.registry.get_path_components(id)
        }
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
         self.registry.get_record_ids(id)
    }

    fn get_type(&self, id: TypeId) -> Type {
        if let Some(b) = id.builtin() {
            Type::Builtin(b)
        } else if id.is_tree() {
            self.tree.get_type(id)
        } else {
            self.registry.get_type(id)
        }
    }

    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id == Id::empty() {
            &[]
        } else if id.is_tree() {
            self.tree.get_type_ids(id)
        } else {
            self.registry.get_type_ids(id)
        }
    }
}
