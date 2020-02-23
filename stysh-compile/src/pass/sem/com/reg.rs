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
/// A Registry unifiying over a local and a distant Registry.
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
    fn enums(&self) -> Vec<EnumId> { self.registry.enums() }

    fn get_enum(&self, id: EnumId) -> Enum { self.registry.get_enum(id) }

    fn extensions(&self) -> Vec<ExtensionId> { self.registry.extensions() }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        self.registry.get_extension(id)
    }

    fn functions(&self) -> Vec<FunctionId> { self.registry.functions() }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        self.registry.get_function(id)
    }

    fn records(&self) -> Vec<RecordId> { self.registry.records() }

    fn get_record(&self, id: RecordId) -> Record { self.registry.get_record(id) }

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
