//! Registry
//!
//! A read-only interface to obtain the definition of items.

use std::{cell, fmt};

use crate::model::hir::*;

/// Registry.
pub trait Registry: fmt::Debug {
    /// Returns the list of known enum IDs.
    fn enums(&self) -> Vec<EnumId>;

    /// Returns the definition of the enum associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_enum(&self, id: EnumId) -> Enum;

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

    /// Returns the list of known record IDs.
    fn records(&self) -> Vec<RecordId>;

    /// Returns the definition of the record associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_record(&self, id: RecordId) -> Record;

    /// Returns the names associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier];

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

impl<T: Registry> Registry for cell::RefCell<T> {
    fn enums(&self) -> Vec<EnumId> { self.borrow().enums() }

    fn get_enum(&self, id: EnumId) -> Enum { self.borrow().get_enum(id) }

    fn extensions(&self) -> Vec<ExtensionId> { self.borrow().extensions() }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        self.borrow().get_extension(id)
    }

    fn functions(&self) -> Vec<FunctionId> { self.borrow().functions() }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        self.borrow().get_function(id)
    }

    fn records(&self) -> Vec<RecordId> { self.borrow().records() }

    fn get_record(&self, id: RecordId) -> Record {
        self.borrow().get_record(id)
    }

    fn get_names(&self, _id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        panic!("Cannot be implemented for cell::RefCell<T>");
    }

    fn get_path_components(&self, _id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        panic!("Cannot be implemented for cell::RefCell<T>");
    }

    fn get_record_ids(&self, _id: Id<[RecordId]>) -> &[RecordId] {
        panic!("Cannot be implemented for cell::RefCell<T>");
    }

    fn get_type(&self, _id: TypeId) -> Type {
        panic!("Cannot be implemented for cell::RefCell<T>");
    }

    fn get_type_ids(&self, _id: Id<[TypeId]>) -> &[TypeId] {
        panic!("Cannot be implemented for cell::RefCell<T>");
    }
}
