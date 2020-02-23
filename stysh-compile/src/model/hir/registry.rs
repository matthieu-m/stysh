//! Registry
//!
//! A read-only interface to obtain the definition of items.

use std::fmt;

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
