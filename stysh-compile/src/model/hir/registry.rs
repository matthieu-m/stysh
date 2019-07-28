//! Registry
//!
//! A read-only interface to obtain the definition of items.

use std::fmt;

use model::hir::*;

/// Registry.
pub trait Registry: fmt::Debug {
    /// Returns the definition of the enum associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_enum(&self, id: EnumId) -> Enum;

    /// Returns the definition of the record associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_record(&self, id: RecordId) -> Record;

    /// Returns the signature of the function associated to the ID.
    ///
    /// #   Panics
    ///
    /// Panics if the ID is incorrect.
    fn get_function(&self, id: FunctionId) -> FunctionSignature;

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
