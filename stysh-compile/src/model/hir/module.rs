//! Module
//!
//! A stand-alone flat representation of the various items which compose a
//! module: functions, types, ... all items within a given module, no matter
//! their nesting, are stored in a single Module object.
//!
//! A stand-alone representation has the benefit that the Module may be
//! understood in isolation.
//!
//! A flat representation has 3 key benefits:
//! -   Direct addressing: it is possible to keep the ID of any node.
//! -   Efficient allocation: few allocations, and compact representation.
//! -   Efficient in-place mutation.
//!
//! Furthermore, those benefits are achieved whilst retaining control over
//! mutability, allowing to "freeze" the graph at some point, unlike inner
//! mutability which persists long after it ceased to be necessary.
//!
//! Note:   this layout is inspired by the realization that ECS are a great fit
//!         for Rust.

use std::collections::HashMap;

use basic::com::{Range, Store, MultiStore};
use basic::sea::{MultiTable, Table};

use model::com::ModuleId;
use model::hir::*;

/// Module.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Module {
    /// Id.
    module_id: ModuleId,

    //
    //  Enums
    //

    /// Enum canonical name to EnumId.
    enum_lookup: HashMap<ItemIdentifier, EnumId>,
    /// Definition of a given Enum.
    enum_: Table<EnumId, Enum>,

    //
    //  Records
    //

    /// Record canonical name to RecordId.
    record_lookup: HashMap<ItemIdentifier, RecordId>,
    /// Definition of a given Record.
    record: Table<RecordId, Record>,

    //
    //  Functions
    //

    /// Function canonical name to FunctionId.
    function_lookup: HashMap<ItemIdentifier, FunctionId>,
    /// Signature of a given Function.
    function: Table<FunctionId, FunctionSignature>,

    //
    //  Components
    //

    /// Names
    names: KeyedMulti<ValueIdentifier>,
    /// Path
    path_components: KeyedMulti<PathComponent>,
    /// Path
    record_ids: KeyedMulti<RecordId>,
    /// Types
    type_: Table<TypeId, Type>,
    /// TypeIds
    type_ids: KeyedMulti<TypeId>,
}

impl Module {
    /// Creates a new instance.
    pub fn new(module_id: ModuleId) -> Self {
        Module { module_id, ..Module::default() }
    }

    /// Returns the ID of the module.
    pub fn module_id(&self) -> ModuleId { self.module_id }


    //
    //  Enum
    //

    /// Returns the list of EnumId stored within.
    pub fn enums(&self) -> Vec<EnumId> {
        let mut result: Vec<_> = self.enum_lookup.values().cloned().collect();
        result.sort();
        result
    }

    /// Returns the EnumId corresponding to the ItemIdentifier, if any.
    pub fn lookup_enum(&self, name: ItemIdentifier) -> Option<EnumId> {
        self.enum_lookup.get(&name).cloned()
    }

    /// Inserts an Enum name.
    ///
    /// Returns the EnumId created for it.
    pub fn push_enum_name(&mut self, name: ItemIdentifier) -> EnumId {
        debug_assert!(self.enum_.len() == self.enum_lookup.len());
        debug_assert!(!self.enum_lookup.contains_key(&name));

        let id = EnumId::new_module(self.enum_.len() as u32);
        let local_id = Self::localize(id);

        self.enum_.push(&local_id, Default::default());
        self.enum_lookup.insert(name, id);

        id
    }

    /// Sets an Enum.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_enum(&mut self, id: EnumId, enum_: Enum) {
        debug_assert!(id.is_module());
        debug_assert!(self.enum_.len() == self.enum_lookup.len());

        let id = Self::localize(id);
        *self.enum_.at_mut(&id) = enum_;
    }


    //
    //  Records
    //

    /// Returns the list of RecordId stored within.
    pub fn records(&self) -> Vec<RecordId> {
        let mut result: Vec<_> = self.record_lookup.values().cloned().collect();
        result.sort();
        result
    }

    /// Returns the RecordId corresponding to the ItemIdentifier, if any.
    pub fn lookup_record(&self, name: ItemIdentifier) -> Option<RecordId> {
        self.record_lookup.get(&name).cloned()
    }

    /// Inserts a Record name.
    ///
    /// Returns the RecordId created for it.
    pub fn push_record_name(&mut self, name: ItemIdentifier) -> RecordId {
        debug_assert!(self.record.len() == self.record_lookup.len());
        debug_assert!(!self.record_lookup.contains_key(&name));

        let id = RecordId::new_module(self.record.len() as u32);
        let local_id = Self::localize(id);

        self.record.push(&local_id, Default::default());
        self.record_lookup.insert(name, id);

        id
    }

    /// Sets a Record.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_record(&mut self, id: RecordId, record: Record) {
        debug_assert!(id.is_module());
        debug_assert!(self.record.len() == self.record_lookup.len());

        let id = Self::localize(id);
        *self.record.at_mut(&id) = record;
    }


    //
    //  Functions
    //

    /// Returns the list of FunctionId stored within.
    pub fn functions(&self) -> Vec<FunctionId> {
        let mut result: Vec<_> = self.function_lookup.values().cloned().collect();
        result.sort();
        result
    }

    /// Returns the FunctionId corresponding to the ItemIdentifier, if any.
    pub fn lookup_function(&self, name: ItemIdentifier) -> Option<FunctionId> {
        self.function_lookup.get(&name).cloned()
    }

    /// Inserts a Function name.
    ///
    /// Returns the FunctionId created for it.
    pub fn push_function_name(&mut self, name: ItemIdentifier) -> FunctionId {
        debug_assert!(self.function.len() == self.function_lookup.len());
        debug_assert!(!self.function_lookup.contains_key(&name));

        let id = FunctionId::new_module(self.function.len() as u32);
        let local_id = Self::localize(id);

        self.function.push(&local_id, Default::default());
        self.function_lookup.insert(name, id);

        id
    }

    /// Sets a Function.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_function(&mut self, id: FunctionId, function: FunctionSignature) {
        debug_assert!(id.is_module());
        debug_assert!(self.function.len() == self.function_lookup.len());

        let id = Self::localize(id);
        *self.function.at_mut(&id) = function;
    }


    //
    //  Components
    //

    /// Pushes a new slice of names.
    ///
    /// Returns the ID created for it.
    pub fn push_names<I>(&mut self, names: I) -> Id<[ValueIdentifier]>
        where
            I: IntoIterator<Item = ValueIdentifier>
    {
        self.names.create(names)
            .map(Self::modularize)
            .unwrap_or(Id::empty())
    }

    /// Pushes a new slice of path components.
    ///
    /// Returns the ID created for it.
    pub fn push_path_components<I>(&mut self, path_components: I)
        -> Id<[PathComponent]>
        where
            I: IntoIterator<Item = PathComponent>
    {
        self.path_components.create(path_components)
            .map(Self::modularize)
            .unwrap_or(Id::empty())
    }

    /// Pushes a new slice of record IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_record_ids<I>(&mut self, record_ids: I) -> Id<[RecordId]>
        where
            I: IntoIterator<Item = RecordId>
    {
        self.record_ids.create(record_ids)
            .map(Self::modularize)
            .unwrap_or(Id::empty())
    }

    /// Sets the type associated to the id.
    ///
    /// #   Panics
    ///
    /// Panics if attempting to associate a non built-in Type to a
    /// built-in TypeId.
    pub fn set_type(&mut self, id: TypeId, ty: Type) -> TypeId {
        if let Some(_) = id.builtin() {
            if let Type::Builtin(b) = ty {
                return TypeId::from(b);
            }

            panic!("Cannot update a built-in to a non built-in");
        }

        let local_id = Self::localize(id);
        *self.type_.at_mut(&local_id) = ty;

        if let Type::Builtin(b) = ty {
            TypeId::from(b)
        } else {
            id
        }
    }

    /// Inserts a new type.
    ///
    /// Returns the ID created for it.
    pub fn push_type(&mut self, typ: Type) -> TypeId {
        let ty = Self::modularize(self.type_.extend(typ));

        if let Type::Builtin(b) = typ {
            TypeId::from(b)
        } else {
            ty
        }
    }

    /// Pushes a new slice of Type IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_type_ids<I>(&mut self, type_ids: I) -> Id<[TypeId]>
        where
            I: IntoIterator<Item = TypeId>
    {
        self.type_ids.create(type_ids)
            .map(Self::modularize)
            .unwrap_or(Id::empty())
    }
}

impl Module {
    fn localize<I: ItemId>(id: I) -> I {
        I::new_tree(id.get_module().expect("module"))
    }

    fn modularize<I: ItemId>(id: I) -> I {
        I::new_module(id.get_tree().expect("tree"))
    }
}

impl Registry for Module {
    fn get_enum(&self, id: EnumId) -> Enum {
        let id = Self::localize(id);
        *self.enum_.at(&id)
    }

    fn get_record(&self, id: RecordId) -> Record {
        let id = Self::localize(id);
        *self.record.at(&id)
    }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        let id = Self::localize(id);
        *self.function.at(&id)
    }

    fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        let id = Self::localize(id);
        self.names.get(&id)
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        let id = Self::localize(id);
        self.path_components.get(&id)
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        let id = Self::localize(id);
        self.record_ids.get(&id)
    }

    fn get_type(&self, id: TypeId) -> Type {
        let id = Self::localize(id);
        *self.type_.at(&id)
    }

    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        let id = Self::localize(id);
        self.type_ids.get(&id)
    }
}

impl Store<Type, TypeId> for Module {
    fn len(&self) -> usize { self.type_.len() }

    fn get(&self, id: TypeId) -> Type { self.get_type(id) }

    fn get_range(&self, _: TypeId) -> Range {
        unimplemented!("<Module as Store<Type>>::get_range")
    }

    fn push(&mut self, item: Type, _: Range) -> TypeId {
        self.push_type(item)
    }
}

impl MultiStore<ValueIdentifier> for Module {
    fn get_slice(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        if id.is_empty() { &[] } else { self.get_names(id) }
    }

    fn push_slice(&mut self, items: &[ValueIdentifier]) -> Id<[ValueIdentifier]> {
        self.push_names(items.iter().cloned())
    }
}

impl MultiStore<PathComponent> for Module {
    fn get_slice(&self, id: Id<[PathComponent]>) -> &[PathComponent] {
        if id.is_empty() { &[] } else { self.get_path_components(id) }
    }

    fn push_slice(&mut self, items: &[PathComponent]) -> Id<[PathComponent]> {
        self.push_path_components(items.iter().cloned())
    }
}

impl MultiStore<TypeId> for Module {
    fn get_slice(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id.is_empty() { &[] } else { self.get_type_ids(id) }
    }

    fn push_slice(&mut self, items: &[TypeId]) -> Id<[TypeId]> {
        self.push_type_ids(items.iter().cloned())
    }
}

//
//  Private Type
//

type KeyedMulti<T> = MultiTable<Id<[T]>, T>;