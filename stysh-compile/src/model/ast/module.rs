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

use std::fmt;
use std::collections::BTreeMap;

use crate::basic::com::{Range, Span, Store, MultiStore};
use crate::basic::sea::{MultiTable, Table};

use crate::model::com::ModuleId;
use crate::model::ast::*;

//
//  Public Types
//

/// Module.
///
/// A container for top-level items of a scope (such as a module).
///
/// Unlike the expression tree, this may contain cycles.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct Module {
    /// Id.
    module_id: ModuleId,

    //
    //  Look-ups
    //

    /// Enums.
    enum_lookup: BTreeMap<TypeIdentifier, EnumId>,
    /// Extensions.
    extension_lookup: BTreeMap<TypeIdentifier, ExtensionId>,
    /// Functions.
    function_lookup: BTreeMap<VariableIdentifier, FunctionId>,
    /// Records.
    record_lookup: BTreeMap<TypeIdentifier, RecordId>,

    //
    //  Single
    //

    /// Enum.
    enum_: KeyedSingle<Enum>,
    /// Extension.
    extension: KeyedSingle<Extension>,
    /// Function.
    function: KeyedSingle<Function>,
    /// Range of a function.
    function_range: KeyedRange<Function>,
    /// Body of a function.
    function_body: Table<FunctionId, Tree>,
    /// Extension of a function, if any.
    function_extension: Table<FunctionId, Option<ExtensionId>>,
    /// Record.
    record: KeyedSingle<Record>,
    /// Type.
    type_: KeyedSingle<Type>,
    /// Range of a type.
    type_range: KeyedRange<Type>,

    //
    //  Multi
    //

    /// Arguments.
    arguments: KeyedMulti<Argument>,
    /// Functions.
    functions: KeyedMulti<FunctionId>,
    /// Identifiers.
    identifiers: KeyedMulti<Identifier>,
    /// InnerRecords.
    inner_records: KeyedMulti<InnerRecord>,
    /// Positions.
    positions: KeyedMulti<u32>,
    /// Type IDs.
    type_ids: KeyedMulti<TypeId>,
}

//
//  Public Methods
//

impl Module {
    //  Generic.

    /// Creates a new instance.
    pub fn new(module_id: ModuleId) -> Self {
        Module { module_id, ..Module::default() }
    }

    /// Returns the ID of the module.
    pub fn module_id(&self) -> ModuleId { self.module_id }


    //  Enums.

    /// Returns the number of enums.
    pub fn len_enums(&self) -> usize { self.enum_lookup.len() }

    /// Looks up an enum ID by identifier.
    pub fn get_enum_id(&self, id: TypeIdentifier) -> Option<EnumId> {
        self.enum_lookup.get(&id).copied()
    }

    /// Returns the enum associated to the ID.
    pub fn get_enum(&self, id: EnumId) -> Enum {
        *self.enum_.at(&id)
    }

    /// Returns the range of the enum associated to the ID.
    pub fn get_enum_range(&self, id: EnumId) -> Range {
        self.enum_.at(&id).span()
    }

    /// Pushes a new enum.
    ///
    /// Returns the ID created for it.
    pub fn push_enum(&mut self, enum_: Enum) -> EnumId {
        let id = Id::new(self.enum_.len() as u32);
        self.enum_.push(&id, enum_);

        self.enum_lookup.insert(enum_.name, id);

        id
    }


    //  Extensions.

    /// Returns the number of extensions.
    pub fn len_extensions(&self) -> usize { self.extension_lookup.len() }

    /// Looks up an extension ID by identifier.
    pub fn get_extension_id(&self, id: TypeIdentifier) -> Option<ExtensionId> {
        self.extension_lookup.get(&id).copied()
    }

    /// Returns the extension associated to the ID.
    pub fn get_extension(&self, id: ExtensionId) -> Extension {
        *self.extension.at(&id)
    }

    /// Returns the range of the extension associated to the ID.
    pub fn get_extension_range(&self, id: ExtensionId) -> Range {
        self.extension.at(&id).span()
    }

    /// Pushes a new extension.
    ///
    /// Returns the ID created for it.
    pub fn push_extension(&mut self, ext: Extension) -> ExtensionId {
        let id = Id::new(self.extension.len() as u32);
        self.extension.push(&id, ext);

        self.extension_lookup.insert(ext.name, id);

        id
    }


    //  Functions.

    /// Returns the number of functions.
    pub fn len_functions(&self) -> usize { self.function_lookup.len() }

    /// Looks up an function ID by identifier.
    pub fn get_function_id(&self, id: VariableIdentifier) -> Option<FunctionId> {
        self.function_lookup.get(&id).copied()
    }

    /// Returns the function associated to the ID.
    pub fn get_function(&self, id: FunctionId) -> Function {
        *self.function.at(&id)
    }

    /// Returns the range of the function associated to the ID.
    pub fn get_function_range(&self, id: FunctionId) -> Range {
        *self.function_range.at(&id)
    }

    /// Returns a reference to the body of the function associated to the ID.
    pub fn get_function_body(&self, id: FunctionId) -> &Tree {
        self.function_body.at(&id)
    }

    /// Returns the extension to which the function belongs to, if any.
    pub fn get_function_extension(&self, id: FunctionId) -> Option<ExtensionId> {
        *self.function_extension.at(&id)
    }

    /// Associate function to an extension.
    pub fn set_function_extension(&mut self, fun: FunctionId, ext: ExtensionId) {
        self.function_extension.replace(&fun, Some(ext));
    }

    /// Pushes a new function.
    ///
    /// Returns the ID created for it.
    pub fn push_function(&mut self, tree: Tree) -> FunctionId {
        debug_assert!(self.function.len() == self.function_range.len());
        debug_assert!(self.function.len() == self.function_body.len());
        debug_assert!(self.function.len() == self.function_extension.len());

        let (mut function, body) = if let Some(Root::Function(fun, body)) = tree.get_root() {
            (fun, body)
        } else {
            unreachable!("Expected a function root, got {:?}", tree.get_root());
        };

        function.arguments = {
            let mut arguments = tree.get_arguments(function.arguments).to_vec();
            for a in &mut arguments {
                a.type_ = replicate_type(a.type_, &tree, self);
            }
            self.push_arguments(&arguments)
        };

        function.result = replicate_type(function.result, &tree, self);

        let range = Range::new(function.keyword as usize, 4)
            .extend(tree.get_expression_range(body));

        let id = Id::new(self.function.len() as u32);
        self.function.push(&id, function);
        self.function_range.push(&id, range);
        self.function_body.push(&id, tree);
        self.function_extension.push(&id, None);

        self.function_lookup.insert(function.name, id);

        id
    }


    //  Records.

    /// Returns the number of records.
    pub fn len_records(&self) -> usize { self.record_lookup.len() }

    /// Looks up an record ID by identifier.
    pub fn get_record_id(&self, id: TypeIdentifier) -> Option<RecordId> {
        self.record_lookup.get(&id).copied()
    }

    /// Returns the record associated to the ID.
    pub fn get_record(&self, id: RecordId) -> Record {
        *self.record.at(&id)
    }

    /// Returns the range of the record associated to the ID.
    pub fn get_record_range(&self, id: RecordId) -> Range {
        self.record.at(&id).span()
    }

    /// Pushes a new record.
    ///
    /// Returns the ID created for it.
    pub fn push_record(&mut self, record: Record) -> RecordId {
        let id = Id::new(self.record.len() as u32);
        self.record.push(&id, record);
        self.record_lookup.insert(record.name(), id);

        id
    }


    //  Types.

    /// Returns the number of types.
    pub fn len_types(&self) -> usize { self.type_.len() }

    /// Returns the type associated to the ID.
    pub fn get_type(&self, id: TypeId) -> Type {
        *self.type_.at(&id)
    }

    /// Returns the range of the type associated to the ID.
    pub fn get_type_range(&self, id: TypeId) -> Range {
        *self.type_range.at(&id)
    }

    /// Pushes a new type.
    ///
    /// Returns the ID created for it.
    pub fn push_type(&mut self, ty: Type, range: Range) -> TypeId {
        debug_assert!(self.type_.len() == self.type_range.len());

        let id = Id::new(self.type_.len() as u32);
        self.type_.push(&id, ty);
        self.type_range.push(&id, range);

        id
    }


    //  Arguments

    /// Returns the arguments associated to the ID.
    pub fn get_arguments(&self, id: Id<[Argument]>) -> &[Argument] {
        if id.is_empty() { &[] } else { self.arguments.get(&id) }
    }

    /// Pushes a new slice of arguments.
    ///
    /// Returns the ID created for it.
    pub fn push_arguments(&mut self, arguments: &[Argument]) -> Id<[Argument]> {
        Self::push_slice(&mut self.arguments, arguments)
    }


    //  Functions

    /// Returns the function IDs associated to the ID.
    pub fn get_function_ids(&self, id: Id<[FunctionId]>) -> &[FunctionId] {
        if id.is_empty() { &[] } else { self.functions.get(&id) }
    }

    /// Pushes a new slice of function IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_function_ids(&mut self, functions: &[FunctionId]) -> Id<[FunctionId]> {
        Self::push_slice(&mut self.functions, functions)
    }


    //  Identifiers

    /// Returns the identifiers associated to the ID.
    pub fn get_identifiers(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id.is_empty() { &[] } else { self.identifiers.get(&id) }
    }

    /// Pushes a new slice of expression IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_identifiers(&mut self, identifiers: &[Identifier]) -> Id<[Identifier]> {
        Self::push_slice(&mut self.identifiers, identifiers)
    }


    //  Inner Records

    /// Returns the inner records associated to the ID.
    pub fn get_inner_records(&self, id: Id<[InnerRecord]>) -> &[InnerRecord] {
        if id.is_empty() { &[] } else { self.inner_records.get(&id) }
    }

    /// Pushes a new slice of inner records.
    ///
    /// Returns the ID created for it.
    pub fn push_inner_records(&mut self, inner_records: &[InnerRecord]) -> Id<[InnerRecord]> {
        Self::push_slice(&mut self.inner_records, inner_records)
    }


    //  Positions

    /// Returns the positions associated to the ID.
    pub fn get_positions(&self, id: Id<[u32]>) -> &[u32] {
        if id.is_empty() { &[] } else { self.positions.get(&id) }
    }

    /// Pushes a new slice of positions.
    ///
    /// Returns the ID created for it.
    pub fn push_positions(&mut self, positions: &[u32]) -> Id<[u32]> {
        Self::push_slice(&mut self.positions, positions)
    }


    //  Type IDs

    /// Returns the type IDs associated to the ID.
    pub fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id.is_empty() { &[] } else { self.type_ids.get(&id) }
    }

    /// Pushes a new slice of type IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_type_ids(&mut self, type_ids: &[TypeId]) -> Id<[TypeId]> {
        Self::push_slice(&mut self.type_ids, type_ids)
    }
}


//
//  Implementation of Store
//

impl Store<Enum> for Module {
    fn len(&self) -> usize { self.len_enums() }
    fn get(&self, id: EnumId) -> Enum { self.get_enum(id) }
    fn get_range(&self, id: EnumId) -> Range { self.get_enum_range(id) }
    fn push(&mut self, e: Enum, _: Range) -> EnumId { self.push_enum(e) }
}

impl Store<Extension> for Module {
    fn len(&self) -> usize { self.len_extensions() }
    fn get(&self, id: ExtensionId) -> Extension { self.get_extension(id) }
    fn get_range(&self, id: ExtensionId) -> Range { self.get_extension_range(id) }
    fn push(&mut self, e: Extension, _: Range) -> ExtensionId { self.push_extension(e) }
}

impl Store<Record> for Module {
    fn len(&self) -> usize { self.len_records() }
    fn get(&self, id: RecordId) -> Record { self.get_record(id) }
    fn get_range(&self, id: RecordId) -> Range { self.get_record_range(id) }
    fn push(&mut self, r: Record, _: Range) -> RecordId { self.push_record(r) }
}

impl Store<Type> for Module {
    fn len(&self) -> usize { self.len_types() }
    fn get(&self, id: TypeId) -> Type { self.get_type(id) }
    fn get_range(&self, id: TypeId) -> Range { self.get_type_range(id) }
    fn push(&mut self, ty: Type, r: Range) -> TypeId { self.push_type(ty, r) }
}


//
//  Implementation of MultiStore
//

impl MultiStore<Argument> for Module {
    fn get_slice(&self, id: Id<[Argument]>) -> &[Argument] { self.get_arguments(id) }
    fn push_slice(&mut self, items: &[Argument]) -> Id<[Argument]> { self.push_arguments(items) }
}

impl MultiStore<FunctionId> for Module {
    fn get_slice(&self, id: Id<[FunctionId]>) -> &[FunctionId] { self.get_function_ids(id) }
    fn push_slice(&mut self, items: &[FunctionId]) -> Id<[FunctionId]> { self.push_function_ids(items) }
}

impl MultiStore<Identifier> for Module {
    fn get_slice(&self, id: Id<[Identifier]>) -> &[Identifier] { self.get_identifiers(id) }
    fn push_slice(&mut self, items: &[Identifier]) -> Id<[Identifier]> { self.push_identifiers(items) }
}

impl MultiStore<InnerRecord> for Module {
    fn get_slice(&self, id: Id<[InnerRecord]>) -> &[InnerRecord] { self.get_inner_records(id) }
    fn push_slice(&mut self, items: &[InnerRecord]) -> Id<[InnerRecord]> { self.push_inner_records(items) }
}

impl MultiStore<u32> for Module {
    fn get_slice(&self, id: Id<[u32]>) -> &[u32] { self.get_positions(id) }
    fn push_slice(&mut self, items: &[u32]) -> Id<[u32]> { self.push_positions(items) }
}

impl MultiStore<TypeId> for Module {
    fn get_slice(&self, id: Id<[TypeId]>) -> &[TypeId] { self.get_type_ids(id) }
    fn push_slice(&mut self, items: &[TypeId]) -> Id<[TypeId]> { self.push_type_ids(items) }
}


//
//  Implementations of std traits
//

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Module {{ module_id: {:?}, ", self.module_id)?;

        if !self.enum_lookup.is_empty() {
            write!(f, "enum_lookup: {:?}, ", self.enum_lookup)?;
        }
        if !self.extension_lookup.is_empty() {
            write!(f, "extension_lookup: {:?}, ", self.extension_lookup)?;
        }
        if !self.function_lookup.is_empty() {
            write!(f, "function_lookup: {:?}, ", self.function_lookup)?;
        }
        if !self.record_lookup.is_empty() {
            write!(f, "record_lookup: {:?}, ", self.record_lookup)?;
        }

        if !self.enum_.is_empty() {
            write!(f, "enum_: {:?}, ", self.enum_)?;
        }
        if !self.extension.is_empty() {
            write!(f, "extension: {:?}, ", self.extension)?;
        }
        if !self.function.is_empty() {
            write!(f, "function: {:?}, ", self.function)?;
            write!(f, "function_range: {:?}, ", self.function_range)?;
            write!(f, "function_body: {:?}, ", self.function_body)?;
            write!(f, "function_extension: {:?}, ", self.function_extension)?;
        }
        if !self.record.is_empty() {
            write!(f, "record: {:?}, ", self.record)?;
        }
        if !self.type_.is_empty() {
            write!(f, "type_: {:?}, ", self.type_)?;
            write!(f, "type_range: {:?}, ", self.type_range)?;
        }

        if !self.arguments.is_empty() {
            write!(f, "arguments: {:?}, ", self.arguments)?;
        }
        if !self.functions.is_empty() {
            write!(f, "functions: {:?}, ", self.functions)?;
        }
        if !self.identifiers.is_empty() {
            write!(f, "identifiers: {:?}, ", self.identifiers)?;
        }
        if !self.inner_records.is_empty() {
            write!(f, "inner_records: {:?}, ", self.inner_records)?;
        }
        if !self.positions.is_empty() {
            write!(f, "positions: {:?}, ", self.positions)?;
        }
        if !self.type_ids.is_empty() {
            write!(f, "type_ids: {:?}, ", self.type_ids)?;
        }
        write!(f, "}}")
    }
}


//
//  Private Types
//

type KeyedSingle<T> = Table<Id<T>, T>;
type KeyedRange<T> = Table<Id<T>, Range>;
type KeyedMulti<T> = MultiTable<Id<[T]>, T>;


//
//  Private methods
//

impl Module {
    /// Generic Push.
    fn push_slice<T: Copy>(
        table: &mut KeyedMulti<T>,
        value: &[T]
    )
        -> Id<[T]>
    {
        table.create(value.iter().cloned()).unwrap_or(Id::empty())
    }
}
