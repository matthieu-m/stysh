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

use crate::basic::com::{IdIterator, Range, Span, Store, MultiStore};
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
    /// Functions.
    function_lookup: BTreeMap<VariableIdentifier, FunctionId>,
    /// Interfaces.
    interface_lookup: BTreeMap<TypeIdentifier, InterfaceId>,
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
    function_body: Table<FunctionId, Option<Tree>>,
    /// Scope in which the function is defined.
    function_scope: Table<FunctionId, Scope>,
    /// Implementation.
    implementation: KeyedSingle<Implementation>,
    /// Interface.
    interface: KeyedSingle<Interface>,
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
    /// Generic Parameter Pack.
    generic_parameter_pack: KeyedSingle<GenericParameterPack>,
    /// Generic Variable Pack.
    generic_variable_pack: KeyedSingle<GenericVariablePack>,
    /// Generic Variables.
    generic_variables: KeyedMulti<GenericVariable>,
    /// Identifiers.
    identifiers: KeyedMulti<Identifier>,
    /// Positions.
    positions: KeyedMulti<u32>,
    /// Record IDs.
    record_ids: KeyedMulti<RecordId>,
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

    /// Returns the enum IDs.
    pub fn enums(&self) -> IdIterator<Enum> {
        IdIterator::new(0, self.len_enums() as u32)
    }

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
    pub fn len_extensions(&self) -> usize { self.extension.len() }

    /// Returns the extension IDs.
    pub fn extensions(&self) -> IdIterator<Extension> {
        IdIterator::new(0, self.len_extensions() as u32)
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
        id
    }


    //  Functions.

    /// Returns the number of functions.
    pub fn len_functions(&self) -> usize { self.function_lookup.len() }

    /// Returns the function IDs.
    pub fn functions(&self) -> IdIterator<Function> {
        IdIterator::new(0, self.len_functions() as u32)
    }

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
    pub fn get_function_body(&self, id: FunctionId) -> &Option<Tree> {
        self.function_body.at(&id)
    }

    /// Prepares the body of the function.
    pub fn prepare_function_body(&self, id: FunctionId, tree: &mut Tree) {
        let mut function = self.get_function(id);
        debug_assert!(function.semi_colon == 0);

        let mut arguments =
            self.get_arguments(function.arguments).to_vec();
        for a in &mut arguments {
            a.type_ = replicate_type(a.type_, self, tree);
        }
        function.arguments = tree.push_arguments(&arguments);

        function.result = replicate_type(function.result, self, tree);

        tree.set_root(Root::Function(function, Default::default()));
    }

    /// Sets the body of the function, if any.
    pub fn set_function_body(&mut self, id: FunctionId, tree: Tree) {
        debug_assert!(self.get_function(id).semi_colon == 0);
        debug_assert!(self.function.len() == self.function_range.len());
        debug_assert!(self.function.len() == self.function_body.len());
        debug_assert!(self.function.len() == self.function_scope.len());

        let body = if let Some(Root::Function(_, body)) = tree.get_root() {
            body
        } else {
            unreachable!("Expected a function root, got {:?}", tree.get_root());
        };

        let range = self.get_function_range(id)
            .extend(tree.get_expression_range(body));

        self.function_range.replace(&id, range);
        self.function_body.replace(&id, Some(tree));
    }

    /// Returns the scope to which the function belongs to.
    pub fn get_function_scope(&self, id: FunctionId) -> Scope {
        *self.function_scope.at(&id)
    }

    /// Associate function to a scope.
    pub fn set_function_scope(&mut self, fun: FunctionId, scp: Scope) {
        debug_assert!(self.get_function_scope(fun) == Scope::Module);
        self.function_scope.replace(&fun, scp);
    }

    /// Pushes a new function signature.
    ///
    /// Returns the ID created for it.
    pub fn push_function_signature(&mut self, function: Function) -> FunctionId {
        debug_assert!(self.function.len() == self.function_range.len());
        debug_assert!(self.function.len() == self.function_body.len());
        debug_assert!(self.function.len() == self.function_scope.len());

        let end_offset = if function.semi_colon != 0 {
            (function.semi_colon + 1) as usize
        } else {
            self.get_type_range(function.result).end_offset()
        };

        let range = Range::new(
            function.keyword as usize,
            end_offset - function.keyword as usize,
        );

        let id = Id::new(self.function.len() as u32);
        self.function.push(&id, function);
        self.function_range.push(&id, range);
        self.function_body.push(&id, None);
        self.function_scope.push(&id, Scope::Module);

        self.function_lookup.insert(function.name, id);

        id
    }


    //  Implementations.

    /// Returns the number of implementations.
    pub fn len_implementations(&self) -> usize { self.implementation.len() }

    /// Returns the implementation IDs.
    pub fn implementations(&self) -> IdIterator<Implementation> {
        IdIterator::new(0, self.len_implementations() as u32)
    }

    /// Returns the implementation associated to the ID.
    pub fn get_implementation(&self, id: ImplementationId) -> Implementation {
        *self.implementation.at(&id)
    }

    /// Returns the range of the implementation associated to the ID.
    pub fn get_implementation_range(&self, id: ImplementationId) -> Range {
        self.implementation.at(&id).span()
    }

    /// Pushes a new implementation.
    ///
    /// Returns the ID created for it.
    pub fn push_implementation(&mut self, imp: Implementation) -> ImplementationId {
        let id = Id::new(self.implementation.len() as u32);
        self.implementation.push(&id, imp);
        id
    }


    //  Interfaces.

    /// Returns the number of interfaces.
    pub fn len_interfaces(&self) -> usize { self.interface_lookup.len() }

    /// Returns the interface IDs.
    pub fn interfaces(&self) -> IdIterator<Interface> {
        IdIterator::new(0, self.len_interfaces() as u32)
    }

    /// Looks up an interface ID by identifier.
    pub fn get_interface_id(&self, id: TypeIdentifier) -> Option<InterfaceId> {
        self.interface_lookup.get(&id).copied()
    }

    /// Returns the interface associated to the ID.
    pub fn get_interface(&self, id: InterfaceId) -> Interface {
        *self.interface.at(&id)
    }

    /// Returns the range of the interface associated to the ID.
    pub fn get_interface_range(&self, id: InterfaceId) -> Range {
        self.interface.at(&id).span()
    }

    /// Pushes a new interface.
    ///
    /// Returns the ID created for it.
    pub fn push_interface(&mut self, int: Interface) -> InterfaceId {
        let id = Id::new(self.interface.len() as u32);
        self.interface.push(&id, int);

        self.interface_lookup.insert(int.name, id);

        id
    }


    //  Records.

    /// Returns the number of records.
    pub fn len_records(&self) -> usize { self.record_lookup.len() }

    /// Returns the record IDs.
    pub fn records(&self) -> IdIterator<Record> {
        IdIterator::new(0, self.len_records() as u32)
    }

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


    //  Generics.

    /// Returns the generic parameter pack associated to the ID.
    pub fn get_generic_parameter_pack(&self, id: Id<GenericParameterPack>) -> GenericParameterPack {
        *self.generic_parameter_pack.at(&id)
    }

    /// Pushes a new generic parameter pack.
    ///
    /// Returns the ID created for it.
    pub fn push_generic_parameter_pack(&mut self, pack: GenericParameterPack) -> Id<GenericParameterPack> {
        let id = Id::new(self.generic_parameter_pack.len() as u32);
        self.generic_parameter_pack.push(&id, pack);

        id
    }

    /// Returns the generic variable pack associated to the ID.
    pub fn get_generic_variable_pack(&self, id: Id<GenericVariablePack>) -> GenericVariablePack {
        *self.generic_variable_pack.at(&id)
    }

    /// Pushes a new generic variable pack.
    ///
    /// Returns the ID created for it.
    pub fn push_generic_variable_pack(&mut self, pack: GenericVariablePack) -> Id<GenericVariablePack> {
        let id = Id::new(self.generic_variable_pack.len() as u32);
        self.generic_variable_pack.push(&id, pack);

        id
    }

    /// Returns the generic variables associated to the ID.
    pub fn get_generic_variables(&self, id: Id<[GenericVariable]>) -> &[GenericVariable] {
        if id.is_empty() { &[] } else { self.generic_variables.get(&id) }
    }

    /// Pushes a new slice of generic variables.
    ///
    /// Returns the ID created for it.
    pub fn push_generic_variables(&mut self, variables: &[GenericVariable]) -> Id<[GenericVariable]> {
        Self::push_slice(&mut self.generic_variables, variables)
    }


    //  Identifiers

    /// Returns the identifiers associated to the ID.
    pub fn get_identifiers(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id.is_empty() { &[] } else { self.identifiers.get(&id) }
    }

    /// Pushes a new slice of identifiers.
    ///
    /// Returns the ID created for it.
    pub fn push_identifiers(&mut self, identifiers: &[Identifier]) -> Id<[Identifier]> {
        Self::push_slice(&mut self.identifiers, identifiers)
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


    //  Records

    /// Returns the record IDs associated to the ID.
    pub fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        if id.is_empty() { &[] } else { self.record_ids.get(&id) }
    }

    /// Pushes a new slice of record IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_record_ids(&mut self, record_ids: &[RecordId]) -> Id<[RecordId]> {
        Self::push_slice(&mut self.record_ids, record_ids)
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

impl Store<GenericParameterPack> for Module {
    fn len(&self) -> usize { self.generic_parameter_pack.len() }
    fn get(&self, id: Id<GenericParameterPack>) -> GenericParameterPack { self.get_generic_parameter_pack(id) }
    fn get_range(&self, id: Id<GenericParameterPack>) -> Range { self.get_generic_parameter_pack(id).span() }
    fn push(&mut self, e: GenericParameterPack, _: Range) -> Id<GenericParameterPack> { self.push_generic_parameter_pack(e) }
}

impl Store<GenericVariablePack> for Module {
    fn len(&self) -> usize { self.generic_variable_pack.len() }
    fn get(&self, id: Id<GenericVariablePack>) -> GenericVariablePack { self.get_generic_variable_pack(id) }
    fn get_range(&self, id: Id<GenericVariablePack>) -> Range { self.get_generic_variable_pack(id).span() }
    fn push(&mut self, e: GenericVariablePack, _: Range) -> Id<GenericVariablePack> { self.push_generic_variable_pack(e) }
}

impl Store<Implementation> for Module {
    fn len(&self) -> usize { self.len_implementations() }
    fn get(&self, id: ImplementationId) -> Implementation { self.get_implementation(id) }
    fn get_range(&self, id: ImplementationId) -> Range { self.get_implementation_range(id) }
    fn push(&mut self, i: Implementation, _: Range) -> ImplementationId { self.push_implementation(i) }
}

impl Store<Interface> for Module {
    fn len(&self) -> usize { self.len_interfaces() }
    fn get(&self, id: InterfaceId) -> Interface { self.get_interface(id) }
    fn get_range(&self, id: InterfaceId) -> Range { self.get_interface_range(id) }
    fn push(&mut self, i: Interface, _: Range) -> InterfaceId { self.push_interface(i) }
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

impl MultiStore<GenericVariable> for Module {
    fn get_slice(&self, id: Id<[GenericVariable]>) -> &[GenericVariable] { self.get_generic_variables(id) }
    fn push_slice(&mut self, items: &[GenericVariable]) -> Id<[GenericVariable]> { self.push_generic_variables(items) }
}

impl MultiStore<Identifier> for Module {
    fn get_slice(&self, id: Id<[Identifier]>) -> &[Identifier] { self.get_identifiers(id) }
    fn push_slice(&mut self, items: &[Identifier]) -> Id<[Identifier]> { self.push_identifiers(items) }
}

impl MultiStore<u32> for Module {
    fn get_slice(&self, id: Id<[u32]>) -> &[u32] { self.get_positions(id) }
    fn push_slice(&mut self, items: &[u32]) -> Id<[u32]> { self.push_positions(items) }
}

impl MultiStore<RecordId> for Module {
    fn get_slice(&self, id: Id<[RecordId]>) -> &[RecordId] { self.get_record_ids(id) }
    fn push_slice(&mut self, items: &[RecordId]) -> Id<[RecordId]> { self.push_record_ids(items) }
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
        if !self.function_lookup.is_empty() {
            write!(f, "function_lookup: {:?}, ", self.function_lookup)?;
        }
        if !self.interface_lookup.is_empty() {
            write!(f, "interface_lookup: {:?}, ", self.interface_lookup)?;
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
            write!(f, "function_scope: {:?}, ", self.function_scope)?;
        }
        if !self.implementation.is_empty() {
            write!(f, "implementation: {:?}, ", self.implementation)?;
        }
        if !self.interface.is_empty() {
            write!(f, "interface: {:?}, ", self.interface)?;
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
        if !self.generic_parameter_pack.is_empty() {
            write!(f, "generic_parameter_pack: {:?}, ", self.generic_parameter_pack)?;
        }
        if !self.generic_variable_pack.is_empty() {
            write!(f, "generic_variable_pack: {:?}, ", self.generic_variable_pack)?;
        }
        if !self.generic_variables.is_empty() {
            write!(f, "generic_variables: {:?}, ", self.generic_variables)?;
        }
        if !self.identifiers.is_empty() {
            write!(f, "identifiers: {:?}, ", self.identifiers)?;
        }
        if !self.positions.is_empty() {
            write!(f, "positions: {:?}, ", self.positions)?;
        }
        if !self.record_ids.is_empty() {
            write!(f, "record_ids: {:?}, ", self.record_ids)?;
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
