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

use std::cmp;
use std::collections::BTreeMap;

use crate::basic::com::{Range, Store, MultiStore};
use crate::basic::sea::{MultiTable, Table, TableIndex};

use crate::model::com::ModuleId;
use crate::model::hir::*;

/// Module.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Module {
    /// Id.
    module_id: ModuleId,

    //
    //  Builtins
    //
    builtin_functions: [Vec<(Identifier, FunctionId)>; BuiltinType::NUMBER],

    //
    //  Enums
    //

    /// Enum canonical name to EnumId.
    enum_lookup: BTreeMap<ItemIdentifier, EnumId>,
    /// Definition of a given Enum.
    enum_: Table<EnumId, Enum>,
    /// Functions associated to a given Enum.
    enum_functions: Table<EnumId, Vec<(Identifier, FunctionId)>>,

    //
    //  Extensions
    //

    /// Extension canonical name to ExtensionId.
    extension_lookup: BTreeMap<ItemIdentifier, ExtensionId>,
    /// Definition of a given Extension.
    extension: Table<ExtensionId, Extension>,

    //
    //  Functions
    //

    /// Function canonical name to FunctionId.
    function_lookup: BTreeMap<ItemIdentifier, FunctionId>,
    /// Signature of a given Function.
    function: Table<FunctionId, FunctionSignature>,

    //
    //  Implementations
    //

    /// Implementation's extended canonical name to ImplementationId.
    implementation_lookup: BTreeMap<ItemIdentifier, ImplementationId>,
    /// Definition of a given Implementation.
    implementation: Table<ImplementationId, Implementation>,
    /// Functions associated to a given Implementation.
    implementation_functions: Table<ImplementationId, Vec<(Identifier, FunctionId)>>,
    /// Implementations indexed by InterfaceId/BuiltinType.
    implementation_of_builtins: BTreeMap<(InterfaceId, BuiltinType), ImplementationId>,
    /// Implementations indexed by InterfaceId/EnumId.
    implementation_of_enum: BTreeMap<(InterfaceId, EnumId), ImplementationId>,
    /// Implementations indexed by InterfaceId/InterfaceId.
    implementation_of_interface: BTreeMap<(InterfaceId, InterfaceId), ImplementationId>,
    /// Implementations indexed by InterfaceId/RecordId.
    implementation_of_record: BTreeMap<(InterfaceId, RecordId), ImplementationId>,

    //
    //  Interfaces.
    //

    /// Interface canonical name to InterfaceId.
    interface_lookup: BTreeMap<ItemIdentifier, InterfaceId>,
    /// Definition of a given Interface.
    interface: Table<InterfaceId, Interface>,
    /// Functions associated to a given Interface.
    interface_functions: Table<InterfaceId, Vec<(Identifier, FunctionId)>>,

    //
    //  Records
    //

    /// Record canonical name to RecordId.
    record_lookup: BTreeMap<ItemIdentifier, RecordId>,
    /// Definition of a given Record.
    record: Table<RecordId, Record>,
    /// Functions associated to a given Record.
    record_functions: Table<RecordId, Vec<(Identifier, FunctionId)>>,

    //
    //  Components
    //

    /// Arguments of functions.
    arguments: KeyedMulti<ValueIdentifier>,
    /// Elaborate Types
    elaborate_type: Table<ElaborateTypeId, ElaborateType>,
    /// Elaborate Type Ids
    elaborate_type_ids: KeyedMulti<ElaborateTypeId>,
    /// Names
    names: KeyedMulti<Identifier>,
    canonical_names: CanonicalMulti<Identifier>,
    /// Path
    path_components: KeyedMulti<PathComponent>,
    /// Path
    record_ids: KeyedMulti<RecordId>,
    /// Types
    type_: Table<TypeId, Type>,
    canonical_types: Canonical<Type, TypeId>,
    /// TypeIds
    type_ids: KeyedMulti<TypeId>,
    canonical_type_ids: CanonicalMulti<TypeId>,
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

    /// Returns the EnumId corresponding to the ItemIdentifier, if any.
    pub fn lookup_enum(&self, name: ItemIdentifier) -> Option<EnumId> {
        self.enum_lookup.get(&name).copied()
    }

    /// Inserts an Enum name.
    ///
    /// Returns the EnumId created for it.
    pub fn push_enum_name(&mut self, name: ItemIdentifier) -> EnumId {
        debug_assert!(self.enum_.len() == self.enum_lookup.len());
        debug_assert!(self.enum_.len() == self.enum_functions.len());
        debug_assert!(!self.enum_lookup.contains_key(&name));

        let id = EnumId::new_module(self.enum_.len() as u32);
        let local_id = Self::localize(id);

        self.enum_.push(&local_id, Default::default());
        self.enum_functions.push(&local_id, Default::default());
        self.enum_lookup.insert(name, id);

        id
    }

    /// Sets an Enum.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_enum(&mut self, id: EnumId, enum_: Enum) {
        debug_assert!(id.is_module());
        debug_assert!(self.enum_.len() == self.enum_lookup.len());
        debug_assert!(self.enum_.len() == self.enum_functions.len());

        let id = Self::localize(id);
        *self.enum_.at_mut(&id) = enum_;
    }


    //
    //  Extensions
    //

    /// Returns the ExtensionId corresponding to the ItemIdentifier, if any.
    pub fn lookup_extension(&self, name: ItemIdentifier) -> Option<ExtensionId> {
        self.extension_lookup.get(&name).copied()
    }

    /// Inserts an Extension name.
    ///
    /// Returns the ExtensionId created for it.
    pub fn push_extension_name(&mut self, name: ItemIdentifier) -> ExtensionId {
        debug_assert!(self.extension.len() == self.extension_lookup.len());
        debug_assert!(!self.extension_lookup.contains_key(&name));

        let id = ExtensionId::new_module(self.extension.len() as u32);
        let local_id = Self::localize(id);

        self.extension.push(&local_id, Default::default());
        self.extension_lookup.insert(name, id);

        id
    }

    /// Sets an Extension.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_extension(&mut self, id: ExtensionId, ext: Extension) {
        debug_assert!(id.is_module());
        debug_assert!(self.extension.len() == self.extension_lookup.len());

        let id = Self::localize(id);
        *self.extension.at_mut(&id) = ext;
    }


    //
    //  Functions
    //

    /// Returns the FunctionId corresponding to the ItemIdentifier, if any.
    pub fn lookup_function(&self, name: ItemIdentifier) -> Option<FunctionId> {
        self.function_lookup.get(&name).copied()
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

        let local_id = Self::localize(id);
        *self.function.at_mut(&local_id) = function;

        let name = function.name.id();

        match function.scope {
            Scope::Module => (),
            Scope::Imp(imp) => {
                let imp = Self::localize(imp);
                let functions = self.implementation_functions.at_mut(&imp);
                debug_assert!(!functions.contains(&(name, id)));

                functions.push((name, id));
                functions.sort_unstable();
            },
            Scope::Int(int) => {
                let int = Self::localize(int);
                let functions = self.interface_functions.at_mut(&int);
                debug_assert!(!functions.contains(&(name, id)));

                functions.push((name, id));
                functions.sort_unstable();
            },
            Scope::Ext(ext) => {
                let ext = self.get_extension(ext);
                self.push_type_function(ext.extended, name, id);
            },
        }
    }


    //
    //  Implementations
    //

    /// Returns the ImplementationId corresponding to the ItemIdentifier, if any.
    pub fn lookup_implementation(&self, name: ItemIdentifier) -> Option<ImplementationId> {
        self.implementation_lookup.get(&name).copied()
    }

    /// Inserts an Implementation name.
    ///
    /// Returns the ImplementationId created for it.
    pub fn push_implementation_name(&mut self, name: ItemIdentifier) -> ImplementationId {
        debug_assert!(self.implementation.len() == self.implementation_lookup.len());
        debug_assert!(self.implementation.len() == self.implementation_functions.len());
        debug_assert!(!self.implementation_lookup.contains_key(&name));

        let id = ImplementationId::new_module(self.implementation.len() as u32);
        let local_id = Self::localize(id);

        self.implementation.push(&local_id, Default::default());
        self.implementation_functions.push(&local_id, Default::default());
        self.implementation_lookup.insert(name, id);

        id
    }

    /// Sets an Implementation.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_implementation(&mut self, id: ImplementationId, imp: Implementation) {
        use self::Type::*;

        debug_assert!(id.is_module());
        debug_assert!(self.implementation.len() == self.implementation_lookup.len());

        let local_id = Self::localize(id);
        *self.implementation.at_mut(&local_id) = imp;

        let int = imp.implemented;
        match self.get_type(imp.extended) {
            Builtin(ty) => { self.implementation_of_builtins.insert((int, ty), id); },
            Enum(e) => { self.implementation_of_enum.insert((int, e), id); },
            Int(i) => { self.implementation_of_interface.insert((int, i), id); },
            Rec(r) => { self.implementation_of_record.insert((int, r), id); },
            Tuple(..) | Unresolved =>
                unimplemented!("Implementations for {:?}", imp.extended),
        }
    }



    //
    //  Interfaces
    //

    /// Returns the InterfaceId corresponding to the ItemIdentifier, if any.
    pub fn lookup_interface(&self, name: ItemIdentifier) -> Option<InterfaceId> {
        self.interface_lookup.get(&name).copied()
    }

    /// Inserts an Interface name.
    ///
    /// Returns the InterfaceId created for it.
    pub fn push_interface_name(&mut self, name: ItemIdentifier) -> InterfaceId {
        debug_assert!(self.interface.len() == self.interface_lookup.len());
        debug_assert!(self.interface.len() == self.interface_functions.len());
        debug_assert!(!self.interface_lookup.contains_key(&name));

        let id = InterfaceId::new_module(self.interface.len() as u32);
        let local_id = Self::localize(id);

        self.interface.push(&local_id, Default::default());
        self.interface_functions.push(&local_id, Default::default());
        self.interface_lookup.insert(name, id);

        id
    }

    /// Sets an Interface.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_interface(&mut self, id: InterfaceId, int: Interface) {
        debug_assert!(id.is_module());
        debug_assert!(self.interface.len() == self.interface_lookup.len());
        debug_assert!(self.interface.len() == self.interface_functions.len());

        let id = Self::localize(id);
        *self.interface.at_mut(&id) = int;
    }


    //
    //  Records
    //

    /// Returns the RecordId corresponding to the ItemIdentifier, if any.
    pub fn lookup_record(&self, name: ItemIdentifier) -> Option<RecordId> {
        self.record_lookup.get(&name).copied()
    }

    /// Inserts a Record name.
    ///
    /// Returns the RecordId created for it.
    pub fn push_record_name(&mut self, name: ItemIdentifier) -> RecordId {
        debug_assert!(self.record.len() == self.record_lookup.len());
        debug_assert!(self.record.len() == self.record_functions.len());
        debug_assert!(!self.record_lookup.contains_key(&name));

        let id = RecordId::new_module(self.record.len() as u32);
        let local_id = Self::localize(id);

        self.record.push(&local_id, Default::default());
        self.record_functions.push(&local_id, Default::default());
        self.record_lookup.insert(name, id);

        id
    }

    /// Sets a Record.
    ///
    /// Overrides any existing definition for this ID.
    pub fn set_record(&mut self, id: RecordId, record: Record) {
        debug_assert!(id.is_module());
        debug_assert!(self.record.len() == self.record_lookup.len());
        debug_assert!(self.record.len() == self.record_functions.len());

        let id = Self::localize(id);
        *self.record.at_mut(&id) = record;
    }


    //
    //  Components
    //

    /// Pushes a new slice of arguments.
    ///
    /// Returns the ID created for it.
    pub fn push_arguments<I>(&mut self, arguments: I) -> Id<[ValueIdentifier]>
        where
            I: IntoIterator<Item = ValueIdentifier>
    {
        self.arguments.create(arguments)
            .map(Self::modularize)
            .unwrap_or(Id::empty())
    }

    /// Sets the elaborate type associated to the id.
    ///
    /// #   Panics
    ///
    /// Panics if attempting to associate a non built-in ElaborateType to a
    /// built-in ElaborateTypeId.
    pub fn set_elaborate_type(&mut self, id: ElaborateTypeId, ty: ElaborateType)
        -> ElaborateTypeId
    {
        if let Some(_) = id.builtin() {
            if let ElaborateType::Builtin(b) = ty {
                return ElaborateTypeId::from(b);
            }

            panic!("Cannot update a built-in to a non built-in");
        }

        let local_id = Self::localize(id);
        *self.elaborate_type.at_mut(&local_id) = ty;

        if let ElaborateType::Builtin(b) = ty {
            ElaborateTypeId::from(b)
        } else {
            id
        }
    }

    /// Inserts a new Elaborate Type.
    ///
    /// Returns the ID created for it.
    pub fn push_elaborate_type(&mut self, typ: ElaborateType)
        -> ElaborateTypeId
    {
        let ty = Self::modularize(self.elaborate_type.extend(typ));

        if let ElaborateType::Builtin(b) = typ {
            ElaborateTypeId::from(b)
        } else {
            ty
        }
    }

    /// Pushes a new slice of Elaborate Type IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_elaborate_type_ids<I>(&mut self, type_ids: I)
        -> Id<[ElaborateTypeId]>
        where
            I: IntoIterator<Item = ElaborateTypeId>
    {
        self.elaborate_type_ids.create(type_ids)
            .map(Self::modularize)
            .unwrap_or(Id::empty())
    }

    /// Pushes a new slice of names.
    ///
    /// Returns the ID created for it.
    pub fn push_names<I>(&mut self, names: I) -> Id<[Identifier]>
        where
            I: IntoIterator<Item = Identifier>
    {
        Self::canonicalize_multi(
            names,
            &mut self.names,
            &mut self.canonical_names
        )
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

    /// Inserts a new type.
    ///
    /// Returns the ID created for it.
    pub fn push_type(&mut self, typ: Type) -> TypeId {
        match typ {
            Type::Builtin(b) => return TypeId::from(b),
            Type::Tuple(tup) =>
                //  FIXME(matthieum): replace with `is_sorted` when stable.
                debug_assert!(
                    self.get_names(tup.names)
                        .windows(2)
                        .all(|w| w[0] <= w[1])
                ),
            _ => (),
        };
        Self::canonicalize(typ, &mut self.type_, &mut self.canonical_types)
    }

    /// Pushes a new slice of Type IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_type_ids<I>(&mut self, type_ids: I) -> Id<[TypeId]>
        where
            I: IntoIterator<Item = TypeId>
    {
        Self::canonicalize_multi(
            type_ids,
            &mut self.type_ids,
            &mut self.canonical_type_ids
        )
    }
}

impl Module {
    fn localize<I: ItemId>(id: I) -> I {
        I::new_tree(id.get_module().expect("module"))
    }

    fn modularize<I: ItemId>(id: I) -> I {
        I::new_module(id.get_tree().expect("tree"))
    }

    fn canonicalize<I, T>(
        element: T,
        keyed: &mut Table<I, T>,
        canonical: &mut BTreeMap<T, I>,
    )
        -> I
        where
            I: Copy + ItemId + TableIndex,
            T: Copy + cmp::Ord,
    {
        *canonical.entry(element)
            .or_insert_with(|| Self::modularize(keyed.extend(element)))
    }

    fn canonicalize_multi<T>(
        elements: impl IntoIterator<Item = T>,
        keyed: &mut KeyedMulti<T>,
        canonical: &mut BTreeMap<Vec<T>, Id<[T]>>,
    )
        -> Id<[T]>
        where
            T: Copy + cmp::Ord,
    {
        let elements: Vec<_> = elements.into_iter().collect();

        if elements.is_empty() {
            return Id::empty();
        }

        *canonical.entry(elements.clone())
            .or_insert_with(|| {
                keyed.create(elements.into_iter())
                    .map(Self::modularize)
                    .unwrap_or(Id::empty())
            })
    }

    fn push_type_function(&mut self, ty: TypeId, name: Identifier, id: FunctionId) {
        use self::Type::*;

        let functions = match self.get_type(ty) {
            Tuple(..) | Unresolved => return,
            Builtin(ty) => &mut self.builtin_functions[ty.index()],
            Enum(e) => self.enum_functions.at_mut(&Self::localize(e)),
            Int(i) => self.interface_functions.at_mut(&Self::localize(i)),
            Rec(r) => self.record_functions.at_mut(&Self::localize(r)),
        };

        debug_assert!(!functions.contains(&(name, id)));

        functions.push((name, id));
        functions.sort_unstable();
    }
}

impl Registry for Module {
    fn get_builtin_functions(&self, ty: BuiltinType) -> &[(Identifier, FunctionId)] {
        &self.builtin_functions[ty.index()]
    }

    fn enums(&self) -> Vec<EnumId> {
        create_ids(self.enum_.len())
    }

    fn get_enum(&self, id: EnumId) -> Enum {
        let id = Self::localize(id);
        *self.enum_.at(&id)
    }

    fn get_enum_functions(&self, id: EnumId) -> &[(Identifier, FunctionId)] {
        let id = Self::localize(id);
        self.enum_functions.at(&id)
    }

    fn extensions(&self) -> Vec<ExtensionId> {
        create_ids(self.extension.len())
    }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        let id = Self::localize(id);
        *self.extension.at(&id)
    }

    fn functions(&self) -> Vec<FunctionId> {
        create_ids(self.function.len())
    }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        let id = Self::localize(id);
        *self.function.at(&id)
    }

    fn implementations(&self) -> Vec<ImplementationId> {
        create_ids(self.implementation.len())
    }

    fn get_implementation(&self, id: ImplementationId) -> Implementation {
        let id = Self::localize(id);
        *self.implementation.at(&id)
    }

    fn get_implementation_functions(&self, id: ImplementationId)
        -> &[(Identifier, FunctionId)]
    {
        let id = Self::localize(id);
        self.implementation_functions.at(&id)
    }

    fn get_implementation_of(&self, int: InterfaceId, ty: Type)
        -> Option<ImplementationId>
    {
        use self::Type::*;

        match ty {
            Builtin(ty) => self.implementation_of_builtins.get(&(int, ty)).copied(),
            Enum(e) => self.implementation_of_enum.get(&(int, e)).copied(),
            Int(i) => self.implementation_of_interface.get(&(int, i)).copied(),
            Rec(r) => self.implementation_of_record.get(&(int, r)).copied(),
            Tuple(..) | Unresolved => None,
        }
    }

    fn interfaces(&self) -> Vec<InterfaceId> {
        create_ids(self.interface.len())
    }

    fn get_interface(&self, id: InterfaceId) -> Interface {
        let id = Self::localize(id);
        *self.interface.at(&id)
    }

    fn get_interface_functions(&self, id: InterfaceId) -> &[(Identifier, FunctionId)] {
        let id = Self::localize(id);
        self.interface_functions.at(&id)
    }

    fn records(&self) -> Vec<RecordId> {
        create_ids(self.record.len())
    }

    fn get_record(&self, id: RecordId) -> Record {
        let id = Self::localize(id);
        *self.record.at(&id)
    }

    fn get_record_functions(&self, id: RecordId) -> &[(Identifier, FunctionId)] {
        let id = Self::localize(id);
        self.record_functions.at(&id)
    }

    fn get_arguments(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        if id == Id::empty() {
            &[]
        } else {
            self.arguments.get(&Self::localize(id))
        }
    }

    fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType {
        if let Some(b) = id.builtin() {
            ElaborateType::Builtin(b)
        } else {
            let id = Self::localize(id);
            *self.elaborate_type.at(&id)
        }
    }

    fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>)
        -> &[ElaborateTypeId]
    {
        if id == Id::empty() {
            &[]
        } else {
            self.elaborate_type_ids.get(&Self::localize(id))
        }
    }

    fn get_names(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id == Id::empty() {
            &[]
        } else {
            self.names.get(&Self::localize(id))
        }
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        if id == Id::empty() {
            &[]
        } else {
            self.path_components.get(&Self::localize(id))
        }
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        if id == Id::empty() {
            &[]
        } else {
            self.record_ids.get(&Self::localize(id))
        }
    }

    fn get_type(&self, id: TypeId) -> Type {
        if let Some(b) = id.builtin() {
            Type::Builtin(b)
        } else {
            let id = Self::localize(id);
            *self.type_.at(&id)
        }
    }

    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id == Id::empty() {
            &[]
        } else {
            self.type_ids.get(&Self::localize(id))
        }
    }
}

impl Store<ElaborateType, ElaborateTypeId> for Module {
    fn len(&self) -> usize { self.elaborate_type.len() }

    fn get(&self, id: ElaborateTypeId) -> ElaborateType {
        self.get_elaborate_type(id)
    }

    fn get_range(&self, _: ElaborateTypeId) -> Range {
        unimplemented!("<Module as Store<Elaborate>>::get_range")
    }

    fn push(&mut self, item: ElaborateType, _: Range) -> ElaborateTypeId {
        self.push_elaborate_type(item)
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

impl MultiStore<ElaborateTypeId> for Module {
    fn get_slice(&self, id: Id<[ElaborateTypeId]>) -> &[ElaborateTypeId] {
        if id.is_empty() { &[] } else { self.get_elaborate_type_ids(id) }
    }

    fn push_slice(&mut self, items: &[ElaborateTypeId]) -> Id<[ElaborateTypeId]> {
        self.push_elaborate_type_ids(items.iter().copied())
    }
}

impl MultiStore<Identifier> for Module {
    fn get_slice(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id.is_empty() { &[] } else { self.get_names(id) }
    }

    fn push_slice(&mut self, items: &[Identifier]) -> Id<[Identifier]> {
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
//  Private
//

type Canonical<T, I> = BTreeMap<T, I>;
type CanonicalMulti<T> = BTreeMap<Vec<T>, Id<[T]>>;
type KeyedMulti<T> = MultiTable<Id<[T]>, T>;

fn create_ids<I: ItemId>(n: usize) -> Vec<I> {
    (0..n).map(|i| I::new_module(i as u32)).collect()
}

#[cfg(test)]
mod tests {
    use std::rc;

    use crate::basic::{com, mem};

    use super::*;

    #[test]
    fn canonicalize_names() {
        let env = Env::new(b"a b c");
        let (a, b, c) = (env.id(0, 1), env.id(2, 1), env.id(4, 1));

        {
            let mut local = env.local();
            let one = local.push_names(&[]);

            assert_eq!(Id::empty(), one);
        }
        {
            let mut local = env.local();
            let one = local.push_names(&[a]);
            let two = local.push_names(&[a]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_names(&[a, b]);
            let two = local.push_names(&[a, b]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_names(&[a]);
            let two = local.push_names(&[b]);

            assert!(one != two);
        }
        {
            let mut local = env.local();
            let one = local.push_names(&[a, b]);
            let two = local.push_names(&[a, c]);

            assert!(one != two);
        }
    }

    #[test]
    fn canonicalize_types_builtin() {
        let facts = [
            (TypeId::bool_(), Type::bool_()),
            (TypeId::int(), Type::int()),
            (TypeId::string(), Type::string()),
            (TypeId::void(), Type::void()),
        ];

        for &(id, type_) in &facts {
            let mut module = Module::default();
            let ty = module.push_type(type_);

            assert_eq!(id, ty, "Expected {:?}, got {:?} for {:?}", id, ty, type_);
        }
    }

    #[test]
    fn canonicalize_types_mashup() {
        let env = Env::new(b"");
        let (e, ee) = (Type::Enum(EnumId::new(0)), Type::Enum(EnumId::new(1)));
        let r = Type::Rec(RecordId::new(0));

        {
            let mut local = env.local();
            let one = local.push_type(e);
            let two = local.push_type(e);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_type(e);
            let two = local.push_type(ee);

            assert!(one != two);
        }
        {
            let mut local = env.local();
            let one = local.push_type(e);
            let two = local.push_type(r);

            assert!(one != two);
        }
    }

    #[test]
    fn canonicalize_types_tuple_named() {
        let env = Env::new(b"a b c");
        let (a, b, c) = (env.id(0, 1), env.id(2, 1), env.id(4, 1));
        let (t, u, v) = (TypeId::new(0), TypeId::new(1), TypeId::new(2));

        {
            let mut local = env.local();
            let one = local.push_tuple(&[t, u], &[a, b]);
            let two = local.push_tuple(&[t, u], &[a, b]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_tuple(&[t, u], &[a, b]);
            let two = local.push_tuple(&[t, v], &[a, b]);

            assert!(one != two);
        }
        {
            let mut local = env.local();
            let one = local.push_tuple(&[t, u], &[a, b]);
            let two = local.push_tuple(&[t, u], &[a, c]);

            assert!(one != two);
        }
        {
            let mut local = env.local();
            let one = local.push_tuple(&[t, u], &[a, b]);
            let two = local.push_tuple(&[t, v], &[a, c]);

            assert!(one != two);
        }
    }

    #[test]
    fn canonicalize_types_tuple_unnamed() {
        let env = Env::new(b"a b c");
        let (t, u, v) = (TypeId::new(0), TypeId::new(1), TypeId::new(2));

        {
            let mut local = env.local();
            let one = local.push_tuple(&[], &[]);
            let two = local.push_tuple(&[], &[]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_tuple(&[t, u], &[]);
            let two = local.push_tuple(&[t, u], &[]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_tuple(&[t, u], &[]);
            let two = local.push_tuple(&[t, v], &[]);

            assert!(one != two);
        }
    }

    #[test]
    fn canonicalize_type_ids() {
        let env = Env::new(b"");
        let (a, b, c) = (TypeId::new(0), TypeId::new(1), TypeId::new(2));

        {
            let mut local = env.local();
            let one = local.push_type_ids(&[]);

            assert_eq!(Id::empty(), one);
        }
        {
            let mut local = env.local();
            let one = local.push_type_ids(&[a]);
            let two = local.push_type_ids(&[a]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_type_ids(&[a, b]);
            let two = local.push_type_ids(&[a, b]);

            assert_eq!(one, two);
        }
        {
            let mut local = env.local();
            let one = local.push_type_ids(&[a]);
            let two = local.push_type_ids(&[c]);

            assert!(one != two);
        }
        {
            let mut local = env.local();
            let one = local.push_type_ids(&[b, a]);
            let two = local.push_type_ids(&[b, c]);

            assert!(one != two);
        }
    }

    struct Env {
        resolver: super::interning::Resolver,
    }

    struct LocalEnv {
        module: Module,
    }

    impl Env {
        fn new(fragment: &[u8]) -> Env {
            let interner = rc::Rc::new(mem::Interner::new());
            Env {
                resolver: interning::Resolver::new(fragment, interner),
            }
        }

        fn local(&self) -> LocalEnv { LocalEnv::new() }

        fn id(&self, pos: usize, len: usize) -> Identifier {
            let range = com::Range::new(pos, len);
            self.resolver.from_range(range)
        }
    }

    impl LocalEnv {
        fn new() -> LocalEnv { Self { module: Module::default() } }

        fn push_names(&mut self, names: &[Identifier]) -> Id<[Identifier]> {
            self.module.push_names(names.iter().copied())
        }

        fn push_tuple(&mut self, fields: &[TypeId], names: &[Identifier])
            -> TypeId
        {
            let fields = self.push_type_ids(fields);
            let names = self.push_names(names);

            let ty = Type::Tuple(Tuple { fields, names, });
            self.push_type(ty)
        }

        fn push_type(&mut self, ty: Type) -> TypeId {
            self.module.push_type(ty)
        }

        fn push_type_ids(&mut self, type_ids: &[TypeId])
            -> Id<[TypeId]>
        {
            self.module.push_type_ids(type_ids.iter().copied())
        }
    }
}
