//! A process repository of all items.

use std::{cell, ops, sync};
use std::collections::BTreeMap;

use crate::basic::com::{Store, MultiStore};
use crate::basic::mem::{JaggedArray, JaggedArraySnapshot, JaggedHashMap, JaggedHashMapSnapshot};
use crate::basic::sea::Table;

use crate::model::hir::*;

/// Repository.
///
/// WIP: The Repository is not yet Sync, and thus cannot today be accessed
///      across threads.
///
/// The Repository is a process-wide repository of all items:
/// -   It links an item *name* to its ID, for search purposes.
/// -   It links the ID to the item *definition* or *signature*.
///
/// The Repository is supposed to be accessed across threads, and specifically
/// designed to minimize synchronization costs:
/// -   Insertions on different sub-tables can be performed concurrently.
/// -   Reads can be performed concurrently with other reads and writes.
///
/// A read-only snapshot of the completely Repository can be obtained using the
/// `snapshot` method which returns a RepositorySnapshot.
///
/// As noted, the Repository is append-only.
#[derive(Clone, Debug)]
pub struct Repository {
    //
    //  Builtins
    //
    builtin_functions: [Functions; BuiltinType::NUMBER],

    //
    //  Enums
    //

    /// Enum canonical name to EnumId.
    enum_lookup: JaggedHashMap<ItemIdentifier, EnumId>,
    /// Definition of a given Enum.
    enum_: JaggedArray<Enum>,
    /// Functions associated to a given Enum.
    enum_functions: JaggedArray<Functions>,

    //
    //  Extensions
    //

    /// Definition of a given Extension.
    extension: JaggedArray<Extension>,

    //
    //  Functions
    //

    /// Function canonical name to FunctionId.
    function_lookup: JaggedHashMap<ItemIdentifier, FunctionId>,
    /// Signature of a given Function.
    function: JaggedArray<FunctionSignature>,

    //
    //  Implementations
    //

    /// Definition of a given Implementation.
    implementation: JaggedArray<Implementation>,
    /// Functions associated to a given Implementation.
    implementation_functions: JaggedArray<Functions>,
    /// Implementations indexed by InterfaceId/BuiltinType.
    implementation_of_builtin: JaggedHashMap<(InterfaceId, BuiltinType), ImplementationId>,
    /// Implementations indexed by InterfaceId/EnumId.
    implementation_of_enum: JaggedHashMap<(InterfaceId, EnumId), ImplementationId>,
    /// Implementations indexed by InterfaceId/InterfaceId.
    implementation_of_interface: JaggedHashMap<(InterfaceId, InterfaceId), ImplementationId>,
    /// Implementations indexed by InterfaceId/RecordId.
    implementation_of_record: JaggedHashMap<(InterfaceId, RecordId), ImplementationId>,
    /// Implementations for tuples indexed by InterfaceId,
    implementation_of_tuple: JaggedHashMap<InterfaceId, ImplementationId>,

    //
    //  Interfaces
    //

    /// Interface canonical name to InterfaceId.
    interface_lookup: JaggedHashMap<ItemIdentifier, InterfaceId>,
    /// Definition of a given Interface.
    interface: JaggedArray<Interface>,
    /// Functions associated to a given Interface.
    interface_functions: JaggedArray<Functions>,

    //
    //  Records
    //

    /// Record canonical name to RecordId.
    record_lookup: JaggedHashMap<ItemIdentifier, RecordId>,
    /// Definition of a given Record.
    record: JaggedArray<Record>,
    /// Functions associated to a given record.
    record_functions: JaggedArray<Functions>,

    //
    //  Tuples
    //

    /// Functions associated to tuples.
    tuple_functions: sync::Arc<Functions>,

    //
    //  Components
    //

    /// Arguments
    arguments: JaggedMultiArray<ValueIdentifier>,

    /// ElaborateTypes
    elaborate_type: JaggedArray<ElaborateType>,

    /// ElaborateTypeIds
    elaborate_type_ids: JaggedMultiArray<ElaborateTypeId>,

    /// Names
    names: JaggedMultiArray<Identifier>,
    canonical_names: CanonicalMulti<Identifier>,

    /// Path
    path_components: JaggedMultiArray<PathComponent>,

    /// RecordIds
    record_ids: JaggedMultiArray<RecordId>,

    /// Types
    type_: JaggedArray<Type>,
    canonical_type: Canonical<Type, TypeId>,

    /// TypeIds
    type_ids: JaggedMultiArray<TypeId>,
    canonical_type_ids: CanonicalMulti<TypeId>,
}

impl Repository {
    /// Creates a new instance.
    pub fn new() -> Repository {
        Repository {
            builtin_functions: Default::default(),

            enum_lookup: JaggedHashMap::new(5),
            enum_: JaggedArray::new(5),
            enum_functions: JaggedArray::new(5),

            extension: JaggedArray::new(5),

            function_lookup: JaggedHashMap::new(5),
            function: JaggedArray::new(5),

            implementation: JaggedArray::new(5),
            implementation_functions: JaggedArray::new(5),
            implementation_of_builtin: JaggedHashMap::new(5),
            implementation_of_enum: JaggedHashMap::new(5),
            implementation_of_interface: JaggedHashMap::new(5),
            implementation_of_record: JaggedHashMap::new(5),
            implementation_of_tuple: JaggedHashMap::new(5),

            interface_lookup: JaggedHashMap::new(5),
            interface: JaggedArray::new(5),
            interface_functions: JaggedArray::new(5),

            record_lookup: JaggedHashMap::new(5),
            record: JaggedArray::new(5),
            record_functions: JaggedArray::new(5),

            tuple_functions: Default::default(),

            arguments: JaggedMultiArray::new(5),
            elaborate_type: JaggedArray::new(5),
            elaborate_type_ids: JaggedMultiArray::new(5),
            names: JaggedMultiArray::new(5),
            canonical_names: Default::default(),
            path_components: JaggedMultiArray::new(5),
            record_ids: JaggedMultiArray::new(5),
            type_: JaggedArray::new(5),
            canonical_type: Default::default(),
            type_ids: JaggedMultiArray::new(5),
            canonical_type_ids: Default::default(),
        }
    }

    /// Copies the content of a Module.
    ///
    /// The Module may contain content that has already been internalized, if
    /// this is the case the items are skipped.
    pub fn internalize_module(&mut self, module: &Module) {
        //  Only named items need be inserted, they will recursively lead to
        //  inserting the components that they need.
        let mapper = self.create_id_mapper(module);

        for record in module.records() {
            self.insert_record(record, module, &mapper);
        }

        for enum_ in module.enums() {
            self.insert_enum(enum_, module, &mapper);
        }

        for extension in module.extensions() {
            self.insert_extension(extension, module, &mapper);
        }

        if !module.get_tuple_functions().is_empty() {
            debug_assert!(self.tuple_functions.is_empty());
            self.tuple_functions = sync::Arc::new(mapper.map_functions(module.get_tuple_functions()));
        }

        for implementation in module.implementations() {
            self.insert_implementation(implementation, module, &mapper);
        }

        for interface in module.interfaces() {
            self.insert_interface(interface, module, &mapper);
        }

        for function in module.functions() {
            self.insert_function(function, module, &mapper);
        }
    }

    /// Copies the content of a Tree.
    ///
    /// The Tree cannot contain definitions of items, however it can contain
    /// tuple types, from literals, not present anywhere else.
    pub fn internalize_tree(&self, tree: &mut Tree) {
        let mapper = IdMapper::default();

        //  Mapping from tree to repository Id.
        let mut translation = Table::new();

        for id in 0..tree.len_types() {
            let id = TypeId::new(id as u32);
            let result = self.insert_type(id, tree, &mapper);
            translation.push(&id, result);
        }

        tree.retype(&translation);
    }

    /// Creates a snapshot of the repository.
    pub fn snapshot(&self) -> RepositorySnapshot {
        RepositorySnapshot {
            builtin_functions: self.builtin_functions.clone(),
            enum_lookup: self.enum_lookup.snapshot(),
            enum_: self.enum_.snapshot(),
            enum_functions: self.enum_functions.snapshot(),
            extension: self.extension.snapshot(),
            function_lookup: self.function_lookup.snapshot(),
            function: self.function.snapshot(),
            implementation: self.implementation.snapshot(),
            implementation_functions: self.implementation_functions.snapshot(),
            implementation_of_builtin: self.implementation_of_builtin.snapshot(),
            implementation_of_enum: self.implementation_of_enum.snapshot(),
            implementation_of_interface: self.implementation_of_interface.snapshot(),
            implementation_of_record: self.implementation_of_record.snapshot(),
            implementation_of_tuple: self.implementation_of_tuple.snapshot(),
            interface_lookup: self.interface_lookup.snapshot(),
            interface: self.interface.snapshot(),
            interface_functions: self.interface_functions.snapshot(),
            record_lookup: self.record_lookup.snapshot(),
            record: self.record.snapshot(),
            record_functions: self.record_functions.snapshot(),
            tuple_functions: self.tuple_functions.clone(),
            arguments: self.arguments.snapshot(),
            elaborate_type: self.elaborate_type.snapshot(),
            elaborate_type_ids: self.elaborate_type_ids.snapshot(),
            names: self.names.snapshot(),
            path_components: self.path_components.snapshot(),
            record_ids: self.record_ids.snapshot(),
            type_: self.type_.snapshot(),
            type_ids: self.type_ids.snapshot(),
        }
    }
}

impl Default for Repository {
    fn default() -> Self { Self::new() }
}

impl Repository {
    //  In order to handle recursive definitions, such as
    //
    //      :rec ConsList[T] { element: T, next: Box[ConsList[T]], }
    //
    //  we need to pre-map the ID of all named elements, in this case enums
    //  and records.
    //
    //  Local IDs are monotonically increasing, therefore the set of known
    //  enums will be [0, n) for some value of n (often 0), and the new enums
    //  will be [n, m) for m >= n.
    //
    //  A simple generation scheme for EnumId is therefore to compute the
    //  (fixed) offset between the module and repository ID before starting the
    //  insertions:
    //  -   Determine n.
    //  -   Determine the next EnumId to be generated (len...).
    //  -   The offset is (signed) next - n.
    //
    //  Rinse and repeat for Records, as well as any other items concerned by
    //  the potential for recursive definition, such as Alias or Trait.
    fn create_id_mapper(&self, _module: &Module) -> IdMapper {
        //  FIXME(matthieum): Actually compute the first unknown...
        IdMapper::new(
            self.enum_.len() as i64,
            self.extension.len() as i64,
            self.function.len() as i64,
            self.implementation.len() as i64,
            self.interface.len() as i64,
            self.record.len() as i64,
        )
    }

    fn insert_enum(
        &self,
        e: EnumId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.enum_.len() == self.enum_lookup.len());
        debug_assert!(self.enum_.len() == self.enum_functions.len());

        let id = mapper.map_enum(e);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.enum_.len() as u32,
            "{:?} == {:?}", id.get_repository(), self.enum_.len());

        let enum_ = module.get_enum(e);
        let functions = mapper.map_functions(module.get_enum_functions(e));

        let mut records = vec!();
        for &record in module.get_record_ids(enum_.variants) {
            records.push(mapper.map_record(record));
        }

        let name = enum_.name;
        let range = enum_.range;
        let variants = self.record_ids.push(&records)
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty());

        self.enum_.push(Enum { name, range, variants, });
        self.enum_functions.push(functions);
        self.enum_lookup.insert(name, id);
    }

    fn insert_extension(
        &self,
        extension: ExtensionId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        let id = mapper.map_extension(extension);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.extension.len() as u32,
            "{:?} == {:?}", id.get_repository(), self.extension.len());

        let ext = module.get_extension(extension);

        let range = ext.range;
        let extended = self.insert_type(ext.extended, module, mapper);
        let elaborate_extended =
            self.insert_elaborate_type(ext.elaborate_extended, module, mapper);

        let ext = Extension { range, extended, elaborate_extended, };

        self.extension.push(ext);
    }

    fn insert_function(
        &self,
        function: FunctionId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.function_lookup.len() <= self.function.len());

        let id = mapper.map_function(function);
        debug_assert!(id.is_repository());

        //  Interfaces may have functions with empty bodies.
        if let Some(index) = id.get_repository() {
            debug_assert!(index >= self.function.len() as u32,
                "{:?} == {:?}", index, self.function.len());

            let missing = index - self.function.len() as u32;
            for _ in 0..missing {
                self.function.push(Default::default());
            }
        }

        debug_assert!(id.get_repository().unwrap() == self.function.len() as u32,
            "{:?} == {:?}", id.get_repository(), self.function.len());

        let signature = module.get_function(function);

        let name = signature.name;
        let range = signature.range;
        let scope = match signature.scope {
            Scope::Module => Scope::Module,
            Scope::Ext(e) => Scope::Ext(mapper.map_extension(e)),
            Scope::Imp(i) => Scope::Imp(mapper.map_implementation(i)),
            Scope::Int(i) => Scope::Int(mapper.map_interface(i)),
        };
        let arguments = self.arguments.push(module.get_arguments(signature.arguments))
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty());
        let argument_types =
            self.insert_type_ids(signature.argument_types, module, mapper);
        let result = self.insert_type(signature.result, module, mapper);
        let elaborate_argument_types =
            self.insert_elaborate_type_ids(signature.elaborate_argument_types, module, mapper);
        let elaborate_result =
            self.insert_elaborate_type(signature.elaborate_result, module, mapper);

        let signature = FunctionSignature {
            name,
            scope,
            range,
            arguments,
            argument_types,
            result,
            elaborate_argument_types,
            elaborate_result,
        };

        self.function.push(signature);
        self.function_lookup.insert(name, id);
    }

    fn insert_implementation(
        &self,
        implementation: ImplementationId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        use self::Type::*;

        debug_assert!(self.implementation_functions.len() == self.implementation.len());

        let id = mapper.map_implementation(implementation);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.implementation.len() as u32,
            "{:?} == {:?}", id.get_repository(), self.implementation.len());

        let imp = module.get_implementation(implementation);
        let functions = mapper.map_functions(module.get_implementation_functions(implementation));

        let range = imp.range;
        let implemented = mapper.map_interface(imp.implemented);
        let extended = self.insert_type(imp.extended, module, mapper);
        let elaborate_implemented =
            self.insert_elaborate_type(imp.elaborate_implemented, module, mapper);
        let elaborate_extended = 
            self.insert_elaborate_type(imp.elaborate_extended, module, mapper);

        let imp = Implementation {
            range, 
            implemented, 
            extended,
            elaborate_implemented,
            elaborate_extended,
        };

        self.implementation.push(imp);
        self.implementation_functions.push(functions);

        match self.snapshot().get_type(imp.extended) {
            Builtin(ty) =>
                { self.implementation_of_builtin.insert((implemented, ty), id); },
            Enum(e) =>
                { self.implementation_of_enum.insert((implemented, e), id); },
            Int(int) =>
                { self.implementation_of_interface.insert((implemented, int), id); },
            Rec(rec) =>
                { self.implementation_of_record.insert((implemented, rec), id); },
            Tuple(..) =>
                { self.implementation_of_tuple.insert(implemented, id); },
            Unresolved =>
                unimplemented!("Implementations for {:?}", extended),
        }
    }

    fn insert_interface(
        &self,
        interface: InterfaceId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.interface.len() == self.interface_lookup.len());
        debug_assert!(self.interface.len() == self.interface_functions.len());

        let id = mapper.map_interface(interface);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.interface.len() as u32,
            "{:?} == {:?}", id.get_repository(), self.interface.len());

        let int = module.get_interface(interface);
        let functions = mapper.map_functions(module.get_interface_functions(interface));

        let name = int.name;
        let range = int.range;

        let int = Interface { name, range, };

        self.interface.push(int);
        self.interface_functions.push(functions);
        self.interface_lookup.insert(name, id);
    }

    fn insert_record(
        &self,
        r: RecordId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.record.len() == self.record_lookup.len());
        debug_assert!(self.record.len() == self.record_functions.len());

        let id = mapper.map_record(r);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.record.len() as u32,
            "{:?} == {:?}", id.get_repository(), self.record.len());

        let record = module.get_record(r);
        let functions = mapper.map_functions(module.get_record_functions(r));

        let name = record.name;
        let range = record.range;
        let enum_ = record.enum_.map(|e| mapper.map_enum(e));
        let definition = self.insert_tuple(record.definition, module, mapper);
        let elaborate_definition =
            self.insert_elaborate_tuple(record.elaborate_definition, module, mapper);

        self.record.push(Record { name, range, enum_, definition, elaborate_definition, });
        self.record_functions.push(functions);
        self.record_lookup.insert(name, id);
    }

    fn insert_elaborate_type(
        &self,
        ty: ElaborateTypeId,
        module: &Module,
        mapper: &IdMapper,
    )
        -> ElaborateTypeId
    {
        use self::ElaborateType::*;

        let ty = match module.get_elaborate_type(ty) {
            Builtin(b) => return ElaborateTypeId::from(b),
            ty => ty,
        };

        let ty = self.insert_elaborate_type_impl(ty, module, mapper);

        let index = self.type_.len();
        self.elaborate_type.push(ty);
        ElaborateTypeId::new_repository(index as u32)
    }

    fn insert_elaborate_tuple(
        &self,
        tuple: Tuple<ElaborateTypeId>,
        module: &Module,
        mapper: &IdMapper,
    )
        -> Tuple<ElaborateTypeId>
    {
        let fields = self.insert_elaborate_type_ids(tuple.fields, module, mapper);

        let names = self.names.push(module.get_names(tuple.names))
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty());

        Tuple { fields, names, }
    }

    fn insert_elaborate_type_impl(
        &self,
        type_: ElaborateType,
        module: &Module,
        mapper: &IdMapper,
    )
        -> ElaborateType
    {
        use self::ElaborateType::*;

        match type_ {
            Alias(a) => Alias(self.insert_type(a, module, mapper)),
            Builtin(b) => Builtin(b),
            Enum(e, p) => {
                let e = mapper.map_enum(e);
                let p = self.insert_path_components(p, module);
                Enum(e, p)
            },
            Int(i, p) => {
                let i = mapper.map_interface(i);
                let p = self.insert_path_components(p, module);
                Int(i, p)
            }
            Rec(r, p) => {
                let r = mapper.map_record(r);
                let p = self.insert_path_components(p, module);
                Rec(r, p)
            },
            Tuple(tuple) => {
                let tuple = self.insert_elaborate_tuple(tuple, module, mapper);
                Tuple(tuple)
            },
            Unresolved(name, path) =>
                unreachable!("Cannot insert Unresolved({:?}, {:?})",
                    name, module.get_path_components(path)),
        }
    }

    fn insert_elaborate_type_ids(
        &self,
        tys: Id<[ElaborateTypeId]>,
        module: &Module,
        mapper: &IdMapper
    )
        -> Id<[ElaborateTypeId]>
    {
        let mut type_ids = vec!();

        for &id in module.get_elaborate_type_ids(tys) {
            type_ids.push(self.insert_elaborate_type(id, module, mapper));
        }

        self.elaborate_type_ids.push(&type_ids)
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty())
    }

    fn insert_names<S>(
        &self,
        names: Id<[Identifier]>,
        store: &S,
    )
        -> Id<[Identifier]>
        where
            S: MultiStore<Identifier>
    {
        if names == Id::empty() {
            return Id::empty();
        }

        let names = store.get_slice(names);
        let names: Vec<_> = names.iter().copied().collect();

        *self.canonical_names.borrow_mut().entry(names.clone())
            .or_insert_with(|| {
                self.names.push(&names)
                .map(|index| Id::new_repository(index as u32))
                .unwrap_or(Id::empty())
            })
    }

    fn insert_path_components(
        &self,
        path: PathId,
        module: &Module,
    )
        -> PathId
    {
        self.path_components.push(module.get_path_components(path))
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty())
    }

    fn insert_tuple<S>(
        &self,
        tuple: Tuple<TypeId>,
        store: &S,
        mapper: &IdMapper,
    )
        -> Tuple<TypeId>
        where
            S: Store<Type, TypeId> + MultiStore<Identifier> + MultiStore<TypeId>
    {
        let fields = self.insert_type_ids(tuple.fields, store, mapper);
        let names = self.insert_names(tuple.names, store);

        Tuple { fields, names, }
    }

    fn insert_type<S>(
        &self,
        ty: TypeId,
        store: &S,
        mapper: &IdMapper,
    )
        -> TypeId
        where
            S: Store<Type, TypeId> + MultiStore<Identifier> + MultiStore<TypeId>
    {
        use self::Type::*;

        let ty = match store.get(ty) {
            Builtin(b) => return TypeId::from(b),
            ty => ty,
        };

        let ty = self.insert_type_impl(ty, store, mapper);

        *self.canonical_type.borrow_mut().entry(ty)
            .or_insert_with(|| {
                let index = self.type_.len();
                self.type_.push(ty);
                TypeId::new_repository(index as u32)
            })
    }

    fn insert_type_impl<S>(
        &self,
        type_: Type,
        store: &S,
        mapper: &IdMapper,
    )
        -> Type
        where
            S: Store<Type, TypeId> + MultiStore<Identifier> + MultiStore<TypeId>
    {
        use self::Type::*;

        match type_ {
            Builtin(b) => Builtin(b),
            Enum(e) => Enum(mapper.map_enum(e)),
            Int(i) => Int(mapper.map_interface(i)),
            Rec(r) => Rec(mapper.map_record(r)),
            Tuple(tuple) => Tuple(self.insert_tuple(tuple, store, mapper)),
            Unresolved => unreachable!("Cannot insert Unresolved"),
        }
    }

    fn insert_type_ids<S>(
        &self,
        tys: Id<[TypeId]>,
        store: &S,
        mapper: &IdMapper,
    )
        -> Id<[TypeId]>
        where
            S: Store<Type, TypeId> + MultiStore<Identifier> + MultiStore<TypeId>
    {
        if tys == Id::empty() {
            return Id::empty();
        }

        let tys = store.get_slice(tys);
        let tys: Vec<_> = tys.iter()
            .map(|&id| self.insert_type(id, store, mapper))
            .collect();

        *self.canonical_type_ids.borrow_mut().entry(tys.clone())
            .or_insert_with(|| {
                self.type_ids.push(&tys)
                    .map(|index| Id::new_repository(index as u32))
                    .unwrap_or(Id::empty())
            })
    }
}


/// RepositorySnapshot.
///
/// The RepositorySnapshot, as the name implies, presents a read-only snapshot
/// of the Repository at the time the snapshot was created.
///
/// Multiple snapshots, each from a different time, can coexist.
#[derive(Clone, Debug, Default)]
pub struct RepositorySnapshot {
    //  Builtins
    builtin_functions: [Functions; BuiltinType::NUMBER],

    //  Enums
    enum_lookup: JaggedHashMapSnapshot<ItemIdentifier, EnumId>,
    enum_: JaggedArraySnapshot<Enum>,
    enum_functions: JaggedArraySnapshot<Functions>,

    //  Extensions
    extension: JaggedArraySnapshot<Extension>,

    //  Functions
    function_lookup: JaggedHashMapSnapshot<ItemIdentifier, FunctionId>,
    function: JaggedArraySnapshot<FunctionSignature>,

    //  Implementations
    implementation: JaggedArraySnapshot<Implementation>,
    implementation_functions: JaggedArraySnapshot<Functions>,
    implementation_of_builtin: JaggedHashMapSnapshot<(InterfaceId, BuiltinType), ImplementationId>,
    implementation_of_enum: JaggedHashMapSnapshot<(InterfaceId, EnumId), ImplementationId>,
    implementation_of_interface: JaggedHashMapSnapshot<(InterfaceId, InterfaceId), ImplementationId>,
    implementation_of_record: JaggedHashMapSnapshot<(InterfaceId, RecordId), ImplementationId>,
    implementation_of_tuple: JaggedHashMapSnapshot<InterfaceId, ImplementationId>,

    //  Interfaces
    interface_lookup: JaggedHashMapSnapshot<ItemIdentifier, InterfaceId>,
    interface: JaggedArraySnapshot<Interface>,
    interface_functions: JaggedArraySnapshot<Functions>,

    //  Records
    record_lookup: JaggedHashMapSnapshot<ItemIdentifier, RecordId>,
    record: JaggedArraySnapshot<Record>,
    record_functions: JaggedArraySnapshot<Functions>,

    //  Tuples
    tuple_functions: sync::Arc<Functions>,

    //  Components
    arguments: JaggedMultiArraySnapshot<ValueIdentifier>,
    elaborate_type: JaggedArraySnapshot<ElaborateType>,
    elaborate_type_ids: JaggedMultiArraySnapshot<ElaborateTypeId>,
    names: JaggedMultiArraySnapshot<Identifier>,
    path_components: JaggedMultiArraySnapshot<PathComponent>,
    record_ids: JaggedMultiArraySnapshot<RecordId>,
    type_: JaggedArraySnapshot<Type>,
    type_ids: JaggedMultiArraySnapshot<TypeId>,
}

impl Registry for RepositorySnapshot {
    fn get_builtin_functions(&self, ty: BuiltinType) -> &[(Identifier, FunctionId)] {
        &self.builtin_functions[ty.index()]
    }

    fn enums(&self) -> Vec<EnumId> {
        ids_of(self.enum_.len())
    }

    fn get_enum(&self, id: EnumId) -> Enum {
        *self.enum_.at(index_of(id))
    }

    fn get_enum_functions(&self, id: EnumId) -> &[(Identifier, FunctionId)] {
        self.enum_functions.at(index_of(id))
    }

    fn extensions(&self) -> Vec<ExtensionId> {
        ids_of(self.extension.len())
    }

    fn get_extension(&self, id: ExtensionId) -> Extension {
        *self.extension.at(index_of(id))
    }

    fn functions(&self) -> Vec<FunctionId> {
        ids_of(self.function.len())
    }

    fn get_function(&self, id: FunctionId) -> FunctionSignature {
        *self.function.at(index_of(id))
    }

    fn implementations(&self) -> Vec<ImplementationId> {
        ids_of(self.implementation.len())
    }

    fn get_implementation(&self, id: ImplementationId) -> Implementation {
        *self.implementation.at(index_of(id))
    }

    fn get_implementation_functions(&self, id: ImplementationId)
        -> &[(Identifier, FunctionId)]
    {
        self.implementation_functions.at(index_of(id))
    }

    fn get_implementation_of(&self, int: InterfaceId, ty: Type)
        -> Option<ImplementationId>
    {
        use self::Type::*;

        match ty {
            Builtin(ty) => self.implementation_of_builtin.get(&(int, ty)).copied(),
            Enum(e) => self.implementation_of_enum.get(&(int, e)).copied(),
            Int(i) => self.implementation_of_interface.get(&(int, i)).copied(),
            Rec(r) => self.implementation_of_record.get(&(int, r)).copied(),
            Tuple(..) => self.implementation_of_tuple.get(&int).copied(),
            Unresolved => None,
        }
    }

    fn interfaces(&self) -> Vec<InterfaceId> {
        ids_of(self.interface.len())
    }

    fn get_interface(&self, id: InterfaceId) -> Interface {
        *self.interface.at(index_of(id))
    }

    fn get_interface_functions(&self, id: InterfaceId) -> &[(Identifier, FunctionId)] {
        self.interface_functions.at(index_of(id))
    }

    fn records(&self) -> Vec<RecordId> {
        ids_of(self.record.len())
    }

    fn get_record(&self, id: RecordId) -> Record {
        *self.record.at(index_of(id))
    }

    fn get_record_functions(&self, id: RecordId) -> &[(Identifier, FunctionId)] {
        self.record_functions.at(index_of(id))
    }

    fn get_tuple_functions(&self) -> &[(Identifier, FunctionId)] {
        &self.tuple_functions
    }

    fn get_arguments(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        if id == Id::empty() {
            &[]
        } else {
            self.arguments.get_slice(index_of(id))
        }
    }

    fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType {
        if let Some(b) = id.builtin() {
            ElaborateType::Builtin(b)
        } else {
            *self.elaborate_type.at(index_of(id))
        }
    }
  
    fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>) -> &[ElaborateTypeId] {
        if id == Id::empty() {
            &[]
        } else {
            self.elaborate_type_ids.get_slice(index_of(id))
        }
    }

    fn get_names(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id == Id::empty() {
            &[]
        } else {
            self.names.get_slice(index_of(id))
        }
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        if id == Id::empty() {
            &[]
        } else {
            self.path_components.get_slice(index_of(id))
        }
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        if id == Id::empty() {
            &[]
        } else {
            self.record_ids.get_slice(index_of(id))
        }
    }

    fn get_type(&self, id: TypeId) -> Type {
        if let Some(b) = id.builtin() {
            Type::Builtin(b)
        } else {
            *self.type_.at(index_of(id))
        }
    }

    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id == Id::empty() {
            &[]
        } else {
            self.type_ids.get_slice(index_of(id))
        }
    }
}

fn ids_of<T: ItemId>(len: usize) -> Vec<T> {
    (0..(len as u32)).into_iter().map(T::new_repository).collect()
}

fn index_of<T: ItemId>(id: T) -> usize {
    id.get_repository().expect("repository") as usize
}


//
//  Private Types
//

type Canonical<T, I> = cell::RefCell<BTreeMap<T, I>>;
type CanonicalMulti<T> = cell::RefCell<BTreeMap<Vec<T>, Id<[T]>>>;
type Functions = Vec<(Identifier, FunctionId)>;

/// JaggedMultiArray
///
/// A simple interface over a Jagged Array for slices.
#[derive(Clone, Debug, Default)]
struct JaggedMultiArray<T> {
    /// The position and length of slices.
    index: JaggedArray<ops::Range<u32>>,
    /// The actual slices.
    slices: JaggedArray<T>,
}

impl<T> JaggedMultiArray<T> {
    /// Creates an instance.
    fn new(log2_capacity: usize) -> Self {
        JaggedMultiArray {
            index: JaggedArray::new(log2_capacity),
            slices: JaggedArray::new(log2_capacity + 1),
        }
    }

    /// Returns whether the array is empty, or not.
    #[allow(dead_code)]
    fn is_empty(&self) -> bool { self.index.len() == 0 }

    /// Returns the length of the array.
    #[allow(dead_code)]
    fn len(&self) -> usize { self.index.len() }

    /// Returns a snapshot.
    fn snapshot(&self) -> JaggedMultiArraySnapshot<T> {
        JaggedMultiArraySnapshot {
            index: self.index.snapshot(),
            slices: self.slices.snapshot(),
        }
    }

    /// Gets the slice.
    #[allow(dead_code)]
    fn get_slice(&self, i: usize) -> &[T] {
        let range = self.index.get(i).cloned().expect("Valid index");
        let slice = self.slices.get_slice(range.start as usize);
        &slice[..range.len()]
    }

    /// Push a new slice, return its index.
    fn push(&self, slice: &[T]) -> Option<usize>
        where
            T: Copy + Default,
    {
        if slice.len() == 0 {
            return None;
        }

        let index = self.index.len();

        self.slices.extend_contiguous_copy(slice);

        let end = self.slices.len() as u32;
        let start = end - slice.len() as u32;
        self.index.push(start..end);

        Some(index)
    }
}


/// JaggedMultiArraySnapshot.
///
/// A simple Snapshot interface.
#[derive(Clone, Debug, Default)]
struct JaggedMultiArraySnapshot<T> {
    /// The position and length of slices.
    index: JaggedArraySnapshot<ops::Range<u32>>,
    /// The actual slices.
    slices: JaggedArraySnapshot<T>,
}

impl<T> JaggedMultiArraySnapshot<T> {
    /// Gets the slice.
    fn get_slice(&self, i: usize) -> &[T] {
        let range = self.index.get(i).cloned().expect("Valid index");
        let slice = self.slices.get_slice(range.start as usize);
        &slice[0..((range.end - range.start) as usize)]
    } 
}


//  IdMapper
//
//  A mapper between module and repository ID, for recursive types.
struct IdMapper {
    enum_offset: i64,
    extension_offset: i64,
    function_offset: i64,
    implementation_offset: i64,
    interface_offset: i64,
    record_offset: i64,
}

impl IdMapper {
    fn new(
        enum_offset: i64,
        extension_offset: i64,
        function_offset: i64,
        implementation_offset: i64,
        interface_offset: i64,
        record_offset: i64,
    )
        -> IdMapper
    {
        IdMapper {
            enum_offset,
            extension_offset,
            function_offset,
            implementation_offset,
            interface_offset,
            record_offset,
        }
    }

    fn map_enum(&self, id: EnumId) -> EnumId {
        Self::map_item_id(id, self.enum_offset)
    }

    fn map_extension(&self, id: ExtensionId) -> ExtensionId {
        Self::map_item_id(id, self.extension_offset)
    }

    fn map_function(&self, id: FunctionId) -> FunctionId {
        Self::map_item_id(id, self.function_offset)
    }

    fn map_functions(&self, functions: &[(Identifier, FunctionId)])
        -> Functions
    {
        functions.iter()
            .map(|&(name, id)| (name, self.map_function(id)))
            .collect()
    }

    fn map_implementation(&self, id: ImplementationId) -> ImplementationId {
        Self::map_item_id(id, self.implementation_offset)
    }

    fn map_interface(&self, id: InterfaceId) -> InterfaceId {
        Self::map_item_id(id, self.interface_offset)
    }

    fn map_record(&self, id: RecordId) -> RecordId {
        Self::map_item_id(id, self.record_offset)
    }

    fn map_item_id<I: ItemId>(id: I, offset: i64) -> I {
        if let Some(id) = id.get_module() {
            let local = id as i64;
            I::new_repository((local + offset) as u32)
        } else {
            debug_assert!(id.is_repository());
            id
        }
    }
}

impl Default for IdMapper {
    fn default() -> Self {
        //  Force an error if the offset is ever used.
        let offset = u32::max_value() as i64 / 2;
        IdMapper::new(offset, offset, offset, offset, offset, offset)
    }
}

#[cfg(test)]
mod tests {

}
