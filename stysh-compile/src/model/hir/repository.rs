//! A process repository of all items.

use std::ops;

use crate::basic::mem::{JaggedArray, JaggedArraySnapshot, JaggedHashMap, JaggedHashMapSnapshot};

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
    //  Enums
    //

    /// Enum canonical name to EnumId.
    enum_lookup: JaggedHashMap<ItemIdentifier, EnumId>,
    /// Definition of a given Enum.
    enum_: JaggedArray<Enum>,

    //
    //  Extensions
    //

    /// Extension canonical name to ExtensionId.
    extension_lookup: JaggedHashMap<ItemIdentifier, ExtensionId>,
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
    //  Records
    //

    /// Record canonical name to RecordId.
    record_lookup: JaggedHashMap<ItemIdentifier, RecordId>,
    /// Definition of a given Record.
    record: JaggedArray<Record>,

    //
    //  Components
    //

    /// Names
    names: JaggedMultiArray<ValueIdentifier>,

    /// Path
    path_components: JaggedMultiArray<PathComponent>,

    /// RecordIds
    record_ids: JaggedMultiArray<RecordId>,

    /// Types
    type_: JaggedArray<Type>,

    /// TypeIds
    type_ids: JaggedMultiArray<TypeId>,
}

impl Repository {
    /// Creates a new instance.
    pub fn new() -> Repository {
        Repository {
            enum_lookup: JaggedHashMap::new(5),
            enum_: JaggedArray::new(5),

            extension_lookup: JaggedHashMap::new(5),
            extension: JaggedArray::new(5),

            function_lookup: JaggedHashMap::new(5),
            function: JaggedArray::new(5),

            record_lookup: JaggedHashMap::new(5),
            record: JaggedArray::new(5),

            names: JaggedMultiArray::new(5),
            path_components: JaggedMultiArray::new(5),
            record_ids: JaggedMultiArray::new(5),
            type_: JaggedArray::new(5),
            type_ids: JaggedMultiArray::new(5),
        }
    }

    /// Copies the content of a Module.
    ///
    /// The Module may contain content that has already been internalized, if
    /// this is the case the items are skipped.
    pub fn internalize(&mut self, module: &Module) {
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

        for function in module.functions() {
            self.insert_function(function, module, &mapper);
        }
    }

    /// Creates a snapshot of the repository.
    pub fn snapshot(&self) -> RepositorySnapshot {
        RepositorySnapshot {
            enum_lookup: self.enum_lookup.snapshot(),
            enum_: self.enum_.snapshot(),
            extension_lookup: self.extension_lookup.snapshot(),
            extension: self.extension.snapshot(),
            function_lookup: self.function_lookup.snapshot(),
            function: self.function.snapshot(),
            record_lookup: self.record_lookup.snapshot(),
            record: self.record.snapshot(),
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
            self.record.len() as i64,
        )
    }

    fn insert_enum(
        &mut self,
        e: EnumId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.enum_.len() == self.enum_lookup.len());

        let id = mapper.map_enum(e);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.enum_.len() as u32 + 1);

        let enum_ = module.get_enum(e);

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
        self.enum_lookup.insert(name, id);
    }

    fn insert_extension(
        &mut self,
        extension: ExtensionId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.extension_lookup.len() == self.extension.len());

        let id = mapper.map_extension(extension);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.extension.len() as u32 + 1);

        let ext = module.get_extension(extension);

        let name = ext.name;
        let range = ext.range;
        let extended = self.insert_type_impl(ext.extended, module, mapper);

        let ext = Extension { name, range, extended, };

        self.extension.push(ext);
        self.extension_lookup.insert(name, id);
    }

    fn insert_function(
        &mut self,
        function: FunctionId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.function_lookup.len() == self.function.len());

        let signature = module.get_function(function);

        let name = signature.name;
        let range = signature.range;
        let extension = signature.extension.map(|e| mapper.map_extension(e));
        let arguments = self.insert_tuple(signature.arguments, module, mapper);
        let result = self.insert_type(signature.result, module, mapper);

        let id = FunctionId::new_repository(self.function.len() as u32);
        let signature = FunctionSignature { name, extension, range, arguments, result, };

        self.function.push(signature);
        self.function_lookup.insert(name, id);
    }

    fn insert_record(
        &mut self,
        r: RecordId,
        module: &Module,
        mapper: &IdMapper,
    )
    {
        debug_assert!(self.record.len() == self.record_lookup.len());

        let id = mapper.map_record(r);
        debug_assert!(id.is_repository());
        debug_assert!(id.get_repository().unwrap() == self.record.len() as u32 + 1);

        let record = module.get_record(r);

        let name = record.name;
        let range = record.range;
        let enum_ = record.enum_.map(|e| mapper.map_enum(e));
        let definition = self.insert_tuple(record.definition, module, mapper);

        self.record.push(Record { name, range, enum_, definition, });
        self.record_lookup.insert(name, id);
    }

    fn insert_path_components(
        &mut self,
        path: PathId,
        module: &Module,
    )
        -> PathId
    {
        self.path_components.push(module.get_path_components(path))
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty())
    }

    fn insert_tuple(
        &mut self,
        tuple: Tuple<TypeId>,
        module: &Module,
        mapper: &IdMapper,
    )
        -> Tuple<TypeId>
    {
        let mut fields = vec!();
        for &id in module.get_type_ids(tuple.fields) {
            fields.push(self.insert_type(id, module, &mapper));
        }

        let fields = self.type_ids.push(&fields)
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty());

        let names = self.names.push(module.get_names(tuple.names))
            .map(|index| Id::new_repository(index as u32))
            .unwrap_or(Id::empty());

        Tuple { fields, names, }
    }

    fn insert_type(
        &mut self,
        ty: TypeId,
        module: &Module,
        mapper: &IdMapper,
    )
        -> TypeId
    {
        use self::Type::*;

        let ty = match module.get_type(ty) {
            Builtin(b) => return TypeId::from(b),
            ty => ty,
        };

        let ty = self.insert_type_impl(ty, module, mapper);

        let index = self.type_.len();
        self.type_.push(ty);
        TypeId::new_repository(index as u32)
    }

    fn insert_type_impl(&mut self, type_: Type, module: &Module, mapper: &IdMapper)
        -> Type
    {
        use self::Type::*;

        match type_ {
            Builtin(b) => Builtin(b),
            Enum(e, p) => {
                let e = mapper.map_enum(e);
                let p = self.insert_path_components(p, module);
                Enum(e, p)
            },
            Rec(r, p) => {
                let r = mapper.map_record(r);
                let p = self.insert_path_components(p, module);
                Rec(r, p)
            },
            Tuple(tuple) => {
                let tuple = self.insert_tuple(tuple, module, mapper);
                Tuple(tuple)
            },
            Unresolved(name, path) =>
                unreachable!("Cannot insert Unresolved({:?}, {:?})",
                    name, module.get_path_components(path)),
        }
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
    //  Enums
    enum_lookup: JaggedHashMapSnapshot<ItemIdentifier, EnumId>,
    enum_: JaggedArraySnapshot<Enum>,

    //  Extensions
    extension_lookup: JaggedHashMapSnapshot<ItemIdentifier, ExtensionId>,
    extension: JaggedArraySnapshot<Extension>,

    //  Functions
    function_lookup: JaggedHashMapSnapshot<ItemIdentifier, FunctionId>,
    function: JaggedArraySnapshot<FunctionSignature>,

    //  Records
    record_lookup: JaggedHashMapSnapshot<ItemIdentifier, RecordId>,
    record: JaggedArraySnapshot<Record>,

    //  Components
    names: JaggedMultiArraySnapshot<ValueIdentifier>,
    path_components: JaggedMultiArraySnapshot<PathComponent>,
    record_ids: JaggedMultiArraySnapshot<RecordId>,
    type_: JaggedArraySnapshot<Type>,
    type_ids: JaggedMultiArraySnapshot<TypeId>,
}

impl Registry for RepositorySnapshot {
    fn enums(&self) -> Vec<EnumId> {
        ids_of(self.enum_.len())
    }

    fn get_enum(&self, id: EnumId) -> Enum {
        *self.enum_.at(index_of(id))
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

    fn records(&self) -> Vec<RecordId> {
        ids_of(self.record.len())
    }

    fn get_record(&self, id: RecordId) -> Record {
        *self.record.at(index_of(id))
    }

    fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        self.names.get_slice(index_of(id))
    }

    fn get_path_components(&self, id: Id<[PathComponent]>)
        -> &[PathComponent]
    {
        self.path_components.get_slice(index_of(id))
    }

    fn get_record_ids(&self, id: Id<[RecordId]>) -> &[RecordId] {
        self.record_ids.get_slice(index_of(id))
    }

    fn get_type(&self, id: TypeId) -> Type {
        *self.type_.at(index_of(id))
    }
  
    fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        self.type_ids.get_slice(index_of(id))
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
        &slice[0..(range.end as usize)]
    } 
}


//  IdMapper
//
//  A mapper between module and repository ID, for recursive types.
struct IdMapper {
    enum_offset: i64,
    extension_offset: i64,
    record_offset: i64,
}

impl IdMapper {
    fn new(enum_offset: i64, extension_offset: i64, record_offset: i64) -> IdMapper {
        IdMapper { enum_offset, extension_offset, record_offset, }
    }

    fn map_enum(&self, id: EnumId) -> EnumId {
        debug_assert!(id.is_module());
        let local = id.get_module().expect("module") as i64;
        EnumId::new_repository((local + self.enum_offset) as u32)
    }

    fn map_extension(&self, id: ExtensionId) -> ExtensionId {
        debug_assert!(id.is_module());
        let local = id.get_module().expect("module") as i64;
        ExtensionId::new_repository((local + self.extension_offset) as u32)
    }

    fn map_record(&self, id: RecordId) -> RecordId {
        debug_assert!(id.is_module());
        let local = id.get_module().expect("module") as i64;
        RecordId::new_repository((local + self.record_offset) as u32)
    }
}

#[cfg(test)]
mod tests {

}
