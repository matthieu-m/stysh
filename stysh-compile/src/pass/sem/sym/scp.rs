//! Lexical scopes for name resolution

use std::collections::{BTreeSet, HashMap};
use std::fmt;

use crate::basic::mem;

use crate::model::hir::*;

/// A Lexical Scope trait.
pub trait Scope: fmt::Debug {
    /// Find the definition of a binding, if known.
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier>;

    /// Find the definition of a function, if known.
    fn lookup_callable(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate;

    /// Find the definition of a method for a given receiver, if known.
    fn lookup_method(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate;

    /// Find the definition of a type, if known.
    fn lookup_type(&self, name: ItemIdentifier) -> Type;

    /// Returns an unresolved reference.
    fn unresolved_function(&self, name: ValueIdentifier) -> CallableCandidate {
        CallableCandidate::Unknown(name)
    }

    /// Returns an unresolved reference.
    fn unresolved_type(&self, _: ItemIdentifier) -> Type {
        Type::Unresolved
    }
}

/// CallableCandidate.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum CallableCandidate {
    /// A built-in function.
    Builtin(BuiltinFunction),
    /// A static user-defined function.
    Function(FunctionId),
    /// A dynamic user-defined function.
    Method(FunctionId),
    /// An unknown callable binding.
    Unknown(ValueIdentifier),
    /// An unresolved callable binding.
    ///
    /// Note: this variant only contains possible resolutions.
    /// Note: this variant contains at least two possible resolutions.
    Unresolved(BTreeSet<CallableCandidate>),
}

impl CallableCandidate {
    /// Convert a CallableCandidate into a hir::Callable.
    pub fn into_callable(&self, tree: &mut Tree) -> Callable {
        use self::CallableCandidate as C;

        match self {
            C::Builtin(f) => Callable::Builtin(*f),
            C::Function(f) => Callable::Function(*f),
            C::Method(f) => Callable::Method(*f),
            C::Unknown(name) => Callable::Unknown(*name),
            C::Unresolved(candidates) => {
                let callables: Vec<_> = candidates.iter()
                    .map(|c| c.into_callable(tree))
                    .collect();
                let callables = tree.push_callables(callables);
                Callable::Unresolved(callables)
            },
        }
    }
}

/// A Builtin Scope.
#[derive(Debug)]
pub struct BuiltinScope;

impl BuiltinScope {
    /// Creates an instance of BuiltinScope.
    pub fn new() -> BuiltinScope { BuiltinScope }
}

/// A Type Scope.
pub struct TypeScope<'a> {
    parent: &'a dyn Scope,
    type_: Type,
    self_: bool,
}

impl<'a> TypeScope<'a> {
    /// Creates an instance of TypeScope.
    pub fn new(parent: &'a dyn Scope, type_: Type) ->  Self {
        debug_assert!(Self::is_defined(type_), "Cannot create TypeScope of {:?}", type_);
        TypeScope { parent: parent, type_, self_: false,  }
    }

    /// Enables "Self" reserved identifier.
    ///
    /// Note: to be used within type definitions' scopes, but not paths.
    pub fn enable_self(&mut self) { self.self_ = true }

    /// Find the definition of an associated function, for this specific receiver.
    pub fn lookup_associated_function(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry
    )
        -> CallableCandidate
    {
        use self::Type::*;
        type Constructor = dyn Fn(FunctionId) -> CallableCandidate;

        let function: &Constructor = &CallableCandidate::Function;
        let method: &Constructor = &CallableCandidate::Method;

        let (constructor, functions) = match self.type_ {
            //  No extensions on those.
            Tuple(..) | Unresolved =>
                return self.unresolved_function(name),
            //  Always a method call, resolved at run-time.
            Int(i) => (method, registry.get_interface_functions(i)),
            //  Always a function call, resolve at compile-time.
            Builtin(ty) => (function, registry.get_builtin_functions(ty)),
            Enum(e) => (function, registry.get_enum_functions(e)),
            Rec(r) => (function, registry.get_record_functions(r)),
        };

        self.lookup_associated_among(constructor, name, functions)
    }

    /// Find the definition of a method, for this specific receiver.
    pub fn lookup_associated_method(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry
    )
        -> CallableCandidate
    {
        let methods = self.lookup_method(name, registry);

        let associated = if let Type::Int(..) = self.type_ {
            self.unresolved_function(name)
        } else {
            self.lookup_associated_function(name, registry)
        };

        merge(associated, methods)
    }
}

impl<'a> TypeScope<'a> {
    fn is_defined(typ: Type) -> bool {
        use self::Type::*;

        match typ {
            Builtin(..) | Enum(..) | Int(..) | Rec(..) => true,
            _ => false,
        }
    }

    fn lookup_associated_among(
        &self,
        constructor: impl Fn(FunctionId) -> CallableCandidate,
        name: ValueIdentifier,
        functions: &[(Identifier, FunctionId)],
    )
        ->  CallableCandidate
    {
        let candidates = find_functions_by_identifier(name.id(), functions);

        match candidates {
            [] => self.unresolved_function(name),
            [(_, fun)] => constructor(*fun),
            _ => CallableCandidate::Unresolved(
                candidates.iter().copied()
                    .map(|(_, fun)| constructor(fun))
                    .collect()
            ),
        }
    }
}

/// A Function Prototype Scope.
pub struct FunctionScope<'a> {
    parent: &'a dyn Scope,
    arguments: IdMap<ValueIdentifier>,
}

impl<'a> FunctionScope<'a> {
    /// Creates an instance of FunctionScope.
    pub fn new<I>(parent: &'a dyn Scope, args: I) -> Self
        where
            I: Iterator<Item = ValueIdentifier>,
    {
        let mut arguments = IdMap::new();

        for a in args {
            arguments.insert(a.0, a);
        }

        FunctionScope { parent, arguments }
    }
}

/// A Block Scope.
pub struct BlockScope<'a> {
    parent: &'a dyn Scope,
    functions: IdMap<CallableCandidate>,
    methods: IdMap<CallableCandidate>,
    types: IdMap<Type>,
    values: IdMap<ValueIdentifier>,
}

impl<'a> BlockScope<'a> {
    /// Create a new instance of BlockScope.
    pub fn new(parent: &'a dyn Scope) -> BlockScope<'a> {
        BlockScope {
            parent: parent,
            functions: IdMap::new(),
            methods: IdMap::new(),
            types: IdMap::new(),
            values: IdMap::new(),
        }
    }

    /// Adds a new enum to the scope.
    pub fn add_enum(&mut self, name: ItemIdentifier, id: EnumId) {
        debug_assert!(!self.types.contains_key(&name.id()));

        self.types.insert(name.id(), Type::Enum(id));
    }

    /// Adds a new function identifier to the scope.
    pub fn add_function(&mut self, name: ItemIdentifier, id: FunctionId) {
        use std::collections::hash_map::Entry::*;

        let callable = CallableCandidate::Function(id);

        match self.functions.entry(name.id()) {
            Occupied(mut o) => merge_in_place(o.get_mut(), CallableCandidate::Function(id)),
            Vacant(v) => { v.insert(callable); },
        };
    }

    /// Adds a new interface identifier to the scope.
    ///
    /// Including its associated methods.
    pub fn add_interface(&mut self, name: ItemIdentifier, id: InterfaceId) {
        debug_assert!(!self.types.contains_key(&name.id()));

        self.types.insert(name.id(), Type::Int(id));
    }

    /// Adds a methods to an interface.
    pub fn add_method(&mut self, name: Identifier, id: FunctionId) {
        use std::collections::hash_map::Entry::*;

        let callable = CallableCandidate::Method(id);

        match self.methods.entry(name) {
            Occupied(mut o) => merge_in_place(o.get_mut(), callable),
            Vacant(v) => { v.insert(callable); },
        }
    }

    /// Adds a new pattern to the scope.
    pub fn add_pattern(&mut self, pattern: PatternId, tree: &Tree) {
        let tuple = match tree.get_pattern(pattern) {
            Pattern::Ignored(..) => return,
            Pattern::Var(id) => {
                self.add_value(id);
                return;
            },
            Pattern::Constructor(tuple) | Pattern::Tuple(tuple) => tuple,
        };

        let fields = tree.get_pattern_ids(tuple.fields);
        for p in fields { self.add_pattern(*p, tree); }
    }

    /// Adds a new record to the scope.
    pub fn add_record(&mut self, name: ItemIdentifier, id: RecordId) {
        debug_assert!(!self.types.contains_key(&name.id()));

        self.types.insert(name.id(), Type::Rec(id));
    }

    /// Adds a new value identifier to the scope.
    pub fn add_value(&mut self, name: ValueIdentifier) {
        self.values.insert(name.id(), name);
    }
}

//
//  Implementations of Scope
//
impl Scope for BuiltinScope {
    fn lookup_binding(&self, _: ValueIdentifier) -> Option<ValueIdentifier> {
        None
    }

    fn lookup_callable(
        &self,
        name: ValueIdentifier,
        _registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        self.unresolved_function(name)
    }

    fn lookup_method(
        &self,
        name: ValueIdentifier,
        _registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        self.unresolved_function(name)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type {
        use self::BuiltinType::*;

        let builtin = match name.id() {
            id if id == mem::InternId::bool_() => Some(Bool),
            id if id == mem::InternId::int() => Some(Int),
            id if id == mem::InternId::string() => Some(String),
            _ => None,
        };

        builtin.map(|b| Type::Builtin(b)).unwrap_or(self.unresolved_type(name))
    }
}

impl<'a> Scope for TypeScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier> {
        self.parent.lookup_binding(name)
    }

    fn lookup_callable(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        let immediate = self.lookup_associated_function(name, registry);
        let parent = self.parent.lookup_callable(name, registry);
        merge(immediate, parent)
    }

    fn lookup_method(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        self.parent.lookup_method(name, registry)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type {
        if self.self_ && name.id() == mem::InternId::self_type() {
            return self.type_;
        }

        self.parent.lookup_type(name)
    }
}

impl<'a> Scope for FunctionScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier> {
        self.arguments
            .get(&name.id())
            .cloned()
            .or_else(|| self.parent.lookup_binding(name))
    }

    fn lookup_callable(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        self.parent.lookup_callable(name, registry)
    }

    fn lookup_method(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        self.parent.lookup_method(name, registry)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type {
        self.parent.lookup_type(name)
    }
}

impl<'a> Scope for BlockScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier> {
        self.values
            .get(&name.id())
            .copied()
            .or_else(|| self.parent.lookup_binding(name))
    }

    fn lookup_callable(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        let parent = self.parent.lookup_callable(name, registry);

        if let Some(callable) = self.functions.get(&name.id()).cloned() {
            merge(callable, parent)
        } else {
            parent
        }
    }

    fn lookup_method(
        &self,
        name: ValueIdentifier,
        registry: &dyn Registry,
    )
        -> CallableCandidate
    {
        let callables = self.lookup_callable(name, registry);

        let parent = self.parent.lookup_method(name, registry);

        let immediate = self.methods.get(&name.id())
            .cloned()
            .unwrap_or(self.unresolved_function(name));

        merge(merge(immediate, parent), callables)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type {
        if let Some(type_) = self.types.get(&name.id()).copied() {
            return type_;
        }

        self.parent.lookup_type(name)
    }
}

//
//  Implementation Details
//
type IdMap<V> = HashMap<mem::InternId, V>;

fn find_functions_by_identifier(
    name: Identifier,
    functions: &[(Identifier, FunctionId)],
)
    -> &[(Identifier, FunctionId)]
{
    functions.binary_search_by(|&(n, _)| n.cmp(&name))
        .map(|root| {
            assert!(root < functions.len());

            let mut left = root;
            while left > 0 && functions[left - 1].0 == name {
                left -= 1;
            }

            let mut right = root + 1;
            while right < functions.len() && functions[right].0 == name {
                right += 1
            }

            &functions[left..right]
        })
        .unwrap_or(&[])
}

fn merge(immediate: CallableCandidate, parent: CallableCandidate)
    -> CallableCandidate
{
    use self::CallableCandidate::*;

    match (immediate, parent) {
        //  No merge actually required.
        (Unknown(_), parent) => parent,
        (immediate, Unknown(_)) => immediate,

        //  BTreeSet merge required.
        (Unresolved(mut immediate), Unresolved(parent)) => {
            immediate.extend(parent.into_iter());
            Unresolved(immediate)
        },
        (Unresolved(mut immediate), parent) => {
            immediate.insert(parent);
            Unresolved(immediate)
        },
        (immediate, Unresolved(mut parent)) => {
            parent.insert(immediate);
            Unresolved(parent)
        },

        //  BTreeSet creation required.
        (immediate, parent) => {
            let mut callables = BTreeSet::new();
            callables.insert(immediate);
            callables.insert(parent);
            Unresolved(callables)
        },
    }
}

fn merge_in_place(existing: &mut CallableCandidate, new: CallableCandidate)
{
    let tmp = CallableCandidate::Unknown(Default::default());
    let extracted = std::mem::replace(existing, tmp);
    let extracted = merge(extracted, new);
    std::mem::replace(existing, extracted);
}

//
//  Trait Implementations
//
impl<'a> fmt::Debug for TypeScope<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "TypeScope {{ parent: {:?}, type: {:?} }}",
            self.parent,
            self.type_,
        )
    }
}

impl<'a> fmt::Debug for FunctionScope<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "FunctionScope {{ parent: {:?}, arguments: {:?} }}",
            self.parent,
            self.arguments,
        )
    }
}

impl<'a> fmt::Debug for BlockScope<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "BlockScope {{ parent: {:?}, functions: {:?}, types: {:?}, values: {:?} }}",
            self.parent,
            self.functions,
            self.types,
            self.values,
        )
    }
}

/// Mocks for the traits.
#[cfg(test)]
pub mod mocks {
    use std::collections::HashMap;

    use crate::model::hir::*;

    use super::{BuiltinScope, CallableCandidate, IdMap, Scope};

    /// A mock for the Scope trait.
    #[derive(Debug)]
    pub struct MockScope {
        /// Map of callables for lookup_callable.
        pub callables: HashMap<ValueIdentifier, CallableCandidate>,
        /// Map of methods for lookup_method.
        pub methods: HashMap<ValueIdentifier, CallableCandidate>,
        /// Map of types for lookup_type.
        pub types: HashMap<ItemIdentifier, Type>,
        /// Map of values for lookup_binding.
        values: IdMap<ValueIdentifier>,
        /// Parent scope, automatically get all builtins.
        pub parent: BuiltinScope,
    }

    impl MockScope {
        /// Creates a new MockScope.
        pub fn new() -> MockScope {
            MockScope {
                callables: HashMap::new(),
                methods: HashMap::new(),
                types: HashMap::new(),
                values: HashMap::new(),
                parent: BuiltinScope::new(),
            }
        }

        /// Inserts Binding.
        pub fn insert_binding(&mut self, name: ValueIdentifier) {
            self.values.insert(name.id(), name);
        }
    }

    impl Scope for MockScope {
        fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier> {
            self.values.get(&name.id()).cloned()
        }

        fn lookup_callable(
            &self,
            name: ValueIdentifier,
            registry: &dyn Registry,
        )
            -> CallableCandidate
        {
            if let Some(v) = self.callables.get(&name) {
                return v.clone();
            }

            self.parent.lookup_callable(name, registry)
        }

        fn lookup_method(
            &self,
            name: ValueIdentifier,
            registry: &dyn Registry,
        )
            -> CallableCandidate
        {
            if let Some(v) = self.methods.get(&name) {
                return v.clone();
            }

            self.parent.lookup_method(name, registry)
        }

        fn lookup_type(&self, name: ItemIdentifier) -> Type {
            if let Some(v) = self.types.get(&name) {
                return v.clone();
            }

            self.parent.lookup_type(name)
        }
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use crate::basic::{com, mem};
    use crate::model::ast;
    use crate::model::hir::{self, *};

    use super::*;

    #[test]
    fn block_interface_methods() {
        //  :int Interface {
        //      :fun foo(self) {}
        //      :fun bar(self) {}
        //  }
        let interner = mem::Interner::new();
        let interface = interner.insert(b"Interface");
        let (foo, foo_id) = (interner.insert(b"foo"), FunctionId::new_tree(0));
        let (bar, bar_id) = (interner.insert(b"bar"), FunctionId::new_tree(1));

        let builtin = BuiltinScope::new();
        let registry = MockRegistry::new();

        let scope = {
            let mut scope = BlockScope::new(&builtin);
            scope.add_interface(
                item_id(interface, 5, 9),
                InterfaceId::new_tree(0),
            );
            scope.add_method(foo, foo_id);
            scope.add_method(bar, bar_id);
            scope
        };

        assert_eq!(
            scope.lookup_method(var_id(bar, 48, 3), &registry),
            CallableCandidate::Method(bar_id)
        );
    }

    #[test]
    fn function_no_arguments() {
        //  b":fun random() -> Int { a }";

        let interner = mem::Interner::new();
        let a = var_id(interner.insert(b"a"), 23, 1);

        let builtin = BuiltinScope::new();
        let scope = FunctionScope::new(&builtin, [].iter().cloned());

        assert_eq!(scope.lookup_binding(a), None);
    }

    #[test]
    fn function_with_arguments() {
        //  b":fun add(a: Int, b: Int) -> Int { a + b + c }";

        let interner = mem::Interner::new();
        let a = interner.insert(b"a");
        let b = interner.insert(b"b");
        let c = interner.insert(b"c");

        let builtin = BuiltinScope::new();
        let scope = FunctionScope::new(
            &builtin,
            [var_id(a, 9, 1), var_id(b, 17, 1)].iter().cloned()
        );

        assert_eq!(
            scope.lookup_binding(var_id(c, 42, 1)),
            None
        );

        assert_eq!(
            scope.lookup_binding(var_id(a, 34, 1)),
            Some(var_id(a, 9, 1))
        );

        assert_eq!(
            scope.lookup_binding(var_id(b, 38, 1)),
            Some(var_id(b, 17, 1))
        );
    }

    #[test]
    fn type_associated_function_lookup() {
        //  :rec Simple;
        //
        //  :ext Simple {
        //      :fun foo() { bar(); }
        //      :fun bar() { baz(); }
        //  }
        use std::convert::TryInto;

        let interner = mem::Interner::new();
        let bar = var_id(interner.insert(b"bar"), 45, 3);
        let baz = var_id(interner.insert(b"baz"), 71, 3);

        let (module, extended, result) = {
            let mut module = Module::new(Default::default());

            let range = Default::default();

            let ext = Type::Rec(module.push_record_name(ast::RecordId::new(0)));
            let elaborate_extended = ext.try_into().expect("Record");

            let ext_id = module.push_extension_name(ast::ExtensionId::new(0));
            let extended = module.push_type(ext);
            let elaborate_extended = module.push_elaborate_type(elaborate_extended);

            module.set_extension(ext_id, Extension { range, extended, elaborate_extended, });

            let bar = item_id(bar.id(), 63, 3);
            let fun = module.push_function_name(ast::FunctionId::new(0));
            let signature = FunctionSignature {
                name: bar,
                range: Default::default(),
                scope: hir::Scope::Ext(ext_id),
                arguments: Default::default(),
                argument_types: Default::default(),
                result: TypeId::void(),
                elaborate_argument_types: Default::default(),
                elaborate_result: ElaborateTypeId::void(),
            };

            module.set_function(fun, signature);

            (module, ext, fun)
        };

        let builtin = BuiltinScope::new();
        let scope = TypeScope::new(&builtin, extended);

        assert_eq!(
            scope.lookup_associated_function(baz, &module),
            CallableCandidate::Unknown(baz)
        );
        assert_eq!(
            scope.lookup_associated_function(bar, &module),
            CallableCandidate::Function(result)
        );
    }

    #[test]
    fn type_injected_self_lookup() {
        //  :rec Simple;
        //
        //  :ext Simple {
        //      :fun new() -> Self { 0 }
        //  }
        let interner = mem::Interner::new();
        let self_ = item_id(interner.insert(b"Self"), 46, 4);

        let (_module, extended) = {
            let mut module = Module::new(Default::default());

            let extended = Type::Rec(module.push_record_name(ast::RecordId::new(0)));

            (module, extended)
        };

        let builtin = BuiltinScope::new();
        let mut scope = TypeScope::new(&builtin, extended);

        assert_eq!(scope.lookup_type(self_), Type::Unresolved);

        scope.enable_self();

        assert_eq!(scope.lookup_type(self_), extended);
    }

    fn item_id(id: mem::InternId, pos: usize, len: usize) -> ItemIdentifier {
        ItemIdentifier(id, com::Range::new(pos, len))
    }

    fn var_id(id: mem::InternId, pos: usize, len: usize) -> ValueIdentifier {
        ValueIdentifier(id, com::Range::new(pos, len))
    }
}
