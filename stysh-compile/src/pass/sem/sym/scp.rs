//! Lexical scopes for name resolution

use std::collections::HashMap;
use std::fmt;

use crate::basic::mem;

use crate::model::hir::{self, *};

/// A Lexical Scope trait.
pub trait Scope: fmt::Debug {
    /// Find the definition of a binding, if known.
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier>;

    /// Find the definition of a function, if known.
    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate;

    /// Find the definition of a type, if known.
    fn lookup_type(&self, name: ItemIdentifier) -> Type;

    /// Returns an unresolved reference.
    fn unresolved_function(&self, name: ValueIdentifier) -> CallableCandidate {
        CallableCandidate::Unknown(name)
    }

    /// Returns an unresolved reference.
    fn unresolved_type(&self, name: ItemIdentifier) -> Type {
        Type::Unresolved(name, PathId::empty())
    }
}

/// CallableCandidate.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum CallableCandidate {
    /// A built-in function.
    Builtin(BuiltinFunction),
    /// A static user-defined function.
    Function(FunctionId),
    /// An unknown callable binding.
    Unknown(ValueIdentifier),
    /// An unresolved callable binding.
    ///
    /// Note: this variant only contains possible resolutions.
    /// Note: this variant contains at least two possible resolutions.
    Unresolved(Vec<CallableCandidate>),
}

impl CallableCandidate {
    /// Convert a CallableCandidate into a hir::Callable.
    pub fn into_callable(&self, tree: &mut Tree) -> Callable {
        use self::CallableCandidate as C;

        match self {
            C::Builtin(f) => Callable::Builtin(*f),
            C::Function(f) => Callable::Function(*f),
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
    parent: Option<&'a dyn Scope>,
    registry: &'a dyn Registry,
    type_: Type,
    self_: bool,
}

impl<'a> TypeScope<'a> {
    /// Creates an instance of TypeScope.
    pub fn new(
        parent: &'a dyn Scope,
        registry: &'a dyn Registry,
        type_: Type
    )
        ->  Self
    {
        debug_assert!(Self::is_defined(type_), "Cannot create TypeScope of {:?}", type_);
        TypeScope { parent: Some(parent), registry, type_, self_: false,  }
    }

    /// Creates a stand-alone instance of TypeScope.
    pub fn stand_alone(registry: &'a dyn Registry, type_: Type) -> Self {
        debug_assert!(Self::is_defined(type_), "Cannot create TypeScope of {:?}", type_);
        TypeScope { parent: None, registry, type_, self_: false, }
    }

    /// Enables "Self" reserved identifier.
    ///
    /// Note: to be used within type definitions' scopes, but not paths.
    pub fn enable_self(&mut self) { self.self_ = true }

    /// Returns the matching callables within type.
    pub fn lookup_associated_callables(&self, name: ValueIdentifier) -> CallableCandidate {
        use self::hir::Scope::*;

        let mut associated = vec!();
        for id in self.registry.functions() {
            let fun = self.registry.get_function(id);
            if fun.name.id() != name.id() {
                continue;
            }
            match fun.scope {
                Module => (),
                Ext(e) => {
                    let ext = self.registry.get_extension(e);
                    if Self::are_compatible(ext.extended, self.type_) {
                        associated.push(CallableCandidate::Function(id));
                    }
                },
                Int(i) => {
                    let int = Type::Int(i, PathId::empty());
                    if Self::are_compatible(int, self.type_) {
                        associated.push(CallableCandidate::Function(id));
                    }
                }
            }
        }

        match associated.len() {
            0 => self.unresolved_function(name),
            1 => associated[0].clone(),
            _ => CallableCandidate::Unresolved(associated),
        }
    }

    fn is_defined(typ: Type) -> bool {
        match typ {
            Type::Enum(..) | Type::Int(..) | Type::Rec(..) => true,
            _ => false,
        }
    }

    fn are_compatible(left: Type, right: Type) -> bool {
        match (left, right) {
            (Type::Enum(l, ..), Type::Enum(r, ..)) => l == r,
            (Type::Int(l, ..), Type::Int(r, ..)) => l == r,
            (Type::Rec(l, ..), Type::Rec(r, ..)) => l == r,
            _ => false,
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
    types: IdMap<Type>,
    values: IdMap<ValueIdentifier>,
}

impl<'a> BlockScope<'a> {
    /// Create a new instance of BlockScope.
    pub fn new(parent: &'a dyn Scope) -> BlockScope<'a> {
        BlockScope {
            parent: parent,
            functions: IdMap::new(),
            types: IdMap::new(),
            values: IdMap::new(),
        }
    }

    /// Adds a new enum to the scope.
    pub fn add_enum(&mut self, name: ItemIdentifier, id: EnumId) {
        self.types.insert(name.id(), Type::Enum(id, PathId::empty()));
    }

    /// Adds a new function identifier to the scope.
    pub fn add_function(&mut self, name: ItemIdentifier, id: FunctionId) {
        self.functions.insert(name.id(), CallableCandidate::Function(id));
    }

    /// Adds a new interface identifier to the scope.
    pub fn add_interface(&mut self, name: ItemIdentifier, id: InterfaceId) {
        self.types.insert(name.id(), Type::Int(id, PathId::empty()));
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
        self.types.insert(name.id(), Type::Rec(id, PathId::empty()));
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

    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate {
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
        self.parent.and_then(|parent| parent.lookup_binding(name))
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate {
        let associated = self.lookup_associated_callables(name);

        if let Some(parent) = self.parent {
            let parent = parent.lookup_callable(name);
            merge(associated, parent)
        } else {
            associated
        }
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type {
        if self.self_ && name.id() == mem::InternId::self_type() {
            return self.type_;
        }

        self.parent.map(|parent| parent.lookup_type(name))
            .unwrap_or(self.unresolved_type(name))
    }
}

impl<'a> Scope for FunctionScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier> {
        self.arguments
            .get(&name.id())
            .cloned()
            .or_else(|| self.parent.lookup_binding(name))
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate {
        self.parent.lookup_callable(name)
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

    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate {
        let parent = self.parent.lookup_callable(name);

        if let Some(callable) = self.functions.get(&name.id()).cloned() {
            merge(callable, parent)
        } else {
            parent
        }
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

fn merge(immediate: CallableCandidate, parent: CallableCandidate)
    -> CallableCandidate
{
    use self::CallableCandidate::*;

    match (immediate, parent) {
        //  No merge actually required.
        (Unknown(_), parent) => parent,
        (immediate, Unknown(_)) => immediate,

        //  Vec merge required.
        (Unresolved(mut immediate), Unresolved(parent)) => {
            immediate.extend(parent.into_iter());
            Unresolved(immediate)
        },
        (Unresolved(mut immediate), parent) => {
            immediate.push(parent);
            Unresolved(immediate)
        },
        (immediate, Unresolved(mut parent)) => {
            parent.insert(0, immediate);
            Unresolved(parent)
        },

        //  Vec creation required.
        (immediate, parent) => Unresolved(vec!(immediate, parent)),
    }
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

        fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate {
            if let Some(v) = self.callables.get(&name) {
                return v.clone();
            }

            self.parent.lookup_callable(name)
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
    use crate::model::hir::{self, *};

    use super::{CallableCandidate, Scope, BuiltinScope, FunctionScope, TypeScope};

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
        let interner = mem::Interner::new();
        let bar = var_id(interner.insert(b"bar"), 45, 3);
        let baz = var_id(interner.insert(b"baz"), 71, 3);

        let (module, extended, result) = {
            let mut module = Module::new(Default::default());

            let name = item_id(interner.insert(b"Simple"), 19, 6);
            let range = Default::default();

            let rec = item_id(interner.insert(b"Simple"), 5, 6);
            let extended = Type::Rec(module.push_record_name(rec), Default::default());

            let ext = module.push_extension_name(name);

            module.set_extension(ext, Extension { name, range, extended, });

            let bar = item_id(bar.id(), 63, 3);
            let fun = module.push_function_name(bar);
            let signature = FunctionSignature {
                name: bar,
                range: Default::default(),
                scope: hir::Scope::Ext(ext),
                arguments: Tuple::default(),
                result: TypeId::void(),
            };

            module.set_function(fun, signature);

            (module, extended, fun)
        };

        let builtin = BuiltinScope::new();
        let scope = TypeScope::new(&builtin, &module, extended);

        assert_eq!(scope.lookup_callable(baz), CallableCandidate::Unknown(baz));
        assert_eq!(scope.lookup_callable(bar), CallableCandidate::Function(result));
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

        let (module, extended) = {
            let mut module = Module::new(Default::default());

            let rec = item_id(interner.insert(b"Simple"), 5, 6);
            let extended = Type::Rec(module.push_record_name(rec), Default::default());

            (module, extended)
        };

        let builtin = BuiltinScope::new();
        let mut scope = TypeScope::new(&builtin, &module, extended);

        assert_eq!(scope.lookup_type(self_), Type::Unresolved(self_, Id::empty()));

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
