//! Lexical scopes for name resolution

use std::collections::HashMap;
use std::fmt;

use basic::mem;

use model::hir::*;

/// A Lexical Scope trait.
pub trait Scope: fmt::Debug {
    /// Find the definition of a binding, if known.
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier>;

    /// Find the definition of a function, if known.
    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate;

    /// Find the definition of a type, if known.
    fn lookup_type(&self, name: ItemIdentifier) -> TypeDefinition;

    /// Returns an unresolved reference.
    fn unresolved_function(&self, name: ValueIdentifier) -> CallableCandidate {
        CallableCandidate::Unknown(name)
    }

    /// Returns an unresolved reference.
    fn unresolved_type(&self, name: ItemIdentifier) -> TypeDefinition {
        TypeDefinition::Unresolved(name, Path::default())
    }
}

/// CallableCandidate.
#[derive(Clone, Debug)]
pub enum CallableCandidate {
    /// A built-in function.
    Builtin(BuiltinFunction),
    /// A static user-defined function.
    Function(FunctionProto),
    /// An unknown callable binding.
    Unknown(ValueIdentifier),
    /// An unresolved callable binding.
    ///
    /// Note: this variant only contains possible resolutions.
    /// Note: this variant contains at least two possible resolutions.
    Unresolved(Vec<CallableCandidate>),
}

/// A Builtin Scope.
#[derive(Debug)]
pub struct BuiltinScope;

impl BuiltinScope {
    /// Creates an instance of BuiltinScope.
    pub fn new() -> BuiltinScope { BuiltinScope }
}

/// A Function Prototype Scope.
pub struct FunctionScope<'a> {
    parent: &'a Scope,
    arguments: IdMap<ValueIdentifier>,
}

impl<'a> FunctionScope<'a> {
    /// Creates an instance of FunctionScope.
    pub fn new(parent: &'a Scope, fun: FunctionProto) -> FunctionScope {
        let mut arguments = IdMap::new();

        for a in fun.arguments {
            arguments.insert(a.name.id(), a.name);
        }

        FunctionScope { parent, arguments }
    }
}

/// A Block Scope.
pub struct BlockScope<'a> {
    parent: &'a Scope,
    functions: IdMap<CallableCandidate>,
    types: IdMap<TypeDefinition>,
    values: IdMap<ValueIdentifier>,
}

impl<'a> BlockScope<'a> {
    /// Create a new instance of BlockScope.
    pub fn new(parent: &'a Scope) -> BlockScope<'a> {
        BlockScope {
            parent: parent,
            functions: IdMap::new(),
            types: IdMap::new(),
            values: IdMap::new(),
        }
    }

    /// Adds a new enum to the scope.
    pub fn add_enum(&mut self, proto: EnumProto) {
        self.types.insert(
            proto.name.id(),
            TypeDefinition::UnresolvedEnum(proto, Path::default()),
        );
    }

    /// Adds a new function identifier to the scope.
    pub fn add_function(&mut self, proto: FunctionProto) {
        self.functions.insert(proto.name.id(), CallableCandidate::Function(proto));
    }

    /// Adds a new pattern to the scope.
    pub fn add_pattern(&mut self, pattern: PatternId, tree: &Tree) {
        let tuple = match *tree.get_pattern(pattern) {
            Pattern::Ignored(..) => return,
            Pattern::Var(id) => {
                self.add_value(id);
                return;
            },
            Pattern::Constructor(tuple) | Pattern::Tuple(tuple) => tuple,
        };

        let fields = tree.get_patterns(tuple.fields);
        for p in fields { self.add_pattern(*p, tree); }
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

    fn lookup_type(&self, name: ItemIdentifier) -> TypeDefinition {
        use model::hir::BuiltinType::*;

        let builtin = match name.id() {
            id if id == mem::InternId::bool_() => Some(Bool),
            id if id == mem::InternId::int() => Some(Int),
            id if id == mem::InternId::string() => Some(String),
            _ => None,
        };

        builtin.map(|b| TypeDefinition::Builtin(b)).unwrap_or(self.unresolved_type(name))
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

    fn lookup_type(&self, name: ItemIdentifier) -> TypeDefinition {
        self.parent.lookup_type(name)
    }
}

impl<'a> Scope for BlockScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Option<ValueIdentifier> {
        self.values
            .get(&name.id())
            .cloned()
            .or_else(|| self.parent.lookup_binding(name))
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> CallableCandidate {
        use self::CallableCandidate::*;

        let mut collection = vec!();

        match self.parent.lookup_callable(name) {
            Unknown(_) => (),
            Builtin(fun) => collection.push(Builtin(fun)),
            Function(fun) => collection.push(Function(fun)),
            Unresolved(callables) => collection.extend(callables),
        }

        if let Some(callable) = self.functions.get(&name.id()).cloned() {
            if collection.is_empty() {
                return callable;
            }
            collection.push(callable);
        }

        match collection.len() {
            0 => Unknown(name),
            1 => collection[0].clone(),
            _ => Unresolved(collection)
        }
    }

    fn lookup_type(&self, name: ItemIdentifier) -> TypeDefinition {
        if let Some(type_) = self.types.get(&name.id()).cloned() {
            return type_;
        }

        self.parent.lookup_type(name)
    }
}

//
//  Implementation Details
//
type IdMap<V> = HashMap<mem::InternId, V>;

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

    use model::hir::*;

    use super::{BuiltinScope, CallableCandidate, IdMap, Scope};

    /// A mock for the Scope trait.
    #[derive(Debug)]
    pub struct MockScope {
        /// Map of callables for lookup_callable.
        pub callables: HashMap<ValueIdentifier, CallableCandidate>,
        /// Map of types for lookup_type.
        pub types: HashMap<ItemIdentifier, TypeDefinition>,
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

        fn lookup_type(&self, name: ItemIdentifier) -> TypeDefinition {
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
    use basic::mem;
    use model::hir::builder::Factory;

    use super::{Scope, BuiltinScope, FunctionScope};

    #[test]
    fn function_no_arguments() {
        //  b":fun random() -> Int { a }";

        let interner = mem::Interner::new();
        let a = interner.insert(b"a");

        let f = Factory::new(Default::default());
        let (i, p, t, v) = (f.item(), f.proto(), f.type_definition(), f.value());

        let prot = p.fun(i.id(5, 6), t.int()).range(0, 20).build();

        let builtin = BuiltinScope::new();
        let scope = FunctionScope::new(&builtin, prot);

        assert_eq!(
            scope.lookup_binding(v.id(23, 1).with_id(a)),
            None
        );
    }

    #[test]
    fn function_with_arguments() {
        //  b":fun add(a: Int, b: Int) -> Int { a + b + c }";

        let interner = mem::Interner::new();
        let a = interner.insert(b"a");
        let b = interner.insert(b"b");
        let c = interner.insert(b"c");

        let f = Factory::new(Default::default());
        let (i, p, t, v) = (f.item(), f.proto(), f.type_definition(), f.value());

        let prot =
            p.fun(i.id(5, 3), t.int())
                .push(v.id(9, 1).with_id(a), t.int())
                .push(v.id(17, 1).with_id(b), t.int())
                .build();

        let builtin = BuiltinScope::new();
        let scope = FunctionScope::new(&builtin, prot);

        assert_eq!(
            scope.lookup_binding(v.id(42, 1).with_id(c)),
            None
        );

        assert_eq!(
            scope.lookup_binding(v.id(34, 1).with_id(a)),
            Some(v.id(9, 1).with_id(a))
        );

        assert_eq!(
            scope.lookup_binding(v.id(38, 1).with_id(b)),
            Some(v.id(17, 1).with_id(b))
        );
    }
}
