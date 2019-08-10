//! Lexical scopes for name resolution

use std::collections::HashMap;
use std::fmt;

use crate::basic::mem;

use crate::model::hir::*;

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
#[derive(Clone, Debug)]
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
    pub fn new<I>(parent: &'a Scope, args: I) -> Self
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
    parent: &'a Scope,
    functions: IdMap<CallableCandidate>,
    types: IdMap<Type>,
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
    pub fn add_enum(&mut self, name: ItemIdentifier, id: EnumId) {
        self.types.insert(
            name.id(),
            Type::Enum(id, PathId::empty()),
        );
    }

    /// Adds a new function identifier to the scope.
    pub fn add_function(&mut self, name: ItemIdentifier, id: FunctionId) {
        self.functions.insert(name.id(), CallableCandidate::Function(id));
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

    fn lookup_type(&self, name: ItemIdentifier) -> Type {
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

    use super::{ValueIdentifier, Scope, BuiltinScope, FunctionScope};

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

    fn var_id(id: mem::InternId, pos: usize, len: usize) -> ValueIdentifier {
        ValueIdentifier(id, com::Range::new(pos, len))
    }
}
