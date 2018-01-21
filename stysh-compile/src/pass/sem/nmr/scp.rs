//! Lexical scopes for name resolution

use std::fmt;

use basic::mem;

use model::sem::*;

/// A Lexical Scope trait.
pub trait Scope<'g>: fmt::Debug {
    /// Find the definition of a binding, if known.
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g>;

    /// Find the definition of a function, if known.
    fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g>;

    /// Find the definition of a type, if known.
    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g>;

    /// Returns an unresolved reference.
    fn unresolved_binding(&self, name: ValueIdentifier) -> Value<'g> {
        Value {
            type_: Type::Unresolved(ItemIdentifier::unresolved()),
            range: name.0,
            expr: Expr::UnresolvedRef(name),
        }
    }

    /// Returns an unresolved reference.
    fn unresolved_function(&self, name: ValueIdentifier) -> Callable<'g> {
        Callable::Unknown(name)
    }

    /// Returns an unresolved reference.
    fn unresolved_type(&self, name: ItemIdentifier) -> Type<'g> {
        Type::Unresolved(name)
    }
}

/// A Builtin Scope.
#[derive(Debug)]
pub struct BuiltinScope<'a> {
    source: &'a [u8],
}

impl<'a> BuiltinScope<'a> {
    /// Creates an instance of BuiltinScope.
    pub fn new(source: &'a [u8]) -> BuiltinScope<'a> {
        BuiltinScope { source: source }
    }

    /// Returns the source it is created from.
    pub fn source(&self) -> &'a [u8] {
        self.source
    }
}

/// A Function Prototype Scope.
pub struct FunctionScope<'a, 'g>
    where 'g: 'a
{
    source: &'a [u8],
    parent: &'a Scope<'g>,
    arguments: &'a [(ValueIdentifier, Type<'a>)],
    global_arena: &'g mem::Arena,
}

impl<'a, 'g> FunctionScope<'a, 'g> {
    /// Creates an instance of FunctionScope.
    pub fn new(
        source: &'a [u8],
        parent: &'a Scope<'g>,
        fun: &'a FunctionProto<'a>,
        global_arena: &'g mem::Arena,
        local_arena: &'a mem::Arena,
    )
        -> FunctionScope<'a, 'g>
    {
        let mut array =
            mem::Array::with_capacity(fun.arguments.len(), local_arena);

        for &arg in fun.arguments {
            if let Binding::Argument(value, type_, _) = arg {
                array.push((value, type_));
                continue;
            }
            panic!("All bindings in a function argument must be arguments!");
        }

        FunctionScope {
            source: source,
            parent: parent,
            arguments: array.into_slice(),
            global_arena: global_arena
        }
    }
}

/// A Block Scope.
pub struct BlockScope<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    source: &'local [u8],
    parent: &'a Scope<'g>,
    functions: SourceMap<'local, Callable<'g>>,
    types: SourceMap<'local, Type<'g>>,
    values: SourceMap<'local, (ValueIdentifier, Type<'g>)>,
    global_arena: &'g mem::Arena,
}

impl<'a, 'g, 'local> BlockScope<'a, 'g, 'local> {
    /// Create a new instance of BlockScope.
    pub fn new(
        source: &'local [u8],
        parent: &'a Scope<'g>,
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
    )
        -> BlockScope<'a, 'g, 'local>
    {
        BlockScope {
            source: source,
            parent: parent,
            functions: SourceMap::new(local_arena),
            types: SourceMap::new(local_arena),
            values: SourceMap::new(local_arena),
            global_arena: global_arena
        }
    }

    /// Adds a new enum to the scope.
    pub fn add_enum(&mut self, proto: EnumProto) {
        self.types.insert(&self.source[proto.name.0], Type::Enum(proto));
    }

    /// Adds a new function identifier to the scope.
    pub fn add_function(&mut self, proto: FunctionProto<'g>) {
        self.functions.insert(
            &self.source[proto.name.0],
            Callable::Function(proto)
        );
    }

    /// Adds a new pattern to the scope.
    pub fn add_pattern(&mut self, pat: Pattern<'g>, type_: Type<'g>) {
        fn type_of_field<'b>(type_: Type<'b>, index: usize) -> Type<'b> {
            let unresolved = Type::Unresolved(ItemIdentifier::unresolved());
            if let Type::Tuple(type_) = type_ {
                type_.fields.get(index).cloned().unwrap_or(unresolved)
            } else {
                unresolved
            }
        }

        match pat {
            Pattern::Ignored(_) => (),
            Pattern::Tuple(tuple, _) => {
                for (index, pat) in tuple.fields.iter().enumerate() {
                    self.add_pattern(*pat, type_of_field(type_, index))
                }
            },
            Pattern::Var(id) => self.add_value(id, type_),
        }
    }

    /// Adds a new value identifier to the scope.
    pub fn add_value(&mut self, id: ValueIdentifier, type_: Type<'g>) {
        self.values.insert(&self.source[id.0], (id, type_));
    }
}

//
//  Implementations of Scope
//
impl<'a, 'g> Scope<'g> for BuiltinScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        self.unresolved_binding(name)
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g> {
        self.unresolved_function(name)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
        use model::sem::BuiltinType::*;

        let builtin = match &self.source[name.0] {
            b"Bool" => Some(Bool),
            b"Int" => Some(Int),
            b"String" => Some(String),
            _ => None,
        };

        builtin.map(|b| Type::Builtin(b)).unwrap_or(self.unresolved_type(name))
    }
}

impl<'a, 'g> Scope<'g> for FunctionScope<'a, 'g> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        for &(identifier, type_) in self.arguments {
            if &self.source[identifier.0] == &self.source[name.0] {
                return Value {
                    type_: self.global_arena.intern(&type_),
                    range: name.0,
                    expr: Expr::ArgumentRef(identifier),
                };
            }
        }

        self.unresolved_binding(name)
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g> {
        self.parent.lookup_callable(name)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
        self.parent.lookup_type(name)
    }
}

impl<'a, 'g, 'local> Scope<'g> for BlockScope<'a, 'g, 'local> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        if let Some(&(id, type_)) = self.values.get(&&self.source[name.0]) {
            return Value {
                type_: self.global_arena.intern(&type_),
                range: name.0,
                expr: Expr::VariableRef(id)
            }
        }

        self.parent.lookup_binding(name)
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g> {
        use model::sem::Callable::*;

        let mut collection = mem::Array::new(self.functions.arena());

        match self.parent.lookup_callable(name) {
            Unknown(_) => (),
            Builtin(fun) => collection.push(Builtin(fun)),
            Function(fun) => collection.push(Function(fun)),
            Unresolved(slice) => collection.extend(slice),
        }

        if let Some(&callable) = self.functions.get(&&self.source[name.0]) {
            if collection.is_empty() {
                return callable;
            }
            collection.push(callable);
        }

        match collection.len() {
            0 => Unknown(name),
            1 => collection[0],
            _ => Unresolved(self.global_arena.insert_slice(&*collection))
        }
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
        if let Some(&type_) = self.types.get(&&self.source[name.0]) {
            return type_;
        }

        self.parent.lookup_type(name)
    }
}

//
//  Implementation Details
//
type SourceMap<'a, V> = mem::ArrayMap<'a, &'a [u8], V>;

impl<'a, 'g> fmt::Debug for FunctionScope<'a, 'g> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "FunctionScope {{ parent: {:?}, arguments: {:?} }}",
            self.parent,
            self.arguments,
        )
    }
}

impl<'a, 'g, 'local> fmt::Debug for BlockScope<'a, 'g, 'local> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "BlockScope {{ parent: {:?}, functions: {:?}, values: {:?} }}",
            self.parent,
            self.functions,
            self.values,
        )
    }
}

/// Mocks for the traits.
pub mod mocks {
    use basic::mem;
    use model::sem::*;

    use super::{Scope, BuiltinScope};

    /// A mock for the Scope trait.
    #[derive(Debug)]
    pub struct MockScope<'g> {
        /// Map of callables for lookup_callable.
        pub callables: mem::ArrayMap<'g, ValueIdentifier, Callable<'g>>,
        /// Map of types for lookup_type.
        pub types: mem::ArrayMap<'g, ItemIdentifier, Type<'g>>,
        /// Map of values for lookup_binding.
        pub values: mem::ArrayMap<'g, ValueIdentifier, Value<'g>>,
        /// Parent scope, automatically get all builtins.
        pub parent: BuiltinScope<'g>,
    }

    impl<'g> MockScope<'g> {
        /// Creates a new MockScope.
        pub fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> MockScope<'g> {
            MockScope {
                callables: mem::ArrayMap::new(arena),
                types: mem::ArrayMap::new(arena),
                values: mem::ArrayMap::new(arena),
                parent: BuiltinScope::new(fragment),
            }
        }
    }

    impl<'g> Scope<'g> for MockScope<'g> {
        fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
            if let Some(&v) = self.values.get(&name) {
                return v;
            }

            self.parent.lookup_binding(name)
        }

        fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g> {
            if let Some(&v) = self.callables.get(&name) {
                return v;
            }

            self.parent.lookup_callable(name)
        }

        fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
            if let Some(&v) = self.types.get(&name) {
                return v;
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
    use model::sem_builder::Factory as SemFactory;

    use super::{Scope, BuiltinScope, FunctionScope};

    #[test]
    fn function_no_arguments() {
        let source = b":fun random() -> Int { a }";

        let arena = mem::Arena::new();
        let f = SemFactory::new(&arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        let prot = p.fun(i.id(5, 6), t.int()).range(0, 20).build();

        let builtin = BuiltinScope::new(source);
        let scope = FunctionScope::new(source, &builtin, &prot, &arena, &arena);

        assert_eq!(
            scope.lookup_binding(v.id(23, 1)),
            v.unresolved_ref(v.id(23, 1))
        );
    }

    #[test]
    fn function_with_arguments() {
        let source = b":fun add(a: Int, b: Int) -> Int { a + b + c }";

        let arena = mem::Arena::new();
        let f = SemFactory::new(&arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        let prot =
            p.fun(i.id(5, 3), t.int())
                .push(v.id(9, 1), t.int())
                .push(v.id(17, 1), t.int())
                .build();

        let builtin = BuiltinScope::new(source);
        let scope = FunctionScope::new(source, &builtin, &prot, &arena, &arena);

        assert_eq!(
            scope.lookup_binding(v.id(42, 1)),
            v.unresolved_ref(v.id(42, 1))
        );

        assert_eq!(
            scope.lookup_binding(v.id(34, 1)),
            v.arg_ref(t.int(), v.id(9, 1), 34)
        );

        assert_eq!(
            scope.lookup_binding(v.id(38, 1)),
            v.arg_ref(t.int(), v.id(17, 1), 38)
        );
    }
}
