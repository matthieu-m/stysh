//! Lexical scopes for name resolution

use std::fmt;

use basic::com::Span;
use basic::mem;

use model::hir::*;

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
            range: name.span(),
            expr: Expr::UnresolvedRef(name),
            gvn: Default::default(),
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
pub struct BuiltinScope;

impl BuiltinScope {
    /// Creates an instance of BuiltinScope.
    pub fn new() -> BuiltinScope { BuiltinScope }
}

/// A Function Prototype Scope.
pub struct FunctionScope<'a, 'g>
    where 'g: 'a
{
    parent: &'a Scope<'g>,
    arguments: IdMap<'a, (ValueIdentifier, Type<'g>)>,
}

impl<'a, 'g> FunctionScope<'a, 'g> {
    /// Creates an instance of FunctionScope.
    pub fn new(
        parent: &'a Scope<'g>,
        fun: &'a FunctionProto<'a>,
        global_arena: &'g mem::Arena,
        local_arena: &'a mem::Arena,
    )
        -> FunctionScope<'a, 'g>
    {
        let mut arguments =
            IdMap::with_capacity(fun.arguments.len(), local_arena);

        for &arg in fun.arguments {
            if let Binding::Argument(value, _, type_, _) = arg {
                let type_ = global_arena.intern(&type_);
                arguments.insert(value.id(), (value, type_));
                continue;
            }
            panic!("All bindings in a function argument must be arguments!");
        }

        FunctionScope { parent, arguments }
    }
}

/// A Block Scope.
pub struct BlockScope<'a, 'g, 'local>
    where 'g: 'a + 'local
{
    parent: &'a Scope<'g>,
    registry: &'a Registry<'g>,
    functions: IdMap<'local, Callable<'g>>,
    types: IdMap<'local, Type<'g>>,
    values: IdMap<'local, (ValueIdentifier, Type<'g>)>,
    global_arena: &'g mem::Arena,
}

impl<'a, 'g, 'local> BlockScope<'a, 'g, 'local> {
    /// Create a new instance of BlockScope.
    pub fn new(
        parent: &'a Scope<'g>,
        registry: &'a Registry<'g>,
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
    )
        -> BlockScope<'a, 'g, 'local>
    {
        BlockScope {
            parent: parent,
            registry: registry,
            functions: IdMap::new(local_arena),
            types: IdMap::new(local_arena),
            values: IdMap::new(local_arena),
            global_arena: global_arena
        }
    }

    /// Adds a new enum to the scope.
    pub fn add_enum(&mut self, proto: EnumProto) {
        self.types.insert(proto.name.id(), Type::Enum(proto));
    }

    /// Adds a new function identifier to the scope.
    pub fn add_function(&mut self, proto: FunctionProto<'g>) {
        self.functions.insert(proto.name.id(), Callable::Function(proto));
    }

    /// Adds a new pattern to the scope.
    pub fn add_pattern(&mut self, pat: Pattern<'g>, type_: Type<'g>) {
        let tuple = match pat {
            Pattern::Ignored(_) => return,
            Pattern::Var(id, _) => {
                self.add_value(id, type_);
                return;
            },
            Pattern::Constructor(c) => c.arguments,
            Pattern::Tuple(tuple, _) => tuple,
        };
        for (index, pat) in tuple.fields.iter().enumerate() {
            let type_ = self.type_of_field(type_, index);
            self.add_pattern(*pat, type_)
        }
    }

    /// Adds a new value identifier to the scope.
    pub fn add_value(&mut self, name: ValueIdentifier, type_: Type<'g>) {
        self.values.insert(name.id(), (name, type_));
    }

    fn type_of_field(&self, type_: Type<'g>, index: usize) -> Type<'g> {
        let fields = match type_ {
            Type::Rec(r)
                => self.registry
                    .lookup_record(r.name)
                    .map(|r| r.definition.fields)
                    .unwrap_or(&[]),
            Type::Tuple(t) => t.fields,
            _ => &[],
        };
        fields.get(index).cloned().unwrap_or(Type::unresolved())
    }
}

//
//  Implementations of Scope
//
impl<'g> Scope<'g> for BuiltinScope {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        self.unresolved_binding(name)
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g> {
        self.unresolved_function(name)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
        use model::hir::BuiltinType::*;

        let builtin = match name.id() {
            id if id == mem::InternId::bool_() => Some(Bool),
            id if id == mem::InternId::int() => Some(Int),
            id if id == mem::InternId::string() => Some(String),
            _ => None,
        };

        builtin.map(|b| Type::Builtin(b)).unwrap_or(self.unresolved_type(name))
    }
}

impl<'a, 'g> Scope<'g> for FunctionScope<'a, 'g> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        if let Some(&(id, type_)) = self.arguments.get(&name.id()) {
            return Value {
                type_: type_,
                range: name.span(),
                expr: Expr::ArgumentRef(id, Default::default()),
                gvn: Default::default(),
            };
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
        if let Some(&(id, type_)) = self.values.get(&name.id()) {
            return Value {
                type_: self.global_arena.intern(&type_),
                range: name.span(),
                expr: Expr::VariableRef(id, Default::default()),
                gvn: Default::default(),
            };
        }

        self.parent.lookup_binding(name)
    }

    fn lookup_callable(&self, name: ValueIdentifier) -> Callable<'g> {
        use model::hir::Callable::*;

        let mut collection = mem::Array::new(self.functions.arena());

        match self.parent.lookup_callable(name) {
            Unknown(_) => (),
            Builtin(fun) => collection.push(Builtin(fun)),
            Function(fun) => collection.push(Function(fun)),
            Unresolved(slice) => collection.extend(slice),
        }

        if let Some(&callable) = self.functions.get(&name.id()) {
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
        if let Some(&type_) = self.types.get(&name.id()) {
            return type_;
        }

        self.parent.lookup_type(name)
    }
}

//
//  Implementation Details
//
type IdMap<'a, V> = mem::ArrayMap<'a, mem::InternId, V>;

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
    use basic::mem;
    use model::hir::*;

    use super::{BuiltinScope, Scope};

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
        pub parent: BuiltinScope,
    }

    impl<'g> MockScope<'g> {
        /// Creates a new MockScope.
        pub fn new(arena: &'g mem::Arena) -> MockScope<'g> {
            MockScope {
                callables: mem::ArrayMap::new(arena),
                types: mem::ArrayMap::new(arena),
                values: mem::ArrayMap::new(arena),
                parent: BuiltinScope::new(),
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
    use model::hir::builder::Factory;

    use super::{Scope, BuiltinScope, FunctionScope};

    #[test]
    fn function_no_arguments() {
        //  b":fun random() -> Int { a }";

        let interner = mem::Interner::new();
        let a = interner.insert(b"a");

        let arena = mem::Arena::new();
        let f = Factory::new(&arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        let prot = p.fun(i.id(5, 6), t.int()).range(0, 20).build();

        let builtin = BuiltinScope::new();
        let scope = FunctionScope::new(&builtin, &prot, &arena, &arena);

        assert_eq!(
            scope.lookup_binding(v.id(23, 1).with_id(a)),
            v.unresolved_ref(v.id(23, 1).with_id(a))
        );
    }

    #[test]
    fn function_with_arguments() {
        //  b":fun add(a: Int, b: Int) -> Int { a + b + c }";

        let interner = mem::Interner::new();
        let a = interner.insert(b"a");
        let b = interner.insert(b"b");
        let c = interner.insert(b"c");

        let arena = mem::Arena::new();
        let f = Factory::new(&arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        let prot =
            p.fun(i.id(5, 3), t.int())
                .push(v.id(9, 1).with_id(a), t.int())
                .push(v.id(17, 1).with_id(b), t.int())
                .build();

        let builtin = BuiltinScope::new();
        let scope = FunctionScope::new(&builtin, &prot, &arena, &arena);

        assert_eq!(
            scope.lookup_binding(v.id(42, 1).with_id(c)),
            v.unresolved_ref(v.id(42, 1).with_id(c))
        );

        assert_eq!(
            scope.lookup_binding(v.id(34, 1).with_id(a)),
            v.arg_ref(t.int(), v.id(9, 1).with_id(a), 34)
        );

        assert_eq!(
            scope.lookup_binding(v.id(38, 1).with_id(b)),
            v.arg_ref(t.int(), v.id(17, 1).with_id(b), 38)
        );
    }
}
