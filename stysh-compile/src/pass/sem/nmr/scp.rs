//! Lexical scopes for name resolution

use basic::{com, mem};

use model::sem::{
    Binding, Expr, FunctionProto, ItemIdentifier, Type, Value, ValueIdentifier
};

/// A Lexical Scope trait.
pub trait Scope<'g> {
    /// Find the definition of a binding, if known.
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g>;

    /// Find the definition of a function, if known.
    fn lookup_function(&self, name: ItemIdentifier) -> FunctionProto<'g>;

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
    fn unresolved_function(&self, name: ItemIdentifier) -> FunctionProto<'g> {
        FunctionProto {
            name: ItemIdentifier::unresolved(),
            range: com::Range::default(),
            arguments: &[],
            result: Type::Unresolved(name),
        }
    }

    /// Returns an unresolved reference.
    fn unresolved_type(&self, name: ItemIdentifier) -> Type<'g> {
        Type::Unresolved(name)
    }
}

/// A Builtin Scope.
pub struct BuiltinScope<'a> {
    source: &'a [u8],
}

impl<'a> BuiltinScope<'a> {
    /// Creates an instance of BuiltinScope.
    pub fn new(source: &'a [u8]) -> BuiltinScope<'a> {
        BuiltinScope { source: source }
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
    where 'g: 'a
{
    source: &'local [u8],
    parent: &'a Scope<'g>,
    functions: SourceMap<'local, FunctionProto<'local>>,
    values: SourceMap<'local, (ValueIdentifier, Type<'local>)>,
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
            values: SourceMap::new(local_arena),
            global_arena: global_arena
        }
    }

    /// Adds a new value identifier to the scope.
    pub fn add_value(&mut self, id: ValueIdentifier, type_: Type<'local>) {
        self.values.insert(&self.source[id.0], (id, type_));
    }
}

impl<'g> Scope<'g> for () {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        self.unresolved_binding(name)
    }

    fn lookup_function(&self, name: ItemIdentifier) -> FunctionProto<'g> {
        self.unresolved_function(name)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
        self.unresolved_type(name)
    }
}

impl<'a, 'g> Scope<'g> for BuiltinScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        self.unresolved_binding(name)
    }

    fn lookup_function(&self, name: ItemIdentifier) -> FunctionProto<'g> {
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

    fn lookup_function(&self, name: ItemIdentifier) -> FunctionProto<'g> {
        self.parent.lookup_function(name)
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

    fn lookup_function(&self, name: ItemIdentifier) -> FunctionProto<'g> {
        if let Some(proto) = self.functions.get(&&self.source[name.0]) {
            return self.global_arena.intern(proto);
        }

        self.parent.lookup_function(name)
    }

    fn lookup_type(&self, name: ItemIdentifier) -> Type<'g> {
        self.parent.lookup_type(name)
    }
}

//
//  Implementation Details
//
type SourceMap<'a, V> = mem::ArrayMap<'a, &'a [u8], V>;

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::sem::*;

    use super::{Scope, BuiltinScope, FunctionScope};

    #[test]
    fn function_no_arguments() {
        let arena = mem::Arena::new();

        let source = b":fun random() -> Int { a }";

        let prot = FunctionProto {
            name: ItemIdentifier(range(5, 6)),
            range: range(0, 20),
            arguments: &[],
            result: Type::Builtin(BuiltinType::Int),
        };

        let builtin = BuiltinScope::new(source);
        let scope = FunctionScope::new(source, &builtin, &prot, &arena, &arena);

        assert_eq!(
            scope.lookup_binding(value(23, 1)),
            unresolved_binding(value(23, 1))
        );
    }

    #[test]
    fn function_with_arguments() {
        let arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        let source = b":fun add(a: Int, b: Int) -> Int { a + b + c }";

        let prot = FunctionProto {
            name: ItemIdentifier(range(5, 3)),
            range: range(0, 55),
            arguments: &[
                Binding::Argument(
                    ValueIdentifier(range(9, 1)),
                    int,
                    range(9, 7),
                ),
                Binding::Argument(
                    ValueIdentifier(range(17, 1)),
                    int,
                    range(17, 6),
                ),
            ],
            result: Type::Builtin(BuiltinType::Int),
        };

        let builtin = BuiltinScope::new(source);
        let scope = FunctionScope::new(source, &builtin, &prot, &arena, &arena);

        assert_eq!(
            scope.lookup_binding(value(42, 1)),
            unresolved_binding(value(42, 1))
        );

        assert_eq!(
            scope.lookup_binding(value(34, 1)),
            resolved_argument(value(9, 1), range(34, 1), int)
        );

        assert_eq!(
            scope.lookup_binding(value(38, 1)),
            resolved_argument(value(17, 1), range(38, 1), int)
        );
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }

    fn resolved_argument<'a>(
        value: ValueIdentifier,
        range: com::Range,
        type_: Type<'a>
    )
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: range,
            expr: Expr::ArgumentRef(value),
        }
    }

    fn unresolved_binding(value: ValueIdentifier) -> Value<'static> {
        Value {
            type_: Type::Unresolved(ItemIdentifier::unresolved()),
            range: value.0,
            expr: Expr::UnresolvedRef(value),
        }
    }

    fn value(start: usize, length: usize) -> ValueIdentifier {
        ValueIdentifier(range(start, length))
    }
}
