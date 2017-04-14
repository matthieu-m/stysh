//! Lexical scopes for name resolution

use basic::mem;

use model::sem::{Binding, Expr, FunctionProto, Type, Value, ValueIdentifier};

/// A Lexical Scope trait.
pub trait Scope<'g> {
    /// Find the definition of a binding, if known.
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g>;

    /// Returns an unresolved reference.
    fn unresolved(&self, name: ValueIdentifier) -> Value<'g> {
        Value {
            type_: Type::Unresolved,
            range: name.0,
            expr: Expr::UnresolvedRef(name),
        }
    }
}

/// A Function Prototype Scope.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionScope<'a> {
    source: &'a [u8],
    arguments: &'a [(ValueIdentifier, Type)],
}

impl<'a> FunctionScope<'a> {
    /// Creates an instance of FunctionScope.
    pub fn new(
        source: &'a [u8],
        fun: &'a FunctionProto<'a>,
        arena: &'a mem::Arena,
    )
        -> FunctionScope<'a>
    {
        let mut array = mem::Array::with_capacity(fun.arguments.len(), arena);

        for &arg in fun.arguments {
            if let Binding::Argument(value, type_, _) = arg {
                array.push((value, type_));
                continue;
            }
            panic!("All bindings in a function argument must be arguments!");
        }

        FunctionScope {
            source: source,
            arguments: array.into_slice(),
        }
    }
}

/// A Block Scope.
pub struct BlockScope<'a, 'g, 'local>
    where 'g: 'a
{
    source: &'a [u8],
    parent: &'a Scope<'g>,
    elements: mem::Array<'local, (ValueIdentifier, Type)>,
}

impl<'a, 'g, 'local> BlockScope<'a, 'g, 'local> {
    /// Create a new instance of BlockScope.
    pub fn new(
        source: &'a [u8],
        parent: &'a Scope<'g>,
        arena: &'local mem::Arena
    )
        -> BlockScope<'a, 'g, 'local>
    {
        BlockScope {
            source: source,
            parent: parent,
            elements: mem::Array::new(arena),
        }
    }

    /// Adds a new identifier to the scope.
    pub fn add(&mut self, id: ValueIdentifier, type_: Type) {
        self.elements.push((id, type_))
    }
}

impl<'g> Scope<'g> for () {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        self.unresolved(name)
    }
}

impl<'a, 'g> Scope<'g> for FunctionScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        for &(identifier, type_) in self.arguments {
            if &self.source[identifier.0] == &self.source[name.0] {
                return Value {
                    type_: type_,
                    range: name.0,
                    expr: Expr::ArgumentRef(identifier),
                };
            }
        }

        self.unresolved(name)
    }
}

impl<'a, 'g, 'local> Scope<'g> for BlockScope<'a, 'g, 'local> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        for &(identifier, type_) in &*self.elements {
            if &self.source[identifier.0] == &self.source[name.0] {
                return Value {
                    type_: type_,
                    range: name.0,
                    expr: Expr::VarRef(identifier)
                }
            }
        }

        self.parent.lookup_binding(name)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::sem::*;

    use super::{Scope, FunctionScope};

    #[test]
    fn function_no_arguments() {
        let arena = mem::Arena::new();

        let source = b":fun random() -> Int { a }";

        let proto = FunctionProto {
            arguments: &[],
            result: Type::Builtin(BuiltinType::Int),
        };

        let scope = FunctionScope::new(source, &proto, &arena);

        assert_eq!(
            scope.lookup_binding(value(23, 1)),
            unresolved(value(23, 1))
        );
    }

    #[test]
    fn function_with_arguments() {
        let arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        let source = b":fun add(a: Int, b: Int) -> Int { a + b + c }";

        let proto = FunctionProto {
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

        let scope = FunctionScope::new(source, &proto, &arena);

        assert_eq!(
            scope.lookup_binding(value(42, 1)),
            unresolved(value(42, 1))
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

    fn resolved_argument(value: ValueIdentifier, range: com::Range, type_: Type)
        -> Value<'static>
    {
        Value {
            type_: type_,
            range: range,
            expr: Expr::ArgumentRef(value),
        }
    }

    fn unresolved(value: ValueIdentifier) -> Value<'static> {
        Value {
            type_: Type::Unresolved,
            range: value.0,
            expr: Expr::UnresolvedRef(value),
        }
    }

    fn value(start: usize, length: usize) -> ValueIdentifier {
        ValueIdentifier(range(start, length))
    }
}
