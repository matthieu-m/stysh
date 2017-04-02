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

impl<'g> Scope<'g> for () {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        self.unresolved(name)
    }
}

impl<'a, 'g> Scope<'g> for FunctionScope<'a> {
    fn lookup_binding(&self, name: ValueIdentifier) -> Value<'g> {
        use basic::com::Slice;

        println!("{} {:?}", Slice(self.source), self);

        for &(identifier, type_) in self.arguments {
            println!(
                "'{}' ({}) == '{}' ({}) ?",
                Slice(&self.source[identifier.0]),
                identifier.0,
                Slice(&self.source[name.0]),
                name.0
            );
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
