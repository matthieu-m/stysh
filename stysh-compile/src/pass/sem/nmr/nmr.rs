//! Semantic pass: name resolution.
//!
//! High-level item in charge of name resolution.

use basic::{com, mem};

use model::{syn, sem};

use super::scp::Scope;

/// The Name Resolver.
///
/// Resolves which definition(s) each name refers to.
pub struct NameResolver<'a, 'g, 'local>
    where 'g: 'a
{
    code_fragment: &'a [u8],
    scope: &'a Scope<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> NameResolver<'a, 'g, 'local>
    where 'g: 'a
{
    /// Creates a new instance of the name resolver.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(
        source: &'a [u8],
        scope: &'a Scope<'g>,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> NameResolver<'a, 'g, 'local>
    {
        NameResolver {
            code_fragment: source,
            scope: scope,
            global_arena: global,
            local_arena: local,
        }
    }

    /// Translates an expression into a value.
    pub fn value(&mut self, e: &syn::Expression) -> sem::Value<'g> {
        self.value_of_expr(e)
    }
}

//
//  Implementation Details
//
impl<'a, 'g, 'local> NameResolver<'a, 'g, 'local>
    where 'g: 'a
{
    fn value_of_expr(&mut self, expr: &syn::Expression) -> sem::Value<'g> {
        use model::syn::Expression;

        match *expr {
            Expression::BinOp(op, left, right) =>
                self.value_of_binary_operator(op, left, right),
            Expression::Block(e, _) => self.value_of_expr(e),
            Expression::Lit(lit, range) => self.value_of_literal(lit, range),
            Expression::Var(id) => self.value_of_variable(id),
        }
    }

    fn value_of_binary_operator(
        &mut self,
        op: syn::BinaryOperator,
        left: &syn::Expression,
        right: &syn::Expression
    )
        -> sem::Value<'g>
    {
        let range = left.range().extend(right.range());

        let left = self.value_of_expr(left);
        let right = self.value_of_expr(right);

        let op = match op {
            syn::BinaryOperator::Plus => sem::BuiltinFunction::Add,
        };

        let mut buffer = mem::Array::with_capacity(2, self.local_arena);
        buffer.push(left);
        buffer.push(right);

        let arguments = self.global_arena.insert_slice(buffer.into_slice());

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range,
            expr: sem::Expr::BuiltinCall(op, arguments),
        }
    }

    fn value_of_literal(&mut self, lit: syn::Literal, range: com::Range)
        -> sem::Value<'g>
    {
        match lit {
            syn::Literal::Integral => self.value_of_literal_integral(range),
        }
    }

    fn value_of_literal_integral(&mut self, range: com::Range)
        -> sem::Value<'g>
    {
        let mut value = 0;
        for byte in self.source(range) {
            match *byte {
                b'0'...b'9' => value += (byte - b'0') as i64,
                b'_' => (),
                _ => unimplemented!(),
            }
        }

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range,
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(value)),
        }
    }

    fn value_of_variable(&mut self, var: syn::VariableIdentifier)
        -> sem::Value<'g>
    {
        self.scope.lookup_binding(sem::ValueIdentifier(var.0))
    }

    fn source(&self, range: com::Range) -> &[u8] {
        &self.code_fragment[range]
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::syn;
    use model::sem::*;

    #[test]
    fn value_basic_add() {
        let global_arena = mem::Arena::new();

        let fragment = b"1 + 2";

        let (left_range, right_range) = (range(0, 1), range(4, 1));

        let left_hand = lit_integral(left_range);
        let right_hand = lit_integral(right_range);

        let expr = syn::Expression::BinOp(
            syn::BinaryOperator::Plus,
            &left_hand,
            &right_hand,
        );

        assert_eq!(
            valueit(&global_arena, fragment, &expr),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(0, 5),
                expr: Expr::BuiltinCall(
                    BuiltinFunction::Add,
                    &[ int(1, left_range), int(2, right_range) ],
                )
            }
        );
    }

    fn valueit<'g>(
        global_arena: &'g mem::Arena,
        fragment: &'g [u8],
        expr: &syn::Expression
    )
        -> Value<'g>
    {
        use super::NameResolver;

        let mut local_arena = mem::Arena::new();

        let scope = ();

        let result =
            NameResolver::new(fragment, &scope, global_arena, &local_arena)
                .value(expr);
        local_arena.recycle();

        result
    }

    fn lit_integral(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Lit(syn::Literal::Integral, range)
    }

    fn int(value: i64, range: com::Range) -> Value<'static> {
        Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: range,
            expr: Expr::BuiltinVal(BuiltinValue::Int(value)),
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
