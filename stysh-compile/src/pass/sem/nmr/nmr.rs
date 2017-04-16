//! Semantic pass: name resolution.
//!
//! High-level item in charge of name resolution.

use basic::{com, mem};

use model::{syn, sem};

use super::scp::{BlockScope, Scope};

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
            Expression::Block(s, e, r) => self.value_of_block(s, e, r),
            Expression::Lit(lit, range) => self.value_of_literal(lit, range),
            Expression::Tuple(_) => unimplemented!(),
            Expression::Var(id) => self.value_of_variable(id),
        }
    }

    fn value_of_block(
        &mut self,
        stmts: &[syn::Statement],
        expr: &syn::Expression,
        range: com::Range
    )
        -> sem::Value<'g>
    {
        let mut scope =
            BlockScope::new(self.code_fragment, self.scope, self.local_arena);
        let mut statements = mem::Array::new(self.local_arena);

        for &s in stmts {
            let (binding, type_, stmt) = match s {
                syn::Statement::Var(var) => {
                    let value = self.rescope(&scope).value_of_expr(&var.expr);
                    let id = sem::ValueIdentifier(var.name.0);
                    (
                        id,
                        value.type_,
                        sem::Stmt::Var(
                            sem::Binding::Variable(id, value, var.range())
                        )
                    )
                },
            };
            scope.add(binding, type_);
            statements.push(stmt);
        }

        let value = self.rescope(&scope).value_of_expr(expr);

        sem::Value {
            type_: value.type_,
            range: range,
            expr: sem::Expr::Block(
                self.global_arena.insert_slice(statements.into_slice()),
                self.global_arena.insert(value.expr),
            ),
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
            syn::Literal::Bytes(b) => self.value_of_literal_bytes(b, range),
            syn::Literal::Integral => self.value_of_literal_integral(range),
            syn::Literal::String(s) => self.value_of_literal_string(s, range),
        }
    }

    fn value_of_literal_bytes(
        &mut self,
        bytes: &[syn::StringFragment],
        range: com::Range
    )
        -> sem::Value<'g>
    {
        let value = self.catenate_fragments(bytes);

        //  TODO(matthieum): Fix type, should be Array[[Byte]].
        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::String),
            range: range,
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::String(value)),
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

    fn value_of_literal_string(
        &mut self,
        string: &[syn::StringFragment],
        range: com::Range
    )
        -> sem::Value<'g>
    {
        let value = self.catenate_fragments(string);

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::String),
            range: range,
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::String(value)),
        }
    }

    fn value_of_variable(&mut self, var: syn::VariableIdentifier)
        -> sem::Value<'g>
    {
        self.scope.lookup_binding(sem::ValueIdentifier(var.0))
    }

    fn catenate_fragments(&self, f: &[syn::StringFragment]) -> &'g [u8] {
        use model::syn::StringFragment::*;

        let mut buffer = mem::Array::new(self.global_arena);
        for &fragment in f {
            match fragment {
                Text(tok) => buffer.extend(self.source(tok.range())),
                SpecialCharacter(tok) => match self.source(tok.range()) {
                    b"N" => buffer.push(b'\n'),
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        }

        buffer.into_slice()
    }

    fn rescope<'b>(&self, scope: &'b Scope<'g>) -> NameResolver<'b, 'g, 'local>
        where 'a: 'b
    {
        NameResolver {
            code_fragment: self.code_fragment,
            scope: scope,
            global_arena: self.global_arena,
            local_arena: self.local_arena,
        }
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

        let (left_range, right_range) = (range(0, 1), range(4, 1));

        let left_hand = lit_integral(left_range);
        let right_hand = lit_integral(right_range);

        let expr = syn::Expression::BinOp(
            syn::BinaryOperator::Plus,
            &left_hand,
            &right_hand,
        );

        assert_eq!(
            valueit(&global_arena, b"1 + 2", &expr),
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

    #[test]
    fn value_basic_helloworld() {
        use model::tt;

        let global_arena = mem::Arena::new();

        assert_eq!(
            valueit(
                &global_arena,
                b"'Hello, World!'",
                &syn::Expression::Lit(
                    syn::Literal::String(&[
                        tt::StringFragment::Text(
                            tt::Token::new(tt::Kind::StringText, 1, 13)
                        ),
                    ]),
                    range(0, 15)
                )
            ),
            Value {
                type_: Type::Builtin(BuiltinType::String),
                range: range(0, 15),
                expr: Expr::BuiltinVal(
                    BuiltinValue::String(b"Hello, World!")
                )
            }
        );
    }

    #[test]
    fn value_basic_var() {
        let global_arena = mem::Arena::new();

        let complete_range = range(0, 35);
        let (a, b) = (range(7, 1), range(20, 1));
        let (a_lit, b_lit) = (range(12, 1), range(25, 1));
        let (a_ref, b_ref) = (range(28, 1), range(32, 1));

        assert_eq!(
            valueit(
                &global_arena,
                b"{ :var a := 1; :var b := 2; a + b }",
                &syn::Expression::Block(
                    &[
                        syn::Statement::Var(syn::VariableBinding {
                            name: syn::VariableIdentifier(a),
                            type_: None,
                            expr: lit_integral(a_lit),
                            var: 2,
                            colon: 0,
                            bind: 9,
                            semi: 13,
                        }),
                        syn::Statement::Var(syn::VariableBinding {
                            name: syn::VariableIdentifier(b),
                            type_: None,
                            expr: lit_integral(b_lit),
                            var: 15,
                            colon: 0,
                            bind: 22,
                            semi: 26,
                        }),
                    ],
                    &syn::Expression::BinOp(
                        syn::BinaryOperator::Plus,
                        &var(a_ref),
                        &var(b_ref),
                    ),
                    complete_range
                )
            ),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: complete_range,
                expr: Expr::Block(
                    &[
                        Stmt::Var(Binding::Variable(
                            ValueIdentifier(a),
                            int(1, a_lit),
                            range(2, 12)
                        )),
                        Stmt::Var(Binding::Variable(
                            ValueIdentifier(b),
                            int(2, b_lit),
                            range(15, 12)
                        )),
                    ],
                    &Expr::BuiltinCall(
                        BuiltinFunction::Add,
                        &[ int_ref(a, a_ref), int_ref(b, b_ref) ]
                    )
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

    fn var(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Var(syn::VariableIdentifier(range))
    }

    fn int(value: i64, range: com::Range) -> Value<'static> {
        Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: range,
            expr: Expr::BuiltinVal(BuiltinValue::Int(value)),
        }
    }

    fn int_ref(name: com::Range, range: com::Range) -> Value<'static> {
        Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: range,
            expr: Expr::VariableRef(ValueIdentifier(name)),
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
