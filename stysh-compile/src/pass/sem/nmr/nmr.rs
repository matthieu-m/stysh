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
    registry: &'a sem::Registry<'g>,
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
        registry: &'a sem::Registry<'g>,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> NameResolver<'a, 'g, 'local>
    {
        NameResolver {
            code_fragment: source,
            scope: scope,
            registry: registry,
            global_arena: global,
            local_arena: local,
        }
    }

    /// Translates a type into... a type!
    pub fn type_of(&mut self, t: &syn::Type) -> sem::Type<'g> {
        use model::syn::Type;

        match *t {
            Type::Missing(_) => unimplemented!(),
            Type::Nested(t, p) => self.type_of_nested(t, p),
            Type::Simple(t) => self.type_of_simple(t),
            Type::Tuple(ref t) => self.type_of_tuple(t),
        }
    }

    /// Translates an expression into a value.
    pub fn value_of(&mut self, e: &syn::Expression) -> sem::Value<'g> {
        self.value_of_expr(e)
    }
}

//
//  Implementation Details
//
impl<'a, 'g, 'local> NameResolver<'a, 'g, 'local>
    where 'g: 'a
{
    fn type_of_nested(&self, t: syn::TypeIdentifier, p: syn::Path)
        -> sem::Type<'g>
    {
        //  TODO: need a type repository, and to handle nesting properly.
        assert_eq!(p.components.len(), 1);

        if let sem::Type::Rec(rec) = self.scope.lookup_type(t.into()) {
            if self.source(rec.enum_.0) == self.source(p.components[0].0) {
                return sem::Type::Rec(rec);
            }
        }
        sem::Type::unresolved()
    }

    fn type_of_simple(&self, t: syn::TypeIdentifier) -> sem::Type<'g> {
        self.scope.lookup_type(t.into())
    }

    fn type_of_tuple(&mut self, t: &syn::Tuple<syn::Type>) -> sem::Type<'g> {
        let mut fields =
            mem::Array::with_capacity(t.fields.len(), self.global_arena);

        for f in t.fields {
            fields.push(self.type_of(f));
        }

        sem::Type::Tuple(sem::Tuple { fields: fields.into_slice() })
    }

    fn value_of_expr(&mut self, expr: &syn::Expression) -> sem::Value<'g> {
        use model::syn::Expression::*;

        match *expr {
            BinOp(op, _, left, right)
                 => self.value_of_binary_operator(op, left, right),
            Block(s, e, r) => self.value_of_block(s, e, r),
            Constructor(c) => self.value_of_constructor(c),
            FunctionCall(fun) => self.value_of_call(fun),
            If(if_else) => self.value_of_if_else(if_else),
            Lit(lit, range) => self.value_of_literal(lit, range),
            PreOp(op, _, e)
                => self.value_of_prefix_operator(op, e, expr.range()),
            Tuple(t) => self.value_of_tuple(&t),
            Var(id) => self.value_of_variable(id),
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
            BlockScope::new(
                self.code_fragment,
                self.scope,
                self.global_arena,
                self.local_arena
            );
        let mut statements = mem::Array::new(self.local_arena);

        for &s in stmts {
            let (binding, type_, stmt) = match s {
                syn::Statement::Var(var) => {
                    let value = self.rescope(&scope).value_of_expr(&var.expr);
                    let id = var.name.into();
                    (
                        id,
                        value.type_,
                        sem::Stmt::Var(
                            sem::Binding::Variable(id, value, var.range())
                        )
                    )
                },
            };
            scope.add_value(binding, type_);
            statements.push(stmt);
        }

        let value = self.rescope(&scope).value_of_expr(expr);

        sem::Value {
            type_: value.type_,
            range: range,
            expr: sem::Expr::Block(
                self.global_arena.insert_slice(statements.into_slice()),
                self.global_arena.insert(value),
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
        use model::syn::BinaryOperator as O;
        use model::sem::BuiltinFunction as F;

        let range = left.range().extend(right.range());

        let left = self.value_of_expr(left);
        let right = self.value_of_expr(right);

        let op = match op {
            O::And => F::And,
            O::Different => F::Differ,
            O::Equal => F::Equal,
            O::FloorBy => F::FloorDivide,
            O::GreaterThan => F::GreaterThan,
            O::GreaterThanOrEqual => F::GreaterThanOrEqual,
            O::LessThan => F::LessThan,
            O::LessThanOrEqual => F::LessThanOrEqual,
            O::Minus => F::Substract,
            O::Or => F::Or,
            O::Plus => F::Add,
            O::Times => F::Multiply,
            O::Xor => F::Xor,
        };

        sem::Value {
            type_: op.result_type(),
            range: range,
            expr: sem::Expr::Call(
                sem::Callable::Builtin(op),
                self.global_arena.insert_slice(&[left, right])
            ),
        }
    }

    fn value_of_constructor(&mut self, c: syn::Constructor) -> sem::Value<'g> {
        if let sem::Type::Rec(rec) = self.type_of(&c.type_) {
            let callable = sem::Callable::ConstructorRec(rec);

            return sem::Value {
                type_: callable.result_type(),
                range: c.range(),
                expr: sem::Expr::Call(callable, &[]),
            };
        }

        println!("Unimplemented - {:?} -> {:?}", c, self.type_of(&c.type_));
        unimplemented!()
    }

    fn value_of_call(&mut self, fun: syn::FunctionCall) -> sem::Value<'g> {
        if let &syn::Expression::Var(id) = fun.function {
            let mut values = mem::Array::with_capacity(
                fun.arguments.len(),
                self.global_arena
            );

            for e in fun.arguments {
                values.push(self.value_of(e));
            }

            let callable = self.scope.lookup_callable(id.into());

            return sem::Value {
                type_: callable.result_type(),
                range: fun.range(),
                expr: sem::Expr::Call(callable, values.into_slice()),
            };
        }

        unimplemented!()
    }

    fn value_of_if_else(&mut self, if_else: syn::IfElse) -> sem::Value<'g> {
        let condition = self.value_of_expr(if_else.condition);
        let true_branch = self.value_of_expr(if_else.true_expr);
        let false_branch = self.value_of_expr(if_else.false_expr);

        sem::Value {
            type_: true_branch.type_,
            range: if_else.range(),
            expr: sem::Expr::If(
                self.global_arena.insert(condition),
                self.global_arena.insert(true_branch),
                self.global_arena.insert(false_branch),
            ),
        }
    }

    fn value_of_literal(&mut self, lit: syn::Literal, range: com::Range)
        -> sem::Value<'g>
    {
        match lit {
            syn::Literal::Bool(b) => self.value_of_literal_bool(b, range),
            syn::Literal::Bytes(b) => self.value_of_literal_bytes(b, range),
            syn::Literal::Integral => self.value_of_literal_integral(range),
            syn::Literal::String(s) => self.value_of_literal_string(s, range),
        }
    }

    fn value_of_literal_bool(
        &mut self,
        value: bool,
        range: com::Range
    )
        -> sem::Value<'g>
    {
        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Bool),
            range: range,
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Bool(value)),
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

    fn value_of_prefix_operator(&mut self, 
        op: syn::PrefixOperator,
        expr: &syn::Expression,
        range: com::Range,
    )
        -> sem::Value<'g>
    {
        use model::syn::PrefixOperator as O;
        use model::sem::BuiltinFunction as F;

        let expr = self.value_of_expr(expr);

        let op = match op {
            O::Not => F::Not,
        };

        sem::Value {
            type_: op.result_type(),
            range: range,
            expr: sem::Expr::Call(
                sem::Callable::Builtin(op),
                self.global_arena.insert_slice(&[expr])
            ),
        }
    }

    fn value_of_tuple(&mut self, tup: &syn::Tuple<syn::Expression>)
        -> sem::Value<'g>
    {
        let mut types =
            mem::Array::with_capacity(tup.fields.len(), self.global_arena);
        let mut values =
            mem::Array::with_capacity(tup.fields.len(), self.global_arena);

        for e in tup.fields {
            let v = self.value_of(e);
            types.push(v.type_);
            values.push(v);
        }

        sem::Value {
            type_: sem::Type::Tuple(sem::Tuple { fields: types.into_slice() }),
            range: tup.range(),
            expr: sem::Expr::Tuple(sem::Tuple { fields: values.into_slice() }),
        }
    }

    fn value_of_variable(&mut self, var: syn::VariableIdentifier)
        -> sem::Value<'g>
    {
        self.scope.lookup_binding(var.into())
    }

    fn catenate_fragments(&self, f: &[syn::StringFragment]) -> &'g [u8] {
        use model::syn::StringFragment::*;

        let mut buffer = mem::Array::new(self.local_arena);
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

        self.global_arena.insert_slice(buffer.into_slice())
    }

    fn rescope<'b>(&self, scope: &'b Scope<'g>) -> NameResolver<'b, 'g, 'local>
        where 'a: 'b
    {
        NameResolver {
            code_fragment: self.code_fragment,
            scope: scope,
            registry: self.registry,
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
    use model::sem::mocks::MockRegistry;
    use super::super::scp::mocks::MockScope;

    #[test]
    fn value_basic_add() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"1 + 2", &global_arena);

        let (left_range, right_range) = (range(0, 1), range(4, 1));

        let left_hand = lit_integral(left_range);
        let right_hand = lit_integral(right_range);

        let expr = syn::Expression::BinOp(
            syn::BinaryOperator::Plus,
            2,
            &left_hand,
            &right_hand,
        );

        assert_eq!(
            env.value_of(&expr),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(0, 5),
                expr: Expr::Call(
                    Callable::Builtin(BuiltinFunction::Add),
                    &[ int(1, left_range), int(2, right_range) ],
                )
            }
        );
    }

    #[test]
    fn value_basic_boolean() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"true", &global_arena);

        assert_eq!(
            env.value_of(
                &syn::Expression::Lit(syn::Literal::Bool(true), range(0, 4))
            ),
            boolean(true, range(0, 4))
        );
    }

    #[test]
    fn value_basic_constructor() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(b"Rec", &global_arena);

        let registered = ItemIdentifier(range(0, 3));
        let basic_rec_prototype = RecordProto {
            name: registered,
            range: range(45, 3),
            enum_: ItemIdentifier::unresolved()
        };

        env.scope.types.insert(registered, Type::Rec(basic_rec_prototype));

        assert_eq!(
            env.value_of(
                &syn::Expression::Constructor(syn::Constructor {
                    type_: syn::Type::Simple(syn::TypeIdentifier(registered.0)),
                }),
            ),
            Value {
                type_: Type::Rec(basic_rec_prototype),
                range: range(0, 3),
                expr: Expr::Call(
                    Callable::ConstructorRec(basic_rec_prototype),
                    &[],
                ),
            }
        );
    }

    #[test]
    fn value_basic_nested_constructor() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(b":enum Simple { Unit }         Simple::Unit", &global_arena);

        let registered = ItemIdentifier(range(38, 4));

        let basic_rec_prototype = RecordProto {
            name: ItemIdentifier(range(15, 4)),
            range: range(15, 4),
            enum_: ItemIdentifier(range(6, 6)),
        };

        env.scope.types.insert(registered, Type::Rec(basic_rec_prototype));

        assert_eq!(
            env.value_of(
                &syn::Expression::Constructor(syn::Constructor {
                    type_: syn::Type::Nested(
                        syn::TypeIdentifier(range(38, 4)),
                        syn::Path {
                            components: &[syn::TypeIdentifier(range(30, 6))],
                            colons: &[36],
                        },
                    ),
                }),
            ),
            Value {
                type_: Type::Rec(basic_rec_prototype),
                range: range(30, 12),
                expr: Expr::Call(
                    Callable::ConstructorRec(basic_rec_prototype),
                    &[],
                ),
            }
        );
    }

    #[test]
    fn value_basic_call() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(b"basic(1, 2)", &global_arena);

        let fragment_range = range(0, 5);
        let (arg0, arg1) = (range(6, 1), range(9, 1));

        let registered = ItemIdentifier(range(42, 5));
        let basic_fun_prototype = FunctionProto {
            name: registered,
            range: range(37, 20),
            arguments: &[],
            result: Type::Builtin(BuiltinType::Int),
        };

        env.scope.callables.insert(
            ValueIdentifier(fragment_range),
            Callable::Function(basic_fun_prototype),
        );

        assert_eq!(
            env.value_of(
                &syn::Expression::FunctionCall(syn::FunctionCall {
                    function: &var(fragment_range),
                    arguments: &[ lit_integral(arg0), lit_integral(arg1), ],
                    commas: &[7, 9],
                    open: 5,
                    close: 10,
                }),
            ),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(0, 11),
                expr: Expr::Call(
                    Callable::Function(basic_fun_prototype),
                    &[ int(1, arg0), int(2, arg1), ]
                )
            }
        )
    }

    #[test]
    fn value_basic_if_else() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":if true { 1 } :else { 0 }", &global_arena);

        let condition_range = range(0, 4);
        let true_branch_range = range(9, 5);
        let false_branch_range = range(21, 5);

        assert_eq!(
            env.value_of(
                &syn::Expression::If(syn::IfElse {
                    condition: &lit_boolean(true, condition_range),
                    true_expr: &syn::Expression::Block(
                        &[],
                        &lit_integral(range(11, 1)),
                        true_branch_range
                    ),
                    false_expr: &syn::Expression::Block(
                        &[],
                        &lit_integral(range(23, 1)),
                        false_branch_range
                    ),
                    if_: 0,
                    else_: 15,
                })
            ),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(0, 26),
                expr: Expr::If(
                    &boolean(true, condition_range),
                    &block(&int(1, range(11, 1)), true_branch_range),
                    &block(&int(0, range(23, 1)), false_branch_range),
                )
            }
        )
    }

    #[test]
    fn value_basic_helloworld() {
        use model::tt;

        let global_arena = mem::Arena::new();
        let env = Env::new(b"'Hello, World!'", &global_arena);

        assert_eq!(
            env.value_of(
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
        let env = Env::new(b"{ :var a := 1; :var b := 2; a + b }", &global_arena);

        let complete_range = range(0, 35);
        let (a, b) = (range(7, 1), range(20, 1));
        let (a_lit, b_lit) = (range(12, 1), range(25, 1));
        let (a_ref, b_ref) = (range(28, 1), range(32, 1));

        assert_eq!(
            env.value_of(
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
                        30,
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
                    &Value {
                        type_: Type::Builtin(BuiltinType::Int),
                        range: range(28, 5),
                        expr: Expr::Call(
                            Callable::Builtin(BuiltinFunction::Add),
                            &[ int_ref(a, a_ref), int_ref(b, b_ref) ]
                        ),
                    }
                )
            }
        );
    }

    struct Env<'g> {
        scope: MockScope<'g>,
        registry: MockRegistry<'g>,
        fragment: &'g [u8],
        arena: &'g mem::Arena,
    }

    impl<'g> Env<'g> {
        fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> Env<'g> {
            Env {
                scope: MockScope::new(fragment, arena),
                registry: MockRegistry::new(arena),
                fragment: fragment,
                arena: arena,
            }
        }

        fn resolver<'a, 'local>(&'a self, local: &'local mem::Arena)
            -> super::NameResolver<'a, 'g, 'local>
        {
            super::NameResolver::new(
                self.fragment,
                &self.scope,
                &self.registry, 
                self.arena, 
                local
            )
        }

        fn value_of(&self, expr: &syn::Expression) -> Value<'g> {
            let mut local_arena = mem::Arena::new();
            let result = self.resolver(&local_arena).value_of(expr);
            local_arena.recycle();
            result
        }
    }

    fn lit_boolean(value: bool, range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Lit(syn::Literal::Bool(value), range)
    }

    fn lit_integral(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Lit(syn::Literal::Integral, range)
    }

    fn var(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Var(syn::VariableIdentifier(range))
    }

    fn block<'a>(value: &'a Value<'a>, range: com::Range) -> Value<'a> {
        Value {
            type_: value.type_,
            range: range,
            expr: Expr::Block(&[], &value),
        }
    }

    fn boolean(value: bool, range: com::Range) -> Value<'static> {
        Value {
            type_: Type::Builtin(BuiltinType::Bool),
            range: range,
            expr: Expr::BuiltinVal(BuiltinValue::Bool(value)),
        }
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
