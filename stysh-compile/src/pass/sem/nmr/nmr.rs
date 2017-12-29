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
        //  TODO: need to handle multi-level nesting.
        assert_eq!(p.components.len(), 1);

        if let sem::Type::Enum(e) = self.scope.lookup_type(p.components[0].into()) {
            if let Some(e) = self.registry.lookup_enum(e.name) {
                for r in e.variants {
                    if self.source(r.prototype.name.0) == self.source(t.0) {
                        return sem::Type::Rec(*r.prototype);
                    }
                }
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
        if let sem::Type::Rec(record) = self.type_of(&c.type_) {
            let mut values = mem::Array::with_capacity(
                c.arguments.len(),
                self.global_arena
            );

            for e in c.arguments.fields {
                values.push(self.value_of(e));
            }

            return sem::Value {
                type_: sem::Type::Rec(record),
                range: c.range(),
                expr: sem::Expr::Constructor(record, values.into_slice()),
            };
        }

        unimplemented!("Unknown constructor call {:?}", c);
    }

    fn value_of_call(&mut self, fun: syn::FunctionCall) -> sem::Value<'g> {
        let callable = if let syn::Expression::Var(id) = *fun.function {
            self.scope.lookup_callable(id.into())
        } else {
            unimplemented!()
        };

        let mut values = mem::Array::with_capacity(
            fun.arguments.len(),
            self.global_arena
        );

        for e in fun.arguments.fields {
            values.push(self.value_of(e));
        }

        sem::Value {
            type_: callable.result_type(),
            range: fun.range(),
            expr: sem::Expr::Call(callable, values.into_slice()),
        }
    }

    fn value_of_if_else(&mut self, if_else: syn::IfElse) -> sem::Value<'g> {
        let condition = self.value_of_expr(if_else.condition);
        let mut true_branch = self.value_of_expr(if_else.true_expr);
        let mut false_branch = self.value_of_expr(if_else.false_expr);

        if true_branch.type_ != false_branch.type_ {
            let common = self.common_type(
                &[true_branch.type_, false_branch.type_]
            );

            if let Some(common) = common {
                true_branch = self.implicit_cast(true_branch, common);
                false_branch = self.implicit_cast(false_branch, common);
            }
        }

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

    fn common_type(&self, types: &[sem::Type<'g>]) -> Option<sem::Type<'g>> {
        let original = types[0];

        types[1..].iter().fold(Some(original), |acc, current| {
            self.common_type_impl(acc?, *current)
        })
    }

    fn common_type_impl(&self, left: sem::Type<'g>, right: sem::Type<'g>)
        -> Option<sem::Type<'g>>
    {
        use model::sem::*;

        fn extract_enum_record<'a>(left: Type<'a>, right: Type<'a>)
            -> Option<(EnumProto, RecordProto)>
        {
            if let Type::Enum(e) = left {
                if let Type::Rec(r) = right {
                    return Some((e, r));
                }
            }

            if let Type::Enum(e) = right {
                if let Type::Rec(r) = left {
                    return Some((e, r));
                }
            }

            None
        }

        if left == right {
            return Some(left);
        }

        if let Type::Rec(left) = left {
            if let Type::Rec(right) = right {
                if left.enum_ == right.enum_ {
                    if let Some(e) = self.registry.lookup_enum(left.enum_) {
                        return Some(Type::Enum(*e.prototype));
                    }
                }
            }
        }

        let (e, r) = extract_enum_record(left, right)?;

        if e.name == r.enum_ {
            Some(Type::Enum(e))
        } else {
            None
        }
    }

    fn implicit_cast(&self, original: sem::Value<'g>, target: sem::Type<'g>)
        -> sem::Value<'g>
    {
        use model::sem::Type::*;

        match target {
            Enum(e) => self.implicit_to_enum(original, e),
            _ => unimplemented!("Conversion to {:?}", target),
        }
    }

    fn implicit_to_enum(&self, original: sem::Value<'g>, e: sem::EnumProto)
        -> sem::Value<'g>
    {
        use model::sem::*;

        if let Type::Enum(o) = original.type_ {
            debug_assert!(o == e);
            return original;
        }

        if let Type::Rec(r) = original.type_ {
            debug_assert!(r.enum_ == e.name);

            return self.implicit_cast_impl(original, |v| {
                let v = self.global_arena.insert(v);
                Value {
                    type_: Type::Enum(e),
                    range: v.range,
                    expr: Expr::Implicit(Implicit::ToEnum(e, v)),
                }
            });
        }

        unreachable!("Should not cast anything but Enum or Rec: {:?}", original);
    }

    fn implicit_cast_impl<F>(&self, original: sem::Value<'g>, f: F)
        -> sem::Value<'g>
        where F: FnOnce(sem::Value<'g>) -> sem::Value<'g>
    {
        use model::sem::*;

        match original.expr {
            Expr::Block(stmts, v) => {
                let v = self.global_arena.insert(f(*v));
                Value {
                    type_: v.type_,
                    range: original.range,
                    expr: Expr::Block(stmts, v), 
                }
            },
            _ => f(original),
        }
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
                    arguments: Default::default()
                }),
            ),
            rec(basic_rec_prototype, &[], range(0, 3))
        );
    }

    #[test]
    fn value_basic_constructor_arguments() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(b"Rec(1)", &global_arena);

        let arg = range(4, 1);

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
                    type_: syn::Type::Simple(
                        syn::TypeIdentifier(registered.0)
                    ),
                    arguments: syn::Tuple {
                        fields: &[ lit_integral(arg) ],
                        commas: &[],
                        open: 3,
                        close: 5,
                    },
                }),
            ),
            rec(basic_rec_prototype, &[ int(1, arg) ], range(0, 6))
        );
    }

    #[test]
    fn value_basic_nested_constructor() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(
            b":enum Simple { Unit }         Simple::Unit",
            &global_arena
        );

        let enum_ = env.add_enum_unit(range(6, 6), range(0, 21), &[range(15, 4)]);
        let rec_prototype = enum_.variants[0].prototype;

        env.scope.types.insert(
            ItemIdentifier(range(30, 6)),
            Type::Enum(*enum_.prototype)
        );

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
                    arguments: Default::default(),
                }),
            ),
            rec(*rec_prototype, &[], range(30, 12))
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
                    arguments: syn::Tuple {
                        fields: &[ lit_integral(arg0), lit_integral(arg1), ],
                        commas: &[7, 9],
                        open: 5,
                        close: 10,
                    },
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

    #[test]
    fn value_implicit_cast_to_enum() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(
            b":enum B { T, F } :if true { B::T } else { B::F }", 
            &global_arena
        );

        let b = env.add_enum_unit(
            range(6, 1),
            range(0, 16),
            &[range(10, 1), range(13, 1)]
        );

        let t = b.variants[0].prototype;
        let f = b.variants[1].prototype;

        env.scope.types.insert(
            ItemIdentifier(range(28, 1)),
            Type::Enum(*b.prototype)
        );

        env.scope.types.insert(
            ItemIdentifier(range(42, 1)),
            Type::Enum(*b.prototype)
        );

        assert_eq!(
            env.value_of(
                &syn::Expression::If(syn::IfElse {
                    condition: &lit_boolean(true, range(21, 4)),
                    true_expr: &syn::Expression::Block(
                        &[],
                        &syn::Expression::Constructor(syn::Constructor {
                            type_: syn::Type::Nested(
                                syn::TypeIdentifier(range(31, 1)),
                                syn::Path {
                                    components: &[syn::TypeIdentifier(range(28, 1))],
                                    colons: &[29],
                                },
                            ),
                            arguments: Default::default(),
                        }),
                        range(26, 8)
                    ),
                    false_expr: &syn::Expression::Block(
                        &[],
                        &syn::Expression::Constructor(syn::Constructor {
                            type_: syn::Type::Nested(
                                syn::TypeIdentifier(range(45, 1)),
                                syn::Path {
                                    components: &[syn::TypeIdentifier(range(42, 1))],
                                    colons: &[43],
                                },
                            ),
                            arguments: Default::default(),
                        }),
                        range(40, 8)
                    ),
                    if_: 17,
                    else_: 35,
                })
            ),
            Value {
                type_: Type::Enum(*b.prototype),
                range: range(17, 31),
                expr: Expr::If(
                    &boolean(true, range(21, 4)),
                    &block(
                        &implicit_to_enum(*b.prototype, &rec(*t, &[], range(28, 4))),
                        range(26, 8)
                    ),
                    &block(
                        &implicit_to_enum(*b.prototype, &rec(*f, &[], range(42, 4))),
                        range(40, 8)
                    ),
                )
            }
        )
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

        fn add_enum_unit(
            &mut self,
            name: com::Range,
            range: com::Range,
            units: &[com::Range],
        )
            -> Enum<'g>
        {
            let enum_prototype = EnumProto {
                name: ItemIdentifier(name),
                range: range,
            };

            let mut records = mem::Array::with_capacity(units.len(), self.arena);

            for u in units {
                records.push(Record {
                    prototype: self.arena.insert(RecordProto {
                        name: ItemIdentifier(*u),
                        range: *u,
                        enum_: enum_prototype.name,
                    }),
                    fields: &[],
                });
            }

            let enum_ = Enum {
                prototype: self.arena.intern_ref(&enum_prototype),
                variants: records.into_slice(),
            };

            self.registry.enums.insert(enum_prototype.name, enum_);

            enum_
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

    fn implicit_to_enum<'a>(enum_: EnumProto, value: &'a Value<'a>) -> Value<'a> {
        Value {
            type_: Type::Enum(enum_),
            range: value.range,
            expr: Expr::Implicit(Implicit::ToEnum(enum_, value))
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

    fn rec<'a>(name: RecordProto, args: &'a [Value<'a>], range: com::Range)
        -> Value<'a>
    {
        Value {
            type_: Type::Rec(name),
            range: range,
            expr: Expr::Constructor(name, args),
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
