//! Semantic pass: name resolution.
//!
//! High-level item in charge of name resolution.

use basic::{com, mem};

use model::syn::{self, Range};
use model::sem;

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

    /// Translates a pattern into... a pattern!
    pub fn pattern_of(&mut self, p: &syn::Pattern) -> sem::Pattern<'g> {
        use model::syn::Pattern;

        match *p {
            Pattern::Constructor(c) => self.pattern_of_constructor(c),
            Pattern::Ignored(v) => self.pattern_of_ignored(v),
            Pattern::Tuple(t) => self.pattern_of_tuple(&t),
            Pattern::Var(v) => self.pattern_of_var(v),
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
    fn pattern_of_constructor(
        &mut self,
        c: syn::Constructor<syn::Pattern>,
    )
        -> sem::Pattern<'g>
    {
        let rec = if let sem::Type::Rec(p) = self.type_of(&c.type_) {
            p
        } else {
            unimplemented!("Unknown type - {:?}", c.type_)
        };

        let mut arguments =
            mem::Array::with_capacity(c.arguments.len(), self.global_arena);

        for f in c.arguments.fields {
            arguments.push(self.pattern_of(f));
        }

        sem::Pattern::Constructor(sem::Constructor {
            type_: rec,
            arguments: arguments.into_slice(),
            range: c.range(),
        })
    }

    fn pattern_of_ignored(&mut self, underscore: syn::VariableIdentifier)
        -> sem::Pattern<'static>
    {
        sem::Pattern::Ignored(underscore.range())
    }

    fn pattern_of_tuple(&mut self, t: &syn::Tuple<syn::Pattern>)
        -> sem::Pattern<'g>
    {
        let mut fields =
            mem::Array::with_capacity(t.fields.len(), self.global_arena);

        for f in t.fields {
            fields.push(self.pattern_of(f));
        }

        sem::Pattern::Tuple(
            sem::Tuple { fields: fields.into_slice() },
            t.range(),
        )
    }

    fn pattern_of_var(&self, var: syn::VariableIdentifier)
        -> sem::Pattern<'static>
    {
        sem::Pattern::Var(var.into())
    }

    fn type_of_field_index(&self, value: &sem::Value<'g>, index: u16)
        -> sem::Type<'g>
    {
        let index = index as usize;

        let type_ = match value.type_ {
            sem::Type::Rec(rec) =>
                self.registry
                    .lookup_record(rec.name)
                    .and_then(|r| r.fields.get(index)),
            sem::Type::Tuple(tup) => tup.fields.get(index),
            _ => None,
        };

        type_.cloned().unwrap_or(sem::Type::unresolved())
    }

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
            Block(b) => self.value_of_block(b),
            Constructor(c) => self.value_of_constructor(c),
            FieldAccess(f) => self.value_of_field(f),
            FunctionCall(fun) => self.value_of_call(fun),
            If(if_else) => self.value_of_if_else(if_else),
            Lit(lit) => self.value_of_literal(lit),
            Loop(loop_) => self.value_of_loop(loop_),
            PreOp(op, _, e)
                => self.value_of_prefix_operator(op, e, expr.range()),
            Tuple(t) => self.value_of_tuple(&t),
            Var(id) => self.value_of_variable(id),
        }
    }

    fn value_of_block(&mut self, block: &syn::Block) -> sem::Value<'g> {
        let mut scope =
            BlockScope::new(
                self.code_fragment,
                self.scope,
                self.registry,
                self.global_arena,
                self.local_arena
            );
        let mut statements = mem::Array::new(self.local_arena);

        for &s in block.statements {
            let stmt = match s {
                syn::Statement::Set(set) => {
                    let left = self.rescope(&scope).value_of_expr(&set.left);
                    let right = self.rescope(&scope).value_of_expr(&set.expr);
                    sem::Stmt::Set(
                        sem::ReBinding {
                            left: left,
                            right: right,
                            range: set.range()
                        }
                    )
                },
                syn::Statement::Var(var) => {
                    let value = self.rescope(&scope).value_of_expr(&var.expr);
                    let pattern = self.pattern_of(&var.pattern);
                    scope.add_pattern(pattern, value.type_);
                    sem::Stmt::Var(
                        sem::Binding::Variable(pattern, value, var.range())
                    )
                },
            };
            statements.push(stmt);
        }

        let value = self.rescope(&scope).value_of_expr(&block.expression);

        sem::Value {
            type_: value.type_,
            range: block.range(),
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

    fn value_of_constructor(&mut self, c: syn::Constructor<syn::Expression>)
        -> sem::Value<'g>
    {
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
                expr: sem::Expr::Constructor(sem::Constructor {
                    type_: record,
                    arguments: values.into_slice(),
                    range: c.range(),
                }),
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

    fn value_of_field(&mut self, field: syn::FieldAccess) -> sem::Value<'g> {
        let accessed = self.global_arena.insert(self.value_of(field.accessed));

        if let Some(index) = self.parse_integral(field.field.0, 1, false) {
            let index = index as _;

            sem::Value {
                type_: self.type_of_field_index(accessed, index),
                range: field.range(),
                expr: sem::Expr::FieldAccess(accessed, index),
            }
        } else {
            let id = sem::ValueIdentifier(field.field.0);

            sem::Value {
                type_: sem::Type::unresolved(),
                range: field.range(),
                expr: sem::Expr::UnresolvedField(accessed, id)
            }
        }
    }

    fn value_of_if_else(&mut self, if_else: &syn::IfElse) -> sem::Value<'g> {
        let condition = self.value_of_expr(&if_else.condition);
        let mut true_branch = self.value_of_block(&if_else.true_expr);
        let mut false_branch = self.value_of_block(&if_else.false_expr);

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

    fn value_of_literal(&mut self, lit: syn::Literal)
        -> sem::Value<'g>
    {
        match lit {
            syn::Literal::Bool(b, r) => self.value_of_literal_bool(b, r),
            syn::Literal::Bytes(b, r) => self.value_of_literal_bytes(b, r),
            syn::Literal::Integral(r) => self.value_of_literal_integral(r),
            syn::Literal::String(s, r) => self.value_of_literal_string(s, r),
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
        let value = self.parse_integral(range, 0, true).expect("TODO: handle");

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

    fn value_of_loop(&mut self, loop_: &syn::Loop) -> sem::Value<'g> {
        let block = self.value_of_block(&loop_.block);

        if let sem::Expr::Block(stmts, expr) = block.expr {
            return sem::Value {
                type_: sem::Type::Builtin(sem::BuiltinType::Void),
                range: loop_.range(),
                expr: sem::Expr::Loop(stmts, expr),
            };
        }

        unreachable!("value_of_block returns a Expr::Block!");
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

    fn parse_integral(
        &self,
        range: com::Range,
        offset: usize,
        underscores: bool
    )
        -> Option<i64> 
    {
        let mut value = 0;
        for byte in &self.source(range)[offset..] {
            match *byte {
                b'0'...b'9' => {
                    value *= 10;
                    value += (byte - b'0') as i64;
                },
                b'_' if underscores => (),
                _ => return None,
            }
        }

        Some(value)
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
    use model::{syn, sem};
    use model::syn_builder::Factory as SynFactory;
    use model::sem_builder::Factory as SemFactory;
    use super::super::scp::mocks::MockScope;

    #[test]
    fn value_basic_add() {
        let global_arena = mem::Arena::new();
        let e = SynFactory::new(&global_arena).expr();
        let v = SemFactory::new(&global_arena).value();
        let env = Env::new(b"1 + 2", &global_arena);

        assert_eq!(
            env.value_of(&e.bin_op(e.int(0, 1), e.int(4, 1)).build()),
            v.call().push(v.int(1, 0)).push(v.int(2, 4)).build()
        );
    }

    #[test]
    fn value_basic_boolean() {
        let global_arena = mem::Arena::new();
        let e = SynFactory::new(&global_arena).expr();
        let v = SemFactory::new(&global_arena).value();
        let env = Env::new(b"true", &global_arena);

        assert_eq!(
            env.value_of(&e.bool_(0, 4)),
            v.bool_(true, 0)
        );
    }

    #[test]
    fn value_basic_constructor() {
        let global_arena = mem::Arena::new();
        let syn = SynFactory::new(&global_arena);
        let sem = SemFactory::new(&global_arena);
        let (i, p, v) = (sem.item(), sem.proto(), sem.value());

        let mut env = Env::new(b"Rec", &global_arena);

        let rec = p.rec(i.id(45, 3), 0).build();
        env.scope.types.insert(i.id(0, 3), sem::Type::Rec(rec));

        assert_eq!(
            env.value_of(
                &syn.expr().constructor(syn.type_().simple(0, 3)).build()
            ),
            v.constructor(rec, 0, 3).build_value()
        );
    }

    #[test]
    fn value_basic_constructor_arguments() {
        let global_arena = mem::Arena::new();
        let syn = SynFactory::new(&global_arena);
        let e = syn.expr();

        let sem = SemFactory::new(&global_arena);
        let (i, p, v) = (sem.item(), sem.proto(), sem.value());

        let mut env = Env::new(b"Rec(1)", &global_arena);

        let rec = p.rec(i.id(45, 3), 0).build();
        env.scope.types.insert(i.id(0, 3), sem::Type::Rec(rec));

        assert_eq!(
            env.value_of(
                &e.constructor(syn.type_().simple(0, 3))
                    .parens(3, 5)
                    .push(e.int(4, 1))
                    .build()
            ),
            v.constructor(rec, 0, 6).push(v.int(1, 4)).build_value()
        );
    }

    #[test]
    fn value_basic_record_field_access() {
        let global_arena = mem::Arena::new();
        let syn = SynFactory::new(&global_arena);
        let e = syn.expr();

        let sem = SemFactory::new(&global_arena);
        let (i, p, t, v) = (sem.item(), sem.proto(), sem.type_(), sem.value());

        let mut env = Env::new(b":rec Rec(Int); Rec(42).0", &global_arena);

        let rec =
            i.rec(p.rec(i.id(5, 3), 0).range(0, 14).build())
                .push(t.int())
                .build();

        env.insert_record(rec, &[15]);

        assert_eq!(
            env.value_of(
                &e.field_access(
                    e.constructor(syn.type_().simple(15, 3))
                        .parens(18, 21)
                        .push(e.int(19, 2))
                        .build(),
                ).build()
            ),
            v.field_access(
                0,
                v.constructor(*rec.prototype, 15, 7)
                    .push(v.int(42, 19))
                    .build_value()
            ).build()
        );
    }

    #[test]
    fn value_basic_tuple_field_access() {
        let global_arena = mem::Arena::new();
        let e = SynFactory::new(&global_arena).expr();

        let v = SemFactory::new(&global_arena).value();

        let env = Env::new(b"(42, 43).1", &global_arena);

        assert_eq!(
            env.value_of(
                &e.field_access(
                    e.tuple().push(e.int(1, 2)).push(e.int(5, 2)).build(),
                ).build()
            ),
            v.field_access(
                1,
                v.tuple().push(v.int(42, 1)).push(v.int(43, 5)).build()
            ).build()
        )
    }

    #[test]
    fn value_basic_nested_constructor() {
        let global_arena = mem::Arena::new();
        let syn = SynFactory::new(&global_arena);
        let e = syn.expr();

        let sem = SemFactory::new(&global_arena);
        let (i, p, v) = (sem.item(), sem.proto(), sem.value());

        let mut env = Env::new(
            b":enum Simple { Unit }         Simple::Unit",
            &global_arena
        );

        let enum_ =
            i.enum_(p.enum_(i.id(6, 6)).build())
                .push(i.unit(15, 4))
                .build();
        let rec = enum_.variants[0];

        env.insert_enum(enum_, &[30]);

        assert_eq!(
            env.value_of(
                &e.constructor(
                    syn.type_().nested(38, 4).push(30, 6).build()
                ).build()
            ),
            v.constructor(*rec.prototype, 30, 12).build_value()
        );
    }

    #[test]
    fn value_basic_call() {
        let global_arena = mem::Arena::new();
        let e = SynFactory::new(&global_arena).expr();

        let sem = SemFactory::new(&global_arena);
        let (i, p, t, v) = (sem.item(), sem.proto(), sem.type_(), sem.value());

        let mut env = Env::new(b"basic(1, 2)", &global_arena);

        let proto = p.fun(i.id(42, 5), t.int()).build();
        env.insert_function_prototype(proto, &[0]);

        assert_eq!(
            env.value_of(
                &e.function_call(e.var(0, 5), 5, 10)
                    .push(e.int(6, 1))
                    .push(e.int(9, 1))
                    .build()
            ),
            v.call()
                .function(proto)
                .push(v.int(1, 6))
                .push(v.int(2, 9))
                .build()
        )
    }

    #[test]
    fn value_basic_if_else() {
        let global_arena = mem::Arena::new();
        let e = SynFactory::new(&global_arena).expr();

        let v = SemFactory::new(&global_arena).value();

        let env = Env::new(b":if true { 1 } :else { 0 }", &global_arena);

        assert_eq!(
            env.value_of(&syn::Expression::If(
                &e.if_else(
                    e.bool_(4, 4),
                    e.block(e.int(11, 1)).build(),
                    e.block(e.int(23, 1)).build(),
                ).build()
            )),
            v.if_(
                v.bool_(true, 4),
                v.block(v.int(1, 11)).build(),
                v.block(v.int(0, 23)).build(),
            ).build()
        )
    }

    #[test]
    fn value_basic_helloworld() {
        let global_arena = mem::Arena::new();
        let e = SynFactory::new(&global_arena).expr();

        let v = SemFactory::new(&global_arena).value();

        let env = Env::new(b"'Hello, World!'", &global_arena);

        assert_eq!(
            env.value_of(&e.literal(0, 15).push_text(1, 13).string().build()),
            v.string("Hello, World!", 0)
        );
    }

    #[test]
    fn value_loop() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":loop { 1 }", &global_arena);

        let syn = {
            let e = SynFactory::new(&global_arena).expr();
            e.loop_(e.block(e.int(8, 1)).build())
        };

        let sem = {
            let v = SemFactory::new(&global_arena).value();
            v.loop_(v.int(1, 8)).build()
        };

        assert_eq!(env.value_of(&syn), sem);
    }

    #[test]
    fn value_set_basic() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var a := 1; :set a := 2; a }", &global_arena);

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(28, 1))
                    .push_stmt(s.var(p.var(7, 1), e.int(12, 1)).build())
                    .push_stmt(s.set(e.var(20, 1), e.int(25, 1)).build())
                    .build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = v.id(7, 1);

            v.block(v.int_ref(a, 28))
                .push(s.var(p.var(a), v.int(1, 12)))
                .push(s.set(v.int_ref(a, 20), v.int(2, 25)))
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::Block(&syn)), sem);
    }

    #[test]
    fn value_set_field() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b"{ :var a := (1,); :set a.0 := 2; a }",
            &global_arena
        );

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(33, 1))
                .push_stmt(s.var(
                    p.var(7, 1),
                    e.tuple().push(e.int(13, 1)).comma(14).build()
                ).build())
                .push_stmt(s.set(
                    e.field_access(e.var(23, 1)).build(),
                    e.int(30, 1)
                ).build())
                .build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = v.id(7, 1);
            let tup = v.tuple().push(v.int(1, 13)).build().with_range(12, 4);

            v.block(v.var_ref(tup.type_, a, 33))
                .push(s.var(p.var(a), tup))
                .push(s.set(
                    v.field_access(0, v.var_ref(tup.type_, a, 23)).build(),
                    v.int(2, 30)
                ))
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::Block(&syn)), sem);
    }

    #[test]
    fn value_var_basic() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var a := 1; :var b := 2; a + b }", &global_arena);

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.bin_op(e.var(28, 1), e.var(32, 1)).build())
                .push_stmt(s.var(p.var(7, 1), e.int(12, 1)).build())
                .push_stmt(s.var(p.var(20, 1), e.int(25, 1)).build())
                .build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let (a, b) = (v.id(7, 1), v.id(20, 1));
            let add =
                v.call().push(v.int_ref(a, 28)).push(v.int_ref(b, 32)).build();

            v.block(add)
                .push(s.var(p.var(a), v.int(1, 12)))
                .push(s.var(p.var(b), v.int(2, 25)))
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::Block(&syn)), sem);
    }

    #[test]
    fn value_var_ignored() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var a := 1; :var _ := 2; a }", &global_arena);

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(28, 1))
                .push_stmt(s.var(p.var(7, 1), e.int(12, 1)).build())
                .push_stmt(s.var(p.ignored(20), e.int(25, 1)).build())
                .build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = v.id(7, 1);

            v.block(v.int_ref(a, 28))
                .push(s.var(p.var(a), v.int(1, 12)))
                .push(s.var(p.ignored(20), v.int(2, 25)))
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::Block(&syn)), sem);

    }

    #[test]
    fn value_var_pattern_constructor() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(
            b":rec Some(Int); { :var Some(a) := Some(1); a }",
            &global_arena
        );

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, p, s, t) = (f.expr(), f.pat(), f.stmt(), f.type_());

            e.block(e.var(43, 1))
                .push_stmt(s.var(
                    p.constructor(t.simple(23, 4)).push(p.var(28, 1)).build(),
                    e.constructor(t.simple(34, 4)).push(e.int(39, 1)).build(),
                ).build())
                .build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (i, p, s, t, v) =
                (f.item(), f.pat(), f.stmt(), f.type_(), f.value());

            let a = v.id(28, 1);

            let rec =
                i.rec(f.proto().rec(i.id(5, 4), 0).build())
                    .push(t.int())
                    .build();
            env.insert_record(rec, &[23, 34]);

            v.block(v.int_ref(a, 43))
                .push(s.var(
                    p.constructor(*rec.prototype, 23, 7)
                        .push(p.var(a))
                        .build(),
                    v.constructor(*rec.prototype, 34, 7)
                        .push(v.int(1, 39))
                        .build(),
                ))
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::Block(&syn)), sem);
    }

    #[test]
    fn value_var_pattern_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var (a, b) := (1, 2); a }", &global_arena);

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            let pat = p.tuple().push(p.var(8, 1)).push(p.var(11, 1)).build();
            let expr = e.tuple().push(e.int(18, 1)).push(e.int(21, 1)).build();

            e.block(e.var(25, 1))
                    .push_stmt(s.var(pat, expr).build())
                    .build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let (a, b) = (v.id(8, 1), v.id(11, 1));

            v.block(v.int_ref(a, 25))
                .push(s.var(
                    p.tuple().push(p.var(a)).push(p.var(b)).build(),
                    v.tuple().push(v.int(1, 18)).push(v.int(2, 21)).build(),
                ))
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::Block(&syn)), sem);
    }

    #[test]
    fn value_implicit_cast_to_enum() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(
            b":enum B { T, F } :if true { B::T } else { B::F }", 
            &global_arena
        );

        let syn = {
            let f = SynFactory::new(&global_arena);
            let (e, t) = (f.expr(), f.type_());

            e.if_else(
                e.bool_(21, 4),
                e.block(
                    e.constructor(t.nested(31, 1).push(28, 1).build()).build(),
                ).build(),
                e.block(
                    e.constructor(t.nested(45, 1).push(42, 1).build()).build(),
                ).build(),
            ).build()
        };

        let sem = {
            let f = SemFactory::new(&global_arena);
            let (i, p, v) = (f.item(), f.proto(), f.value());

            let enum_ =
                i.enum_(p.enum_(i.id(6, 1)).build())
                    .push(i.unit(10, 1))
                    .push(i.unit(13, 1))
                    .build();

            env.insert_enum(enum_, &[28, 42]);

            let block = |index: usize, pos: usize| {
                let rec = *enum_.variants[index].prototype;
                v.block(
                    v.implicit().enum_(
                        *enum_.prototype,
                        v.constructor(rec, pos, 4).build_value(),
                    )
                ).build()
            };

            v.if_(v.bool_(true, 21), block(0, 28), block(1, 42))
                .type_((*enum_.prototype).into())
                .build()
        };

        assert_eq!(env.value_of(&syn::Expression::If(&syn)), sem);
    }

    struct Env<'g> {
        scope: MockScope<'g>,
        registry: sem::mocks::MockRegistry<'g>,
        fragment: &'g [u8],
        arena: &'g mem::Arena,
    }

    impl<'g> Env<'g> {
        fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> Env<'g> {
            Env {
                scope: MockScope::new(fragment, arena),
                registry: sem::mocks::MockRegistry::new(arena),
                fragment: fragment,
                arena: arena,
            }
        }

        fn insert_enum(
            &mut self,
            enum_: sem::Enum<'g>,
            positions: &[usize],
        )
        {
            let len = enum_.prototype.name.0.length();
            for p in positions {
                let id = sem::ItemIdentifier(range(*p, len));
                self.scope.types.insert(id, sem::Type::Enum(*enum_.prototype));
            }
            self.registry.enums.insert(enum_.prototype.name, enum_);
        }

        fn insert_record(
            &mut self,
            record: sem::Record<'g>,
            positions: &[usize],
        )
        {
            let len = record.prototype.name.0.length();
            for p in positions {
                let id = sem::ItemIdentifier(range(*p, len));
                self.scope.types.insert(id, sem::Type::Rec(*record.prototype));
            }
            self.registry.records.insert(record.prototype.name, record);
        }

        fn insert_function_prototype(
            &mut self,
            proto: sem::FunctionProto<'g>,
            positions: &[usize],

        )
        {
            let len = proto.name.0.length();

            for p in positions {
                let id = sem::ValueIdentifier(range(*p, len));
                self.scope.callables.insert(id, sem::Callable::Function(proto));
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

        fn value_of(&self, expr: &syn::Expression) -> sem::Value<'g> {
            let mut local_arena = mem::Arena::new();
            let result = self.resolver(&local_arena).value_of(expr);
            local_arena.recycle();
            result
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
