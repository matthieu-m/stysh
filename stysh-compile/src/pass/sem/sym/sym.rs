//! Semantic pass: Symbol Mapping.
//!
//! Mappings:
//! -   maps variable to binding,
//! -   maps function call to callable (binding or function),
//! -   maps type or module to definition.

use basic::{com, mem};
use basic::com::Span;

use model::{ast, hir};

use super::Context;
use super::scp::{BlockScope, Scope};

/// The Symbol Mapper.
///
/// For each top-level reference to a symbol, resolves the symbol.
pub struct SymbolMapper<'a, 'g, 'local>
    where 'g: 'a
{
    scope: &'a Scope<'g>,
    context: &'a Context<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> SymbolMapper<'a, 'g, 'local>
    where 'g: 'a
{
    /// Creates a new instance.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(
        scope: &'a Scope<'g>,
        context: &'a Context<'g>,
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
    )
        -> Self
    {
        SymbolMapper { scope, context, global_arena, local_arena }
    }

    /// Translates a pattern into... a pattern!
    pub fn pattern_of(&self, p: &ast::Pattern) -> hir::Pattern<'g> {
        use model::ast::Pattern;

        match *p {
            Pattern::Constructor(c) => self.pattern_of_constructor(c),
            Pattern::Ignored(v) => self.pattern_of_ignored(v),
            Pattern::Tuple(t) => self.pattern_of_tuple(&t),
            Pattern::Var(v) => self.pattern_of_var(v),
        }
    }

    /// Translates a type into... a type!
    pub fn type_of(&self, t: &ast::Type) -> hir::Type<'g> {
        use model::ast::Type;

        match *t {
            Type::Missing(_) => unimplemented!(),
            Type::Nested(t, p) => self.type_of_nested(t, p),
            Type::Simple(t) => self.type_of_simple(t),
            Type::Tuple(ref t) => self.type_of_tuple(t),
        }
    }

    /// Translates an expression into a value.
    pub fn value_of(&self, e: &ast::Expression) -> hir::Value<'g> {
        self.value_of_expr(e)
    }
}

//
//  Implementation Details
//
impl<'a, 'g, 'local> SymbolMapper<'a, 'g, 'local>
    where 'g: 'a
{
    fn pattern_of_constructor(
        &self,
        c: ast::Constructor<ast::Pattern>,
    )
        -> hir::Pattern<'g>
    {
        hir::Pattern::Constructor(hir::Constructor {
            type_: self.type_of(&c.type_),
            arguments: self.tuple_of(&c.arguments, |p| self.pattern_of(p)),
            range: c.span(),
        })
    }

    fn pattern_of_ignored(&self, underscore: com::Range)
        -> hir::Pattern<'static>
    {
        hir::Pattern::Ignored(underscore)
    }

    fn pattern_of_tuple(&self, t: &ast::Tuple<ast::Pattern>)
        -> hir::Pattern<'g>
    {
        hir::Pattern::Tuple(
            self.tuple_of(t, |p| self.pattern_of(p)),
            t.span(),
        )
    }

    fn pattern_of_var(&self, var: ast::VariableIdentifier)
        -> hir::Pattern<'static>
    {
        self.context.insert_binding(var.into(), hir::Type::unresolved());
        self.context.mark_unfetched_value(var.into());

        hir::Pattern::Var(var.into(), Default::default())
    }

    fn stmt_of_return<'b>(&self,
        ret: &ast::Return,
        scope: &mut BlockScope<'b, 'g, 'local>,
    )
        -> hir::Stmt<'g>
        where
            'g: 'b
    {
        let range = ret.span();
        let unit = hir::Value::unit().with_range(range.end_offset() - 3, 2);

        let value =
            ret.expr
                .map(|e| self.rescope(scope).value_of_expr(&e))
                .unwrap_or(unit);

        hir::Stmt::Return(hir::Return { value, range })
    }

    fn stmt_of_set<'b>(
        &self,
        set: &ast::VariableReBinding,
        scope: &mut BlockScope<'b, 'g, 'local>,
    )
        -> hir::Stmt<'g>
        where
            'g: 'b
    {
        let range = set.span();
        let left = self.rescope(scope).value_of_expr(&set.left);
        let right = self.rescope(scope).value_of_expr(&set.expr);
        hir::Stmt::Set(hir::ReBinding { left, right, range })
    }

    fn stmt_of_var<'b>(
        &self,
        var: &ast::VariableBinding,
        scope: &mut BlockScope<'b, 'g, 'local>,
    )
        -> hir::Stmt<'g>
        where
            'g: 'b
    {
        let value = self.rescope(scope).value_of_expr(&var.expr);
        let pattern = self.rescope(self.scope).pattern_of(&var.pattern);
        scope.add_pattern(pattern);
        hir::Stmt::Var(hir::Binding::Variable(pattern, value, var.span()))
    }

    fn type_of_nested(&self, t: ast::TypeIdentifier, p: ast::Path)
        -> hir::Type<'g>
    {
        let type_ = self.scope.lookup_type(p.components[0].into());

        let mut path = self.array(p.components.len());
        path.push(type_);
        for &c in &p.components[1..] {
            self.context.mark_unfetched_item(c.into());
            path.push(hir::Type::Unresolved(c.into(), Default::default()));
        }

        hir::Type::Unresolved(
            t.into(),
            hir::Path { components: path.into_slice() }
        )
    }

    fn type_of_simple(&self, t: ast::TypeIdentifier) -> hir::Type<'g> {
        self.scope.lookup_type(t.into())
    }

    fn type_of_tuple(&self, t: &ast::Tuple<ast::Type>) -> hir::Type<'g> {
        hir::Type::Tuple(self.tuple_of(t, |t| self.type_of(t)))
    }

    fn value_of_expr(&self, expr: &ast::Expression) -> hir::Value<'g> {
        use model::ast::Expression::*;

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
                => self.value_of_prefix_operator(op, e, expr.span()),
            Tuple(t) => self.value_of_tuple(&t),
            Var(id) => self.value_of_variable(id),
        }
    }

    fn value_of_block(&self, block: &ast::Block) -> hir::Value<'g> {
        let mut scope =
            BlockScope::new(
                self.scope,
                self.global_arena,
                self.local_arena
            );
        let stmts = self.statements(&block.statements, &mut scope);
        let value = block.expression.map(
            |e| self.rescope(&scope).value_of_expr(e)
        );
        let expr = value.map(|v| &*self.global_arena.insert(v));

        hir::Value {
            type_: hir::Type::unresolved(),
            range: block.span(),
            expr: hir::Expr::Block(stmts, expr),
            gvn: Default::default(),
        }
    }

    fn value_of_binary_operator(
        &self,
        op: ast::BinaryOperator,
        left: &ast::Expression,
        right: &ast::Expression
    )
        -> hir::Value<'g>
    {
        use model::ast::BinaryOperator as O;
        use model::hir::BuiltinFunction as F;

        let range = left.span().extend(right.span());

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

        hir::Value {
            type_: hir::Type::unresolved(),
            range: range,
            expr: hir::Expr::Call(
                hir::Callable::Builtin(op),
                self.global_arena.insert_slice(&[left, right])
            ),
            gvn: Default::default(),
        }
    }

    fn value_of_constructor(&self, c: ast::Constructor<ast::Expression>)
        -> hir::Value<'g>
    {
        let type_ = self.type_of(&c.type_);
        let arguments = self.tuple_of(&c.arguments, |v| self.value_of(v));

        hir::Value {
            type_: type_,
            range: c.span(),
            expr: hir::Expr::Constructor(hir::Constructor {
                type_: type_,
                arguments: arguments,
                range: c.span(),
            }),
            gvn: Default::default(),
        }
    }

    fn value_of_call(&self, fun: ast::FunctionCall) -> hir::Value<'g> {
        let callable = if let ast::Expression::Var(id) = *fun.function {
            self.scope.lookup_callable(id.into())
        } else {
            unimplemented!()
        };

        let values = self.slice_of(fun.arguments.fields, |a| self.value_of(a));

        hir::Value {
            type_: hir::Type::unresolved(),
            range: fun.span(),
            expr: hir::Expr::Call(callable, values),
            gvn: Default::default(),
        }
    }

    fn value_of_field(&self, field: ast::FieldAccess) -> hir::Value<'g> {
        use self::ast::FieldIdentifier::*;

        let accessed = self.global_arena.insert(self.value_of(field.accessed));

        let expr = match field.field {
            Index(index, _) => hir::Expr::FieldAccess(accessed, index),
            Name(n, r) => {
                self.context.mark_unfetched_value(hir::ValueIdentifier(n, r));
                hir::Expr::UnresolvedField(
                    accessed,
                    hir::ValueIdentifier(n, r)
                )
            },
        };

        hir::Value {
            type_: hir::Type::unresolved(),
            range: field.span(),
            expr: expr,
            gvn: Default::default(),
        }
    }

    fn value_of_if_else(&self, if_else: &ast::IfElse) -> hir::Value<'g> {
        let condition = self.value_of_expr(&if_else.condition);
        let true_branch = self.value_of_block(&if_else.true_expr);
        let false_branch = self.value_of_block(&if_else.false_expr);

        hir::Value {
            type_: hir::Type::unresolved(),
            range: if_else.span(),
            expr: hir::Expr::If(
                self.global_arena.insert(condition),
                self.global_arena.insert(true_branch),
                self.global_arena.insert(false_branch),
            ),
            gvn: Default::default(),
        }
    }

    fn value_of_literal(&self, lit: ast::Literal)
        -> hir::Value<'g>
    {
        use model::ast::Literal::*;

        match lit {
            Bool(b, r) => self.value_of_literal_bool(b, r),
            Bytes(_, b, r) => self.value_of_literal_bytes(b, r),
            Integral(i, r) => self.value_of_literal_integral(i, r),
            String(_, s, r) => self.value_of_literal_string(s, r),
        }
    }

    fn value_of_literal_bool(
        &self,
        value: bool,
        range: com::Range
    )
        -> hir::Value<'g>
    {
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::Bool),
            range: range,
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::Bool(value)),
            gvn: Default::default(),
        }
    }

    fn value_of_literal_bytes(&self, bytes: &[u8], range: com::Range)
        -> hir::Value<'g>
    {
        let value = self.global_arena.insert_slice(bytes);

        //  TODO(matthieum): Fix type, should be Array[[Byte]].
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::String),
            range: range,
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::String(value)),
            gvn: Default::default(),
        }
    }

    fn value_of_literal_integral(&self, i: i64, range: com::Range)
        -> hir::Value<'g>
    {
        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::Int),
            range: range,
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::Int(i)),
            gvn: Default::default(),
        }
    }

    fn value_of_literal_string(&self, string: &[u8], range: com::Range)
        -> hir::Value<'g>
    {
        let value = self.global_arena.insert_slice(string);

        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::String),
            range: range,
            expr: hir::Expr::BuiltinVal(hir::BuiltinValue::String(value)),
            gvn: Default::default(),
        }
    }

    fn value_of_loop(&self, loop_: &ast::Loop) -> hir::Value<'g> {
        let mut scope =
            BlockScope::new(
                self.scope,
                self.global_arena,
                self.local_arena
            );
        let stmts = self.statements(&loop_.statements, &mut scope);

        hir::Value {
            type_: hir::Type::Builtin(hir::BuiltinType::Void),
            range: loop_.span(),
            expr: hir::Expr::Loop(stmts),
            gvn: Default::default(),
        }
    }

    fn value_of_prefix_operator(&self, 
        op: ast::PrefixOperator,
        expr: &ast::Expression,
        range: com::Range,
    )
        -> hir::Value<'g>
    {
        use model::ast::PrefixOperator as O;
        use model::hir::BuiltinFunction as F;

        let expr = self.value_of_expr(expr);

        let op = match op {
            O::Not => F::Not,
        };

        hir::Value {
            type_: hir::Type::unresolved(),
            range: range,
            expr: hir::Expr::Call(
                hir::Callable::Builtin(op),
                self.global_arena.insert_slice(&[expr])
            ),
            gvn: Default::default(),
        }
    }

    fn value_of_tuple(&self, tup: &ast::Tuple<ast::Expression>)
        -> hir::Value<'g>
    {
        let expr = self.tuple_of(tup, |v| self.value_of(v));
        hir::Value {
            type_: hir::Type::unresolved(),
            range: tup.span(),
            expr: hir::Expr::Tuple(expr),
            gvn: Default::default(),
        }
    }

    fn value_of_variable(&self, var: ast::VariableIdentifier)
        -> hir::Value<'g>
    {
        self.scope.lookup_binding(var.into())
    }

    fn rescope<'b>(&self, scope: &'b Scope<'g>) -> SymbolMapper<'b, 'g, 'local>
        where 'a: 'b
    {
        SymbolMapper {
            scope: scope,
            context: self.context,
            global_arena: self.global_arena,
            local_arena: self.local_arena,
        }
    }

    fn statements<'b>(
        &self,
        stmts: &[ast::Statement],
        scope: &mut BlockScope<'b, 'g, 'local>,
    )
        -> &'g [hir::Stmt<'g>]
        where
            'g: 'b
    {
        self.slice_of(stmts, |&s| {
            match s {
                ast::Statement::Return(r) => self.stmt_of_return(&r, scope),
                ast::Statement::Set(set) => self.stmt_of_set(&set, scope),
                ast::Statement::Var(var) => self.stmt_of_var(&var, scope),
            }
        })
    }

    fn array<T>(&self, size: usize) -> mem::Array<'g, T> {
        mem::Array::with_capacity(size, self.global_arena)
    }

    fn slice_of<'b, T: 'b, U: 'g, F: FnMut(&'b T) -> U>(
        &self,
        input: &'b [T],
        mut transformer: F,
    )
        -> &'g [U]
    {
        let mut result = self.array(input.len());

        for i in input {
            result.push(transformer(i));
        }

        result.into_slice()
    }

    /// TODO: Move functionality from GraphBuilder to SymbolMapper.
    pub fn tuple_of<'b, T: 'b, U: 'g, F: FnMut(&'b T) -> U>(
        &self,
        tup: &'b ast::Tuple<'b, T>,
        transformer: F,
    )
        -> hir::Tuple<'g, U>
    {
        debug_assert!(
            tup.names.is_empty() || tup.names.len() == tup.fields.len()
        );

        let fields = self.slice_of(tup.fields, transformer);

        let names = if !tup.names.is_empty() {
            self.slice_of(tup.names, |&(i, r)| hir::ValueIdentifier(i, r))
        } else {
            &[]
        };

        hir::Tuple { fields, names }
    }

}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::rc;

    use basic::{com, mem};
    use basic::com::Span;

    use model::{ast, hir};
    use model::ast::builder::Factory as AstFactory;
    use model::hir::builder::Factory as HirFactory;
    use super::super::Context;
    use super::super::scp::mocks::MockScope;

    #[test]
    fn value_block_empty() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{}", &global_arena);

        let ast = {
            let e = AstFactory::new(&global_arena).expr();
            e.block_div().range(0, 2).build()
        };

        let hir = {
            let v = HirFactory::new(&global_arena).value();
            v.block_div().range(0, 2).build().without_type()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_builtin_boolean() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();
        let v = HirFactory::new(&global_arena).value();
        let env = Env::new(b"true", &global_arena);

        assert_eq!(
            env.value_of(&e.bool_(true, 0)),
            v.bool_(true, 0)
        );
    }

    #[test]
    fn value_builtin_string() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();

        let v = HirFactory::new(&global_arena).value();

        let env = Env::new(b"'Hello, World!'", &global_arena);

        assert_eq!(
            env.value_of(&e.literal(0, 15).push_text(1, 13).string(b"Hello, World!").build()),
            v.string("Hello, World!", 0)
        );
    }

    #[test]
    fn value_call_add() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();
        let v = HirFactory::new(&global_arena).value();
        let env = Env::new(b"1 + 2", &global_arena);

        assert_eq!(
            env.value_of(&e.bin_op(e.int(1, 0), e.int(2, 4)).build()),
            v.call().push(v.int(1, 0)).push(v.int(2, 4)).build().without_type()
        );
    }

    #[test]
    fn value_call_basic() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();

        let hir = HirFactory::new(&global_arena);
        let (i, p, t, v) = (hir.item(), hir.proto(), hir.type_(), hir.value());

        let mut env = Env::new(b"basic(1, 2)    basic", &global_arena);

        let proto = p.fun(i.id(15, 5), t.int()).build();
        env.insert_function(proto, &[0]);

        assert_eq!(
            env.value_of(
                &e.function_call(e.var(0, 5), 5, 10)
                    .push(e.int(1, 6))
                    .push(e.int(2, 9))
                    .build()
            ),
            v.call()
                .function(proto)
                .push(v.int(1, 6))
                .push(v.int(2, 9))
                .build()
                .without_type()
        )
    }

    #[test]
    fn value_constructor() {
        let global_arena = mem::Arena::new();
        let ast = AstFactory::new(&global_arena);
        let h = HirFactory::new(&global_arena);
        let (i, p, v) = (h.item(), h.proto(), h.value());

        let mut env = Env::new(b"Rec   Rec", &global_arena);

        let rec = p.rec(i.id(6, 3), 0).build();
        env.insert_record(rec, &[0]);

        assert_eq!(
            env.value_of(
                &ast.expr().constructor(ast.type_().simple(0, 3)).build()
            ),
            v.constructor(hir::Type::Rec(rec, Default::default()), 0, 3)
                .build_value()
        );
    }

    #[test]
    fn value_constructor_arguments() {
        let global_arena = mem::Arena::new();
        let ast = AstFactory::new(&global_arena);
        let e = ast.expr();

        let h = HirFactory::new(&global_arena);
        let (i, p, v) = (h.item(), h.proto(), h.value());

        let mut env = Env::new(b"Rec(1)   Rec", &global_arena);

        let rec = p.rec(i.id(9, 3), 0).build();
        env.insert_record(rec, &[0]);

        assert_eq!(
            env.value_of(
                &e.constructor(ast.type_().simple(0, 3))
                    .parens(3, 5)
                    .push(e.int(1, 4))
                    .build()
            ),
            v.constructor(hir::Type::Rec(rec, Default::default()), 0, 6)
                .push(v.int(1, 4))
                .build_value()
        );
    }

    #[test]
    fn value_constructor_nested() {
        let global_arena = mem::Arena::new();
        let ast = AstFactory::new(&global_arena);
        let e = ast.expr();

        let h = HirFactory::new(&global_arena);
        let (i, p, t, v) = (h.item(), h.proto(), h.type_(), h.value());

        let mut env = Env::new(
            b":enum Simple { Unit }         Simple::Unit",
            &global_arena
        );

        let enum_ = p.enum_(i.id(6, 6)).build();
        env.insert_enum(enum_, &[30]);

        assert_eq!(
            env.value_of(
                &e.constructor(
                    ast.type_().nested(38, 4).push(30, 6).build()
                ).build()
            ),
            v.constructor(
                t.unresolved(i.id(38, 4))
                    .push(hir::Type::Enum(enum_, Default::default()))
                    .build(),
                30,
                12,
            ).build_value()
        );
    }

    #[test]
    fn value_if_else() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();

        let v = HirFactory::new(&global_arena).value();

        let env = Env::new(b":if true { 1 } :else { 0 }", &global_arena);

        assert_eq!(
            env.value_of(&ast::Expression::If(
                &e.if_else(
                    e.bool_(true, 4),
                    e.block(e.int(1, 11)).build(),
                    e.block(e.int(0, 23)).build(),
                ).build()
            )),
            v.if_(
                v.bool_(true, 4),
                v.block(v.int(1, 11)).build().without_type(),
                v.block(v.int(0, 23)).build().without_type(),
            ).build()
        )
    }

    #[test]
    fn value_loop() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":loop { }", &global_arena);

        let ast = {
            let e = AstFactory::new(&global_arena).expr();
            e.loop_(0).build()
        };

        let hir = {
            let v = HirFactory::new(&global_arena).value();
            v.loop_().range(0, 9).build()
        };

        assert_eq!(env.value_of(&ast), hir);
    }

    #[test]
    fn value_record_field_access() {
        let global_arena = mem::Arena::new();
        let ast = AstFactory::new(&global_arena);
        let e = ast.expr();

        let h = HirFactory::new(&global_arena);
        let (i, p, v) = (h.item(), h.proto(), h.value());

        let mut env = Env::new(b":rec Rec(Int); Rec(42).0", &global_arena);

        let rec = p.rec(i.id(5, 3), 0).range(0, 14).build();
        env.insert_record(rec, &[15]);

        assert_eq!(
            env.value_of(
                &e.field_access(
                    e.constructor(ast.type_().simple(15, 3))
                        .parens(18, 21)
                        .push(e.int(42, 19))
                        .build(),
                )
                    .index(0)
                    .build()
            ),
            v.field_access(
                0,
                v.constructor(hir::Type::Rec(rec, Default::default()), 15, 7)
                    .push(v.int(42, 19))
                    .build_value()
            ).build()
            .without_type()
        );
    }

    #[test]
    fn value_return_basic() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :return 1; }", &global_arena);

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, s) = (f.expr(), f.stmt());

            e.block_div()
                .push_stmt(s.ret().expr(e.int(1, 10)).build())
                .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (s, v) = (f.stmt(), f.value());

            v.block_div().push(s.ret(v.int(1, 10))).build().without_type()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_return_if_and_else() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":if true { :return 1; } :else { :return 2; }",
            &global_arena
        );

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, s) = (f.expr(), f.stmt());

            e.if_else(
                e.bool_(true, 4),
                e.block_div()
                    .push_stmt(s.ret().expr(e.int(1, 19)).build())
                    .build(),
                e.block_div()
                    .push_stmt(s.ret().expr(e.int(2, 40)).build())
                    .build(),
            ).build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (s, v) = (f.stmt(), f.value());

            v.if_(
                v.bool_(true, 4),
                v.block_div().push(s.ret(v.int(1, 19))).build().without_type(),
                v.block_div().push(s.ret(v.int(2, 40))).build().without_type(),
            ).build()
        };

        assert_eq!(env.value_of(&ast::Expression::If(&ast)), hir);
    }

    #[test]
    fn value_set_basic() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var a := 1; :set a := 2; a }", &global_arena);

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(28, 1))
                    .push_stmt(s.var(p.var(7, 1), e.int(1, 12)).build())
                    .push_stmt(s.set(e.var(20, 1), e.int(2, 25)).build())
                    .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = v.id(7, 1);

            v.block(v.var_ref(a, 28))
                .push(s.var(p.var(a), v.int(1, 12)))
                .push(s.set(v.var_ref(a, 20), v.int(2, 25)))
                .build()
                .without_type()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_set_field() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b"{ :var a := (1,); :set a.0 := 2; a }",
            &global_arena
        );

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(33, 1))
                .push_stmt(s.var(
                    p.var(7, 1),
                    e.tuple().push(e.int(1, 13)).comma(14).build()
                ).build())
                .push_stmt(s.set(
                    e.field_access(e.var(23, 1)).index(0).build(),
                    e.int(2, 30)
                ).build())
                .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = v.id(7, 1);
            let tup =
                v.tuple()
                    .push(v.int(1, 13))
                    .build()
                    .without_type()
                    .with_range(12, 4);

            v.block(v.var_ref(a, 33))
                .push(s.var(p.var(a), tup))
                .push(s.set(
                    v.field_access(0, v.var_ref(a, 23)).build(),
                    v.int(2, 30)
                ))
                .build()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_tuple_field_access() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();

        let v = HirFactory::new(&global_arena).value();

        let env = Env::new(b"(42, 43).1", &global_arena);

        assert_eq!(
            env.value_of(
                &e.field_access(
                    e.tuple().push(e.int(42, 1)).push(e.int(43, 5)).build(),
                )
                    .index(1)
                    .build()
            ),
            v.field_access(
                1,
                v.tuple().push(v.int(42, 1)).push(v.int(43, 5)).build().without_type()
            ).build()
        )
    }

    #[test]
    fn value_tuple_keyed_field_access() {
        let global_arena = mem::Arena::new();
        let e = AstFactory::new(&global_arena).expr();

        let v = HirFactory::new(&global_arena).value();

        let env = Env::new(b"(.x := 42, .y := 43).y", &global_arena);

        let (x, y) = (env.field_id(1, 2), env.field_id(11, 2));

        assert_eq!(
            env.value_of_resolved(
                &e.field_access(
                    e.tuple()
                        .push(e.int(42, 7)).name(1, 2)
                        .push(e.int(43, 17)).name(11, 2)
                        .build(),
                )
                    .name(20, 2)
                    .build()
            ),
            v.unresolved_field(
                env.field_id(20, 2),
                v.tuple()
                    .push(v.int(42, 7)).name(x)
                    .push(v.int(43, 17)).name(y)
                    .build()
                    .without_type()
            )
        )
    }

    #[test]
    fn value_var_basic() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var a := 1; :var b := 2; a + b }", &global_arena);

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.bin_op(e.var(28, 1), e.var(32, 1)).build())
                .push_stmt(s.var(p.var(7, 1), e.int(1, 12)).build())
                .push_stmt(s.var(p.var(20, 1), e.int(2, 25)).build())
                .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let (a, b) = (v.id(7, 1), v.id(20, 1));
            let add =
                v.call().push(v.var_ref(a, 28)).push(v.var_ref(b, 32)).build();

            v.block(add)
                .push(s.var(p.var(a), v.int(1, 12)))
                .push(s.var(p.var(b), v.int(2, 25)))
                .build()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_var_ignored() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var a := 1; :var _ := 2; a }", &global_arena);

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(28, 1))
                .push_stmt(s.var(p.var(7, 1), e.int(1, 12)).build())
                .push_stmt(s.var(p.ignored(20), e.int(2, 25)).build())
                .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = v.id(7, 1);

            v.block(v.var_ref(a, 28))
                .push(s.var(p.var(a), v.int(1, 12)))
                .push(s.var(p.ignored(20), v.int(2, 25)))
                .build()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);

    }

    #[test]
    fn value_var_pattern_constructor() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(
            b":rec Some(Int); { :var Some(a) := Some(1); a }",
            &global_arena
        );

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s, t) = (f.expr(), f.pat(), f.stmt(), f.type_());

            e.block(e.var(43, 1))
                .push_stmt(s.var(
                    p.constructor(t.simple(23, 4)).push(p.var(28, 1)).build(),
                    e.constructor(t.simple(34, 4)).push(e.int(1, 39)).build(),
                ).build())
                .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (i, p, s, v) =
                (f.item(), f.pat(), f.stmt(), f.value());

            let a = v.id(28, 1);

            let rec = f.proto().rec(i.id(5, 4), 0).build();
            env.insert_record(rec, &[23, 34]);
            let rec = hir::Type::Rec(rec, Default::default());

            v.block(v.var_ref(a, 43))
                .push(s.var(
                    p.constructor(rec, 23, 7)
                        .push(p.var(a))
                        .build_pattern(),
                    v.constructor(rec, 34, 7)
                        .push(v.int(1, 39))
                        .build_value(),
                ))
                .build()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_var_pattern_keyed_constructor() {
        let global_arena = mem::Arena::new();
        let mut env = Env::new(
            b":rec Some(.x: Int); { :var Some(.x: a) := Some(.x: 1); a }",
            &global_arena
        );

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s, t) = (f.expr(), f.pat(), f.stmt(), f.type_());

            e.block(e.var(55, 1))
                .push_stmt(s.var(
                    p.constructor(t.simple(27, 4))
                        .push(p.var(36, 1)).name(32, 2)
                        .build(),
                    e.constructor(t.simple(42, 4))
                        .push(e.int(1, 51)).name(47, 2)
                        .build(),
                ).build())
                .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (i, p, s, v) =
                (f.item(), f.pat(), f.stmt(), f.value());

            let a = env.var_id(36, 1);

            let rec = f.proto().rec(i.id(5, 4), 0).build();
            let rec = hir::Type::Rec(
                env.insert_record(rec, &[27, 42]),
                Default::default(),
            );

            v.block(v.var_ref(a, 55))
                .push(s.var(
                    p.constructor(rec, 27, 11)
                        .push(p.var(a)).name(env.field_id(32, 2))
                        .build_pattern(),
                    v.constructor(rec, 42, 11)
                        .push(v.int(1, 51)).name(env.field_id(47, 2))
                        .build_value(),
                ))
                .build()
        };

        assert_eq!(env.value_of_resolved(&ast::Expression::Block(&ast)), hir);
    }

    #[test]
    fn value_var_pattern_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b"{ :var (a, b) := (1, 2); a }", &global_arena);

        let ast = {
            let f = AstFactory::new(&global_arena);
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            let pat = p.tuple().push(p.var(8, 1)).push(p.var(11, 1)).build();
            let expr = e.tuple().push(e.int(1, 18)).push(e.int(2, 21)).build();

            e.block(e.var(25, 1))
                    .push_stmt(s.var(pat, expr).build())
                    .build()
        };

        let hir = {
            let f = HirFactory::new(&global_arena);
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let (a, b) = (v.id(8, 1), v.id(11, 1));

            v.block(v.var_ref(a, 25))
                .push(s.var(
                    p.tuple().push(p.var(a)).push(p.var(b)).build(),
                    v.tuple()
                        .push(v.int(1, 18))
                        .push(v.int(2, 21))
                        .build()
                        .without_type(),
                ))
                .build()
        };

        assert_eq!(env.value_of(&ast::Expression::Block(&ast)), hir);
    }

    struct Env<'g> {
        scope: MockScope<'g>,
        context: Context<'g>,
        ast_resolver: ast::interning::Resolver<'g>,
        hir_resolver: hir::interning::Resolver<'g>,
        scrubber: hir::interning::Scrubber<'g>,
        arena: &'g mem::Arena,
    }

    impl<'g> Env<'g> {
        fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> Env<'g> {
            let interner = rc::Rc::new(mem::Interner::new());
            Env {
                scope: MockScope::new(arena),
                context: Context::default(),
                ast_resolver: ast::interning::Resolver::new(fragment, interner.clone(), arena),
                hir_resolver: hir::interning::Resolver::new(fragment, interner, arena),
                scrubber: hir::interning::Scrubber::new(arena),
                arena: arena,
            }
        }

        fn insert_enum(&mut self, enum_: hir::EnumProto, positions: &[usize]) {
            let enum_ = self.hir_resolver.resolve_enum_prototype(enum_);
            let name = enum_.name;
            let len = name.span().length();

            for p in positions {
                let id = hir::ItemIdentifier(name.id(), range(*p, len));
                self.scope.types.insert(id, hir::Type::Enum(enum_, Default::default()));
            } 
        }

        fn insert_function(
            &mut self,
            proto: hir::FunctionProto<'g>,
            positions: &[usize],
        )
        {
            let proto = self.hir_resolver.resolve_function_prototype(proto);
            let len = proto.name.span().length();
            println!("Registered function: {:?}", proto.name);

            for p in positions {
                let id = hir::ValueIdentifier(proto.name.id(), range(*p, len));
                self.scope.callables.insert(id, hir::Callable::Function(proto));
            }
        }

        fn insert_record(&mut self, rec: hir::RecordProto, positions: &[usize])
            -> hir::RecordProto
        {
            let rec = self.hir_resolver.resolve_record_prototype(rec);
            let len = rec.name.span().length();
            println!(
                "Registered record prototype: {:?} (of {:?})",
                rec.name, rec.enum_
            );

            for p in positions {
                let id = hir::ItemIdentifier(rec.name.id(), range(*p, len));
                self.scope.types.insert(id, hir::Type::Rec(rec, Default::default()));
            }

            rec
        }

        fn field_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let field = range(pos + 1, len - 1);
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(field), range)
        }

        fn var_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn resolver<'a, 'local>(&'a self, local: &'local mem::Arena)
            -> super::SymbolMapper<'a, 'g, 'local>
        {
            super::SymbolMapper::new(
                &self.scope,
                &self.context,
                self.arena, 
                local
            )
        }

        fn value_of(&self, expr: &ast::Expression) -> hir::Value<'g> {
            self.scrubber.scrub_value(self.value_of_resolved(expr))
        }

        fn value_of_resolved(&self, expr: &ast::Expression) -> hir::Value<'g> {
            let expr = self.ast_resolver.resolve_expr(*expr);

            let mut local_arena = mem::Arena::new();
            let result = self.resolver(&local_arena).value_of(&expr);
            local_arena.recycle();

            result
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
