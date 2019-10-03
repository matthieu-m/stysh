//! Semantic pass: Symbol Mapping.
//!
//! Mappings:
//! -   maps variable to binding,
//! -   maps function call to callable (binding or function),
//! -   maps type or module to definition.

use std::cell;

use crate::basic::{com, mem};
use crate::basic::com::Span;

use crate::model::{ast, hir};
use self::hir::Registry;

use super::{Context, Relation};
use super::scp::{BlockScope, CallableCandidate, Scope};

/// The Symbol Mapper.
///
/// For each top-level reference to a symbol, resolves the symbol.
pub struct SymbolMapper<'a> {
    scope: &'a dyn Scope,
    registry: &'a dyn Registry,
    context: &'a Context,
    ast_tree: &'a ast::Tree,
    tree: &'a cell::RefCell<hir::Tree>,
}

impl<'a> SymbolMapper<'a> {
    /// Creates a new instance.
    pub fn new(
        scope: &'a dyn Scope,
        registry: &'a dyn Registry,
        context: &'a Context,
        ast_tree: &'a ast::Tree,
        tree: &'a cell::RefCell<hir::Tree>,
    )
        -> Self
    {
        SymbolMapper { scope, registry, context, ast_tree, tree }
    }

    /// Translates a pattern into... a pattern!
    pub fn pattern_of(&self, p: ast::PatternId) -> hir::PatternId {
        use self::ast::Pattern;

        match self.ast_tree.get_pattern(p) {
            Pattern::Constructor(c) => self.pattern_of_constructor(c),
            Pattern::Ignored(v) => self.pattern_of_ignored(v),
            Pattern::Tuple(t) => self.pattern_of_tuple(t),
            Pattern::Var(v) => self.pattern_of_var(v),
        }
    }

    /// Translates a type into... a type!
    pub fn type_of(&self, t: ast::TypeId) -> hir::Type {
        use self::ast::Type;

        match self.ast_tree.get_type(t) {
            Type::Missing(_) => unimplemented!(),
            Type::Nested(t, p) => self.type_of_nested(t, p),
            Type::Simple(t) => self.type_of_simple(t),
            Type::Tuple(t) => self.type_of_tuple(t),
        }
    }

    /// Translates an expression into a value.
    pub fn value_of(&self, e: ast::ExpressionId) -> hir::ExpressionId {
        self.value_of_expr(e)
    }
}

//
//  Implementation Details
//
impl<'a> SymbolMapper<'a> {
    fn pattern_of_constructor(
        &self,
        c: ast::Constructor<ast::Pattern>,
    )
        -> hir::PatternId
    {
        let typ = self.type_of(c.type_);
        let range = self.ast_tree.get_type_range(c.type_).extend(c.arguments.span());
        let tuple = self.tuple_of(
            self.ast_tree.get_pattern_ids(c.arguments.fields),
            self.ast_tree.get_identifiers(c.arguments.names),
            |p| self.pattern_of(p),
            |p| self.tree_mut().push_patterns(p.iter().cloned()),
        );
        let pattern = hir::Pattern::Constructor(tuple);

        let result = self.tree_mut().push_pattern(typ, pattern, range);
        self.link_patterns(result.into(), tuple.fields);
        result
    }

    fn pattern_of_ignored(&self, underscore: com::Range)
        -> hir::PatternId
    {
        let pattern = hir::Pattern::Ignored(underscore);

        self.tree_mut().push_pattern(hir::Type::unresolved(), pattern, underscore)
    }

    fn pattern_of_tuple(&self, tup: ast::Tuple<ast::Pattern>)
        -> hir::PatternId
    {
        let pat = self.tuple_of(
            self.ast_tree.get_pattern_ids(tup.fields),
            self.ast_tree.get_identifiers(tup.names),
            |p| self.pattern_of(p),
            |p| self.tree_mut().push_patterns(p.iter().cloned()),
        );
        let typ = self.tuple_type_of(
            pat.names,
            |tree| tree.get_patterns(pat.fields),
            |p| self.tree().get_pattern_type_id(p),
        );

        let typ = hir::Type::Tuple(typ);
        let pattern = hir::Pattern::Tuple(pat);

        let result = self.tree_mut().push_pattern(typ, pattern, tup.span());
        self.link_patterns(result.into(), pat.fields);
        result
    }

    fn pattern_of_var(&self, var: ast::VariableIdentifier)
        -> hir::PatternId
    {
        let pattern = hir::Pattern::Var(var.into());
        let pattern = self.tree_mut()
            .push_pattern(hir::Type::unresolved(), pattern, var.span());

        self.context.insert_value(var.into(), pattern.into());

        pattern
    }

    fn statements<'b>(
        &self,
        stmts: ast::Id<[ast::StatementId]>,
        scope: &mut BlockScope<'b>,
    )
        -> hir::Id<[hir::Statement]>
    {
        let stmts = self.ast_tree.get_statement_ids(stmts);
        let stmts = self.array_of(stmts, |&s| {
            match self.ast_tree.get_statement(s) {
                ast::Statement::Return(r) => self.stmt_of_return(r, scope),
                ast::Statement::Set(set) => self.stmt_of_set(set, scope),
                ast::Statement::Var(var) => self.stmt_of_var(var, scope),
            }
        });

        self.tree_mut().push_statements(stmts)
    }

    fn stmt_of_return<'b>(&self,
        ret: ast::Return,
        scope: &mut BlockScope<'b>,
    )
        -> hir::Statement
    {
        let value = if let Some(e) = ret.expr {
            self.rescope(scope).value_of_expr(e)
        } else {
            let typ = hir::Type::unit();
            let expr = hir::Expression::unit();
            let range = com::Range::new(ret.span().end_offset() - 3, 2);
            self.tree_mut().push_expression(typ, expr, range)
        };

        //  TODO(matthieum): link type of value to type of function's result.

        hir::Statement::Return(hir::Return { value, range: ret.span() })
    }

    fn stmt_of_set<'b>(
        &self,
        set: ast::VariableReBinding,
        scope: &mut BlockScope<'b>,
    )
        -> hir::Statement
    {
        let range = set.span();
        let left = self.rescope(scope).value_of_expr(set.left);
        let right = self.rescope(scope).value_of_expr(set.expr);

        self.context.link_gvns(&[left.into(), right.into()]);
        self.link_gvn_types(left.into(), Relation::SuperTypeOf(right.into()));

        hir::Statement::Set(hir::ReBinding { left, right, range })
    }

    fn stmt_of_var<'b>(
        &self,
        var: ast::VariableBinding,
        scope: &mut BlockScope<'b>,
    )
        -> hir::Statement
    {
        let left = self.rescope(self.scope).pattern_of(var.pattern);
        let right = self.rescope(scope).value_of_expr(var.expr);
        let range = var.span();

        scope.add_pattern(left, &self.tree());
        self.context.link_gvns(&[left.into(), right.into()]);
        self.link_gvn_types(left.into(), Relation::Identical(right.into()));

        hir::Statement::Var(hir::Binding { left, right, range })
    }

    fn type_of_nested(&self, t: ast::TypeIdentifier, p: ast::Path)
        -> hir::Type
    {
        use self::hir::Type::*;

        let components = self.ast_tree.get_identifiers(p.components);

        let mut path = Vec::with_capacity(components.len());
        for &c in components {
            let range = c.span();
            let component = match self.scope.lookup_type(c.into()) {
                Enum(id, _) => hir::PathComponent::Enum(id, range),
                Rec(id, _) => hir::PathComponent::Rec(id, range),
                Unresolved(name, _) => hir::PathComponent::Unresolved(name),
                Builtin(_) | Tuple(_) => hir::PathComponent::default(),
            };
            path.push(component);
        }

        let path = self.tree_mut().push_path(path);

        hir::Type::Unresolved(t.into(), path)
    }

    fn type_of_simple(&self, t: ast::TypeIdentifier) -> hir::Type {
        self.scope.lookup_type(t.into())
    }

    fn type_of_tuple(&self, tup: ast::Tuple<ast::Type>) -> hir::Type {
        hir::Type::Tuple(self.tuple_of(
            self.ast_tree.get_type_ids(tup.fields),
            self.ast_tree.get_identifiers(tup.names),
            |t| self.tree_mut().push_type(self.type_of(t)),
            |t| self.tree_mut().push_type_ids(t.iter().cloned()),
        ))
    }

    fn value_of_expr(&self, expr: ast::ExpressionId) -> hir::ExpressionId {
        use self::ast::Expression::*;

        let range = self.ast_tree.get_expression_range(expr);

        match self.ast_tree.get_expression(expr) {
            BinOp(op, _, left, right)
                 => self.value_of_binary_operator(op, left, right),
            Block(b) => self.value_of_block(b),
            Constructor(c) => self.value_of_constructor(c, range),
            FieldAccess(f) => self.value_of_field(f),
            FunctionCall(fun) => self.value_of_call(fun),
            If(if_else) => self.value_of_if_else(if_else),
            Lit(lit) => self.value_of_literal(lit, range),
            Loop(loop_) => self.value_of_loop(loop_),
            PreOp(op, _, e)
                => self.value_of_prefix_operator(op, e, range),
            Tuple(t) => self.value_of_tuple(t),
            Var(id) => self.value_of_variable(id),
        }
    }

    fn value_of_binary_operator(
        &self,
        op: ast::BinaryOperator,
        left: ast::ExpressionId,
        right: ast::ExpressionId,
    )
        -> hir::ExpressionId
    {
        use self::ast::BinaryOperator as O;
        use self::hir::BuiltinFunction as F;

        let left = self.value_of_expr(left);
        let right = self.value_of_expr(right);

        let arguments = hir::Tuple {
            fields: self.tree_mut().push_expressions([left, right].iter().cloned()),
            names: hir::Id::empty(),
        };

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

        let left_range = self.tree().get_expression_range(left);
        let right_range = self.tree().get_expression_range(right);

        let typ = hir::Type::unresolved();
        let range = left_range.extend(right_range);
        let expr = hir::Expression::Call(hir::Callable::Builtin(op), arguments);

        let result = self.tree_mut().push_expression(typ, expr, range);
        self.link_expressions(result.into(), arguments.fields);
        result
    }

    fn value_of_block(&self, block: ast::Block) -> hir::ExpressionId {
        let mut scope = BlockScope::new(self.scope);

        let stmts = self.statements(block.statements, &mut scope);
        let expr = block.expression.map(
            |e| self.rescope(&scope).value_of_expr(e)
        );

        let range = block.span();
        let block = hir::Expression::Block(stmts, expr);

        if let Some(e) = expr {
            let typ = hir::Type::unresolved();
            let block = self.tree_mut().push_expression(typ, block, range);

            self.context.link_gvns(&[e.into(), block.into()]);
            self.link_gvn_types(e.into(), Relation::SubTypeOf(block.into()));

            block
        } else {
            //  FIXME(matthieum): Deduce type from last statement.
            //                    It is either () or Void.
            let typ = hir::Type::unresolved();
            self.tree_mut().push_expression(typ, block, range)
        }
    }

    fn value_of_constructor(&self, c: ast::Constructor<ast::Expression>, range: com::Range)
        -> hir::ExpressionId
    {
        let arguments = self.tuple_of(
            self.ast_tree.get_expression_ids(c.arguments.fields),
            self.ast_tree.get_identifiers(c.arguments.names),
            |v| self.value_of(v),
            |e| self.tree_mut().push_expressions(e.iter().cloned()),
        );

        let typ = self.type_of(c.type_);
        let expr = hir::Expression::Constructor(arguments);

        let result = self.tree_mut().push_expression(typ, expr, range);
        self.link_expressions(result.into(), arguments.fields);
        result
    }

    fn value_of_call(&self, fun: ast::FunctionCall) -> hir::ExpressionId {
        let function = self.ast_tree.get_expression(fun.function);
        let function_range = self.ast_tree.get_expression_range(fun.function);

        let candidate = if let ast::Expression::Var(id) = function {
            self.scope.lookup_callable(id.into())
        } else {
            unimplemented!()
        };

        let callable = self.convert_callable(&candidate);

        let arguments = self.tuple_of(
            self.ast_tree.get_expression_ids(fun.arguments.fields),
            self.ast_tree.get_identifiers(fun.arguments.names),
            |a| self.value_of(a),
            |e| self.tree_mut().push_expressions(e.iter().cloned()),
        );

        let typ = hir::Type::unresolved();
        let expr = hir::Expression::Call(callable, arguments);
        let range = function_range.extend(fun.arguments.span());

        let result = self.tree_mut().push_expression(typ, expr, range);
        self.link_expressions(result.into(), arguments.fields);
        result
    }

    fn value_of_field(&self, field: ast::FieldAccess) -> hir::ExpressionId {
        use self::ast::FieldIdentifier::*;

        let accessed = self.value_of(field.accessed);

        let f = match field.field {
            Index(i, r) => hir::Field::Index(i, r),
            Name(id) => hir::Field::Unresolved(id.into()),
        };

        let typ = hir::Type::unresolved();
        let expr = hir::Expression::FieldAccess(accessed, f);
        let range = self.ast_tree.get_expression_range(field.accessed)
            .extend(field.field.span());

        let result = self.tree_mut().push_expression(typ, expr, range);

        self.context.link_gvns(&[result.into(), accessed.into()]);

        result
    }

    fn value_of_if_else(&self, if_else: ast::IfElse) -> hir::ExpressionId {
        let condition = self.value_of_expr(if_else.condition);
        let true_branch = self.value_of_expr(if_else.true_expr);
        let false_branch = self.value_of_expr(if_else.false_expr);

        let typ = hir::Type::unresolved();
        let expr = hir::Expression::If(condition, true_branch, false_branch);
        let range = com::Range::new(if_else.if_ as usize, 1)
            .extend(self.ast_tree.get_expression_range(if_else.false_expr));

        let result = self.tree_mut().push_expression(typ, expr, range);

        self.context.link_gvns(
            &[result.into(), true_branch.into(), false_branch.into()]
        );

        self.link_gvn_types(result.into(), Relation::SuperTypeOf(true_branch.into()));
        self.link_gvn_types(result.into(), Relation::SuperTypeOf(false_branch.into()));

        result
    }

    fn value_of_literal(&self, lit: ast::Literal, r: com::Range)
        -> hir::ExpressionId
    {
        use self::ast::Literal::*;

        match lit {
            Bool(b) => self.value_of_literal_bool(b, r),
            Bytes(_, b) => self.value_of_literal_bytes(b, r),
            Integral(i) => self.value_of_literal_integral(i, r),
            String(_, s) => self.value_of_literal_string(s, r),
        }
    }

    fn value_of_literal_bool(
        &self,
        value: bool,
        range: com::Range
    )
        -> hir::ExpressionId
    {
        let typ = hir::Type::Builtin(hir::BuiltinType::Bool);
        let expr = hir::Expression::BuiltinVal(hir::BuiltinValue::Bool(value));

        self.tree_mut().push_expression(typ, expr, range)
    }

    fn value_of_literal_bytes(&self, bytes: mem::InternId, range: com::Range)
        -> hir::ExpressionId
    {
        //  TODO(matthieum): Fix type, should be Array[Byte].
        let typ = hir::Type::Builtin(hir::BuiltinType::String);
        let expr = hir::Expression::BuiltinVal(hir::BuiltinValue::String(bytes));

        self.tree_mut().push_expression(typ, expr, range)
    }

    fn value_of_literal_integral(&self, i: i64, range: com::Range)
        -> hir::ExpressionId
    {
        let typ = hir::Type::Builtin(hir::BuiltinType::Int);
        let expr = hir::Expression::BuiltinVal(hir::BuiltinValue::Int(i));

        self.tree_mut().push_expression(typ, expr, range)
    }

    fn value_of_literal_string(&self, string: mem::InternId, range: com::Range)
        -> hir::ExpressionId
    {
        let typ = hir::Type::Builtin(hir::BuiltinType::String);
        let expr = hir::Expression::BuiltinVal(hir::BuiltinValue::String(string));

        self.tree_mut().push_expression(typ, expr, range)
    }

    fn value_of_loop(&self, loop_: ast::Loop) -> hir::ExpressionId {
        let mut scope = BlockScope::new(self.scope);
        let stmts = self.statements(loop_.statements, &mut scope);

        let typ = hir::Type::Builtin(hir::BuiltinType::Void);
        let expr = hir::Expression::Loop(stmts);

        self.tree_mut().push_expression(typ, expr, loop_.span())
    }

    fn value_of_prefix_operator(&self, 
        op: ast::PrefixOperator,
        expr: ast::ExpressionId,
        range: com::Range,
    )
        -> hir::ExpressionId
    {
        use self::ast::PrefixOperator as O;
        use self::hir::BuiltinFunction as F;

        let arg = self.value_of_expr(expr);
        let arguments = hir::Tuple {
            fields: self.tree_mut().push_expressions([arg].iter().cloned()),
            names: hir::Id::empty(),
        };

        let op = match op {
            O::Not => F::Not,
        };

        let typ = hir::Type::unresolved();
        let expr = hir::Expression::Call(hir::Callable::Builtin(op), arguments);

        let result = self.tree_mut().push_expression(typ, expr, range);
        self.context.link_gvns(&[result.into(), arg.into()]);
        result
    }

    fn value_of_tuple(&self, tup: ast::Tuple<ast::Expression>)
        -> hir::ExpressionId
    {
        let expr = self.tuple_of(
            self.ast_tree.get_expression_ids(tup.fields),
            self.ast_tree.get_identifiers(tup.names),
            |v| self.value_of(v),
            |e| self.tree_mut().push_expressions(e.iter().cloned()),
        );
        let typ = self.tuple_type_of(
            expr.names,
            |tree| tree.get_expressions(expr.fields),
            |e| self.tree().get_expression_type_id(e),
        );

        let fields = expr.fields;
        let typ = hir::Type::Tuple(typ);
        let expr = hir::Expression::Tuple(expr);

        let result = self.tree_mut().push_expression(typ, expr, tup.span());
        self.link_expressions(result.into(), fields);
        result
    }

    fn value_of_variable(&self, var: ast::VariableIdentifier)
        -> hir::ExpressionId
    {
        let typ = hir::Type::unresolved();
        let expr =
            self.scope
                .lookup_binding(var.into())
                .map(|name| {
                    let gvn =
                        self.context
                            .lookup_value(name)
                            .unwrap_or_default();
                    hir::Expression::Ref(name, gvn)
                })
                .unwrap_or(hir::Expression::UnresolvedRef(var.into()));

        let result = self.tree_mut().push_expression(typ, expr, var.span());

        if let hir::Expression::Ref(_, gvn) = expr {
            self.context.link_gvns(&[result.into(), gvn]);
            self.link_gvn_types(result.into(), Relation::Identical(gvn));
        }

        result
    }

    fn rescope<'b>(&self, scope: &'b dyn Scope) -> SymbolMapper<'b>
        where 'a: 'b
    {
        SymbolMapper {
            scope,
            registry: self.registry,
            context: self.context,
            ast_tree: self.ast_tree,
            tree: self.tree,
        }
    }

    fn tree(&self) -> cell::Ref<'a, hir::Tree> { self.tree.borrow() }

    fn tree_mut(&self) -> cell::RefMut<'a, hir::Tree> { self.tree.borrow_mut() }

    fn convert_callable(&self, candidate: &CallableCandidate) -> hir::Callable {
        use self::CallableCandidate as C;

        match candidate {
            C::Builtin(f) => hir::Callable::Builtin(*f),
            C::Function(f) => hir::Callable::Function(*f),
            C::Unknown(name) => hir::Callable::Unknown(*name),
            C::Unresolved(candidates) => {
                let callables: Vec<_> = candidates.iter()
                    .map(|c| self.convert_callable(c))
                    .collect();
                let callables = self.tree_mut().push_callables(callables);
                hir::Callable::Unresolved(callables)
            },
        }
    }

    fn link_expressions(&self, gvn: hir::Gvn, exprs: hir::Id<[hir::ExpressionId]>) {
        for &e in self.tree().get_expressions(exprs) {
            self.context.link_gvns(&[gvn, e.into()]);
        }
    }

    fn link_patterns(&self, gvn: hir::Gvn, patterns: hir::Id<[hir::PatternId]>) {
        for &p in self.tree().get_patterns(patterns) {
            self.context.link_gvns(&[gvn, p.into()]);
        }
    }

    fn link_gvn_types(&self, gvn: hir::Gvn, rel: Relation<hir::Gvn>) {
        let ty = self.tree().get_gvn_type_id(gvn);
        let rel = rel.map(|gvn| self.tree().get_gvn_type_id(gvn));

        self.link_types(ty, rel);
    }

    fn link_types(&self, ty_id: hir::TypeId, rel_id: Relation<hir::TypeId>) {
        use self::hir::Type::*;

        self.context.link_types(ty_id, rel_id);

        let tree = self.tree();

        let ty = tree.get_type(ty_id);
        let related = tree.get_type(*rel_id.get());

        if let (Tuple(ty), Tuple(related)) = (ty, related) {
            let ty = tree.get_type_ids(ty.fields);
            let related = tree.get_type_ids(related.fields);

            if ty.len() == related.len() {
                for (ty, related) in ty.iter().zip(related.iter()) {
                    self.link_types(*ty, rel_id.map(|_| *related));
                }
            }
        }
    }

    fn array_of<'b, T: 'b, U, F: FnMut(&'b T) -> U>(
        &self,
        input: &'b [T],
        mut transformer: F,
    )
        -> Vec<U>
    {
        let mut result = Vec::with_capacity(input.len());

        for i in input {
            result.push(transformer(i));
        }

        result
    }

    fn tuple_of<T, U, F: FnMut(ast::Id<T>) -> U, I: FnOnce(&[U]) -> hir::Id<[U]>>(
        &self,
        fields: &[ast::Id<T>],
        names: &[ast::Identifier],
        mut transformer: F,
        inserter: I,
    )
        -> hir::Tuple<U>
    {
        debug_assert!(names.is_empty() || names.len() == fields.len());

        let fields = self.array_of(fields, |&id| transformer(id));
        let fields = inserter(&fields);

        let names = self.array_of(names, |&id| id.into());
        let names = self.tree_mut().push_names(names);

        hir::Tuple { fields, names }
    }

    fn tuple_type_of<T, A, E>(
        &self,
        names: hir::Id<[hir::ValueIdentifier]>,
        accessor: A,
        mut extractor: E,
    )
        -> hir::Tuple<hir::TypeId>
        where
            T: Copy,
            A: for<'b> FnOnce(&'b cell::Ref<'a, hir::Tree>) -> &'b [T],
            E: FnMut(T) -> hir::TypeId
    {
        let types: Vec<_> = {
            let tree = self.tree();
            accessor(&tree).iter().map(|e| extractor(*e)).collect()
        };
        let fields = self.tree_mut().push_type_ids(types);
        hir::Tuple { fields, names }
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::{cell, rc};

    use crate::basic::{com, mem};
    use crate::basic::com::Span;

    use crate::model::{ast, hir};
    use crate::model::hir::Registry;
    use crate::model::ast::builder::Factory as AstFactory;
    use crate::model::hir::builder::{Factory as HirFactory, RcModule, RcTree};
    use super::super::Context;
    use super::super::scp::{self, mocks::MockScope};

    #[test]
    fn value_block_empty() {
        let env = Env::new(b"{}");

        let ast = env.ast().expr().block_expression_less().range(0, 2).build();

        let hir = {
            let v = env.hir().value();
            v.block_expression_less().range(0, 2).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_builtin_boolean() {
        let env = Env::new(b"true");
        let e = env.ast().expr();
        let v = env.hir().value().bool_(true, 0);

        assert_eq!(
            env.value_of(e.bool_(true, 0)),
            env.expression(v)
        );
    }

    #[test]
    fn value_builtin_string() {
        let env = Env::new(b"'Hello, World!'");
        let e = env.ast().expr();
        let v = env.hir().value();
        let id = env.intern(b"Hello, World!");

        assert_eq!(
            env.value_of(e.literal(0, 15).push_text(1, 13).string().build()),
            env.expression(v.string(id, 0, 15))
        );
    }

    #[test]
    fn value_call_add() {
        let env = Env::new(b"1 + 2");
        let e = env.ast().expr();
        let v = env.hir().value();

        assert_eq!(
            env.value_of(e.bin_op(e.int(1, 0), e.int(2, 4)).build()),
            env.expression(v.call().push(v.int(1, 0)).push(v.int(2, 4)).build())
        );
    }

    #[test]
    fn value_call_basic() {
        let mut env = Env::new(b"basic(1, 2)    basic");

        let ast = {
            let e = env.ast().expr();

            e.function_call(e.var(0, 5), 5, 10)
                    .push(e.int(1, 6))
                    .push(e.int(2, 9))
                    .build()
        };

        let hir = {
            let f = env.hir();
            let (i, td, v) = (f.item(), f.type_module(), f.value());

            let basic = env.item_id(15, 5);
            let basic = env.insert_function(i.fun(basic, td.int()).build(), &[0]);

            v.call()
                .function(basic)
                .push(v.int(1, 6))
                .push(v.int(2, 9))
                .range(0, 11)
                .build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_constructor() {
        let mut env = Env::new(b"Rec   Rec");

        let ast = {
            let f = env.ast();
            let (e, t) = (f.expr(), f.type_());

            e.constructor(t.simple(0, 3)).build()
        };

        let hir = {
            let f = env.hir();
            let (i, t, v) = (f.item(), f.type_(), f.value());

            let name = env.item_id(6, 3);
            let rec = env.insert_record(i.rec(name).build(), &[0]);

            let rec = t.record(rec).build();
            v.constructor(rec).range(0, 3).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_constructor_arguments() {
        let mut env = Env::new(b"Rec(1)   Rec");

        let ast = {
            let f = env.ast();
            let (e, t) = (f.expr(), f.type_());

            e.constructor(t.simple(0, 3)).parens(3, 5).push(e.int(1, 4)).build()
        };

        let hir = {
            let f = env.hir();
            let (i, t, v) = (f.item(), f.type_(), f.value());

            let name = env.item_id(9, 3);
            let rec = env.insert_record(i.rec(name).build(), &[0]);

            let rec = t.record(rec).build();
            v.constructor(rec).push(v.int(1, 4)).range(0, 6).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_constructor_nested() {
        let mut env = Env::new(
            b":enum Simple { Unit }         Simple::Unit",
        );

        let ast = {
            let f = env.ast();
            let (e, t) = (f.expr(), f.type_());

            e.constructor(t.nested(38, 4).push(30, 6).build()).build()
        };

        let hir = {
            let f = env.hir();
            let (i, t, v) = (f.item(), f.type_(), f.value());

            let (simple, unit) = (env.item_id(6, 6), env.item_id(38, 4));
            let enum_ = env.insert_enum(i.enum_(simple).build(), &[30]);
            let rec = t.unresolved(unit)
                .push_component(hir::PathComponent::Enum(enum_, range(30, 6)))
                .build();

            v.constructor(rec).range(30, 12).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_if_else() {
        let env = Env::new(b":if true { 1 } :else { 0 }");
        let e = env.ast().expr();
        let v = env.hir().value();

        assert_eq!(
            env.value_of(e.if_else(
                e.bool_(true, 4),
                e.block(e.int(1, 11)).build(),
                e.block(e.int(0, 23)).build(),
            ).build()),
            env.expression(v.if_(
                v.bool_(true, 4),
                v.block(v.int(1, 11)).build(),
                v.block(v.int(0, 23)).build(),
            ).build())
        )
    }

    #[test]
    fn value_loop() {
        let env = Env::new(b":loop { }");

        let ast = env.ast().expr().loop_(0).build();
        let hir = env.hir().value().loop_().range(0, 9).build();

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_record_field_access() {
        let mut env = Env::new(b":rec Rec(Int); Rec(42).0");

        let ast = {
            let f = env.ast();
            let (e, t) = (f.expr(), f.type_());

            e.field_access(
                    e.constructor(t.simple(15, 3))
                        .parens(18, 21)
                        .push(e.int(42, 19))
                        .build(),
                )
                    .index(0)
                    .build()
        };

        let hir = {
            let f = env.hir();
            let (i, t, v) = (f.item(), f.type_(), f.value());

            let name = env.item_id(5, 3);

            let rec = i.rec(name).range(0, 14).push(hir::TypeId::int()).build();
            let rec = env.insert_record(rec, &[15]);
            let rec = t.record(rec).build();

            v.field_access(
                v.constructor(rec)
                    .push(v.int(42, 19))
                    .range(15, 7)
                    .build()
            ).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_return_basic() {
        let env = Env::new(b"{ :return 1; }");

        let ast = {
            let f = env.ast();
            let (e, s) = (f.expr(), f.stmt());

            e.block_expression_less()
                .push_stmt(s.ret().expr(e.int(1, 10)).build())
                .build()
        };

        let hir = {
            let f = env.hir();
            let (s, v) = (f.stmt(), f.value());

            v.block_expression_less().push(s.ret(v.int(1, 10))).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_return_if_and_else() {
        let env = Env::new(
            b":if true { :return 1; } :else { :return 2; }",
        );

        let ast = {
            let f = env.ast();
            let (e, s) = (f.expr(), f.stmt());

            e.if_else(
                e.bool_(true, 4),
                e.block_expression_less()
                    .push_stmt(s.ret().expr(e.int(1, 19)).build())
                    .build(),
                e.block_expression_less()
                    .push_stmt(s.ret().expr(e.int(2, 40)).build())
                    .build(),
            ).build()
        };

        let hir = {
            let f = env.hir();
            let (s, v) = (f.stmt(), f.value());

            v.if_(
                v.bool_(true, 4),
                v.block_expression_less().push(s.ret(v.int(1, 19))).build(),
                v.block_expression_less().push(s.ret(v.int(2, 40))).build(),
            ).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_set_basic() {
        let env = Env::new(b"{ :var a := 1; :set a := 2; a }");

        let ast = {
            let f = env.ast();
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(28, 1))
                    .push_stmt(s.var(p.var(7, 1), e.int(1, 12)).build())
                    .push_stmt(s.set(e.var(20, 1), e.int(2, 25)).build())
                    .build()
        };

        let hir = {
            let f = env.hir();
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = env.var_id(7, 1);

            let var = s.var(p.var(a), v.int(1, 12));
            let set = s.set(v.name_ref(a, 20).pattern(0).build(), v.int(2, 25));

            v.block(v.name_ref(a, 28).pattern(0).build()).push(var).push(set).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_set_field() {
        let env = Env::new(b"{ :var a := (1,); :set a.0 := 2; a }");

        let ast = {
            let f = env.ast();
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
            let f = env.hir();
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = env.var_id(7, 1);
            let pat = p.var(a);
            let tup =
                v.tuple()
                    .push(v.int(1, 13))
                    .range(12, 4)
                    .build();

            let var = s.var(pat, tup);
            let set = s.set(
                    v.field_access(v.name_ref(a, 23).pattern(0).build()).build(),
                    v.int(2, 30)
            );

            v.block(v.name_ref(a, 33).pattern(0).build()).push(var).push(set).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_tuple_field_access() {
        let env = Env::new(b"(42, 43).1");

        let e = env.ast().expr();
        let v = env.hir().value();

        assert_eq!(
            env.value_of(
                e.field_access(
                    e.tuple().push(e.int(42, 1)).push(e.int(43, 5)).build(),
                )
                    .index(1)
                    .build()
            ),
            env.expression(
                v.field_access(
                    v.tuple()
                        .push(v.int(42, 1))
                        .push(v.int(43, 5))
                        .range(0, 8)
                        .build()
                )
                    .index(1)
                    .type_(hir::Type::unresolved())
                    .build()
            )
        )
    }

    #[test]
    fn value_tuple_keyed_field_access() {
        let env = Env::new(b"(.x := 42, .y := 43).y");

        let ast = {
            let e = env.ast().expr();
            e.field_access(
                e.tuple()
                    .push(e.int(42, 7)).name(1, 2)
                    .push(e.int(43, 17)).name(11, 2)
                    .build(),
            )
                .name(20, 2)
                .build()
        };

        let hir = {
            let v = env.hir().value();
            let (x, y) = (env.field_id(1, 2), env.field_id(11, 2));
            v.field_access(
                v.tuple()
                    .push(v.int(42, 7)).name(x)
                    .push(v.int(43, 17)).name(y)
                    .range(0, 20)
                    .build()
            )
                .unresolved(env.field_id(20, 2))
                .build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir))
    }

    #[test]
    fn value_var_basic() {
        let env = Env::new(b"{ :var a := 1; :var b := 2; a + b }");

        let ast = {
            let f = env.ast();
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.bin_op(e.var(28, 1), e.var(32, 1)).build())
                .push_stmt(s.var(p.var(7, 1), e.int(1, 12)).build())
                .push_stmt(s.var(p.var(20, 1), e.int(2, 25)).build())
                .build()
        };

        let hir = {
            let f = env.hir();
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let (a, b) = (env.var_id(7, 1), env.var_id(20, 1));
            let set_a = s.var(p.var(a), v.int(1, 12));
            let set_b = s.var(p.var(b), v.int(2, 25));
            let add = v.call()
                .push(v.name_ref(a, 28).pattern(0).build())
                .push(v.name_ref(b, 32).pattern(1).build())
                .build();

            v.block(add).push(set_a).push(set_b).build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_var_ignored() {
        let env = Env::new(b"{ :var a := 1; :var _ := 2; a }");

        let ast = {
            let f = env.ast();
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            e.block(e.var(28, 1))
                .push_stmt(s.var(p.var(7, 1), e.int(1, 12)).build())
                .push_stmt(s.var(p.ignored(20), e.int(2, 25)).build())
                .build()
        };

        let hir = {
            let f = env.hir();
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let a = env.var_id(7, 1);
            let set_a = s.var(p.var(a), v.int(1, 12));
            let set_i = s.var(p.ignored(20), v.int(2, 25));

            v.block(v.name_ref(a, 28).pattern(0).build())
                .push(set_a)
                .push(set_i)
                .build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_var_pattern_constructor() {
        let mut env = Env::new(
            b":rec Some(Int); { :var Some(a) := Some(1); a }",
        );

        let ast = {
            let f = env.ast();
            let (e, p, s, t) = (f.expr(), f.pat(), f.stmt(), f.type_());

            e.block(e.var(43, 1))
                .push_stmt(s.var(
                    p.constructor(t.simple(23, 4)).push(p.var(28, 1)).build(),
                    e.constructor(t.simple(34, 4)).push(e.int(1, 39)).build(),
                ).build())
                .build()
        };

        let hir = {
            let f = env.hir();
            let (p, s, t, v) = (f.pat(), f.stmt(), f.type_(), f.value());

            let a = env.var_id(28, 1);
            let some = env.item_id(5, 4);

            let rec = f.item().rec(some).push(hir::TypeId::int()).build();
            let rec = env.insert_record(rec, &[23, 34]);
            let rec = t.record(rec).build();

            let var = s.var(
                p.constructor(rec)
                    .push(p.var(a))
                    .range(23, 7)
                    .build(),
                v.constructor(rec)
                    .push(v.int(1, 39))
                    .range(34, 7)
                    .build(),
            );

            v.block(v.name_ref(a, 43).pattern(0).build())
                .push(var)
                .build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_var_pattern_keyed_constructor() {
        let mut env = Env::new(
            b":rec Some(.x: Int); { :var Some(.x: a) := Some(.x: 1); a }",
        );

        let ast = {
            let f = env.ast();
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
            let f = env.hir();
            let (p, s, t, v) = (f.pat(), f.stmt(), f.type_(), f.value());

            let a = env.var_id(36, 1);
            let some = env.item_id(5, 4);
            let x = env.var_id(11, 1);

            let rec = f.item().rec(some).push(hir::TypeId::int()).name(x).build();
            let rec = env.insert_record(rec, &[27, 42]);
            let rec = t.record(rec).build();

            let var = s.var(
                p.constructor(rec)
                    .push(p.var(a)).name(env.field_id(32, 2))
                    .range(27, 11)
                    .build(),
                v.constructor(rec)
                    .push(v.int(1, 51)).name(env.field_id(47, 2))
                    .range(42, 11)
                    .build(),
            );

            v.block(v.name_ref(a, 55).pattern(0).build())
                .push(var)
                .build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    #[test]
    fn value_var_pattern_tuple() {
        let env = Env::new(b"{ :var (a, b) := (1, 2); a }");

        let ast = {
            let f = env.ast();
            let (e, p, s) = (f.expr(), f.pat(), f.stmt());

            let pat = p.tuple().push(p.var(8, 1)).push(p.var(11, 1)).build();
            let expr = e.tuple().push(e.int(1, 18)).push(e.int(2, 21)).build();

            e.block(e.var(25, 1))
                    .push_stmt(s.var(pat, expr).build())
                    .build()
        };

        let hir = {
            let f = env.hir();
            let (p, s, v) = (f.pat(), f.stmt(), f.value());

            let (a, b) = (env.var_id(8, 1), env.var_id(11, 1));
            let var = s.var(
                p.tuple()
                    .push(p.var(a))
                    .push(p.var(b))
                    .range(7, 6)
                    .build(),
                v.tuple()
                    .push(v.int(1, 18))
                    .push(v.int(2, 21))
                    .range(17, 6)
                    .build(),
            );

            v.block(v.name_ref(a, 25).pattern(0).build())
                .push(var)
                .build()
        };

        assert_eq!(env.value_of(ast), env.expression(hir));
    }

    struct Env {
        scope: MockScope,
        context: Context,
        ast_resolver: ast::interning::Resolver,
        hir_resolver: hir::interning::Resolver,
        ast_module: ast::builder::RcModule,
        ast_tree: ast::builder::RcTree,
        module: RcModule,
        tree: RcTree,
    }

    impl Env {
        fn new(fragment: &[u8]) -> Env {
            let interner = rc::Rc::new(mem::Interner::new());
            Env {
                scope: MockScope::new(),
                context: Context::default(),
                ast_resolver: ast::interning::Resolver::new(fragment, interner.clone()),
                hir_resolver: hir::interning::Resolver::new(fragment, interner),
                ast_module: ast::builder::RcModule::default(),
                ast_tree: ast::builder::RcTree::default(),
                module: RcModule::default(),
                tree: RcTree::default(),
            }
        }

        fn ast(&self) -> AstFactory {
            AstFactory::new(self.ast_module.clone(), self.ast_tree.clone(), self.ast_resolver.clone())
        }

        fn hir(&self) -> HirFactory { HirFactory::new(self.module.clone(), self.tree.clone()) }

        fn expression(&self, e: hir::ExpressionId) -> hir::Tree {
            let mut tree = self.tree.borrow().clone();
            tree.set_root(e);

            println!("expression - {:#?}", tree);
            println!();

            tree
        }

        fn intern(&self, bytes: &[u8]) -> mem::InternId {
            self.hir_resolver.interner().insert(bytes)
        }

        fn insert_enum(&mut self, enum_: hir::EnumId, positions: &[usize])
            -> hir::EnumId
        {
            let name = self.module.borrow().get_enum(enum_).name;

            println!("Registering enum: {:?} -> {:?}", name, enum_);

            let len = name.span().length();

            for p in positions {
                let id = hir::ItemIdentifier(name.id(), range(*p, len));
                self.scope.types.insert(
                    id,
                    hir::Type::Enum(enum_, hir::Id::empty()),
                );
            }

            enum_
        }

        fn insert_function(
            &mut self,
            function: hir::FunctionSignature,
            positions: &[usize],
        )
            -> hir::FunctionId
        {
            let name = function.name;
            let function = self.module.borrow_mut().lookup_function(name)
                .expect("Function to be registered");
            println!("Registering function: {:?} -> {:?}", name, function);

            let len = name.span().length();

            for p in positions {
                let id = hir::ValueIdentifier(name.id(), range(*p, len));
                self.scope.callables.insert(id, scp::CallableCandidate::Function(function));
            }

            function
        }

        fn insert_record(&mut self, rec: hir::RecordId, positions: &[usize])
            -> hir::RecordId
        {
            let name = self.module.borrow().get_record(rec).name;

            println!("Registering record: {:?} -> {:?}", name, rec);

            let len = name.span().length();

            for p in positions {
                let id = hir::ItemIdentifier(name.id(), range(*p, len));
                self.scope.types.insert(
                    id,
                    hir::Type::Rec(rec, hir::Id::empty()),
                );
            }

            rec
        }

        fn field_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let field = range(pos + 1, len - 1);
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(field), range)
        }

        fn item_id(&self, pos: usize, len: usize) -> hir::ItemIdentifier {
            let range = range(pos, len);
            hir::ItemIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn var_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn value_of(&self, expr: ast::ExpressionId) -> hir::Tree {
            use std::ops::Deref;
            use super::SymbolMapper as SM;

            let ast_tree = self.ast_tree.borrow();
            let module = self.module.borrow();
            let tree = cell::RefCell::new(hir::Tree::new());

            println!("value_of - {:#?}", ast_tree);
            println!();

            let expr = SM::new(&self.scope, module.deref(), &self.context, &ast_tree, &tree)
                .value_of(expr);
            tree.borrow_mut().set_root(expr);

            println!("value_of - {:#?}", tree);
            println!();

            tree.into_inner()
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }

}
