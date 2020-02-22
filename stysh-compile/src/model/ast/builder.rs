//! Builder for the syntactic model (aka AST).

use std::{self, cell, rc};

use crate::basic::com::{self, Span, Store, MultiStore};

use crate::model::tt;
use crate::model::ast::*;
use crate::model::ast::interning::Resolver;

//
//  Module and Tree.
//
pub type RcModule = rc::Rc<cell::RefCell<Module>>;
pub type RcTree = rc::Rc<cell::RefCell<Tree>>;

//
//  High-Level Builders
//

/// Factory
#[derive(Clone)]
pub struct Factory {
    module: RcModule,
    tree: RcTree,
    resolver: Resolver,
}

/// ExprFactory
#[derive(Clone)]
pub struct ExprFactory {
    tree: RcTree,
    resolver: Resolver,
}

/// ItemFactory
#[derive(Clone)]
pub struct ItemFactory {
    module: RcModule,
    tree: RcTree,
    resolver: Resolver,
}

/// PatternFactory
#[derive(Clone)]
pub struct PatternFactory {
    tree: RcTree,
    resolver: Resolver,
}

/// StmtFactory
#[derive(Clone)]
pub struct StmtFactory {
    tree: RcTree,
}

//
//  Expression Builders
//

/// BinOpBuilder
#[derive(Clone)]
pub struct BinOpBuilder {
    tree: RcTree,
    op: BinaryOperator,
    pos: u32,
    left: ExpressionId,
    right: ExpressionId,
}

/// BlockBuilder
#[derive(Clone)]
pub struct BlockBuilder {
    tree: RcTree,
    statements: Vec<StatementId>,
    expr: Option<ExpressionId>,
    open: u32,
    close: u32,
}

/// FieldAccessBuilder
#[derive(Clone)]
pub struct FieldAccessBuilder {
    tree: RcTree,
    resolver: Resolver,
    accessed: ExpressionId,
    field: FieldIdentifier,
}

/// FunctionCallBuilder
#[derive(Clone)]
pub struct FunctionCallBuilder {
    tree: RcTree,
    callee: ExpressionId,
    arguments: TupleBuilder<Tree, Expression>,
}

/// IfElseBuilder
#[derive(Clone)]
pub struct IfElseBuilder {
    tree: RcTree,
    condition: ExpressionId,
    true_: ExpressionId,
    false_: ExpressionId,
    if_: u32,
    else_: u32,
}

/// LiteralBuilder
#[derive(Clone)]
pub struct LiteralBuilder {
    tree: RcTree,
    resolver: Resolver,
    literal: Literal,
    range: com::Range,
    fragments: Vec<tt::StringFragment>,
}

/// LoopBuilder
pub struct LoopBuilder {
    tree: RcTree,
    statements: Vec<StatementId>,
    loop_: u32,
    open: u32,
    close: u32,
}

/// MethodCallBuilder
#[derive(Clone)]
pub struct MethodCallBuilder {
    tree: RcTree,
    receiver: ExpressionId,
    method: FieldIdentifier,
    arguments: TupleBuilder<Tree, Expression>,
}

/// NestedVarBuilder
#[derive(Clone)]
pub struct NestedVarBuilder {
    name: VariableIdentifier,
    path: PathBuilder<Tree>,
}

/// PreOpBuilder
#[derive(Clone)]
pub struct PreOpBuilder {
    tree: RcTree,
    op: PrefixOperator,
    pos: u32,
    expr: ExpressionId,
}

//
//  Item Builders
//

/// EnumBuilder
#[derive(Clone)]
pub struct EnumBuilder {
    module: RcModule,
    resolver: Resolver,
    name: TypeIdentifier,
    keyword: u32,
    open: u32,
    close: u32,
    variants: Vec<InnerRecord>,
    commas: Vec<u32>,
}

/// ExtensionBuilder
#[derive(Clone)]
pub struct ExtensionBuilder {
    module: RcModule,
    name: TypeIdentifier,
    keyword: u32,
    open: u32,
    close: u32,
    functions: Vec<FunctionId>,
}

/// FunctionBuilder
#[derive(Clone)]
pub struct FunctionBuilder {
    module: RcModule,
    tree: RcTree,
    resolver: Resolver,
    name: VariableIdentifier,
    result: TypeId,
    body: ExpressionId,
    keyword: u32,
    open: u32,
    close: u32,
    arrow: u32,
    arguments: Vec<Argument>,
}

/// RecordBuilder
#[derive(Clone)]
pub struct RecordBuilder {
    module: RcModule,
    inner: InnerRecord,
    keyword: u32,
    semi_colon: u32,
}

//
//  Pattern Builders
//

//
//  Statement Builders
//

/// ReturnBuilder
#[derive(Clone)]
pub struct ReturnBuilder {
    tree: RcTree,
    expr: Option<ExpressionId>,
    ret: u32,
    semi: u32,
}

/// VariableReBindingBuilder
#[derive(Clone)]
pub struct VariableReBindingBuilder {
    tree: RcTree,
    left: ExpressionId,
    expr: ExpressionId,
    set: u32,
    bind: u32,
    semi: u32,
}

/// VariableBindingBuilder
#[derive(Clone)]
pub struct VariableBindingBuilder {
    tree: RcTree,
    pattern: PatternId,
    expr: ExpressionId,
    var: u32,
    colon: u32,
    bind: u32,
    semi: u32,
    type_: Option<TypeId>,
}

//
//  Low-Level Builders
//

/// ConstructorBuilder
#[derive(Clone)]
pub struct ConstructorBuilder<T> {
    type_: TypeId,
    arguments: TupleBuilder<Tree, T>,
}

/// TupleBuilder
#[derive(Clone)]
pub struct TupleBuilder<S, T> {
    store: rc::Rc<cell::RefCell<S>>,
    resolver: Resolver,
    fields: Vec<Id<T>>,
    commas: Vec<u32>,
    names: Vec<Identifier>,
    separators: Vec<u32>,
    open: u32,
    close: u32,
}

/// TypeFactory
#[derive(Clone)]
pub struct TypeFactory<S> {
    store: rc::Rc<cell::RefCell<S>>,
    resolver: Resolver,
}

/// NestedTypeBuilder
#[derive(Clone)]
pub struct NestedTypeBuilder<S> {
    name: TypeIdentifier,
    path: PathBuilder<S>,
}

/// PathBuilder
#[derive(Clone)]
pub struct PathBuilder<S> {
    store: rc::Rc<cell::RefCell<S>>,
    resolver: Resolver,
    components: Vec<Identifier>,
    colons: Vec<u32>,
}

//
//  Implementations of Factory
//
impl Factory {
    /// Creates an instance.
    pub fn new(module: RcModule, tree: RcTree, resolver: Resolver) -> Factory {
        Factory { module, tree, resolver }
    }

    /// Creates a ExprFactory.
    pub fn expr(&self) -> ExprFactory {
        ExprFactory::new(self.tree.clone(), self.resolver.clone())
    }

    /// Creates an ItemFactory.
    pub fn item(&self) -> ItemFactory {
        ItemFactory::new(self.module.clone(), self.tree.clone(), self.resolver.clone())
    }

    /// Creates a PatternFactory.
    pub fn pat(&self) -> PatternFactory {
        PatternFactory::new(self.tree.clone(), self.resolver.clone())
    }

    /// Creates a StmtFactory.
    pub fn stmt(&self) -> StmtFactory { StmtFactory::new(self.tree.clone()) }

    /// Creates a TupleBuilder.
    pub fn module_tuple<T>(&self) -> TupleBuilder<Module, T> {
        TupleBuilder::new(self.module.clone(), self.resolver.clone())
    }

    /// Creates a TupleBuilder.
    pub fn tuple<T>(&self) -> TupleBuilder<Tree, T> {
        TupleBuilder::new(self.tree.clone(), self.resolver.clone())
    }

    /// Creates an Expression TupleBuilder.
    pub fn expr_tuple(&self) -> TupleBuilder<Tree, Expression> {
        self.tuple()
    }

    /// Creates a TypeFactory.
    pub fn type_(&self) -> TypeFactory<Tree> {
        TypeFactory::new(self.tree.clone(), self.resolver.clone())
    }

    /// Creates a TypeFactory.
    pub fn type_module(&self) -> TypeFactory<Module> {
        TypeFactory::new(self.module.clone(), self.resolver.clone())
    }
}

//
//  Implementations of Expr builders
//
impl ExprFactory {
    /// Creates a new instance.
    pub fn new(tree: RcTree, resolver: Resolver) -> ExprFactory {
        ExprFactory { tree, resolver }
    }

    /// Creates a BinOpBuilder, defaults to Plus.
    pub fn bin_op(&self, left: ExpressionId, right: ExpressionId)
        -> BinOpBuilder
    {
        BinOpBuilder::new(self.tree.clone(), left, right)
    }

    /// Creates a BlockBuilder.
    pub fn block(&self, expr: ExpressionId) -> BlockBuilder {
        BlockBuilder::new(self.tree.clone(), expr)
    }

    /// Creates an expression-less BlockBuilder.
    pub fn block_expression_less(&self) -> BlockBuilder {
        BlockBuilder::expression_less(self.tree.clone())
    }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, name: TypeId)
        -> ConstructorBuilder<Expression>
    {
        ConstructorBuilder::new(self.tree.clone(), self.resolver.clone(), name)
    }

    /// Creates a FieldAccess Expression.
    pub fn field_access(&self, accessed: ExpressionId)
        -> FieldAccessBuilder
    {
        FieldAccessBuilder::new(self.tree.clone(), self.resolver.clone(), accessed)
    }

    /// Creates a FunctionCallBuilder.
    pub fn function_call(&self, callee: ExpressionId, open: u32, close: u32)
        -> FunctionCallBuilder
    {
        FunctionCallBuilder::new(self.tree.clone(), self.resolver.clone(), callee, open, close)
    }

    /// Creates a IfElseBuilder.
    pub fn if_else(
        &self,
        cond: ExpressionId,
        true_: ExpressionId,
        false_: ExpressionId,
    )
        -> IfElseBuilder
    {
        IfElseBuilder::new(self.tree.clone(), cond, true_, false_)
    }

    /// Creates a LiteralBuilder.
    pub fn literal(&self, pos: u32, len: u32) -> LiteralBuilder {
        LiteralBuilder::new(self.tree.clone(), self.resolver.clone(), pos, len)
    }

    /// Shortcut: creates a Bool Lit Expression.
    pub fn bool_(&self, value: bool, pos: u32) -> ExpressionId {
        self.literal(pos, if value { 4 } else { 5 }).bool_(value).build()
    }

    /// Shortcut: creates an Integral Lit Expression.
    pub fn int(&self, value: i64, pos: u32) -> ExpressionId {
        let mut len = 1;
        let mut n = value;
        if n < 0 {
            len += 1;
            n *= -1;
        }
        while n > 10 {
            len += 1;
            n /= 10;
        }
        self.literal(pos, len).integral(value).build()
    }

    /// Creates a Loop.
    pub fn loop_(&self, pos: u32) -> LoopBuilder {
        LoopBuilder::new(self.tree.clone(), pos)
    }

    /// Creates a MethodCallBuilder.
    pub fn method_call(&self, receiver: ExpressionId, open: u32, close: u32)
        -> MethodCallBuilder
    {
        MethodCallBuilder::new(self.tree.clone(), self.resolver.clone(), receiver, open, close)
    }

    /// Creates a nested Var Expression.
    pub fn nested(&self, pos: u32, len: u32) -> NestedVarBuilder {
        NestedVarBuilder::new(self.tree.clone(), self.resolver.clone(), pos, len)
    }

    /// Creates a PreOpBuilder, defaults to Not.
    pub fn pre_op(&self, expr: ExpressionId) -> PreOpBuilder {
        PreOpBuilder::new(self.tree.clone(), expr)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<Tree, Expression> {
        TupleBuilder::new(self.tree.clone(), self.resolver.clone())
    }

    /// Creates a Var Expression.
    pub fn var(&self, pos: u32, len: u32) -> ExpressionId {
        let name = var_id(&self.resolver, pos, len);
        let expr = Expression::Var(name, Path::empty());
        self.tree.borrow_mut().push_expression(expr, range(pos, len))
    }
}

impl BinOpBuilder {
    /// Creates an instance, default to Plus.
    pub fn new(
        tree: RcTree,
        left: ExpressionId,
        right: ExpressionId,
    )
        -> Self
    {
        BinOpBuilder {
            tree,
            op: BinaryOperator::Plus,
            pos: U32_NONE,
            left,
            right,
        }
    }

    /// Sets up the offset of the binary operator.
    pub fn offset(&mut self, pos: u32) -> &mut Self {
        self.pos = pos;
        self
    }

    /// Sets up a BinaryOperator.
    pub fn operator(&mut self, op: BinaryOperator) -> &mut Self {
        self.op = op;
        self
    }

    /// Sets up the And BinaryOperator.
    pub fn and(&mut self) -> &mut Self { self.operator(BinaryOperator::And) }

    /// Sets up the Different BinaryOperator.
    pub fn different(&mut self) -> &mut Self { self.operator(BinaryOperator::Different) }

    /// Sets up the Equal BinaryOperator.
    pub fn equal(&mut self) -> &mut Self { self.operator(BinaryOperator::Equal) }

    /// Sets up the FloorBy BinaryOperator.
    pub fn floor_by(&mut self) -> &mut Self { self.operator(BinaryOperator::FloorBy) }

    /// Sets up the GreaterThan BinaryOperator.
    pub fn greater_than(&mut self) -> &mut Self { self.operator(BinaryOperator::GreaterThan) }

    /// Sets up the GreaterThanOrEqual BinaryOperator.
    pub fn greater_than_or_equal(&mut self) -> &mut Self { self.operator(BinaryOperator::GreaterThanOrEqual) }

    /// Sets up the LessThan BinaryOperator.
    pub fn less_than(&mut self) -> &mut Self { self.operator(BinaryOperator::LessThan) }

    /// Sets up the LessThanOrEqual BinaryOperator.
    pub fn less_than_or_equal(&mut self) -> &mut Self { self.operator(BinaryOperator::LessThanOrEqual) }

    /// Sets up the Minus BinaryOperator.
    pub fn minus(&mut self) -> &mut Self { self.operator(BinaryOperator::Minus) }

    /// Sets up the Or BinaryOperator.
    pub fn or(&mut self) -> &mut Self { self.operator(BinaryOperator::Or) }

    /// Sets up the Plus BinaryOperator.
    pub fn plus(&mut self) -> &mut Self { self.operator(BinaryOperator::Plus) }

    /// Sets up the Times BinaryOperator.
    pub fn times(&mut self) -> &mut Self { self.operator(BinaryOperator::Times) }

    /// Sets up the Xor BinaryOperator.
    pub fn xor(&mut self) -> &mut Self { self.operator(BinaryOperator::Xor) }

    /// Creates a BinOp Expression.
    pub fn build(&self) -> ExpressionId {
        let left_range = self.tree.borrow().get_expression_range(self.left);
        let right_range = self.tree.borrow().get_expression_range(self.right);

        let pos = if self.pos == U32_NONE {
            left_range.end_offset() as u32 + 1
        } else {
            self.pos
        };

        let expr = Expression::BinOp(self.op, pos, self.left, self.right);
        self.tree.borrow_mut().push_expression(expr, left_range.extend(right_range))
    }
}

impl BlockBuilder {
    /// Creates a new instance, defaults the range.
    pub fn new(tree: RcTree, expr: ExpressionId) -> Self {
        BlockBuilder {
            tree,
            statements: vec!(),
            expr: Some(expr),
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Creates a new instance, defaults the range.
    pub fn expression_less(tree: RcTree) -> Self {
        BlockBuilder {
            tree,
            statements: vec!(),
            expr: None,
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Sets the range of the block.
    pub fn range(&mut self, pos: u32, len: u32) -> &mut Self {
        self.open = pos;
        self.close = pos + len - 1;
        self
    }

    /// Appends a statement.
    pub fn push_stmt(&mut self, stmt: StatementId) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Creates a Block.
    pub fn build(&self) -> ExpressionId {
        let stmt_range = |id| self.tree.borrow().get_statement(id).span();

        let expr_range = self.expr.map(|e| self.tree.borrow().get_expression_range(e));

        let ends = ends(&self.statements)
                .map(|(first, last)| (stmt_range(*first), stmt_range(*last)));

        let open = if self.open == U32_NONE {
            let range =
                ends.map(|e| e.0).unwrap_or_else(|| expr_range.unwrap());
            range.offset() as u32 - 2
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            let range = expr_range.unwrap_or_else(|| ends.unwrap().1);
            range.end_offset() as u32 + 1
        } else {
            self.close
        };

        let mut tree = self.tree.borrow_mut();
        let statements = tree.push_statement_ids(&self.statements);

        let expr = Block { statements, expression: self.expr, open, close, };
        tree.push_expression(expr.into(), range(open, close + 1 - open))
    }
}

impl FieldAccessBuilder {
    /// Creates a new instance, default to Named.
    pub fn new(
        tree: RcTree,
        resolver: Resolver,
        accessed: ExpressionId,
    ) -> Self {
        FieldAccessBuilder {
            tree,
            resolver,
            accessed,
            field: FieldIdentifier::Index(0, Default::default()),
        }
    }

    /// Sets the index of the field.
    pub fn index(&mut self, i: u16) -> &mut Self {
        let range = self.field.span();
        self.field = FieldIdentifier::Index(i, range);
        self
    }

    /// Sets the name of the field.
    pub fn name(&mut self, pos: u32, len: u32) -> &mut Self {
        self.field = field_id(&self.resolver, pos, len);
        self
    }

    /// Sets the offset of the field.
    pub fn range(&mut self, pos: u32, len: u32) -> &mut Self {
        self.field = self.field.with_range(range(pos, len));
        self
    }

    /// Creates a FieldAccess.
    pub fn build(&self) -> ExpressionId {
        let accessed_range = self.tree.borrow().get_expression_range(self.accessed);

        let field = if self.field.span() == Default::default() {
            let range = range(accessed_range.end_offset() as u32, 2);
            self.field.with_range(range)
        } else {
            self.field
        };

        let expr = FieldAccess { accessed: self.accessed, field, };
        self.tree.borrow_mut().push_expression(expr.into(), accessed_range.extend(field.span()))
    }
}

impl FunctionCallBuilder {
    /// Creates an instance.
    pub fn new(
        tree: RcTree,
        resolver: Resolver,
        callee: ExpressionId,
        open: u32,
        close: u32,
    )
        -> Self
    {
        let mut arguments = TupleBuilder::new(tree.clone(), resolver);
        arguments.parens(open, close);

        FunctionCallBuilder { tree, callee, arguments, }
    }

    /// Appends an argument.
    pub fn push(&mut self, arg: ExpressionId) -> &mut Self {
        self.arguments.push(arg);
        self
    }

    /// Overrides the position of the last comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        self.arguments.comma(pos);
        self
    }

    /// Creates a FunctionCall instance.
    pub fn build(&mut self) -> ExpressionId {
        let expr = FunctionCall {
            function: self.callee,
            arguments: self.arguments.build_tuple(),
        };
        let range = {
            let callee = self.tree.borrow().get_expression_range(self.callee);
            callee.extend(expr.arguments.span())
        };
        self.tree.borrow_mut().push_expression(expr.into(), range)
    }
}

impl IfElseBuilder {
    /// Creates an instance, defaults the offset of :if and :else.
    pub fn new(
        tree: RcTree,
        condition: ExpressionId,
        true_: ExpressionId,
        false_: ExpressionId,
    )
        -> Self
    {
        IfElseBuilder {
            tree,
            condition,
            true_,
            false_,
            if_: U32_NONE,
            else_: U32_NONE,
        }
    }

    /// Sets the offset of :if.
    pub fn if_(&mut self, pos: u32) -> &mut Self {
        self.if_ = pos;
        self
    }

    /// Sets the offset of :else.
    pub fn else_(&mut self, pos: u32) -> &mut Self {
        self.else_ = pos;
        self
    }

    /// Creates a IfElse.
    pub fn build(&self) -> ExpressionId {
        let false_range = self.tree.borrow().get_expression_range(self.false_);

        let if_ = if self.if_ == U32_NONE {
            self.tree.borrow().get_expression_range(self.condition).offset() as u32 - 4
        } else {
            self.if_
        };

        let else_ = if self.else_ == U32_NONE {
            false_range.offset() as u32 - 6
        } else {
            self.else_
        };

        let expr = IfElse {
            condition: self.condition,
            true_expr: self.true_,
            false_expr: self.false_,
            if_,
            else_,
        };

        let range = range(if_, false_range.end_offset() as u32 - if_);

        self.tree.borrow_mut().push_expression(expr.into(), range)
    }
}

impl LiteralBuilder {
    /// Creates an instance, defaults to an Integral.
    pub fn new(
        tree: RcTree,
        resolver: Resolver,
        pos: u32,
        len: u32,
    )
        -> Self
    {
        LiteralBuilder {
            tree,
            resolver,
            literal: Literal::Integral(0),
            range: range(pos, len),
            fragments: vec!(),
        }
    }

    /// Sets up a boolean.
    pub fn bool_(&mut self, value: bool) -> &mut Self {
        self.literal = Literal::Bool(value);
        self
    }

    /// Sets up bytes.
    pub fn bytes(&mut self) -> &mut Self {
        let id = self.resolver.resolve_range(self.range.skip_left(2).skip_right(1));
        self.literal = Literal::Bytes(Id::empty(), id);
        self
    }

    /// Sets up an integral.
    pub fn integral(&mut self, value: i64) -> &mut Self {
        self.literal = Literal::Integral(value);
        self
    }

    /// Sets up a string.
    pub fn string(&mut self) -> &mut Self {
        let id = self.resolver.resolve_range(self.range.skip_left(1).skip_right(1));
        self.literal = Literal::String(Id::empty(), id);
        self
    }

    /// Appends a Text fragment.
    pub fn push_text(&mut self, pos: u32, len: u32) -> &mut Self {
        self.fragments.push(
            tt::StringFragment::Text(
                tt::Token::new(tt::Kind::StringText, pos as usize, len as usize)
            )
        );
        self
    }

    /// Appends a SpecialCharacters fragment.
    pub fn push_special(&mut self) -> &mut Self {
        unimplemented!()
    }

    /// Appends an Interpolated fragment.
    pub fn push_interpolated(&mut self) -> &mut Self {
        unimplemented!()
    }

    /// Appends an Unexpected fragment.
    pub fn push_unexpected(&mut self, pos: u32, len: u32) -> &mut Self {
        self.fragments.push(
            tt::StringFragment::Unexpected(range(pos, len))
        );
        self
    }

    /// Creates a Literal.
    pub fn build(&self) -> ExpressionId {
        use self::Literal::*;

        let fragments = self.tree.borrow_mut().push_string_fragments(&self.fragments);

        let expr = match self.literal {
            Bytes(_, a) => Bytes(fragments, a),
            String(_, a) => String(fragments, a),
            other => other,
        };

        self.tree.borrow_mut().push_expression(expr.into(), self.range)
    }
}

impl LoopBuilder {
    /// Creates a new instance, defaults the range.
    pub fn new(tree: RcTree, loop_: u32) -> Self {
        LoopBuilder {
            tree,
            statements: vec!(),
            loop_: loop_,
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Sets the offset of the opening and closing braces.
    pub fn braces(&mut self, open: u32, close: u32) -> &mut Self {
        self.open = open;
        self.close = close;
        self
    }

    /// Appends a statement.
    pub fn push_stmt(&mut self, stmt: StatementId) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Creates a Loop.
    pub fn build(&self) -> ExpressionId {
        let open = if self.open == U32_NONE {
            self.loop_ + 6
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            if let Some(s) = self.statements.last() {
                self.tree.borrow().get_statement(*s).span().end_offset() as u32 + 1
            } else {
                open + 2
            }
        } else {
            self.close
        };

        let statements = self.tree.borrow_mut().push_statement_ids(&self.statements);

        let expr = Loop {
            statements,
            loop_: self.loop_,
            open: open,
            close: close,
        }.into();

        self.tree.borrow_mut().push_expression(expr, range(self.loop_, close + 1 - self.loop_))
    }
}

impl MethodCallBuilder {
    pub fn new(
        tree: RcTree,
        resolver: Resolver,
        receiver: ExpressionId,
        open: u32,
        close: u32,
    )
        -> Self
    {
        let mut arguments = TupleBuilder::new(tree.clone(), resolver);
        arguments.parens(open, close);

        let end = tree.borrow().get_expression_range(receiver).end_offset() as u32;

        MethodCallBuilder {
            tree,
            receiver,
            method: FieldIdentifier::Index(0, range(end, open - end)),
            arguments,
        }
    }

    /// Sets the index of the method.
    pub fn index(&mut self, i: u16) -> &mut Self {
        let range = self.method.span();
        self.method = FieldIdentifier::Index(i, range);
        self
    }

    /// Sets the name of the method.
    pub fn name(&mut self, pos: u32, len: u32) -> &mut Self {
        self.method = field_id(&self.arguments.resolver, pos, len);
        self
    }

    /// Sets the offset of the method.
    pub fn range(&mut self, pos: u32, len: u32) -> &mut Self {
        self.method = self.method.with_range(range(pos, len));
        self
    }

    /// Appends an argument.
    pub fn push(&mut self, arg: ExpressionId) -> &mut Self {
        self.arguments.push(arg);
        self
    }

    /// Overrides the position of the last comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        self.arguments.comma(pos);
        self
    }

    /// Creates a MethodCall instance.
    pub fn build(&mut self) -> ExpressionId {
        let expr = MethodCall {
            receiver: self.receiver,
            method: self.method,
            arguments: self.arguments.build_tuple(),
        };
        let range = {
            let receiver = self.tree.borrow().get_expression_range(self.receiver);
            let arguments = expr.arguments.span();
            receiver.extend(arguments)
        };
        self.tree.borrow_mut().push_expression(expr.into(), range)
    }
}

impl NestedVarBuilder {
    /// Creates an instance.
    pub fn new(
        store: RcTree,
        resolver: Resolver,
        pos: u32,
        len: u32,
    )
        -> Self
    {
        let name = var_id(&resolver, pos, len);
        Self::named(store, resolver, name)
    }

    /// Creates an instance, named.
    pub fn named(
        store: RcTree,
        resolver: Resolver,
        name: VariableIdentifier,
    )
        -> Self
    {
        let path = PathBuilder::new(store, resolver);
        NestedVarBuilder { name, path, }
    }

    /// Appends a path component.
    pub fn push(&mut self, pos: u32, len: u32) -> &mut Self {
        self.path.push(pos, len);
        self
    }

    /// Appends a path component.
    pub fn push_named(&mut self, name: Identifier) -> &mut Self {
        self.path.push_named(name);
        self
    }

    /// Overrides the position of the last inserted colon.
    pub fn colon(&mut self, pos: u32) -> &mut Self {
        self.path.colon(pos);
        self
    }

    /// Creates a Nested Variable.
    pub fn build(&self) -> ExpressionId {
        let path = self.path.build();
        let range = path.range(&*self.path.store.borrow())
            .map(|r| r.extend(self.name.1))
            .unwrap_or(self.name.1);
        self.path.store.borrow_mut()
            .push_expression(Expression::Var(self.name, path), range)
    }
}

impl PreOpBuilder {
    /// Creates an instance, default to Not.
    pub fn new(tree: RcTree, expr: ExpressionId) -> Self {
        PreOpBuilder {
            tree,
            op: PrefixOperator::Not,
            pos: U32_NONE,
            expr,
        }
    }

    /// Sets up the offset of the operator.
    pub fn offset(&mut self, pos: u32) -> &mut Self {
        self.pos = pos;
        self
    }

    /// Sets up a PrefixOperator.
    pub fn operator(&mut self, op: PrefixOperator) -> &mut Self {
        self.op = op;
        self
    }

    /// Sets up the Not PrefixOperator.
    pub fn not(&mut self) -> &mut Self { self.operator(PrefixOperator::Not) }

    /// Creates a PreOp Expression.
    pub fn build(&self) -> ExpressionId {
        let expr_range = self.tree.borrow().get_expression_range(self.expr);
        let pos = if self.pos == U32_NONE {
            expr_range.offset() as u32 - 5
        } else {
            self.pos
        };
        let expr = Expression::PreOp(self.op, pos, self.expr);
        let range = range(pos, 1).extend(expr_range);
        self.tree.borrow_mut().push_expression(expr, range)
    }
}

//
//  Implementations of Item builders
//
impl ItemFactory {
    /// Creates a new instance.
    pub fn new(module: RcModule, tree: RcTree, resolver: Resolver)
        -> ItemFactory
    {
        ItemFactory { module, tree, resolver }
    }

    /// Creates a EnumBuilder.
    pub fn enum_(&self, pos: u32, len: u32) -> EnumBuilder {
        EnumBuilder::new(self.module.clone(), self.resolver.clone(), pos, len)
    }

    /// Creates a EnumBuilder, named.
    pub fn enum_named(&self, name: TypeIdentifier) -> EnumBuilder {
        EnumBuilder::named(self.module.clone(), self.resolver.clone(), name)
    }

    /// Creates an ExtensionBuilder.
    pub fn extension(&self, pos: u32, len: u32) -> ExtensionBuilder {
        ExtensionBuilder::new(self.module.clone(), self.resolver.clone(), pos, len)
    }

    /// Creates an ExtensionBuilder, named.
    pub fn extension_named(&self, name: TypeIdentifier) -> ExtensionBuilder {
        ExtensionBuilder::named(self.module.clone(), name)
    }

    /// Creates a FunctionBuilder.
    pub fn function(
        &self,
        pos: u32,
        len: u32,
        result: TypeId,
        body: ExpressionId,
    )
        ->  FunctionBuilder
    {
        FunctionBuilder::new(self.module.clone(), self.tree.clone(), self.resolver.clone(), pos, len, result, body)
    }

    /// Creates a FunctionBuilder, named.
    pub fn function_named(
        &self,
        name: VariableIdentifier,
        result: TypeId,
        body: ExpressionId,
    )
        ->  FunctionBuilder
    {
        FunctionBuilder::named(self.module.clone(), self.tree.clone(), self.resolver.clone(), name, result, body)
    }

    /// Creates a wrapped RecordBuilder.
    pub fn record(&self, pos: u32, len: u32) -> RecordBuilder {
        RecordBuilder::new(self.module.clone(), self.resolver.clone(), pos, len)
    }

    /// Creates a wrapped RecordBuilder, named.
    pub fn record_named(&self, name: TypeIdentifier) -> RecordBuilder {
        RecordBuilder::named(self.module.clone(), name)
    }

    //  TODO: requires special type factory off RcModule.
}

impl EnumBuilder {
    /// Creates a new instance.
    pub fn new(
        module: RcModule,
        resolver: Resolver,
        pos: u32,
        len: u32,
    )
        -> Self
    {
        let name = type_id(&resolver, pos, len);
        Self::named(module, resolver, name)
    }

    /// Creates a new instance.
    pub fn named(
        module: RcModule,
        resolver: Resolver,
        name: TypeIdentifier,
    )
        -> Self
    {
        EnumBuilder {
            module,
            resolver,
            name,
            keyword: U32_NONE,
            open: U32_NONE,
            close: U32_NONE,
            variants: vec!(),
            commas: vec!(),
        }
    }

    /// Sets the position of the :enum keyword.
    pub fn keyword(&mut self, pos: u32) -> &mut Self {
        self.keyword = pos;
        self
    }

    /// Sets the position of the braces.
    pub fn braces(&mut self, open: u32, close: u32) -> &mut Self {
        self.open = open;
        self.close = close;
        self
    }

    /// Appends a Missing InnerRecord.
    pub fn push_missing(&mut self, pos: u32, len: u32) -> &mut Self {
        self.variants.push(InnerRecord::Missing(range(pos, len)));
        self.commas.push(U32_NONE);
        self
    }

    /// Appends a Tuple InnerRecord.
    pub fn push_tuple(
        &mut self,
        pos: u32,
        len: u32,
        fields: Tuple<Type>,
    )
        -> &mut Self
    {
        let name = type_id(&self.resolver, pos, len);
        self.push_named_tuple(name, fields)
    }

    /// Appends a Tuple InnerRecord with its ID.
    pub fn push_named_tuple(
        &mut self,
        name: TypeIdentifier,
        fields: Tuple<Type>,
    )
        -> &mut Self
    {
        self.variants.push(InnerRecord::Tuple(name, fields));
        self.commas.push(U32_NONE);
        self
    }

    /// Appends an Unexpected InnerRecord.
    pub fn push_unexpected(&mut self, pos: u32, len: u32) -> &mut Self {
        self.variants.push(InnerRecord::Unexpected(range(pos, len)));
        self.commas.push(U32_NONE);
        self
    }

    /// Appends a Unit InnerRecord.
    pub fn push_unit(&mut self, pos: u32, len: u32) -> &mut Self {
        let name = type_id(&self.resolver, pos, len);
        self.push_named_unit(name)
    }

    /// Appends a Unit InnerRecord with its ID.
    pub fn push_named_unit(&mut self, name: TypeIdentifier) -> &mut Self {
        self.variants.push(InnerRecord::Unit(name));
        self.commas.push(U32_NONE);
        self
    }

    /// Overrides the position of the last inserted comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        if let Some(c) = self.commas.last_mut() {
            *c = pos;
        }
        self
    }

    /// Creates an Enum.
    pub fn build(&self) -> EnumId {
        assert_eq!(self.variants.len(), self.commas.len());

        let mut commas = self.commas.clone();

        for (i, (c, v)) in commas.iter_mut().zip(self.variants.iter()).enumerate() {
            if *c != U32_NONE { continue; }

            let offset = if i + 1 == self.variants.len() { 1 } else { 0 };
            let pos = v.span().end_offset() - offset;

            *c = pos as u32;
        }

        let keyword = if self.keyword == U32_NONE {
            self.name.span().offset() as u32 - 6
        } else {
            self.keyword
        };

        let open = if self.open == U32_NONE {
            self.name.span().end_offset() as u32 + 1
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            commas.last().cloned().unwrap_or(open) + 2
        } else {
            self.close
        };

        let variants = self.module.borrow_mut().push_inner_records(&self.variants);
        let commas = self.module.borrow_mut().push_positions(&commas);

        let enum_ = Enum {
            name: self.name,
            variants,
            keyword,
            open,
            close,
            commas,
        };

        self.module.borrow_mut().push_enum(enum_)
    }
}

impl ExtensionBuilder {
    /// Creates a new instance.
    pub fn new(
        module: RcModule,
        resolver: Resolver,
        pos: u32,
        len: u32,
    )
        -> Self
    {
        let name = type_id(&resolver, pos, len);
        Self::named(module, name)
    }

    /// Creates a new instance.
    pub fn named(
        module: RcModule,
        name: TypeIdentifier,
    )
        -> Self
    {
        ExtensionBuilder {
            module,
            name,
            keyword: U32_NONE,
            open: U32_NONE,
            close: U32_NONE,
            functions: vec!(),
        }
    }

    /// Sets the position of the :extension keyword.
    pub fn keyword(&mut self, pos: u32) -> &mut Self {
        self.keyword = pos;
        self
    }

    /// Sets the position of the braces.
    pub fn braces(&mut self, open: u32, close: u32) -> &mut Self {
        self.open = open;
        self.close = close;
        self
    }

    /// Pushes a new function.
    pub fn push_function(&mut self, fun: FunctionId) -> &mut Self {
        self.functions.push(fun);
        self
    }

    /// Creates an Enum.
    pub fn build(&self) -> ExtensionId {
        let mut functions = self.functions.clone();
        functions.sort_unstable();

        let keyword = if self.keyword == U32_NONE {
            self.name.span().offset() as u32 - 5
        } else {
            self.keyword
        };

        let open = if self.open == U32_NONE {
            self.name.span().end_offset() as u32 + 1
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            functions.last()
                .map(|&fun| self.module.borrow().get_function_range(fun).end_offset() as u32 + 1)
                .unwrap_or(open + 2)
        } else {
            self.close
        };

        let function_ids = self.module.borrow_mut().push_function_ids(&functions);

        let ext = Extension {
            name: self.name,
            functions: function_ids,
            keyword,
            open,
            close,
        };

        let id = self.module.borrow_mut().push_extension(ext);

        for &fun in &functions {
            self.module.borrow_mut().set_function_extension(fun, id);
        }

        id
    }
}

impl FunctionBuilder {
    /// Creates a new instance.
    pub fn new(
        module: RcModule,
        tree: RcTree,
        resolver: Resolver,
        pos: u32,
        len: u32,
        result: TypeId,
        body: ExpressionId,
    )
        -> Self
    {
        let name = var_id(&resolver, pos, len);
        Self::named(module, tree, resolver, name, result, body)
    }

    /// Creates a new instance, named.
    pub fn named(
        module: RcModule,
        tree: RcTree,
        resolver: Resolver,
        name: VariableIdentifier,
        result: TypeId,
        body: ExpressionId,
    )
        -> Self
    {
        FunctionBuilder {
            module,
            tree,
            resolver,
            name,
            result,
            body,
            keyword: U32_NONE,
            open: U32_NONE,
            close: U32_NONE,
            arrow: U32_NONE,
            arguments: vec!(),
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, pos: u32, len: u32, type_: TypeId)
        -> &mut Self
    {
        let name = var_id(&self.resolver, pos, len);
        self.arguments.push(Argument {
            name,
            type_,
            colon: U32_NONE,
            comma: U32_NONE,
        });
        self
    }

    /// Sets the position of the :fun keyword.
    pub fn keyword(&mut self, pos: u32) -> &mut Self {
        self.keyword = pos;
        self
    }

    /// Sets the position of the parentheses.
    pub fn parens(&mut self, open: u32, close: u32) -> &mut Self {
        self.open = open;
        self.close = close;
        self
    }

    /// Sets the position of the arrow ->.
    pub fn arrow(&mut self, pos: u32) -> &mut Self {
        self.arrow = pos;
        self
    }

    /// Overrides the position of the colon in the last inserted argument.
    pub fn colon(&mut self, pos: u32) -> &mut Self {
        if let Some(a) = self.arguments.last_mut() {
            a.colon = pos;
        }
        self
    }

    /// Overrides the position of the comma in the last inserted argument.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        if let Some(a) = self.arguments.last_mut() {
            a.comma = pos;
        }
        self
    }

    /// Creates a Function instance.
    pub fn build(&self) -> FunctionId {
        let mut arguments = self.arguments.clone();

        for (i, a) in arguments.iter_mut().enumerate() {
            if a.colon == U32_NONE {
                a.colon = a.name.span().end_offset() as u32;
            }

            if a.comma == U32_NONE {
                let offset = if i + 1 == self.arguments.len() { 1 } else { 0 };
                let type_range = self.tree.borrow().get_type_range(a.type_);
                a.comma = type_range.end_offset() as u32 - offset;
            }
        }

        let keyword = if self.keyword == U32_NONE {
            self.name.span().offset() as u32 - 5
        } else {
            self.keyword
        };

        let open = if self.open == U32_NONE {
            self.name.span().end_offset() as u32
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            arguments.last()
                .map(|a| a.span().end_offset() as u32)
                .unwrap_or(open + 1)
        } else {
            self.close
        };

        let arrow = if self.arrow == U32_NONE { close + 2 } else { self.arrow };

        let arguments = self.tree.borrow_mut().push_arguments(&arguments);

        let fun = Function {
            name: self.name,
            result: self.result,
            keyword,
            open,
            close,
            arrow,
            arguments,
        };

        self.tree.borrow_mut().set_root(Root::Function(fun, self.body));

        self.module.borrow_mut().push_function(self.tree.borrow().clone())
    }
}

impl RecordBuilder {
    /// Creates a new instance, defaults to Unit.
    pub fn new(module: RcModule, resolver: Resolver, pos: u32, len: u32) -> Self {
        let name = type_id(&resolver, pos, len);
        Self::named(module, name)
    }

    /// Overrides the name, defaults to Unit.
    pub fn named(module: RcModule, t: TypeIdentifier) -> Self {
        let inner = InnerRecord::Unit(t);
        RecordBuilder {
            module,
            inner,
            keyword: U32_NONE,
            semi_colon: U32_NONE,
        }
    }

    /// Sets the position of the :rec keyword.
    pub fn keyword(&mut self, pos: u32) -> &mut Self {
        self.keyword = pos;
        self
    }

    /// Sets the position of the semi-colon.
    pub fn semi_colon(&mut self, pos: u32) -> &mut Self {
        self.semi_colon = pos;
        self
    }

    /// Sets up a Missing InnerRecord.
    pub fn missing(&mut self) -> &mut Self {
        let range = self.name().span();
        self.inner = InnerRecord::Missing(range);
        self
    }

    /// Sets up a Tuple InnerRecord.
    pub fn tuple(&mut self, fields: Tuple<Type>) -> &mut Self {
        let name = self.name();
        self.inner = InnerRecord::Tuple(name, fields);
        self
    }

    /// Sets up an Unexpected InnerRecord.
    pub fn unexpected(&mut self) -> &mut Self {
        let range = self.name().span();
        self.inner = InnerRecord::Unexpected(range);
        self
    }

    /// Sets up a Unit InnerRecord.
    pub fn unit(&mut self) -> &mut Self {
        let name = self.name();
        self.inner = InnerRecord::Unit(name);
        self
    }

    /// Creates a Record instance.
    pub fn build(&self) -> RecordId {
        let keyword = if self.keyword == U32_NONE {
            self.inner.span().offset() as u32 - 5
        } else {
            self.keyword
        };

        let semi_colon = if self.semi_colon == U32_NONE {
            self.inner.span().end_offset() as u32
        } else {
            self.semi_colon
        };

        let rec = Record {
            inner: self.inner,
            keyword: keyword,
            semi_colon: semi_colon,
        };

        self.module.borrow_mut().push_record(rec)
    }

    /// Extracts the range of the name.
    fn name(&self) -> TypeIdentifier {
        use self::InnerRecord::*;

        match self.inner {
            Missing(r) | Unexpected(r) => TypeIdentifier(Default::default(), r),
            Tuple(id, _) | Unit(id) => id,
        }
    }
}

//
//  Implementations of Pattern builds
//
impl PatternFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree, resolver: Resolver) -> PatternFactory {
        PatternFactory { tree, resolver }
    }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, name: TypeId) -> ConstructorBuilder<Pattern> {
        ConstructorBuilder::new(self.tree.clone(), self.resolver.clone(), name)
    }

    /// Creates an Ignored Pattern.
    pub fn ignored(&self, pos: u32) -> PatternId {
        let range = range(pos, 1);
        self.tree.borrow_mut().push_pattern(Pattern::Ignored(range), range)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<Tree, Pattern> {
        TupleBuilder::new(self.tree.clone(), self.resolver.clone())
    }

    /// Creates a Var Pattern.
    pub fn var(&self, pos: u32, len: u32) -> PatternId {
        let id = var_id(&self.resolver, pos, len);
        let range = range(pos, len);
        self.tree.borrow_mut().push_pattern(Pattern::Var(id), range)
    }
}

//
//  Implementations of Stmt builders
//
impl StmtFactory {
    /// Creates a new instance.
    pub fn new(tree: RcTree) -> Self { StmtFactory { tree } }

    /// Creates a ReturnBuilder.
    pub fn ret(&self) -> ReturnBuilder { ReturnBuilder::new(self.tree.clone()) }

    /// Creates a VariableReBindingBuilder.
    pub fn set(&self, left: ExpressionId, expr: ExpressionId)
        -> VariableReBindingBuilder
    {
        VariableReBindingBuilder::new(self.tree.clone(), left, expr)
    }

    /// Creates a VariableBindingBuilder.
    pub fn var(&self, pattern: PatternId, expr: ExpressionId)
        -> VariableBindingBuilder
    {
        VariableBindingBuilder::new(self.tree.clone(), pattern, expr)
    }
}

impl ReturnBuilder {
    /// Creates a new instance.
    pub fn new(tree: RcTree) -> Self {
        ReturnBuilder { tree, expr: None, ret: U32_NONE, semi: U32_NONE }
    }

    /// Sets up an expression.
    pub fn expr(&mut self, expr: ExpressionId) -> &mut Self {
        self.expr = Some(expr);
        self
    }

    /// Sets up a range.
    pub fn range(&mut self, pos: u32, len: u32) -> &mut Self {
        self.ret = pos;
        self.semi = len - 1 - self.ret;
        self
    }

    /// Creates a Return.
    pub fn build(&self) -> StatementId {
        let expr_range = self.expr.map(|id| self.tree.borrow().get_expression_range(id));

        let ret = if self.ret == U32_NONE {
            expr_range.unwrap().offset() as u32 - 8
        } else {
            self.ret
        };

        let semi = if self.semi == U32_NONE {
            expr_range.unwrap().end_offset() as u32
        } else {
            self.semi
        };

        let stmt = Return { expr: self.expr, ret: ret, semi: semi, };
        self.tree.borrow_mut().push_statement(stmt.into())
    }
}

impl VariableReBindingBuilder {
    /// Creates a new instance.
    pub fn new(tree: RcTree, left: ExpressionId, expr: ExpressionId) -> Self {
        VariableReBindingBuilder {
            tree,
            left,
            expr,
            set: U32_NONE,
            bind: U32_NONE,
            semi: U32_NONE,
        }
    }

    /// Sets up the position of the :set keyword.
    pub fn set(&mut self, pos: u32) -> &mut Self {
        self.set = pos;
        self
    }

    /// Sets up the position of the bind.
    pub fn bind(&mut self, pos: u32) -> &mut Self {
        self.bind = pos;
        self
    }

    /// Sets up the position of the semi-colon.
    pub fn semi_colon(&mut self, pos: u32) -> &mut Self {
        self.semi = pos;
        self
    }

    /// Creates a VariableReBinding.
    pub fn build(&self) -> StatementId {
        let left_range = self.tree.borrow().get_expression_range(self.left);
        let expr_range = self.tree.borrow().get_expression_range(self.expr);

        let set = if self.set == U32_NONE {
            left_range.offset() as u32 - 5
        } else {
            self.set
        };

        let bind = if self.bind == U32_NONE {
            left_range.end_offset() as u32 + 1
        } else {
            self.bind
        };

        let semi = if self.semi == U32_NONE {
            expr_range.end_offset() as u32
        } else {
            self.semi
        };

        let stmt = VariableReBinding {
            left: self.left,
            expr: self.expr,
            set,
            bind,
            semi,
        };
        self.tree.borrow_mut().push_statement(stmt.into())
    }
}

impl VariableBindingBuilder {
    /// Creates a new instance.
    pub fn new(tree: RcTree, pattern: PatternId, expr: ExpressionId) -> Self {
        VariableBindingBuilder {
            tree,
            pattern,
            expr,
            var: U32_NONE,
            colon: U32_NONE,
            bind: U32_NONE,
            semi: U32_NONE,
            type_: None,
        }
    }

    /// Sets up the position of the :var keyword.
    pub fn var(&mut self, pos: u32) -> &mut Self {
        self.var = pos;
        self
    }

    /// Sets up the position of the colon.
    pub fn colon(&mut self, colon: u32) -> &mut Self {
        self.colon = colon;
        self
    }

    /// Sets up the type.
    pub fn type_(&mut self, type_: TypeId) -> &mut Self {
        self.type_ = Some(type_);
        self
    }

    /// Sets up the position of the bind.
    pub fn bind(&mut self, pos: u32) -> &mut Self {
        self.bind = pos;
        self
    }

    /// Sets up the position of the semi-colon.
    pub fn semi_colon(&mut self, pos: u32) -> &mut Self {
        self.semi = pos;
        self
    }

    /// Creates a VariableBinding.
    pub fn build(&self) -> StatementId {
        let pat_range = self.tree.borrow().get_pattern_range(self.pattern);
        let expr_range = self.tree.borrow().get_expression_range(self.expr);
        let typ_range = self.type_.map(|id| self.tree.borrow().get_type_range(id));

        let var = if self.var == U32_NONE {
            pat_range.offset() as u32 - 5
        } else {
            self.var
        };

        let colon = if self.colon == U32_NONE {
            if self.type_.is_some() { pat_range.end_offset() as u32 } else { 0 }
        } else {
            self.colon
        };

        let bind = if self.bind == U32_NONE {
            let r = typ_range.unwrap_or(pat_range);
            r.end_offset() as u32 + 1
        } else {
            self.bind
        };

        let semi = if self.semi == U32_NONE {
            expr_range.end_offset() as u32
        } else {
            self.semi
        };

        let stmt = VariableBinding {
            pattern: self.pattern,
            type_: self.type_,
            expr: self.expr,
            var,
            colon,
            bind,
            semi,
        };
        self.tree.borrow_mut().push_statement(stmt.into())
    }
}

//
//  Implementations of Low-Level builders
//

impl<T> ConstructorBuilder<T> {
    /// Creates a new instance.
    pub fn new(tree: RcTree, resolver: Resolver, name: TypeId) -> Self {
        ConstructorBuilder {
            type_: name,
            arguments: TupleBuilder::new(tree, resolver)
        }
    }

    /// Sets up parentheses.
    pub fn parens(&mut self, open: u32, close: u32) -> &mut Self {
        self.arguments.parens(open, close);
        self
    }

    /// Appends an argument.
    pub fn push(&mut self, arg: Id<T>) -> &mut Self {
        self.arguments.push(arg);
        self
    }

    /// Overrides the position of the last comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        self.arguments.comma(pos);
        self
    }

    /// Appends a name.
    pub fn name(&mut self, pos: u32, length: u32) -> &mut Self {
        self.arguments.name(pos, length);
        self
    }

    /// Overrides the position of the last inserted separator.
    pub fn separator(&mut self, pos: u32) -> &mut Self {
        self.arguments.separator(pos);
        self
    }
}

impl<T> ConstructorBuilder<T>
    where
        Tree: Store<T> + MultiStore<Id<T>>,
        T: Clone + From<Constructor<T>> + From<Tuple<T>>,
{
    /// Creates a Constructor.
    pub fn build(&self) -> Id<T> {
        let type_range = self.arguments.store.borrow().get_type_range(self.type_);

        let cons = Constructor {
            type_: self.type_,
            arguments: self.arguments.build_tuple(),
        };

        let range = if self.arguments.fields.is_empty() {
            type_range
        } else {
            type_range.extend(cons.arguments.span())
        };

        let mut store = self.arguments.store.borrow_mut();
        store.push(T::from(cons), range)
    }
}

impl<S, T> TupleBuilder<S, T> {
    /// Creates a new instance.
    pub fn new(store: rc::Rc<cell::RefCell<S>>, resolver: Resolver) -> Self {
        TupleBuilder {
            store,
            resolver,
            fields: vec!(),
            commas: vec!(),
            names: vec!(),
            separators: vec!(),
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Sets the position of the parentheses.
    pub fn parens(&mut self, open: u32, close: u32) -> &mut Self {
        debug_assert!(
            close >= open,
            "TupleBuilder::parens - open ({}) > close ({})",
            open, close,
        );

        self.open = open;
        self.close = close;
        self
    }

    /// Appends a field.
    pub fn push(&mut self, field: Id<T>) -> &mut Self {
        self.fields.push(field);
        self.commas.push(U32_NONE);
        self
    }

    /// Overrides the position of the last inserted comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        if let Some(c) = self.commas.last_mut() {
            *c = pos;
        }
        self
    }

    /// Appends a name.
    pub fn name(&mut self, pos: u32, length: u32) -> &mut Self {
        let range = range(pos, length);
        let id = self.resolver.resolve_range(range);
        self.names.push(Identifier(id, range));
        self.separators.push(U32_NONE);
        self
    }

    /// Overrides the position of the last inserted separator.
    pub fn separator(&mut self, pos: u32) -> &mut Self {
        if let Some(s) = self.separators.last_mut() {
            *s = pos;
        }
        self
    }
}

impl<S, T> TupleBuilder<S, T>
    where
        S: Store<T> + MultiStore<Id<T>> + MultiStore<Identifier> + MultiStore<u32>,
        T: Clone + From<Tuple<T>>,
{
    /// Creates a new Tuple instance.
    pub fn build(&self) -> Id<T> {
        let tup = self.build_tuple();
        let range = tup.span();

        let mut store = self.store.borrow_mut();
        store.push(T::from(tup), range)
    }

    /// Creates a new Tuple instance.
    pub fn build_tuple(&self) -> Tuple<T> {
        assert_eq!(self.fields.len(), self.commas.len());

        if self.fields.is_empty() {
            let (o, c) = if self.open == U32_NONE {
                (0, 0)
            } else {
                (self.open, self.close)
            };
            let mut result = Tuple::default();
            result.open = o;
            result.close = c;
            return result;
        }

        let mut store = self.store.borrow_mut();

        let names = &self.names;
        let mut separators = self.separators.clone();

        for (s, n) in separators.iter_mut().zip(names.iter()) {
            if *s != U32_NONE { continue; }

            *s = n.1.end_offset() as u32;
        }

        let fields = &self.fields;
        let mut commas = self.commas.clone();

        for (i, (c, f)) in commas.iter_mut().zip(fields.iter()).enumerate() {
            if *c != U32_NONE { continue; }

            let offset = if i + 1 == fields.len() { 1 } else { 0 };
            let pos = store.get_range(*f).end_offset() - offset;

            *c = pos as u32;
        }

        let open = if self.open == U32_NONE {
            names.first()
                .map(|r| r.1.offset())
                .or_else(|| fields.first().map(|f| store.get_range(*f).offset()))
                .map(|o| o as u32 - 1)
                .unwrap_or(0)
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            commas.last().cloned().unwrap_or(open) + 1
        } else {
            self.close
        };

        let fields = store.push_slice(fields);
        let commas = store.push_slice(&commas);
        let names = store.push_slice(names);
        let separators = store.push_slice(&separators);

        Tuple { fields, commas, names, separators, open, close }
    }
}

impl<S> TypeFactory<S>
    where
        S: Store<Type>
{
    /// Creates an instance.
    pub fn new(store: rc::Rc<cell::RefCell<S>>, resolver: Resolver) -> Self {
        TypeFactory { store, resolver }
    }

    /// Creates a Missing Type.
    pub fn missing(&self, pos: u32, len: u32) -> TypeId {
        let range = range(pos, len);
        self.store.borrow_mut().push(Type::Missing(range), range)
    }

    /// Creates a NestedTypeBuilder.
    pub fn nested(&self, pos: u32, len: u32) -> NestedTypeBuilder<S> {
        NestedTypeBuilder::new(self.store.clone(), self.resolver.clone(), pos, len)
    }

    /// Creates a Simple Type.
    pub fn simple(&self, pos: u32, len: u32) -> TypeId {
        let name = type_id(&self.resolver, pos, len);
        self.simple_named(name)
    }

    /// Creates a Simple Type with its ID.
    pub fn simple_named(&self, name: TypeIdentifier) -> TypeId {
        self.store.borrow_mut().push(Type::Simple(name), name.1)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<S, Type> {
        TupleBuilder::new(self.store.clone(), self.resolver.clone())
    }
}

impl<S> NestedTypeBuilder<S> {
    /// Creates an instance.
    pub fn new(
        store: rc::Rc<cell::RefCell<S>>,
        resolver: Resolver,
        pos: u32,
        len: u32,
    )
        -> Self
    {
        let name = type_id(&resolver, pos, len);
        Self::named(store, resolver, name)
    }

    /// Creates an instance, named.
    pub fn named(
        store: rc::Rc<cell::RefCell<S>>,
        resolver: Resolver,
        name: TypeIdentifier,
    )
        -> Self
    {
        let path = PathBuilder::new(store, resolver);
        NestedTypeBuilder {
            name,
            path,
        }
    }

    /// Appends a path component.
    pub fn push(&mut self, pos: u32, len: u32) -> &mut Self {
        self.path.push(pos, len);
        self
    }

    /// Appends a path component.
    pub fn push_named(&mut self, name: Identifier) -> &mut Self {
        self.path.push_named(name);
        self
    }

    /// Overrides the position of the last inserted colon.
    pub fn colon(&mut self, pos: u32) -> &mut Self {
        self.path.colon(pos);
        self
    }
}

impl<S> NestedTypeBuilder<S>
    where
        S: Store<Type> + MultiStore<Identifier> + MultiStore<u32>
{
    /// Creates a Nested Type.
    pub fn build(&self) -> TypeId {
        let path = self.path.build();
        let range = path.range(&*self.path.store.borrow())
            .map(|r| r.extend(self.name.1))
            .unwrap_or(self.name.1);
        self.path.store.borrow_mut().push(Type::Nested(self.name, path), range)
    }
}

impl<S> PathBuilder<S> {
    /// Creates an instance.
    pub fn new(
        store: rc::Rc<cell::RefCell<S>>,
        resolver: Resolver,
    )
        -> Self
    {
        PathBuilder { store, resolver, components: vec!(), colons: vec!(), }
    }

    /// Appends a path component.
    pub fn push(&mut self, pos: u32, len: u32) -> &mut Self {
        let range = range(pos, len);
        let id = self.resolver.resolve_range(range);
        self.push_named(Identifier(id, range))
    }

    /// Appends a path component.
    pub fn push_named(&mut self, name: Identifier) -> &mut Self {
        self.components.push(name);
        self.colons.push(name.span().end_offset() as u32);
        self
    }

    /// Overrides the position of the last inserted colon.
    pub fn colon(&mut self, pos: u32) -> &mut Self {
        if let Some(c) = self.colons.last_mut() {
            *c = pos;
        }
        self
    }
}

impl<S> PathBuilder<S>
    where
        S: Store<Type> + MultiStore<Identifier> + MultiStore<u32>
{
    /// Creates a Nested Type.
    pub fn build(&self) -> Path {
        let mut store = self.store.borrow_mut();
        let components = store.push_slice(&self.components);
        let colons = store.push_slice(&self.colons);

        Path { components, colons, }
    }
}

//
//  Implementation Details
//
const U32_NONE: u32 = std::u32::MAX;

fn ends<'a, T: 'a>(slice: &'a [T]) -> Option<(&'a T, &'a T)> {
    if !slice.is_empty() {
        Some((slice.first().unwrap(), slice.last().unwrap()))
    } else {
        None
    }
}

fn range(pos: u32, len: u32) -> com::Range { com::Range::new(pos as usize, len as usize) }

fn field_id(resolver: &Resolver, pos: u32, len: u32) -> FieldIdentifier {
    resolver.resolve_field_identifier(
        FieldIdentifier::Name(Identifier(Default::default(), range(pos, len)))
    )
}

fn type_id(resolver: &Resolver, pos: u32, len: u32) -> TypeIdentifier {
    resolver.resolve_type_identifier(
        TypeIdentifier(Default::default(), range(pos, len))
    )
}

fn var_id(resolver: &Resolver, pos: u32, len: u32) -> VariableIdentifier {
    resolver.resolve_variable_identifier(
        VariableIdentifier(Default::default(), range(pos, len))
    )
}
