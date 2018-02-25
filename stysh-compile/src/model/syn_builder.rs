//! Builder for the syntactic model (aka AST).

use std::{self, convert, marker};

use basic::{com, mem};
use basic::com::Span;

use model::tt;
use model::syn::*;

//
//  High-Level Builders
//

/// Factory
#[derive(Clone, Copy)]
pub struct Factory<'a> {
    arena: &'a mem::Arena,
}

/// ExprFactory
#[derive(Clone, Copy)]
pub struct ExprFactory<'a> {
    arena: &'a mem::Arena,
}

/// ItemFactory
#[derive(Clone, Copy)]
pub struct ItemFactory<'a> {
    arena: &'a mem::Arena,
}

/// PatternFactory
#[derive(Clone, Copy)]
pub struct PatternFactory<'a> {
    arena: &'a mem::Arena,
}

/// StmtFactory
#[derive(Clone, Copy)]
pub struct StmtFactory<'a>(marker::PhantomData<&'a ()>);

//
//  Expression Builders
//

/// BinOpBuilder
#[derive(Clone)]
pub struct BinOpBuilder<'a> {
    op: BinaryOperator,
    pos: u32,
    left: &'a Expression<'a>,
    right: &'a Expression<'a>,
}

/// BlockBuilder
#[derive(Clone)]
pub struct BlockBuilder<'a> {
    statements: mem::Array<'a, Statement<'a>>,
    expr: Option<&'a Expression<'a>>,
    open: u32,
    close: u32,
}

/// FieldAccessBuilder
#[derive(Clone, Copy)]
pub struct FieldAccessBuilder<'a> {
    accessed: &'a Expression<'a>,
    pos: usize,
    len: usize,
}

/// FunctionCallBuilder
#[derive(Clone)]
pub struct FunctionCallBuilder<'a> {
    callee: &'a Expression<'a>,
    arguments: TupleBuilder<'a, Expression<'a>>,
}

/// IfElseBuilder
#[derive(Clone, Copy)]
pub struct IfElseBuilder<'a> {
    condition: Expression<'a>,
    true_: Block<'a>,
    false_: Block<'a>,
    if_: u32,
    else_: u32,
}

/// LiteralBuilder
#[derive(Clone)]
pub struct LiteralBuilder<'a> {
    literal: Literal<'a>,
    fragments: mem::Array<'a, tt::StringFragment>,
}

/// LoopBuilder
pub struct LoopBuilder<'a> {
    statements: mem::Array<'a, Statement<'a>>,
    loop_: u32,
    open: u32,
    close: u32,
}

/// PreOpBuilder
#[derive(Clone, Copy)]
pub struct PreOpBuilder<'a> {
    op: PrefixOperator,
    pos: u32,
    expr: &'a Expression<'a>,
}

//
//  Item Builders
//

/// EnumBuilder
#[derive(Clone)]
pub struct EnumBuilder<'a> {
    name: TypeIdentifier,
    keyword: u32,
    open: u32,
    close: u32,
    variants: mem::Array<'a, InnerRecord<'a>>,
    commas: mem::Array<'a, u32>,
}

/// FunctionBuilder
#[derive(Clone)]
pub struct FunctionBuilder<'a> {
    name: VariableIdentifier,
    result: Type<'a>,
    body: Block<'a>,
    keyword: u32,
    open: u32,
    close: u32,
    arrow: u32,
    arguments: mem::Array<'a, Argument<'a>>,
}

/// RecordBuilder
#[derive(Clone, Copy)]
pub struct RecordBuilder<'a> {
    inner: InnerRecord<'a>,
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
#[derive(Clone, Copy)]
pub struct ReturnBuilder<'a> {
    expr: Option<Expression<'a>>,
    ret: u32,
    semi: u32,
}

/// VariableReBindingBuilder
#[derive(Clone, Copy)]
pub struct VariableReBindingBuilder<'a> {
    left: Expression<'a>,
    expr: Expression<'a>,
    set: u32,
    bind: u32,
    semi: u32,
}

/// VariableBindingBuilder
#[derive(Clone, Copy)]
pub struct VariableBindingBuilder<'a> {
    pattern: Pattern<'a>,
    expr: Expression<'a>,
    var: u32,
    colon: u32,
    bind: u32,
    semi: u32,
    type_: Option<Type<'a>>,
}

//
//  Low-Level Builders
//

/// ConstructorBuilder
#[derive(Clone)]
pub struct ConstructorBuilder<'a, T:'a> {
    type_: Type<'a>,
    arguments: TupleBuilder<'a, T>,
}

/// TupleBuilder
#[derive(Clone)]
pub struct TupleBuilder<'a, T: 'a> {
    fields: mem::Array<'a, T>,
    commas: mem::Array<'a, u32>,
    open: u32,
    close: u32,
}

/// TypeFactory
#[derive(Clone, Copy)]
pub struct TypeFactory<'a> {
    arena: &'a mem::Arena,
}

/// NestedTypeBuilder
#[derive(Clone)]
pub struct NestedTypeBuilder<'a> {
    name: TypeIdentifier,
    components: mem::Array<'a, TypeIdentifier>,
    colons: mem::Array<'a, u32>,
}

//
//  Implementations of Factory
//
impl<'a> Factory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Factory<'a> {
        Factory { arena: arena }
    }

    /// Creates a ExprFactory.
    pub fn expr(&self) -> ExprFactory<'a> { ExprFactory::new(self.arena) }

    /// Creates an ItemFactory.
    pub fn item(&self) -> ItemFactory<'a> { ItemFactory::new(self.arena) }

    /// Creates a PatternFactory.
    pub fn pat(&self) -> PatternFactory<'a> { PatternFactory::new(self.arena) }

    /// Creates a StmtFactory.
    pub fn stmt(&self) -> StmtFactory<'a> { StmtFactory::new(self.arena) }

    /// Creates a TupleBuilder.
    pub fn tuple<T: 'a>(&self) -> TupleBuilder<'a, T> {
        TupleBuilder::new(self.arena)
    }

    /// Creates an Expression TupleBuilder.
    pub fn expr_tuple(&self) -> TupleBuilder<'a, Expression<'a>> {
        self.tuple()
    }

    /// Creates a Type TupleBuilder.
    pub fn type_tuple(&self) -> TupleBuilder<'a, Type<'a>> { self.tuple() }

    /// Creates a TypeFactory.
    pub fn type_(&self) -> TypeFactory<'a> { TypeFactory::new(self.arena) }
}

//
//  Implementations of Expr builders
//
impl<'a> ExprFactory<'a> {
    /// Creates a new instance.
    pub fn new(arena: &'a mem::Arena) -> ExprFactory<'a> {
        ExprFactory { arena }
    }

    /// Creates a BinOpBuilder, defaults to Plus.
    pub fn bin_op(&self, left: Expression<'a>, right: Expression<'a>)
        -> BinOpBuilder<'a>
    {
        BinOpBuilder::new(self.arena, left, right)
    }

    /// Creates a BlockBuilder.
    pub fn block(&self, expr: Expression<'a>) -> BlockBuilder<'a> {
        BlockBuilder::new(self.arena, expr)
    }

    /// Creates a diverging BlockBuilder.
    pub fn block_div(&self) -> BlockBuilder<'a> {
        BlockBuilder::diverging(self.arena)
    }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, name: Type<'a>)
        -> ConstructorBuilder<'a, Expression<'a>>
    {
        ConstructorBuilder::new(self.arena, name)
    }

    /// Creates a FieldAccess Expression.
    pub fn field_access(&self, accessed: Expression<'a>)
        -> FieldAccessBuilder<'a>
    {
        FieldAccessBuilder::new(self.arena, accessed)
    }

    /// Creates a FunctionCallBuilder.
    pub fn function_call(&self, callee: Expression<'a>, open: u32, close: u32)
        -> FunctionCallBuilder<'a>
    {
        FunctionCallBuilder::new(self.arena, callee, open, close)
    }

    /// Creates a IfElseBuilder.
    pub fn if_else(
        &self,
        cond: Expression<'a>,
        true_: Block<'a>,
        false_: Block<'a>,
    )
        -> IfElseBuilder<'a>
    {
        IfElseBuilder::new(cond, true_, false_)
    }

    /// Creates a LiteralBuilder.
    pub fn literal(&self, pos: usize, len: usize) -> LiteralBuilder<'a> {
        LiteralBuilder::new(self.arena, (pos, len))
    }

    /// Shortcut: creates a Bool Lit Expression.
    pub fn bool_(&self, pos: usize, len: usize) -> Expression<'a> {
        assert!(len == 4 || len == 5, "Should be either 'true' or 'false'");
        self.literal(pos, len).bool_(len == 4).build()
    }

    /// Shortcut: creates an Integral Lit Expression.
    pub fn int(&self, pos: usize, len: usize) -> Expression<'a> {
        self.literal(pos, len).build()
    }

    /// Creates a Loop.
    pub fn loop_(&self, pos: u32) -> LoopBuilder<'a> {
        LoopBuilder::new(self.arena, pos)
    }

    /// Creates a PreOpBuilder, defaults to Not.
    pub fn pre_op(&self, expr: Expression<'a>) -> PreOpBuilder<'a> {
        PreOpBuilder::new(self.arena, expr)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<'a, Expression<'a>> {
        TupleBuilder::new(self.arena)
    }

    /// Creates a Var Expression.
    pub fn var(&self, pos: usize, len: usize) -> Expression<'a> {
        Expression::Var(VariableIdentifier(range((pos, len))))
    }
}

impl<'a> BinOpBuilder<'a> {
    /// Creates an instance, default to Plus.
    pub fn new(
        arena: &'a mem::Arena,
        left: Expression<'a>,
        right: Expression<'a>,
    )
        -> Self
    {
        BinOpBuilder {
            op: BinaryOperator::Plus,
            pos: U32_NONE,
            left: arena.insert(left),
            right: arena.insert(right),
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
    pub fn build(&self) -> Expression<'a> {
        let pos = if self.pos == U32_NONE {
            self.left.span().end_offset() as u32 + 1
        } else {
            self.pos
        };
        Expression::BinOp(self.op, pos, self.left, self.right)
    }
}

impl<'a> BlockBuilder<'a> {
    /// Creates a new instance, defaults the range.
    pub fn new(arena: &'a mem::Arena, expr: Expression<'a>) -> Self {
        BlockBuilder {
            statements: mem::Array::new(arena),
            expr: Some(arena.insert(expr)),
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Creates a new instance, defaults the range.
    pub fn diverging(arena: &'a mem::Arena) -> Self {
        BlockBuilder {
            statements: mem::Array::new(arena),
            expr: None,
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Sets the range of the block.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.open = pos as u32;
        self.close = (pos + len - 1) as u32;
        self
    }

    /// Appends a statement.
    pub fn push_stmt(&mut self, stmt: Statement<'a>) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Creates a Block.
    pub fn build(&self) -> Block<'a> {
        let ends =
            ends(self.statements.as_slice())
                .map(|e| (e.0.span(), e.1.span()));
        let expr_range = self.expr.map(|e| e.span());

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

        Block {
            statements: self.statements.clone().into_slice(),
            expression: self.expr,
            open: open,
            close: close,
        }
    }
}

impl<'a> FieldAccessBuilder<'a> {
    /// Creates a new instance.
    pub fn new(arena: &'a mem::Arena, accessed: Expression<'a>) -> Self {
        FieldAccessBuilder {
            accessed: arena.insert(accessed),
            pos: USIZE_NONE,
            len: USIZE_NONE,
        }
    }

    /// Sets the offset of the field.
    pub fn offset(&mut self, pos: usize) -> &mut Self {
        self.pos = pos;
        self
    }

    /// Sets the length of the field.
    pub fn length(&mut self, len: usize) -> &mut Self {
        self.len = len;
        self
    }

    /// Creates a FieldAccess.
    pub fn build<T: convert::From<FieldAccess<'a>>>(&self) -> T {
        let pos = if self.pos == USIZE_NONE {
            self.accessed.span().end_offset()
        } else {
            self.pos
        };

        let len = if self.len == USIZE_NONE { 2 } else { self.len };

        FieldAccess {
            accessed: self.accessed,
            field: FieldIdentifier(range((pos, len)))
        }.into()
    }
}

impl<'a> FunctionCallBuilder<'a> {
    /// Creates an instance.
    pub fn new(
        arena: &'a mem::Arena,
        callee: Expression<'a>,
        open: u32,
        close: u32,
    )
        -> Self
    {
        let mut arguments = TupleBuilder::new(arena);
        arguments.parens(open, close);

        FunctionCallBuilder {
            callee: arena.insert(callee),
            arguments: arguments,
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, arg: Expression<'a>) -> &mut Self {
        self.arguments.push(arg);
        self
    }

    /// Overrides the position of the last comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        self.arguments.comma(pos);
        self
    }

    /// Creates a FunctionCall instance.
    pub fn build<T: convert::From<FunctionCall<'a>>>(&mut self) -> T {
        FunctionCall {
            function: self.callee,
            arguments: self.arguments.build(),
        }.into()
    }
}

impl<'a> IfElseBuilder<'a> {
    /// Creates an instance, defaults the offset of :if and :else.
    pub fn new(
        condition: Expression<'a>,
        true_: Block<'a>,
        false_: Block<'a>,
    )
        -> Self
    {
        IfElseBuilder {
            condition: condition,
            true_: true_,
            false_: false_,
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
    pub fn build(&self) -> IfElse<'a> {
        let if_ = if self.if_ == U32_NONE {
            self.condition.span().offset() as u32 - 4
        } else {
            self.if_
        };

        let else_ = if self.else_ == U32_NONE {
            self.false_.span().offset() as u32 - 6
        } else {
            self.else_
        };

        IfElse {
            condition: self.condition,
            true_expr: self.true_,
            false_expr: self.false_,
            if_: if_,
            else_: else_,
        }
    }
}

impl<'a> LiteralBuilder<'a> {
    /// Creates an instance, defaults to an Integral.
    pub fn new(arena: &'a mem::Arena, r: (usize, usize)) -> Self {
        LiteralBuilder {
            literal: Literal::Integral(range(r)),
            fragments: mem::Array::new(arena),
        }
    }

    /// Sets up a boolean.
    pub fn bool_(&mut self, value: bool) -> &mut Self {
        let range = self.literal.span();
        self.literal = Literal::Bool(value, range);
        self
    }

    /// Sets up bytes.
    pub fn bytes(&mut self) -> &mut Self {
        let range = self.literal.span();
        self.literal = Literal::Bytes(&[], range);
        self
    }

    /// Sets up an integral.
    pub fn integral(&mut self) -> &mut Self {
        let range = self.literal.span();
        self.literal = Literal::Integral(range);
        self
    }

    /// Sets up a string.
    pub fn string(&mut self) -> &mut Self {
        let range = self.literal.span();
        self.literal = Literal::String(&[], range);
        self
    }

    /// Appends a Text fragment.
    pub fn push_text(&mut self, pos: usize, len: usize) -> &mut Self {
        self.fragments.push(
            tt::StringFragment::Text(
                tt::Token::new(tt::Kind::StringText, pos, len)
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
    pub fn push_unexpected(&mut self, pos: usize, len: usize) -> &mut Self {
        self.fragments.push(
            tt::StringFragment::Unexpected(range((pos, len)))
        );
        self
    }

    /// Creates a Literal.
    pub fn build<T: convert::From<Literal<'a>>>(&self) -> T {
        use self::Literal::*;

        match self.literal {
            Bytes(_, r) => Bytes(self.fragments.clone().into_slice(), r),
            String(_, r) => String(self.fragments.clone().into_slice(), r),
            other => other,
        }.into()
    }
}

impl<'a> LoopBuilder<'a> {
    /// Creates a new instance, defaults the range.
    pub fn new(arena: &'a mem::Arena, loop_: u32) -> Self {
        LoopBuilder {
            statements: mem::Array::new(arena),
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
    pub fn push_stmt(&mut self, stmt: Statement<'a>) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Creates a Loop.
    pub fn build(&self) -> Expression<'a> {
        let open = if self.open == U32_NONE {
            self.loop_ + 6
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            if let Some(s) = self.statements.last() {
                s.span().end_offset() as u32 + 1
            } else {
                open + 2
            }
        } else {
            self.close
        };

        Expression::Loop(self.statements.arena().insert(Loop {
            statements: self.statements.clone().into_slice(),
            loop_: self.loop_,
            open: open,
            close: close,
        }))
    }
}

impl<'a> PreOpBuilder<'a> {
    /// Creates an instance, default to Not.
    pub fn new(arena: &'a mem::Arena, expr: Expression<'a>) -> Self {
        PreOpBuilder {
            op: PrefixOperator::Not,
            pos: U32_NONE,
            expr: arena.insert(expr),
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
    pub fn build(&self) -> Expression<'a> {
        let pos = if self.pos == U32_NONE {
            self.expr.span().offset() as u32 - 5
        } else {
            self.pos
        };
        Expression::PreOp(self.op, pos, self.expr)
    }
}

//
//  Implementations of Item builders
//
impl<'a> ItemFactory<'a> {
    /// Creates a new instance.
    pub fn new(arena: &'a mem::Arena) -> ItemFactory<'a> {
        ItemFactory { arena }
    }

    /// Creates a EnumBuilder.
    pub fn enum_(&self, pos: usize, len: usize) -> EnumBuilder<'a> {
        EnumBuilder::new(self.arena, pos, len)
    }

    /// Creates a FunctionBuilder.
    pub fn function(
        &self,
        pos: usize,
        len: usize,
        result: Type<'a>,
        body: Block<'a>,
    )
        ->  FunctionBuilder<'a>
    {
        FunctionBuilder::new(self.arena, (pos, len), result, body)
    }

    /// Creates a wrapped RecordBuilder.
    pub fn record(&self, pos: usize, len: usize) -> RecordBuilder<'a> {
        RecordBuilder::new(pos, len)
    }
}

impl<'a> EnumBuilder<'a> {
    /// Creates a new instance.
    pub fn new(
        arena: &'a mem::Arena,
        pos: usize,
        len: usize,
    )
        -> Self
    {
        EnumBuilder {
            name: type_id(pos, len),
            keyword: U32_NONE,
            open: U32_NONE,
            close: U32_NONE,
            variants: mem::Array::new(arena),
            commas: mem::Array::new(arena),
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
    pub fn push_missing(&mut self, pos: usize, len: usize) -> &mut Self {
        self.variants.push(InnerRecord::Missing(range((pos, len))));
        self.commas.push(U32_NONE);
        self
    }

    /// Appends a Tuple InnerRecord.
    pub fn push_tuple(
        &mut self,
        pos: usize,
        len: usize,
        fields: Tuple<'a, Type<'a>>,
    )
        -> &mut Self
    {
        self.variants.push(InnerRecord::Tuple(type_id(pos, len), fields));
        self.commas.push(U32_NONE);
        self
    }

    /// Appends an Unexpected InnerRecord.
    pub fn push_unexpected(&mut self, pos: usize, len: usize) -> &mut Self {
        self.variants.push(InnerRecord::Unexpected(range((pos, len))));
        self.commas.push(U32_NONE);
        self
    }

    /// Appends a Unit InnerRecord.
    pub fn push_unit(&mut self, pos: usize, len: usize) -> &mut Self {
        self.variants.push(InnerRecord::Unit(type_id(pos, len)));
        self.commas.push(U32_NONE);
        self
    }

    /// Overrides the position of the last inserted comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        if let Some(c) = self.commas.as_slice_mut().last_mut() {
            *c = pos;
        }
        self
    }

    /// Creates an Enum.
    pub fn build<T: convert::From<Enum<'a>>>(&self) -> T {
        assert_eq!(self.variants.len(), self.commas.len());

        let variants = self.variants.clone().into_slice();
        let commas = self.commas.clone().into_slice();

        for (i, (c, v)) in commas.iter_mut().zip(variants.iter()).enumerate() {
            if *c != U32_NONE { continue; }

            let offset = if i + 1 == variants.len() { 1 } else { 0 };
            let pos = v.span().end_offset() - offset;

            *c = pos as u32;
        }

        let keyword = if self.keyword == U32_NONE {
            self.name.0.offset() as u32 - 6
        } else {
            self.keyword
        };

        let open = if self.open == U32_NONE {
            self.name.0.end_offset() as u32 + 1
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            commas.last().cloned().unwrap_or(open) + 2
        } else {
            self.close
        };

        Enum {
            name: self.name,
            variants: variants,
            keyword: keyword,
            open: open,
            close: close,
            commas: commas,
        }.into()
    }
}

impl<'a> FunctionBuilder<'a> {
    /// Creates a new instance.
    pub fn new(
        arena: &'a mem::Arena,
        name: (usize, usize),
        result: Type<'a>,
        body: Block<'a>,
    )
        -> Self
    {
        FunctionBuilder {
            name: VariableIdentifier(range(name)),
            result: result,
            body: body,
            keyword: U32_NONE,
            open: U32_NONE,
            close: U32_NONE,
            arrow: U32_NONE,
            arguments: mem::Array::new(arena),
        }
    }

    /// Appends an argument.
    pub fn push(
        &mut self,
        pos: usize,
        len: usize,
        type_: Type<'a>,
    )
        -> &mut Self
    {
        self.arguments.push(Argument {
            name: VariableIdentifier(range((pos, len))),
            type_: type_,
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
        if let Some(a) = self.arguments.as_slice_mut().last_mut() {
            a.colon = pos;
        }
        self
    }

    /// Overrides the position of the comma in the last inserted argument.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        if let Some(a) = self.arguments.as_slice_mut().last_mut() {
            a.comma = pos;
        }
        self
    }

    /// Creates a Function instance.
    pub fn build<T: convert::From<Function<'a>>>(&self) -> T {
        let arguments = self.arguments.clone().into_slice();

        for (i, a) in arguments.iter_mut().enumerate() {
            if a.colon == U32_NONE {
                a.colon = a.name.0.end_offset() as u32;
            }

            if a.comma == U32_NONE {
                let offset = if i + 1 == self.arguments.len() { 1 } else { 0 };
                a.comma = a.type_.span().end_offset() as u32 - offset;
            }
        }

        let keyword = if self.keyword == U32_NONE {
            self.name.0.offset() as u32 - 5
        } else {
            self.keyword
        };

        let open = if self.open == U32_NONE {
            self.name.0.end_offset() as u32
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

        let arrow =
            if self.arrow == U32_NONE { close + 2 } else { self.arrow };

        Function {
            name: self.name,
            result: self.result,
            body: self.body,
            keyword: keyword,
            open: open,
            close: close,
            arrow: arrow,
            arguments: arguments,
        }.into()
    }
}

impl<'a> RecordBuilder<'a> {
    /// Creates a new instance, defaults to Unit.
    pub fn new(pos: usize, len: usize) -> Self {
        let inner = InnerRecord::Unit(type_id(pos, len));
        RecordBuilder {
            inner: inner,
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
        let name = self.name();
        self.inner = InnerRecord::Missing(name);
        self
    }

    /// Sets up a Tuple InnerRecord.
    pub fn tuple(&mut self, fields: Tuple<'a, Type<'a>>) -> &mut Self {
        let name = self.name();
        self.inner = InnerRecord::Tuple(TypeIdentifier(name), fields);
        self
    }

    /// Sets up an Unexpected InnerRecord.
    pub fn unexpected(&mut self) -> &mut Self {
        let name = self.name();
        self.inner = InnerRecord::Unexpected(name);
        self
    }

    /// Sets up a Unit InnerRecord.
    pub fn unit(&mut self) -> &mut Self {
        let name = self.name();
        self.inner = InnerRecord::Unit(TypeIdentifier(name));
        self
    }

    /// Creates a Record instance.
    pub fn build<T: convert::From<Record<'a>>>(&self) -> T {
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

        Record {
            inner: self.inner,
            keyword: keyword,
            semi_colon: semi_colon,
        }.into()
    }

    /// Extracts the range of the name.
    fn name(&self) -> com::Range {
        use self::InnerRecord::*;

        match self.inner {
            Missing(r) => r,
            Tuple(id, _) => id.0,
            Unexpected(r) => r,
            Unit(id) => id.0
        }
    }
}

//
//  Implementations of Pattern builds
//
impl<'a> PatternFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> PatternFactory<'a> {
        PatternFactory { arena }
    }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, name: Type<'a>)
        -> ConstructorBuilder<'a, Pattern<'a>>
    {
        ConstructorBuilder::new(self.arena, name)
    }

    /// Creates an Ignored Pattern.
    pub fn ignored(&self, pos: usize) -> Pattern<'static> {
        Pattern::Ignored(self.id(pos, 1))
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<'a, Pattern<'a>> {
        TupleBuilder::new(self.arena)
    }

    /// Creates a Var Pattern.
    pub fn var(&self, pos: usize, len: usize) -> Pattern<'static> {
        Pattern::Var(self.id(pos, len))
    }

    fn id(&self, pos: usize, len: usize) -> VariableIdentifier {
        VariableIdentifier(range((pos, len)))
    }
}

//
//  Implementations of Stmt builders
//
impl<'a> StmtFactory<'a> {
    /// Creates a new instance.
    pub fn new(_: &'a mem::Arena) -> Self { StmtFactory(marker::PhantomData) }

    /// Creates a ReturnBuilder.
    pub fn ret(&self) -> ReturnBuilder<'a> { ReturnBuilder::new() }

    /// Creates a VariableReBindingBuilder.
    pub fn set(&self, left: Expression<'a>, expr: Expression<'a>)
        -> VariableReBindingBuilder<'a>
    {
        VariableReBindingBuilder::new(left, expr)
    }

    /// Creates a VariableBindingBuilder.
    pub fn var(&self, pattern: Pattern<'a>, expr: Expression<'a>)
        -> VariableBindingBuilder<'a>
    {
        VariableBindingBuilder::new(pattern, expr)
    }
}

impl<'a> ReturnBuilder<'a> {
    /// Creates a new instance.
    pub fn new() -> Self {
        ReturnBuilder { expr: None, ret: U32_NONE, semi: U32_NONE }
    }

    /// Sets up an expression.
    pub fn expr(&mut self, expr: Expression<'a>) -> &mut Self {
        self.expr = Some(expr);
        self
    }

    /// Sets up a range.
    pub fn range(&mut self, pos: u32, len: u32) -> &mut Self {
        self.ret = pos;
        self.semi = len - 1 - self.ret;
        self
    }

    pub fn build<T: convert::From<Return<'a>>>(&self) -> T {
        let ret = if self.ret == U32_NONE {
            self.expr.unwrap().span().offset() as u32 - 8
        } else {
            self.ret
        };

        let semi = if self.semi == U32_NONE {
            self.expr.unwrap().span().end_offset() as u32
        } else {
            self.semi
        };

        Return {
            expr: self.expr,
            ret: ret,
            semi: semi,
        }.into()
    }
}

impl<'a> VariableReBindingBuilder<'a> {
    /// Creates a new instance.
    pub fn new(left: Expression<'a>, expr: Expression<'a>) -> Self {
        VariableReBindingBuilder {
            left: left,
            expr: expr,
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
    pub fn build<T: convert::From<VariableReBinding<'a>>>(&self) -> T {
        let range = self.left.span();

        let set = if self.set == U32_NONE {
            range.offset() as u32 - 5
        } else {
            self.set
        };

        let bind = if self.bind == U32_NONE {
            range.end_offset() as u32 + 1
        } else {
            self.bind
        };

        let semi = if self.semi == U32_NONE {
            self.expr.span().end_offset() as u32
        } else {
            self.semi
        };

        VariableReBinding {
            left: self.left,
            expr: self.expr,
            set: set,
            bind: bind,
            semi: semi,
        }.into()
    }
}

impl<'a> VariableBindingBuilder<'a> {
    /// Creates a new instance.
    pub fn new(pattern: Pattern<'a>, expr: Expression<'a>) -> Self {
        VariableBindingBuilder {
            pattern: pattern,
            expr: expr,
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
    pub fn type_(&mut self, type_: Type<'a>) -> &mut Self {
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
    pub fn build<T: convert::From<VariableBinding<'a>>>(&self) -> T {
        let range = self.pattern.span();

        let var = if self.var == U32_NONE {
            range.offset() as u32 - 5
        } else {
            self.var
        };

        let colon = if self.colon == U32_NONE {
            if self.type_.is_some() { range.end_offset() as u32 } else { 0 }
        } else {
            self.colon
        };

        let bind = if self.bind == U32_NONE {
            let r = if let Some(t) = self.type_ { t.span() } else { range };
            r.end_offset() as u32 + 1
        } else {
            self.bind
        };

        let semi = if self.semi == U32_NONE {
            self.expr.span().end_offset() as u32
        } else {
            self.semi
        };

        VariableBinding {
            pattern: self.pattern,
            type_: self.type_,
            expr: self.expr,
            var: var,
            colon: colon,
            bind: bind,
            semi: semi,
        }.into()
    }
}

//
//  Implementations of Low-Level builders
//
impl<'a, T: 'a> ConstructorBuilder<'a, T> {
    /// Creates a new instance.
    pub fn new(arena: &'a mem::Arena, name: Type<'a>) -> Self {
        ConstructorBuilder {
            type_: name,
            arguments: TupleBuilder::new(arena)
        }
    }

    /// Sets up parentheses.
    pub fn parens(&mut self, open: u32, close: u32) -> &mut Self {
        self.arguments.parens(open, close);
        self
    }

    /// Appends an argument.
    pub fn push(&mut self, arg: T) -> &mut Self {
        self.arguments.push(arg);
        self
    }

    /// Overrides the position of the last comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        self.arguments.comma(pos);
        self
    }
}

impl<'a, T: 'a + Clone + Span> ConstructorBuilder<'a, T> {
    /// Creates a Constructor.
    pub fn build<U: convert::From<Constructor<'a, T>>>(&self) -> U {
        Constructor {
            type_: self.type_,
            arguments: self.arguments.build(),
        }.into()
    }
}

impl<'a, T: 'a> TupleBuilder<'a, T> {
    /// Creates a new instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        TupleBuilder {
            fields: mem::Array::new(arena),
            commas: mem::Array::new(arena),
            open: U32_NONE,
            close: U32_NONE,
        }
    }

    /// Sets the position of the parentheses.
    pub fn parens(&mut self, open: u32, close: u32) -> &mut Self {
        self.open = open;
        self.close = close;
        self
    }

    /// Appends a field.
    pub fn push(&mut self, field: T) -> &mut Self {
        self.fields.push(field);
        self.commas.push(U32_NONE);
        self
    }
    
    /// Overrides the position of the last inserted comma.
    pub fn comma(&mut self, pos: u32) -> &mut Self {
        if let Some(c) = self.commas.as_slice_mut().last_mut() {
            *c = pos;
        }
        self
    }
}

impl<'a, T: 'a + Clone + Span> TupleBuilder<'a, T> {
    /// Creates a new Tuple instance.
    pub fn build<U: convert::From<Tuple<'a, T>>>(&self) -> U {
        assert_eq!(self.fields.len(), self.commas.len());

        if self.fields.is_empty() {
            let (o, c) = if self.open == U32_NONE {
                (0, 0)
            } else {
                (self.open, self.close)
            };
            return
                Tuple { fields: &[], commas: &[], open: o, close: c }.into();
        }

        let fields = self.fields.clone().into_slice();
        let commas = self.commas.clone().into_slice();

        for (i, (c, f)) in commas.iter_mut().zip(fields.iter()).enumerate() {
            if *c != U32_NONE { continue; }

            let offset = if i + 1 == fields.len() { 1 } else { 0 };
            let pos = f.span().end_offset() - offset;

            *c = pos as u32;
        }

        let open = if self.open == U32_NONE {
            fields.first()
                .map(|f| f.span().offset() as u32 - 1)
                .unwrap_or(0)
        } else {
            self.open
        };

        let close = if self.close == U32_NONE {
            commas.last().cloned().unwrap_or(open) + 1
        } else {
            self.close
        };

        Tuple {
            fields: fields,
            commas: commas,
            open: open,
            close: close,
        }.into()
    }
}

impl<'a> TypeFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        TypeFactory { arena: arena }
    }

    /// Creates a Missing Type.
    pub fn missing(&self, pos: usize, len: usize) -> Type<'a> {
        Type::Missing(range((pos, len)))
    }

    /// Creates a NestedTypeBuilder.
    pub fn nested(&self, pos: usize, len: usize) -> NestedTypeBuilder<'a> {
        NestedTypeBuilder::new(self.arena, pos, len)
    }

    /// Creates a Simple Type.
    pub fn simple(&self, pos: usize, len: usize) -> Type<'a> {
        Type::Simple(type_id(pos, len))
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<'a, Type<'a>> {
        TupleBuilder::new(self.arena)
    }
}

impl<'a> NestedTypeBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena, pos: usize, len: usize) -> Self {
        NestedTypeBuilder {
            name: type_id(pos, len),
            components: mem::Array::new(arena),
            colons: mem::Array::new(arena),
        }
    }

    /// Appends a path component.
    pub fn push(&mut self, pos: usize, len: usize) -> &mut Self {
        self.components.push(type_id(pos, len));
        self.colons.push((pos + len) as u32);
        self
    }

    /// Overrides the position of the last inserted colon.
    pub fn colon(&mut self, pos: u32) -> &mut Self {
        if let Some(c) = self.colons.as_slice_mut().last_mut() {
            *c = pos;
        }
        self
    }

    /// Creates a Nested Type.
    pub fn build(&self) -> Type<'a> {
        let path = Path {
            components: self.components.clone().into_slice(),
            colons: self.colons.clone().into_slice(),
        };
        Type::Nested(self.name, path)
    }
}

//
//  Implementation Details
//
const U32_NONE: u32 = std::u32::MAX;
const USIZE_NONE: usize = std::usize::MAX;

fn ends<'a, T: 'a>(slice: &'a [T]) -> Option<(&'a T, &'a T)> {
    if !slice.is_empty() {
        Some((slice.first().unwrap(), slice.last().unwrap()))
    } else {
        None
    }
}

fn range(tup: (usize, usize)) -> com::Range { com::Range::new(tup.0, tup.1) }

fn type_id(pos: usize, len: usize) -> TypeIdentifier {
    TypeIdentifier(range((pos, len)))
}
