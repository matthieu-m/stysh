//! Expressions

use std::convert;

use basic::com::{self, Span};
use basic::mem::{self, InternId};

use model::ast::*;
use model::tt;

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expression<'a> {
    /// A binary operation.
    BinOp(BinaryOperator, u32, &'a Expression<'a>, &'a Expression<'a>),
    /// A block expression.
    Block(&'a Block<'a>),
    /// A constructor expression.
    Constructor(Constructor<'a, Expression<'a>>),
    /// A field access expression.
    FieldAccess(FieldAccess<'a>),
    /// A function call expression.
    FunctionCall(FunctionCall<'a>),
    /// A if expression.
    If(&'a IfElse<'a>),
    /// A literal.
    Lit(Literal<'a>),
    /// A loop.
    Loop(&'a Loop<'a>),
    /// A prefix unary operation.
    PreOp(PrefixOperator, u32, &'a Expression<'a>),
    /// A tuple.
    Tuple(Tuple<'a, Expression<'a>>),
    /// A variable identifier.
    Var(VariableIdentifier),
}

/// A Binary Operator such as `+` or `*`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BinaryOperator {
    /// The `:and` operator.
    And,
    /// The `!=` operator.
    Different,
    /// The `==` operator.
    Equal,
    /// The `//` operator.
    FloorBy,
    /// The `>` operator.
    GreaterThan,
    /// The `>=` operator.
    GreaterThanOrEqual,
    /// The `<` operator.
    LessThan,
    /// The `<=` operator.
    LessThanOrEqual,
    /// The `-` operator.
    Minus,
    /// The `:or` operator.
    Or,
    /// The `+` operator.
    Plus,
    /// The `*` operator.
    Times,
    /// The `:xor` operator.
    Xor,
}

/// A Block.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Block<'a> {
    /// Statements.
    pub statements: &'a [Statement<'a>],
    /// Last Expression.
    pub expression: Option<&'a Expression<'a>>,
    /// Offset of open brace.
    pub open: u32,
    /// Offset of close brace.
    pub close: u32,
}

/// A Field Access such .0 or .name.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FieldAccess<'a> {
    /// Record or Tuple accessed.
    pub accessed: &'a Expression<'a>,
    /// Field identifier.
    pub field: FieldIdentifier,
}

/// A Field Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum FieldIdentifier {
    /// Index of the field.
    Index(u16, com::Range),
    /// Interned ID of the name of the field.
    Name(InternId, com::Range),
}

/// A function call expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionCall<'a> {
    /// Function called.
    pub function: &'a Expression<'a>,
    /// Arguments.
    pub arguments: Tuple<'a, Expression<'a>>,
}

/// A if-else expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct IfElse<'a> {
    /// Condition.
    pub condition: Expression<'a>,
    /// Expression evaluated if condition evaluates to true.
    pub true_expr: Block<'a>,
    /// Expression evaluated if condition evaluates to false.
    pub false_expr: Block<'a>,
    /// Offset of :if.
    pub if_: u32,
    /// Offset of :else.
    pub else_: u32,
}

/// A Literal value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Literal<'a> {
    /// A boolean value.
    Bool(bool, com::Range),
    /// A bytes value.
    Bytes(&'a [StringFragment], &'a [u8], com::Range),
    /// An integral value.
    Integral(i64, com::Range),
    /// A string value.
    String(&'a [StringFragment], &'a [u8], com::Range),
}

/// A if-else expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Loop<'a> {
    /// Statements.
    pub statements: &'a [Statement<'a>],
    /// Offset of the :loop keyword.
    pub loop_: u32,
    /// Offset of open brace.
    pub open: u32,
    /// Offset of close brace.
    pub close: u32,
}

/// A Prefix Unary Operator such as `:not`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum PrefixOperator {
    /// The `:not` operator.
    Not,
}

/// A Value Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableIdentifier(pub InternId, pub com::Range);

//
//  Implementations
//
impl<'a> FunctionCall<'a> {
    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        self.arguments.comma(i)
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        self.arguments.parenthesis_open()
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        self.arguments.parenthesis_close()
    }
}

impl FieldIdentifier {
    /// Sets the InternId of the FieldIdentifier.
    pub fn with_id(self, id: InternId) -> Self {
        use self::FieldIdentifier::*;

        match self {
            Index(i, r) => Index(i, r),
            Name(_, r) => Name(id, r),
        }
    }

    /// Sets the range spanned by the FieldIdentifier.
    pub fn with_range(self, range: com::Range) -> Self {
        use self::FieldIdentifier::*;

        match self {
            Index(i, _) => Index(i, range),
            Name(n, _) => Name(n, range),
        }
    }
}

impl VariableIdentifier {
    /// Returns the InternId.
    pub fn id(&self) -> InternId { self.0 }

    /// Sets the InternId of the VariableIdentifier.
    pub fn with_id(self, id: InternId) -> Self {
        VariableIdentifier(id, self.1)
    }
}

//
//  Implementations of Span
//
impl<'a> Span for Expression<'a> {
    /// Returns the range spanned by the expression.
    fn span(&self) -> com::Range {
        use self::Expression::*;

        match *self {
            BinOp(_, _, left, right) => left.span().extend(right.span()),
            Block(b) => b.span(),
            Constructor(c) => c.span(),
            FieldAccess(f) => f.span(),
            FunctionCall(fun) => fun.span(),
            If(if_else) => if_else.span(),
            Lit(lit) => lit.span(),
            Loop(l) => l.span(),
            PreOp(_, pos, expr)
                => com::Range::new(pos as usize, 0).extend(expr.span()),
            Tuple(t) => t.span(),
            Var(v) => v.span(),
        }
    }
}

impl<'a> Span for Block<'a> {
    /// Returns the range spanned by the block.
    fn span(&self) -> com::Range {
        let end_offset = self.close + 1;
        com::Range::new(self.open as usize, (end_offset - self.open) as usize)
    }
}

impl<'a> Span for FieldAccess<'a> {
    /// Returns the range spanned by the constructor.
    fn span(&self) -> com::Range {
        self.accessed.span().extend(self.field.span())
    }
}

impl<'a> Span for FunctionCall<'a> {
    /// Returns the range spanned by the function call.
    fn span(&self) -> com::Range {
        self.function.span().extend(self.parenthesis_close().span())
    }
}

impl<'a> Span for IfElse<'a> {
    /// Returns the range spanned by the argument.
    fn span(&self) -> com::Range {
        let offset = self.if_ as usize;
        let end_offset = self.false_expr.span().end_offset();
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a> Span for Loop<'a> {
    /// Returns the range spanned by the argument.
    fn span(&self) -> com::Range {
        let offset = self.loop_ as usize;
        let end_offset = self.close as usize + 1;
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a> Span for Statement<'a> {
    /// Returns the range spanned by the statement.
    fn span(&self) -> com::Range {
        use self::Statement::*;

        match *self {
            Return(ret) => ret.span(),
            Set(set) => set.span(),
            Var(var) => var.span(),
        }
    }
}

impl<'a> Span for Literal<'a> {
    /// Returns the range spanned by the literal.
    fn span(&self) -> com::Range {
        use self::Literal::*;

        match *self {
            Bool(_, r) => r,
            Bytes(_, _, r) => r,
            Integral(_, r) => r,
            String(_, _, r) => r,
        }
    }
}

impl Span for FieldIdentifier {
    /// Returns the range spanned by the field identifier.
    fn span(&self) -> com::Range {
        use self::FieldIdentifier::*;

        match *self {
            Index(_, r) => r,
            Name(_, r) => r,
        }
    }
}

impl Span for VariableIdentifier {
    /// Returns the range spanned by the variable identifier.
    fn span(&self) -> com::Range { self.1 }
}

//
//  Implementations of CloneInto
//
impl<'a, 'target> mem::CloneInto<'target> for Literal<'a> {
    type Output = Literal<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Literal::*;

        match *self {
            Bool(b, r) => Bool(b, r),
            Bytes(f, b, r) => Bytes(
                arena.insert_slice(f),
                arena.insert_slice(b),
                r,
            ),
            Integral(i, r) => Integral(i, r),
            String(f, s, r) => String(
                arena.insert_slice(f),
                arena.insert_slice(s),
                r,
            ),
        }
    }
}

//
//  Implementations of From
//
impl<'a> convert::From<Constructor<'a, Expression<'a>>> for Expression<'a> {
    fn from(c: Constructor<'a, Expression<'a>>) -> Expression<'a> {
        Expression::Constructor(c)
    }
}

impl<'a> convert::From<FieldAccess<'a>> for Expression<'a> {
    fn from(f: FieldAccess<'a>) -> Expression<'a> {
        Expression::FieldAccess(f)
    }
}

impl<'a> convert::From<FunctionCall<'a>> for Expression<'a> {
    fn from(f: FunctionCall<'a>) -> Expression<'a> {
        Expression::FunctionCall(f)
    }
}

impl<'a> convert::From<Literal<'a>> for Expression<'a> {
    fn from(l: Literal<'a>) -> Expression<'a> {
        Expression::Lit(l)
    }
}

impl<'a> convert::From<Tuple<'a, Expression<'a>>> for Expression<'a> {
    fn from(t: Tuple<'a, Expression<'a>>) -> Expression<'a> {
        Expression::Tuple(t)
    }
}

impl convert::From<VariableIdentifier> for Expression<'static> {
    fn from(v: VariableIdentifier) -> Expression<'static> {
        Expression::Var(v)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use super::*;
    use model::ast::builder::Factory;

    #[test]
    fn range_expression_literal() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  "   1"
        assert_eq!(e.int(1, 3).span(), range(3, 1));
    }

    #[test]
    fn range_expression_binary_operator() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  "   1 + 1"
        assert_eq!(
            e.bin_op(e.int(1, 3), e.int(1, 7)).build().span(),
            range(3, 5)
        );
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
