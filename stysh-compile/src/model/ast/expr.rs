//! Expressions

use std::convert;

use crate::basic::com::{Range, Span};
use crate::basic::mem::InternId;

use crate::model::ast::*;

/// An ExpressionId.
pub type ExpressionId = Id<Expression>;

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expression {
    /// A binary operation.
    BinOp(BinaryOperator, u32, ExpressionId, ExpressionId),
    /// A block expression.
    Block(Block),
    /// A constructor expression.
    Constructor(Constructor<Expression>),
    /// A field access expression.
    FieldAccess(FieldAccess),
    /// A function call expression.
    FunctionCall(FunctionCall),
    /// A if expression.
    If(IfElse),
    /// A literal.
    Lit(Literal),
    /// A loop.
    Loop(Loop),
    /// A method call expression.
    MethodCall(MethodCall),
    /// A prefix unary operation.
    PreOp(PrefixOperator, u32, ExpressionId),
    /// A tuple.
    Tuple(Tuple<Expression>),
    /// A variable identifier.
    Var(VariableIdentifier, Path),
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
pub struct Block {
    /// Statements.
    pub statements: Id<[StatementId]>,
    /// Last Expression.
    pub expression: Option<ExpressionId>,
    /// Offset of open brace.
    pub open: u32,
    /// Offset of close brace.
    pub close: u32,
}

/// A Field Access such .0 or .name.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FieldAccess {
    /// Record or Tuple accessed.
    pub accessed: ExpressionId,
    /// Field identifier.
    pub field: FieldIdentifier,
}

/// A Field Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum FieldIdentifier {
    /// Index of the field.
    Index(u16, Range),
    /// Interned ID of the name of the field.
    Name(Identifier),
}

/// A function call expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionCall {
    /// Function called.
    pub function: ExpressionId,
    /// Generic Arguments.
    pub generics: Option<Id<GenericVariablePack>>,
    /// Arguments.
    pub arguments: Tuple<Expression>,
}

/// A if-else expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct IfElse {
    /// Condition.
    pub condition: ExpressionId,
    /// Expression evaluated if condition evaluates to true.
    pub true_expr: ExpressionId,
    /// Expression evaluated if condition evaluates to false.
    pub false_expr: ExpressionId,
    /// Offset of :if.
    pub if_: u32,
    /// Offset of :else.
    pub else_: u32,
}

/// A Literal value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Literal {
    /// A boolean value.
    Bool(bool),
    /// A bytes value.
    Bytes(Id<[StringFragment]>, InternId),
    /// An integral value.
    Integral(i64),
    /// A string value.
    String(Id<[StringFragment]>, InternId),
}

/// A if-else expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Loop {
    /// Statements.
    pub statements: Id<[StatementId]>,
    /// Offset of the :loop keyword.
    pub loop_: u32,
    /// Offset of open brace.
    pub open: u32,
    /// Offset of close brace.
    pub close: u32,
}

/// A method call expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct MethodCall {
    /// Receiver of the method call.
    pub receiver: ExpressionId,
    /// Method name.
    pub method: FieldIdentifier,
    /// Generic Arguments.
    pub generics: Option<Id<GenericVariablePack>>,
    /// Arguments.
    pub arguments: Tuple<Expression>,
}

/// A Prefix Unary Operator such as `:not`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum PrefixOperator {
    /// The `:not` operator.
    Not,
}

/// A Value Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableIdentifier(pub InternId, pub Range);

//
//  Implementations
//

impl FieldIdentifier {
    /// Sets the InternId of the FieldIdentifier.
    pub fn with_id(self, id: InternId) -> Self {
        use self::FieldIdentifier::*;

        match self {
            Index(i, r) => Index(i, r),
            Name(i) => Name(i.with_id(id)),
        }
    }

    /// Sets the range spanned by the FieldIdentifier.
    pub fn with_range(self, range: Range) -> Self {
        use self::FieldIdentifier::*;

        match self {
            Index(i, _) => Index(i, range),
            Name(i) => Name(i.with_range(range)),
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

impl Span for FieldIdentifier {
    /// Returns the range spanned by the field identifier.
    fn span(&self) -> Range {
        use self::FieldIdentifier::*;

        match self {
            Index(_, r) | Name(Identifier(_, r)) => *r,
        }
    }
}

impl Span for Block {
    /// Returns the range spanned by the loop.
    fn span(&self) -> Range {
        Range::new(self.open as usize, (self.close + 1 - self.open) as usize)
    }
}

impl Span for Loop {
    /// Returns the range spanned by the loop.
    fn span(&self) -> Range {
        Range::new(self.loop_ as usize, (self.close + 1 - self.loop_) as usize)
    }
}

impl Span for VariableIdentifier {
    /// Returns the range spanned by the variable identifier.
    fn span(&self) -> Range { self.1 }
}


//
//  Implementations of From
//

impl convert::From<Constructor<Expression>> for Expression {
    fn from(c: Constructor<Expression>) -> Expression {
        Expression::Constructor(c)
    }
}

impl convert::From<Block> for Expression {
    fn from(b: Block) -> Expression {
        Expression::Block(b)
    }
}

impl convert::From<FieldAccess> for Expression {
    fn from(f: FieldAccess) -> Expression {
        Expression::FieldAccess(f)
    }
}

impl convert::From<FunctionCall> for Expression {
    fn from(f: FunctionCall) -> Expression {
        Expression::FunctionCall(f)
    }
}

impl convert::From<IfElse> for Expression {
    fn from(i: IfElse) -> Expression {
        Expression::If(i)
    }
}

impl convert::From<Literal> for Expression {
    fn from(l: Literal) -> Expression {
        Expression::Lit(l)
    }
}

impl convert::From<Loop> for Expression {
    fn from(l: Loop) -> Expression {
        Expression::Loop(l)
    }
}

impl convert::From<MethodCall> for Expression {
    fn from(m: MethodCall) -> Expression {
        Expression::MethodCall(m)
    }
}

impl convert::From<Tuple<Expression>> for Expression {
    fn from(t: Tuple<Expression>) -> Expression {
        Expression::Tuple(t)
    }
}

impl convert::From<VariableIdentifier> for Expression {
    fn from(v: VariableIdentifier) -> Expression {
        Expression::Var(v, Path::empty())
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use crate::basic::com::Range;
    use super::super::common::tests::Env;

    #[test]
    fn range_expression_literal() {
        let env = Env::new(b"   1");
        let e = env.factory().expr();

        let id = e.int(1, 3);

        assert_eq!(env.tree().borrow().get_expression_range(id), range(3, 1));
    }

    #[test]
    fn range_expression_binary_operator() {
        let env = Env::new(b"   1 + 1");
        let e = env.factory().expr();

        let id = e.bin_op(e.int(1, 3), e.int(1, 7)).build();

        assert_eq!(env.tree().borrow().get_expression_range(id), range(3, 5));
    }

    fn range(offset: usize, length: usize) -> Range {
        Range::new(offset, length)
    }
}
