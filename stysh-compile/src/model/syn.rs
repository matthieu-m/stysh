//! Syntactic model, aka AST.
//!
//! This is the model describing the syntax of the language, with a 1-to-1
//! mapping to the actual textual representation (modulo whitespace).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

use basic::com;

use model::tt;
pub use model::tt::StringFragment;

/// A List of AST nodes.
pub type List<'a> = &'a [Node<'a>];

/// An AST node, the building piece of the graph.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Node<'a> {
    /// An expression.
    Expr(Expression<'a>),
    /// An item.
    Item(Item<'a>),
    /// A statement.
    Stmt(Statement<'a>),
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expression<'a> {
    /// A binary operation.
    BinOp(BinaryOperator, u32, &'a Expression<'a>, &'a Expression<'a>),
    /// A block expression.
    Block(&'a [Statement<'a>], &'a Expression<'a>, com::Range),
    /// A function call expression.
    FunctionCall(FunctionCall<'a>),
    /// A if expression.
    If(IfElse<'a>),
    /// A literal.
    Lit(Literal<'a>, com::Range),
    /// A tuple.
    Tuple(Tuple<'a, Expression<'a>>),
    /// A variable identifier.
    Var(VariableIdentifier),
}

/// An Item.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item<'a> {
    /// A function.
    Fun(Function<'a>),
}

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Statement<'a> {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A variable definition.
    Var(VariableBinding<'a>),
}

/// A Function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function<'a> {
    /// Name of the function.
    pub name: VariableIdentifier,
    /// List of arguments of the function.
    pub arguments: &'a [Argument<'a>],
    /// Return type of the function.
    pub result: Type<'a>,
    /// Body of the function.
    pub body: Expression<'a>,
    /// Offset of the ":fun" keyword.
    pub keyword: u32,
    /// Offset of the "(" token.
    pub open: u32,
    /// Offset of the ")" token.
    pub close: u32,
    /// Offset of the "->" token, if any.
    pub arrow: u32,
}

/// An Argument.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Argument<'a> {
    /// Name of the argument.
    pub name: VariableIdentifier,
    /// Type of the argument.
    pub type_: Type<'a>,
    /// Offset of the colon.
    pub colon: u32,
    /// Offset of the comma, if any.
    pub comma: u32,
}

/// A Binary Operator such as `+` or `*`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BinaryOperator {
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
    /// The `+` operator.
    Plus,
    /// The `*` operator.
    Times,
}

/// A function call expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionCall<'a> {
    /// Function called.
    pub function: &'a Expression<'a>,
    /// Arguments.
    pub arguments: &'a [Expression<'a>],
    /// Offsets of the commas separating the arguments, an absent comma is 
    /// placed at the offset of the last character of the field it would have
    /// followed.
    pub commas: &'a [u32],
    /// Offset of the opening parenthesis.
    pub open: u32,
    /// Offset of the closing parenthesis, an absent parenthesis is placed at
    /// at the offset of the last character of the field it would have followed.
    pub close: u32,
}

/// A if-else expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct IfElse<'a> {
    /// Condition.
    pub condition: &'a Expression<'a>,
    /// Expression evaluated if condition evaluates to true.
    pub true_expr: &'a Expression<'a>,
    /// Expression evaluated if condition evaluates to false.
    pub false_expr: &'a Expression<'a>,
    /// Offset of :if.
    pub if_: u32,
    /// Offset of :else.
    pub else_: u32,
}

/// A Literal value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Literal<'a> {
    /// A boolean value.
    Bool(bool),
    /// A bytes value.
    Bytes(&'a [StringFragment]),
    /// An integral value.
    Integral,
    /// A string value.
    String(&'a [StringFragment]),
}

/// A variable binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableBinding<'a> {
    /// Name of the binding.
    //  TODO(matthieum): make a pattern.
    pub name: VariableIdentifier,
    /// Type of the binding, if specified.
    pub type_: Option<Type<'a>>,
    /// Expression being bound.
    pub expr: Expression<'a>,
    /// Offset of the :var keyword.
    pub var: u32,
    /// Offset of the : sign, or 0 if none.
    pub colon: u32,
    /// Offset of the := sign.
    pub bind: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type<'a> {
    /// A missing type.
    Missing(com::Range),
    /// A simple nominal type.
    Simple(TypeIdentifier),
    /// A tuple.
    Tuple(Tuple<'a, Type<'a>>),
}

/// A Type Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeIdentifier(pub com::Range);

/// A Tuple, either type or value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<'a, T: 'a> {
    /// Fields of the tuple.
    pub fields: &'a [T],
    /// Offsets of the commas separating the fields, an absent comma is placed
    /// at the offset of the last character of the field it would have followed.
    pub commas: &'a [u32],
    /// Offset of the opening parenthesis.
    pub open: u32,
    /// Offset of the closing parenthesis, an absent parenthesis is placed at
    /// at the offset of the last character of the field it would have followed.
    pub close: u32,
}

/// A Value Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableIdentifier(pub com::Range);

impl<'a> Node<'a> {
    /// Returns the range spanned by the node.
    pub fn range(&self) -> com::Range {
        use self::Node::*;

        match *self {
            Expr(expr) => expr.range(),
            Item(item) => item.range(),
            Stmt(stmt) => stmt.range(),
        }
    }
}

impl<'a> Expression<'a> {
    /// Returns the range spanned by the expression.
    pub fn range(&self) -> com::Range {
        use self::Expression::*;

        match *self {
            BinOp(_, _, left, right) => left.range().extend(right.range()),
            Block(_, _, range) => range,
            FunctionCall(fun) => fun.range(),
            If(if_else) => if_else.range(),
            Lit(_, range) => range,
            Tuple(t) => t.range(),
            Var(VariableIdentifier(range)) => range,
        }
    }
}

impl<'a> Item<'a> {
    /// Returns the range spanned by the item.
    pub fn range(&self) -> com::Range {
        use self::Item::Fun;

        match *self {
            Fun(fun) => fun.range(),
        }
    }
}

impl<'a> Function<'a> {
    /// Returns the range spanned by the function.
    pub fn range(&self) -> com::Range {
        com::Range::new(self.keyword as usize, 4).extend(self.body.range())
    }
}

impl<'a> Argument<'a> {
    /// Returns the range spanned by the argument.
    pub fn range(&self) -> com::Range {
        let offset = self.name.0.offset();
        let end_offset = if self.comma != 0 {
            (self.comma + 1) as usize
        } else {
            self.type_.range().end_offset()
        };
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a> FunctionCall<'a> {
    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        self.commas
            .get(i)
            .map(|&o| tt::Token::new(tt::Kind::SignComma, o as usize, 1))
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisOpen, self.open as usize, 1)
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisClose, self.close as usize, 1)
    }

    /// Returns the range spanned by the function call.
    pub fn range(&self) -> com::Range {
        self.function.range().extend(self.parenthesis_close().range())
    }
}

impl<'a> IfElse<'a> {
    /// Returns the range spanned by the argument.
    pub fn range(&self) -> com::Range {
        let offset = self.if_ as usize;
        let end_offset = self.false_expr.range().end_offset();
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a> Statement<'a> {
    /// Returns the range spanned by the statement.
    pub fn range(&self) -> com::Range {
        use self::Statement::*;

        match *self {
            Var(var) => var.range(),
        }
    }
}

impl<'a> VariableBinding<'a> {
    /// Returns the range spanned by the binding.
    pub fn range(&self) -> com::Range {
        debug_assert!(
            self.semi as usize >= self.expr.range().end_offset() - 1,
            "{} should occur after {}", self.semi, self.expr.range()
        );
        com::Range::new(
            self.var as usize,
            (self.semi + 1 - self.var) as usize
        )
    }
}

impl<'a> Type<'a> {
    /// Returns the range spanned by the binding.
    pub fn range(&self) -> com::Range {
        match *self {
            Type::Missing(r) => r,
            Type::Simple(t) => t.0,
            Type::Tuple(t) => t.range(),
        }
    }
}

impl<'a, T: 'a + Clone> Tuple<'a, T> {
    /// Returns whether the tuple is empty.
    pub fn is_empty(&self) -> bool { self.fields.is_empty() }

    /// Returns the number of fields of the tuple.
    pub fn len(&self) -> usize { self.fields.len() }

    /// Returns the field at index i.
    pub fn field(&self, i: usize) -> Option<T> {
        self.fields.get(i).cloned()
    }

    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        self.commas
            .get(i)
            .map(|&o| tt::Token::new(tt::Kind::SignComma, o as usize, 1))
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisOpen, self.open as usize, 1)
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisClose, self.close as usize, 1)
    }

    /// Returns the range spanned by the tuple.
    pub fn range(&self) -> com::Range {
        self.parenthesis_open().range().extend(self.parenthesis_close().range())
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::com;
    use super::*;

    #[test]
    fn range_expression_literal() {
        let expr = expr_lit_integral(3, 4);
        assert_eq!(expr.range(), range(3, 4));
    }

    #[test]
    fn range_expression_binary_operator() {
        let left = expr_lit_integral(3, 1);
        let right = expr_lit_integral(7, 1);
        let expr = Expression::BinOp(BinaryOperator::Plus, 5, &left, &right);

        assert_eq!(expr.range(), range(3, 5));
    }

    #[test]
    fn range_item_fun() {
        let (left, right) =
            (expr_lit_integral(20, 1), expr_lit_integral(24, 1));
        let fun = Function {
            name: VariableIdentifier(range(8, 3)),
            arguments: &[],
            result: type_simple(16, 3),
            body: Expression::BinOp(BinaryOperator::Plus, 22, &left, &right),
            keyword: 3,
            open: 11,
            close: 12,
            arrow: 14,
        };

        assert_eq!(fun.range(), range(3, 22));
    }

    #[test]
    fn range_node_binary_operator() {
        let left = expr_lit_integral(3, 1);
        let right = expr_lit_integral(7, 1);
        let expr = Expression::BinOp(BinaryOperator::Plus, 5, &left, &right);
        let node = Node::Expr(expr);

        assert_eq!(node.range(), range(3, 5));
    }

    #[test]
    fn range_stmt_variable_binding() {
        let with_semi = bind_var_integral(5);

        let without_semi =
            VariableBinding { semi: with_semi.semi - 1, .. with_semi };

        assert_eq!(with_semi.range(), range(5, 18));
        assert_eq!(without_semi.range(), range(5, 17));
    }

    #[test]
    fn range_stmt() {
        let stmt = Statement::Var(bind_var_integral(5));
        assert_eq!(stmt.range(), range(5, 18));
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn bind_var_integral(offset: usize) -> VariableBinding<'static> {
        //  ":var fool := 1234;" at an arbitrary offset.
        VariableBinding {
            name: VariableIdentifier(range(offset + 5, 4)),
            type_: None,
            expr: expr_lit_integral(offset + 13, 4),
            var: offset as u32,
            colon: 0,
            bind: (offset + 10) as u32,
            semi: (offset + 17) as u32,
        }
    }

    fn expr_lit_integral(offset: usize, length: usize) -> Expression<'static> {
        Expression::Lit(Literal::Integral, range(offset, length))
    }

    fn type_simple(offset: usize, length: usize) -> Type<'static> {
        Type::Simple(TypeIdentifier(range(offset, length)))
    }
}
