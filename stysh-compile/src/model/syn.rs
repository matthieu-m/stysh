//! Syntactic model, aka AST.
//!
//! This is the model describing the syntax of the language, with a 1-to-1
//! mapping to the actual textual representation (modulo whitespace).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

use basic::com;

/// A List of AST nodes.
pub type List<'a> = &'a [Node<'a>];

/// An AST node, the building piece of the graph.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Node<'a> {
    /// An expression.
    Expr(Expression<'a>),
    /// An item.
    Item(Item<'a>),
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expression<'a> {
    /// A binary operation.
    BinOp(BinaryOperator, &'a Expression<'a>, &'a Expression<'a>),
    /// A block expression.
    Block(List<'a>, com::Range),
    /// A literal.
    Lit(Literal, com::Range),
    /// A variable identifier.
    Var(VariableIdentifier),
}

/// An Item.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item<'a> {
    /// A function.
    Fun {
        name: VariableIdentifier,
        arguments: &'a [Argument],
        result: TypeIdentifier,
        body: Expression<'a>,
        keyword: u32,
        open: u32,
        close: u32,
        arrow: u32,
    },
}

/// An argument.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Argument {
    name: VariableIdentifier,
    type_: TypeIdentifier,
    colon: u32,
    comma: u32,
}

/// A Binary Operator such as `+` or `*`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BinaryOperator {
    /// The `+` operator.
    Plus,
}

/// A Literal value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Literal {
    /// An integral value.
    Integral
}

/// A Type Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeIdentifier(pub com::Range);

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
        }
    }
}

impl<'a> Expression<'a> {
    /// Returns the range spanned by the expression.
    pub fn range(&self) -> com::Range {
        use self::Expression::*;

        match *self {
            BinOp(_, left, right) => left.range().extend(right.range()),
            Block(_, range) => range,
            Lit(_, range) => range,
            Var(VariableIdentifier(range)) => range,
        }
    }
}

impl<'a> Item<'a> {
    /// Returns the range spanned by the expression.
    pub fn range(&self) -> com::Range {
        use self::Item::Fun;

        match *self {
            Fun { keyword: k, body: b, .. } =>
                com::Range::new(k as usize, 4).extend(b.range()),
        }
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
        let expr = Expression::BinOp(BinaryOperator::Plus, &left, &right);

        assert_eq!(expr.range(), range(3, 5));
    }

    #[test]
    fn range_item_fun() {
        let (left, right) =
            (expr_lit_integral(20, 1), expr_lit_integral(24, 1));
        let node = Node::Item(
            Item::Fun {
                name: VariableIdentifier(range(8, 3)),
                arguments: &[],
                result: TypeIdentifier(range(16, 3)),
                body: Expression::BinOp(BinaryOperator::Plus, &left, &right),
                keyword: 3,
                open: 11,
                close: 12,
                arrow: 14,
            }
        );

        assert_eq!(node.range(), range(3, 22));
    }

    #[test]
    fn range_node_binary_operator() {
        let left = expr_lit_integral(3, 1);
        let right = expr_lit_integral(7, 1);
        let expr = Expression::BinOp(BinaryOperator::Plus, &left, &right);
        let node = Node::Expr(expr);

        assert_eq!(node.range(), range(3, 5));
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn expr_lit_integral<'a>(offset: usize, length: usize) -> Expression<'a> {
        Expression::Lit(Literal::Integral, range(offset, length))
    }
}
