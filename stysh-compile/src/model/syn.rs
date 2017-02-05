//! Syntactic model, aka AST.
//!
//! This is the model describing the syntax of the language, with a 1-to-1
//! mapping to the actual textual representation (modulo whitespace).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

use basic::com;

pub type List<'a> = &'a [Node<'a>];

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Node<'a> {
    Expr(&'a Expression<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expression<'a> {
    BinOp(BinaryOperator, &'a Expression<'a>, &'a Expression<'a>),
    Lit(Literal, com::Range),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BinaryOperator {
    Plus,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Literal {
    Integral
}

impl<'a> Node<'a> {
    pub fn range(&self) -> com::Range {
        use self::Node::Expr;

        match *self {
            Expr(expr) => expr.range(),
        }
    }
}

impl<'a> Expression<'a> {
    pub fn range(&self) -> com::Range {
        use self::Expression::{BinOp, Lit};

        match *self {
            BinOp(_, left, right) => left.range().extend(right.range()),
            Lit(_, range) => range,
        }
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::com;
    use super::{Node, Expression, BinaryOperator, Literal};

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
    fn range_node_binary_operator() {
        let left = expr_lit_integral(3, 1);
        let right = expr_lit_integral(7, 1);
        let expr = Expression::BinOp(BinaryOperator::Plus, &left, &right);
        let node = Node::Expr(&expr);

        assert_eq!(node.range(), range(3, 5));
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn expr_lit_integral<'a>(offset: usize, length: usize) -> Expression<'a> {
        Expression::Lit(Literal::Integral, range(offset, length))
    }
}
