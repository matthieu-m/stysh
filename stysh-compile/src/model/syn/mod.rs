//! Syntactic model, aka AST.
//!
//! This is the model describing the syntax of the language, with a 1-to-1
//! mapping to the actual textual representation (modulo whitespace).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

#[cfg(test)]
pub mod builder;

mod common;
mod expr;
mod item;
mod pattern;
mod stmt;
mod typ;

use basic::com::{self, Span};

pub use self::common::*;
pub use self::expr::*;
pub use self::item::*;
pub use self::pattern::*;
pub use self::stmt::*;
pub use self::typ::*;

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

//
//  Implementations of Span
//
impl<'a> Span for Node<'a> {
    /// Returns the range spanned by the node.
    fn span(&self) -> com::Range {
        use self::Node::*;

        match *self {
            Expr(expr) => expr.span(),
            Item(item) => item.span(),
            Stmt(stmt) => stmt.span(),
        }
    }
}
//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use super::*;
    use model::syn::builder::Factory;

    #[test]
    fn range_node_binary_operator() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  "   1 + 1"
        let node = Node::Expr(e.bin_op(e.int(3, 1), e.int(7, 1)).build());

        assert_eq!(node.span(), range(3, 5));
    }

    #[test]
    fn range_node_prefix_unary_operator() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  " !     1"
        let node = Node::Expr(e.pre_op(e.int(7, 2)).offset(1).build());

        assert_eq!(node.span(), range(1, 8));
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
