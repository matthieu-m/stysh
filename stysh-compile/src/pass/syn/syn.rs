//! Syntactic pass, aka parsing.
//!
//! There is for now a single pass:
//! -   the "parse" pass produces a list of `syn::Node` from raw input.

use std::iter::Peekable;

use basic::mem;

use model::syn::*;
use pass::syn::lex;

/// The Stysh parser.
///
/// The responsibility of the parser is to turn a raw slice of bytes and turn it
/// into an Abstract Syntax Tree retaining all pertinent information (even
/// comments).
///
/// No client should have to parse the raw slice by themselves.
pub struct Parser<'g, 'local> {
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> Parser<'g, 'local> {
    /// Creates a new instance of the parser.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(global: &'g mem::Arena, local: &'local mem::Arena)
        -> Parser<'g, 'local>
    {
        Parser { global_arena: global, local_arena: local }
    }

    /// Parses a raw slice of bytes into a list of top-level AST items.
    ///
    /// Note:   since the T in AST stands for Tree, each top-level AST item may
    ///         have children items.
    pub fn parse(&mut self, raw: &[u8]) -> List<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        let mut tokens = lex::Stream::new(raw).peekable();
        while let Some(_) = tokens.peek() {
            buffer.push(self.parse_node(&mut tokens));
        }

        self.global_arena.insert_slice(buffer.into_slice())
    }
}

//
//  Implementation Details
//
trait IntoExpr<'g> {
    fn into_expr(self) -> Option<Expression<'g>>;
}

impl<'g> IntoExpr<'g> for lex::Token {
    fn into_expr(self) -> Option<Expression<'g>> {
        use self::Expression::*;
        use self::Literal::*;

        match self.kind() {
            lex::Kind::Integral => Some(Lit(Integral, self.range())),
            lex::Kind::OperatorPlus => None,
        }
    }
}

impl<'g, 'local> Parser<'g, 'local> {
    fn parse_node(&mut self, tokens: &mut Peekable<lex::Stream>) -> Node<'g> {
        Node::Expr(self.parse_expression(tokens))
    }

    fn parse_expression(&mut self, tokens: &mut Peekable<lex::Stream>)
        -> Expression<'g>
    {
        let left_operand: &'g Expression<'g> = self.intern(
            tokens
                .next().expect("Left operand")
                .into_expr().expect("Integral")
        );

        let operator = tokens.next().expect("Operator");
        assert_eq!(operator.kind(), lex::Kind::OperatorPlus);

        let right_operand: &Expression = self.intern(
            tokens
                .next().expect("Right operand")
                .into_expr().expect("Integral")
        );

        Expression::BinOp(
            BinaryOperator::Plus,
            left_operand,
            right_operand
        )
    }

    fn intern<T: 'g>(&mut self, t: T) -> &'g T {
        self.global_arena.insert(t)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::syn::*;
    use super::Parser;

    #[test]
    fn first_parse() {
        let global_arena = mem::Arena::new();
        let mut local_arena = mem::Arena::new();

        let items = Parser::new(&global_arena, &local_arena).parse(b"1 + 2");
        local_arena.recycle();

        assert_eq!(
            items,
            &[
                Node::Expr(
                    Expression::BinOp(
                        BinaryOperator::Plus,
                        &Expression::Lit(
                            Literal::Integral,
                            com::Range::new(0, 1)
                        ),
                        &Expression::Lit(
                            Literal::Integral,
                            com::Range::new(4, 1)
                        ),
                    )
                )
            ]
        );
    }
}
