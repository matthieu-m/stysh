//! Syntactic pass, aka parsing.
//!
//! This pass is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

use std::iter;

use basic::mem;

use model::tt;
use model::syn::*;

/// The Stysh parser.
///
/// The responsibility of the parser is to transform the Token Tree into an
/// Abstract Syntax Tree.
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

    /// Parses a slice of raw Token Trees into an Abstract Syntax Tree.
    pub fn parse(&mut self, nodes: &'g [tt::Node<'g>]) -> List<'g> {
        let mut buffer = mem::Array::new(self.local_arena);

        let imp = ParserImpl {
            nodes: nodes,
            global_arena: self.global_arena,
            local_arena: self.local_arena,
        };

        for node in imp {
            buffer.push(node);
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

impl<'g> IntoExpr<'g> for tt::Token {
    fn into_expr(self) -> Option<Expression<'g>> {
        use self::Expression::*;
        use self::Literal::*;

        match self.kind() {
            tt::Kind::Integral => Some(Lit(Integral, self.range())),
            tt::Kind::OperatorPlus => None,
        }
    }
}

struct ParserImpl<'g, 'local> {
    nodes: &'g [tt::Node<'g>],
    global_arena: &'g mem::Arena,
    #[allow(dead_code)]
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> iter::Iterator for ParserImpl<'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        if self.nodes.is_empty() {
            return None;
        }

        let result = match self.nodes[0] {
            tt::Node::Run(tokens) =>
                Some(Node::Expr(self.parse_expression(tokens))),
            _ => unimplemented!(),
        };

        self.nodes = &self.nodes[1..];

        result
    }
}

impl<'g, 'local> ParserImpl<'g, 'local> {
    fn parse_expression(&mut self, tokens: &[tt::Token]) -> Expression<'g> {
        let left_operand: &'g Expression<'g> = self.intern(
            tokens[0].into_expr().expect("Integral")
        );

        let operator = tokens[1];
        assert_eq!(operator.kind(), tt::Kind::OperatorPlus);

        let right_operand: &Expression = self.intern(
            tokens[2].into_expr().expect("Integral")
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
    use model::tt;
    use model::syn::*;
    use super::Parser;

    #[test]
    fn first_parse() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            synit(
                &global_arena,
                &[
                    tt::Node::Run(&[
                        tt::Token::new(tt::Kind::Integral, 0, 1),
                        tt::Token::new(tt::Kind::OperatorPlus, 2, 1),
                        tt::Token::new(tt::Kind::Integral, 4, 1),
                    ])
                ]
            ),
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

    fn synit<'g>(global_arena: &'g mem::Arena, nodes: &'g [tt::Node<'g>])
        -> List<'g>
    {
        let mut local_arena = mem::Arena::new();

        let items = Parser::new(&global_arena, &local_arena).parse(nodes);
        local_arena.recycle();

        items
    }
}
