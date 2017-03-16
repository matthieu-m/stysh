//! Syntactic pass, aka parsing.
//!
//! This pass is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

use std::iter;

use basic::mem;

use model::tt;
use model::syn::*;
use pass::lex;

/// The Stysh parser.
///
/// The responsibility of the parser is to transform raw input into an Abstract
/// Syntax Tree.
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

    /// Parses a raw slite of bytes into an Abstract Syntax Tree.
    pub fn parse(&mut self, raw: &[u8]) -> List<'g> {
        let mut lexer = lex::Lexer::new(self.local_arena, self.local_arena);
        self.transform(lexer.parse(raw))
    }

    /// Transforms a slice of raw Token Trees into an Abstract Syntax Tree.
    pub fn transform(&mut self, nodes: &[tt::Node]) -> List<'g> {
        ParserImpl::new(nodes, self.global_arena, self.local_arena).parse_all()
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
            tt::Kind::LitIntegral => Some(Lit(Integral, self.range())),
            _ => None,
        }
    }
}

struct ParserImpl<'a, 'g, 'local> {
    state: ParserState<'a>,
    global_arena: &'g mem::Arena,
    #[allow(dead_code)]
    local_arena: &'local mem::Arena,
}

struct ParserState<'a> {
    nodes: &'a [tt::Node<'a>],
    run_start: usize,
}

impl<'a, 'g, 'local> iter::Iterator for ParserImpl<'a, 'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        self.peek()
            .map(|node| node.front().kind())
            .map(|kind| {
                match kind {
                    tt::Kind::KeywordFun => Node::Item(self.parse_function()),
                    _ => Node::Expr(self.parse_expression()),
                }
            })
    }
}

impl<'a, 'g, 'local> ParserImpl<'a, 'g, 'local> {
    fn new(
        nodes: &'a [tt::Node<'a>],
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> ParserImpl<'a, 'g, 'local>
    {
        ParserImpl {
            state: ParserState::new(nodes),
            global_arena: global,
            local_arena: local,
        }
    }

    fn parse_all(self) -> List<'g> {
        let global_arena = self.global_arena;
        let mut buffer = mem::Array::new(self.local_arena);

        for node in self {
            buffer.push(node);
        }

        global_arena.insert_slice(buffer.into_slice())
    }

    fn parse_expression(&mut self) -> Expression<'g> {
        let tokens = match self.peek() {
            Some(tt::Node::Run(tokens)) => tokens,
            Some(tt::Node::Braced(o, n, c)) => {
                self.pop_node();

                let inner =
                    ParserImpl::new(n, self.global_arena, self.local_arena)
                        .parse_all();

                return Expression::Block(inner, o.range().extend(c.range()));
            },
            _ => unimplemented!(),
        };

        let left_operand: &'g Expression<'g> = self.intern(
            tokens[0].into_expr().expect("Integral")
        );

        let operator = tokens[1];
        assert_eq!(operator.kind(), tt::Kind::OperatorPlus);

        let right_operand: &Expression = self.intern(
            tokens[2].into_expr().expect("Integral")
        );

        self.pop_tokens(3);

        Expression::BinOp(
            BinaryOperator::Plus,
            left_operand,
            right_operand
        )
    }

    fn parse_function(&mut self) -> Item<'g> {
        let (keyword, name) = {
            let start = match self.peek() {
                Some(tt::Node::Run(tokens)) => tokens,
                _ => unimplemented!(),
            };

            assert_eq!(start.len(), 2);
            assert_eq!(start[0].kind(), tt::Kind::KeywordFun);
            assert_eq!(start[1].kind(), tt::Kind::NameValue);

            self.pop_tokens(2);

            (start[0].offset() as u32, VariableIdentifier(start[1].range()))
        };

        let (open, arguments, close) = {
            let (o, a, c) = match self.peek() {
                Some(tt::Node::Braced(open, a, close)) => (open, a, close),
                _ => unimplemented!(),
            };

            self.pop_node();

            assert_eq!(o.kind(), tt::Kind::ParenthesisOpen);
            assert_eq!(c.kind(), tt::Kind::ParenthesisClose);

            let arguments = self.parse_arguments(a);

            (o.offset() as u32, arguments, c.offset() as u32)
        };

        let (arrow, result) = {
            let result = match self.peek() {
                Some(tt::Node::Run(tokens)) => tokens,
                _ => unimplemented!(),
            };

            assert_eq!(result.len(), 2);
            assert_eq!(result[0].kind(), tt::Kind::SignArrowSingle);
            assert_eq!(result[1].kind(), tt::Kind::NameType);

            self.pop_tokens(2);

            (result[0].offset() as u32, TypeIdentifier(result[1].range()))
        };

        let body = self.parse_expression();

        Item::Fun {
            name: name,
            arguments: arguments,
            result: result,
            body: body,
            keyword: keyword,
            open: open,
            close: close,
            arrow: arrow,
        }
    }

    fn parse_arguments(&self, _arguments: &[tt::Node]) -> &'g [Argument] {
        &[]
    }

    fn peek(&self) -> Option<tt::Node<'a>> { self.state.peek() }

    fn pop_node(&mut self) { self.state.pop_node(); }

    fn pop_tokens(&mut self, nb: usize) { self.state.pop_tokens(nb); }

    fn intern<T: 'g>(&mut self, t: T) -> &'g T {
        self.global_arena.insert(t)
    }
}

impl<'a> ParserState<'a> {
    fn new(nodes: &'a [tt::Node<'a>]) -> ParserState<'a> {
        ParserState { nodes: nodes, run_start: 0 }
    }

    fn peek(&self) -> Option<tt::Node<'a>> {
        match self.nodes.first().cloned() {
            Some(tt::Node::Run(run)) =>
                Some(tt::Node::Run(&run[self.run_start..])),
            other => other,
        }
    }

    fn pop_node(&mut self) {
        self.nodes = &self.nodes[1..];
        self.run_start = 0;
    }

    fn pop_tokens(&mut self, nb: usize) {
        if let Some(tt::Node::Run(run)) = self.nodes.first().cloned() {
            debug_assert!(self.run_start + nb <= run.len());

            self.run_start += nb;

            if self.run_start == run.len() {
                self.run_start = 0;
                self.pop_node();
            }
        } else {
            panic!("Unreachable!");
        }
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

        assert_eq!(
            synit(&global_arena, b"1 + 2"),
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

    #[test]
    fn parse_function_simple() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            synit(&global_arena, b":fun add() -> Int { 1 + 2 }"),
            &[
                Node::Item(
                    Item::Fun {
                        name: VariableIdentifier(range(5, 3)),
                        arguments: &[],
                        result: TypeIdentifier(range(14, 3)),
                        body: Expression::Block(
                            &[
                                Node::Expr(
                                    Expression::BinOp(
                                        BinaryOperator::Plus,
                                        &Expression::Lit(
                                            Literal::Integral,
                                            range(20, 1)
                                        ),
                                        &Expression::Lit(
                                            Literal::Integral,
                                            range(24, 1)
                                        ),
                                    ),
                                )
                            ],
                            range(18, 9),
                        ),
                        keyword: 0,
                        open: 8,
                        close: 9,
                        arrow: 11,
                    }
                )
            ]
        );
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn synit<'g>(global_arena: &'g mem::Arena, raw: &[u8])
        -> List<'g>
    {
        let mut local_arena = mem::Arena::new();

        let items = Parser::new(&global_arena, &local_arena).parse(raw);
        local_arena.recycle();

        items
    }
}
