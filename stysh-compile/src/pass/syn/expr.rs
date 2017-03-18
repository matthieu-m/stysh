//! Syntactic pass, aka parsing.
//!
//! Expression parser.

use model::tt;
use model::syn::*;

use super::com::RawParser;

pub struct ExprParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>
}

pub fn parse_expression<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Expression<'g>
{
    let mut parser = ExprParser::new(*raw);
    let expr = parser.parse();
    *raw = parser.into_raw();
    expr
}

impl<'a, 'g, 'local> ExprParser<'a, 'g, 'local> {
    pub fn new(raw: RawParser<'a, 'g, 'local>) -> ExprParser<'a, 'g, 'local> {
        ExprParser { raw: raw }
    }

    pub fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    pub fn parse(&mut self) -> Expression<'g> {
        let tokens = match self.raw.peek() {
            Some(tt::Node::Run(tokens)) => tokens,
            Some(tt::Node::Braced(o, n, c)) => {
                self.raw.pop_node();

                let inner = ExprParser::new(self.raw.spawn(n)).parse();
                let inner = self.raw.intern(inner);

                return Expression::Block(inner, o.range().extend(c.range()));
            },
            _ => unimplemented!(),
        };

        let left_operand: &'g Expression<'g> = self.raw.intern(
            tokens[0].into_expr().expect("Integral")
        );

        let operator = tokens[1];
        assert_eq!(operator.kind(), tt::Kind::OperatorPlus);

        let right_operand: &Expression = self.raw.intern(
            tokens[2].into_expr().expect("Integral")
        );

        self.raw.pop_tokens(3);

        Expression::BinOp(
            BinaryOperator::Plus,
            left_operand,
            right_operand
        )
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


//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::syn::*;

    #[test]
    fn basic_add() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b"1 + 2"),
            Expression::BinOp(
                BinaryOperator::Plus,
                &Expression::Lit(
                    Literal::Integral,
                    range(0, 1)
                ),
                &Expression::Lit(
                    Literal::Integral,
                    range(4, 1)
                ),
            )
        );
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn exprit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Expression<'g> {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let e = {
            let raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::ExprParser::new(raw).parse()
        };
        local_arena.recycle();

        e
    }
}