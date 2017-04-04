//! Syntactic pass, aka parsing.
//!
//! Expression parser.

use model::tt;
use model::syn::*;

use super::com::RawParser;

pub fn parse_expression<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Expression<'g>
{
    let mut parser = ExprParser::new(*raw);
    let expr = parser.parse();
    *raw = parser.into_raw();
    expr
}

pub fn parse_variable<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> VariableBinding<'g>
{
    let var =
        raw.pop_kind(tt::Kind::KeywordVar)
            .map(|t| t.offset() as u32)
            .expect(":var");

    let name = raw.pop_kind(tt::Kind::NameValue).expect("name");

    let bind =
        raw.pop_kind(tt::Kind::SignBind)
            .map(|t| t.offset() as u32)
            .unwrap_or(0);

    let expr = parse_expression(raw);

    let semi =
        raw.pop_kind(tt::Kind::SignSemiColon)
            .map(|t| t.offset())
            .unwrap_or(expr.range().end_offset() - 1) as u32;

    VariableBinding {
        name: VariableIdentifier(name.range()),
        type_: None,
        expr: expr,
        var: var,
        colon: 0,
        bind: bind,
        semi: semi,
    }
}

//
//  Implementation Details
//
struct ExprParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>
}

impl<'a, 'g, 'local> ExprParser<'a, 'g, 'local> {
    pub fn new(raw: RawParser<'a, 'g, 'local>) -> ExprParser<'a, 'g, 'local> {
        ExprParser { raw: raw }
    }

    pub fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    pub fn parse(&mut self) -> Expression<'g> {
        let tokens = {
            let node = self.raw.peek().expect("WAT?");

            match node {
                tt::Node::Run(tokens) => tokens,
                tt::Node::Braced(_, n, _) => {
                    self.raw.pop_node();

                    let inner = ExprParser::new(self.raw.spawn(n)).parse();
                    let inner = self.raw.intern(inner);

                    return Expression::Block(inner, node.range());
                },
                tt::Node::Bytes(_, f, _) => {
                    self.raw.pop_node();

                    let bytes = self.raw.global().insert_slice(f);
                    return Expression::Lit(Literal::Bytes(bytes), node.range());
                },
                tt::Node::String(_, f, _) => {
                    self.raw.pop_node();

                    let string = self.raw.global().insert_slice(f);
                    return Expression::Lit(
                        Literal::String(string),
                        node.range()
                    );
                },
                _ => unimplemented!(),
            }
        };

        let expr = tokens[0].into_expr().expect("Expression");
        self.raw.pop_tokens(1);

        if tokens.len() == 1 || tokens[1].kind() == tt::Kind::SignSemiColon {
            expr
        } else {
            let left_operand: &Expression = self.raw.intern(expr);

            let operator = tokens[1];
            assert_eq!(operator.kind(), tt::Kind::OperatorPlus);

            let right_operand: &Expression = self.raw.intern(
                tokens[2].into_expr().expect("Operand")
            );

            self.raw.pop_tokens(2);

            Expression::BinOp(
                BinaryOperator::Plus,
                left_operand,
                right_operand
            )
        }
    }
}

trait IntoExpr<'g> {
    fn into_expr(self) -> Option<Expression<'g>>;
}

impl<'g> IntoExpr<'g> for tt::Token {
    fn into_expr(self) -> Option<Expression<'g>> {
        use self::Expression::*;
        use self::Literal::*;

        match self.kind() {
            tt::Kind::LitIntegral => Some(Lit(Integral, self.range())),
            tt::Kind::NameValue => Some(Var(VariableIdentifier(self.range()))),
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

    #[test]
    fn basic_var() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            varit(&global_arena, b" :var fool := 1234;"),
            VariableBinding {
                name: VariableIdentifier(range(6, 4)),
                type_: None,
                expr: Expression::Lit(
                    Literal::Integral,
                    range(14, 4)
                ),
                var: 1,
                colon: 0,
                bind: 11,
                semi: 18,
            }
        );
    }

    #[test]
    fn basic_var_automatic_insertion() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            varit(&global_arena, b" :var fool 1234"),
            VariableBinding {
                name: VariableIdentifier(range(6, 4)),
                type_: None,
                expr: Expression::Lit(
                    Literal::Integral,
                    range(11, 4)
                ),
                var: 1,
                colon: 0,
                bind: 0,
                semi: 14,
            }
        );
    }

    #[test]
    fn basic_bytes() {
        use model::tt;

        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b"b'1 + 2'"),
            Expression::Lit(
                Literal::Bytes(&[
                    StringFragment::Text(
                        tt::Token::new(tt::Kind::StringText, 2, 5)
                    ),
                ]),
                range(0, 8)
            )
        );
    }

    #[test]
    fn basic_string() {
        use model::tt;

        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b"'1 + 2'"),
            Expression::Lit(
                Literal::String(&[
                    StringFragment::Text(
                        tt::Token::new(tt::Kind::StringText, 1, 5)
                    ),
                ]),
                range(0, 7)
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
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_expression(&mut raw)
        };
        local_arena.recycle();

        e
    }

    fn varit<'g>(global_arena: &'g mem::Arena, raw: &[u8])
        -> VariableBinding<'g>
    {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let v = {
            let mut raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::parse_variable(&mut raw)
        };
        local_arena.recycle();

        v
    }
}
