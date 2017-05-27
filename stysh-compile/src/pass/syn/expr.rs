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
    fn new(raw: RawParser<'a, 'g, 'local>) -> ExprParser<'a, 'g, 'local> {
        ExprParser { raw: raw }
    }

    fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    fn parse(&mut self) -> Expression<'g> {
        let tokens = {
            let node = self.raw.peek().expect("WAT?");

            match node {
                tt::Node::Run(tokens) => tokens,
                tt::Node::Braced(o, n, c) => {
                    self.raw.pop_node();

                    return match o.kind() {
                        tt::Kind::BraceOpen => self.parse_braces(n, o, c),
                        tt::Kind::ParenthesisOpen => self.parse_parens(n, o, c),
                        _ => unimplemented!(),
                    };
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

        if tokens[0].kind() == tt::Kind::KeywordIf {
            return self.parse_if_else();
        }

        let expr = tokens[0].into_expr().expect("Expression");
        self.raw.pop_tokens(1);

        if tokens.len() == 1 ||
            tokens[1].kind() == tt::Kind::SignComma ||
            tokens[1].kind() == tt::Kind::SignSemiColon
        {
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

    fn parse_braces(&self, ns: &[tt::Node], o: tt::Token, c: tt::Token)
        -> Expression<'g>
    {
        let mut raw = self.raw.spawn(ns);
        let mut stmts = raw.local_array();

        while let Some(tok) = raw.peek().map(|n| n.front()) {
            if tok.kind() != tt::Kind::KeywordVar {
                break;
            }
            stmts.push(Statement::Var(parse_variable(&mut raw)));
        }

        let stmts = self.raw.intern_slice(&stmts);

        let inner = ExprParser::new(raw).parse();
        let inner = self.raw.intern(inner);

        Expression::Block(stmts, inner, o.range().extend(c.range()))
    }

    fn parse_parens(&self, ns: &[tt::Node], o: tt::Token, c: tt::Token)
        -> Expression<'g>
    {
        let mut inner = ExprParser::new(self.raw.spawn(ns));

        let mut fields = self.raw.local_array();
        let mut commas = self.raw.local_array();

        while let Some(_) = inner.raw.peek() {
            let e = inner.parse();
            fields.push(e);

            if let Some(c) = inner.raw.pop_kind(tt::Kind::SignComma) {
                commas.push(c.range().offset() as u32)
            } else {
                commas.push(e.range().end_offset() as u32 - 1)
            };
        }

        assert!(inner.into_raw().peek().is_none());

        Expression::Tuple(Tuple {
            fields: self.raw.intern_slice(fields.into_slice()),
            commas: self.raw.intern_slice(commas.into_slice()),
            open: o.offset() as u32,
            close: c.offset() as u32,
        })
    }

    fn parse_if_else(&mut self) -> Expression<'g> {
        let if_ = self.raw.pop_kind(tt::Kind::KeywordIf).expect(":if");

        let condition = self.parse();
        let true_expr = self.parse();   //  FIXME(matthieum): possibly missing.

        println!("{:?}", self.raw);

        if let Some(else_) = self.raw.pop_kind(tt::Kind::KeywordElse) {
            //  FIXME(matthieum): only ":if" and "{ ... }" are legal.

            let false_expr = self.parse();

            return Expression::If(IfElse {
                condition: self.raw.intern(condition),
                true_expr: self.raw.intern(true_expr),
                false_expr: self.raw.intern(false_expr),
                if_: if_.offset() as u32,
                else_: else_.offset() as u32,
            });
        }

        //  FIXME(matthieum): ";" is legal.
        unimplemented!()
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
            tt::Kind::LitBoolFalse => Some(Lit(Bool(false), self.range())),
            tt::Kind::LitBoolTrue => Some(Lit(Bool(true), self.range())),
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
                &int(0, 1),
                &int(4, 1),
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
                expr: int(14, 4),
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
                expr: int(11, 4),
                var: 1,
                colon: 0,
                bind: 0,
                semi: 14,
            }
        );
    }

    #[test]
    fn basic_block() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b"{\n    :var fool := 1234;\n    fool\n}"),
            Expression::Block(
                &[
                    Statement::Var(VariableBinding {
                        name: VariableIdentifier(range(11, 4)),
                        type_: None,
                        expr: int(19, 4),
                        var: 6,
                        colon: 0,
                        bind: 16,
                        semi: 23,
                    }),
                ],
                &Expression::Var(VariableIdentifier(range(29, 4))),
                range(0, 35)
            )
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
    fn basic_if_else() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b":if true { 1 } :else { 0 }"),
            Expression::If(IfElse {
                condition: &boolean(true, 4, 4),
                true_expr: &Expression::Block(&[], &int(11, 1), range(9, 5)),
                false_expr: &Expression::Block(&[], &int(23, 1), range(21, 5)),
                if_: 0,
                else_: 15,
            })
        )
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

    #[test]
    fn boolean_basic() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            varit(&global_arena, b":var x := true;"),
            VariableBinding {
                name: VariableIdentifier(range(5, 1)),
                type_: None,
                expr: boolean(true, 10, 4),
                var: 0,
                colon: 0,
                bind: 7,
                semi: 14,
            }
        );
    }

    #[test]
    fn tuple_basic() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b"(1)"),
            Expression::Tuple(
                Tuple {
                    fields: &[int(1, 1)],
                    commas: &[1],
                    open: 0,
                    close: 2,
                }
            )
        );
    }

    #[test]
    fn tuple_nested() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            exprit(&global_arena, b"(1, (2, 3), 4)"),
            Expression::Tuple(
                Tuple {
                    fields: &[
                        int(1, 1),
                        Expression::Tuple(
                            Tuple {
                                fields: &[int(5, 1), int(8, 1)],
                                commas: &[6, 8],
                                open: 4,
                                close: 9,
                            }
                        ),
                        int(12, 1),
                    ],
                    commas: &[2, 10, 12],
                    open: 0,
                    close: 13,
                }
            )
        );
    }

    fn boolean(value: bool, offset: usize, length: usize)
        -> Expression<'static>
    {
        Expression::Lit(Literal::Bool(value), range(offset, length))
    }
    
    fn int(offset: usize, length: usize) -> Expression<'static> {
        Expression::Lit(Literal::Integral, range(offset, length))
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
