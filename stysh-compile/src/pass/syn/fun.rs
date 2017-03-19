//! Syntactic pass, aka parsing.
//!
//! Function parser.

use model::syn::*;
use model::tt;

use super::com::RawParser;
use super::{expr, tok, typ};

pub struct FunParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>
}

pub fn parse_function<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> Function<'g>
{
    let mut parser = FunParser::new(*raw);
    let fun = parser.parse();
    *raw = parser.into_raw();
    fun
}

impl<'a, 'g, 'local> FunParser<'a, 'g, 'local> {
    pub fn new(raw: RawParser<'a, 'g, 'local>) -> FunParser<'a, 'g, 'local> {
        FunParser { raw: raw }
    }

    pub fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    pub fn parse(&mut self) -> Function<'g> {
        use self::tt::Kind::*;

        let (keyword, name) = {
            let keyword = self.pop_token(KeywordFun).unwrap();
            let name = self.pop_token(NameValue).expect("Function Name");

            (keyword.offset() as u32, VariableIdentifier(name.range()))
        };

        let (open, arguments, close) = {
            let (o, a, c) = match self.raw.peek() {
                Some(tt::Node::Braced(open, a, close)) => (open, a, close),
                _ => unimplemented!(),
            };

            self.raw.pop_node();

            assert_eq!(o.kind(), tt::Kind::ParenthesisOpen);
            assert_eq!(c.kind(), tt::Kind::ParenthesisClose);

            let arguments = self.parse_arguments(a);

            (o.offset() as u32, arguments, c.offset() as u32)
        };

        let (arrow, result) =
            if let Some(arrow) = self.pop_token(SignArrowSingle) {
                (arrow.offset() as u32, typ::parse_type(&mut self.raw))
            } else {
                (0, unimplemented!())
            };

        let body = expr::parse_expression(&mut self.raw);

        Function {
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
}

//
//  Implementation Details
//
impl<'a, 'g, 'local> FunParser<'a, 'g, 'local> {
    fn parse_arguments(&self, arguments: &'a [tt::Node]) -> &'g [Argument] {
        use self::tt::Kind::*;

        if arguments.is_empty() {
            return &[];
        }

        let mut raw = self.raw.spawn(arguments);
        let mut buffer = raw.local_array();

        while raw.peek().is_some() {
            let name = tok::pop_token(NameValue, &mut raw).expect("Name");

            let colon = tok::pop_token(SignColon, &mut raw)
                .map(|t| t.offset() as u32).unwrap_or(0);

            let type_ = typ::parse_type(&mut raw);

            let comma = tok::pop_token(SignComma, &mut raw)
                .map(|t| t.offset() as u32).unwrap_or(0);

            buffer.push(Argument {
                name: VariableIdentifier(name.range()),
                type_: type_,
                colon: colon,
                comma: comma,
            });
        }

        self.raw.global().insert_slice(buffer.into_slice())
    }

    fn pop_token(&mut self, kind: tt::Kind) -> Option<tt::Token> {
        tok::pop_token(kind, &mut self.raw)
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
    fn basic_argument_less() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            funit(&global_arena, b":fun add() -> Int { 1 + 2 }"),
            Function {
                name: VariableIdentifier(range(5, 3)),
                arguments: &[],
                result: TypeIdentifier(range(14, 3)),
                body: Expression::Block(
                    &Expression::BinOp(
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
                    range(18, 9),
                ),
                keyword: 0,
                open: 8,
                close: 9,
                arrow: 11,
            }
        );
    }

    #[test]
    fn basic_add() {
        let global_arena = mem::Arena::new();

        assert_eq!(
            funit(&global_arena, b":fun add(a: Int, b: Int) -> Int { a + b }"),
            Function {
                name: VariableIdentifier(range(5, 3)),
                arguments: &[
                    Argument {
                        name: VariableIdentifier(range(9, 1)),
                        type_: TypeIdentifier(range(12, 3)),
                        colon: 10,
                        comma: 15,
                    },
                    Argument {
                        name: VariableIdentifier(range(17, 1)),
                        type_: TypeIdentifier(range(20, 3)),
                        colon: 18,
                        comma: 0,
                    }
                ],
                result: TypeIdentifier(range(28, 3)),
                body: Expression::Block(
                    &Expression::BinOp(
                        BinaryOperator::Plus,
                        &Expression::Var( VariableIdentifier(range(34, 1)) ),
                        &Expression::Var( VariableIdentifier(range(38, 1)) ),
                    ),
                    range(32, 9),
                ),
                keyword: 0,
                open: 8,
                close: 23,
                arrow: 25,
            }
        );
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn funit<'g>(global_arena: &'g mem::Arena, raw: &[u8]) -> Function<'g> {
        use super::super::com::RawParser;

        let mut local_arena = mem::Arena::new();

        let f = {
            let raw = RawParser::from_raw(raw, &global_arena, &local_arena);
            super::FunParser::new(raw).parse()
        };
        local_arena.recycle();

        f
    }
}

