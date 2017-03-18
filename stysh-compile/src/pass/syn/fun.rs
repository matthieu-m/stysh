//! Syntactic pass, aka parsing.
//!
//! Function parser.

use model::syn::*;
use model::tt;

use super::com::RawParser;
use super::expr;

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
        let (keyword, name) = {
            let start = match self.raw.peek() {
                Some(tt::Node::Run(tokens)) => tokens,
                _ => unimplemented!(),
            };

            assert_eq!(start.len(), 2);
            assert_eq!(start[0].kind(), tt::Kind::KeywordFun);
            assert_eq!(start[1].kind(), tt::Kind::NameValue);

            self.raw.pop_tokens(2);

            (start[0].offset() as u32, VariableIdentifier(start[1].range()))
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

        let (arrow, result) = {
            let result = match self.raw.peek() {
                Some(tt::Node::Run(tokens)) => tokens,
                _ => unimplemented!(),
            };

            assert_eq!(result.len(), 2);
            assert_eq!(result[0].kind(), tt::Kind::SignArrowSingle);
            assert_eq!(result[1].kind(), tt::Kind::NameType);

            self.raw.pop_tokens(2);

            (result[0].offset() as u32, TypeIdentifier(result[1].range()))
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
    fn parse_arguments(&self, _arguments: &[tt::Node]) -> &'g [Argument] {
        &[]
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
