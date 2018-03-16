//! Syntactic pass, aka parsing.
//!
//! Function parser.

use basic::com::Span;

use model::ast::*;
use model::tt;

use super::com::RawParser;
use super::{expr, typ};

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

            (keyword.offset() as u32, self.raw.resolve_variable(name))
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
                unimplemented!()
            };

        let body = expr::parse_expression(&mut self.raw);

        let block = if let Expression::Block(body) = body { 
            *body
        } else {
            Block {
                statements: &[],
                expression: Some(self.raw.intern(body)),
                open: body.span().offset() as u32,
                close: body.span().end_offset() as u32 - 1
            }
        };

        Function {
            name: name,
            arguments: arguments,
            result: result,
            body: block,
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
    fn parse_arguments(&self, arguments: &'a [tt::Node]) -> &'g [Argument<'g>] {
        use self::tt::Kind::*;

        if arguments.is_empty() {
            return &[];
        }

        let mut raw = self.raw.spawn(arguments);
        let mut buffer = raw.local_array();

        while raw.peek().is_some() {
            let name = raw.pop_kind(NameValue).expect("Name");

            let colon = raw.pop_kind(SignColon)
                .map(|t| t.offset() as u32).unwrap_or(0);

            let type_ = typ::parse_type(&mut raw);

            let comma = raw.pop_kind(SignComma)
                .map(|t| t.offset() as u32)
                .unwrap_or(type_.span().end_offset() as u32 - 1);

            buffer.push(Argument {
                name: self.raw.resolve_variable(name),
                type_: type_,
                colon: colon,
                comma: comma,
            });
        }

        self.raw.global().insert_slice(buffer.into_slice())
    }

    fn pop_token(&mut self, kind: tt::Kind) -> Option<tt::Token> {
        self.raw.pop_kind(kind)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use super::super::com::tests::Env;
    use model::ast::*;

    #[test]
    fn basic_argument_less() {
        let env = Env::new();
        let (e, i, _, _, t) = env.factories();

        assert_eq!(
            funit(&env, b":fun add() -> Int { 1 + 2 }"),
            i.function(
                5,
                3,
                t.simple(14, 3),
                e.block(e.bin_op(e.int(1, 20), e.int(2, 24)).build()).build(),
            ).build()
        );
    }

    #[test]
    fn basic_add() {
        let env = Env::new();
        let (e, i, _, _, t) = env.factories();

        assert_eq!(
            funit(&env, b":fun add(a: Int, b: Int) -> Int { a + b }"),
            i.function(
                5,
                3,
                t.simple(28, 3),
                e.block(e.bin_op(e.var(34, 1), e.var(38, 1)).build()).build(),
            )
            .push(9, 1, t.simple(12, 3))
            .push(17, 1, t.simple(20, 3))
            .build()
        );
    }

    fn funit<'g>(env: &'g Env, raw: &[u8]) -> Function<'g> {
        let local = env.local();
        env.scrubber().scrub_function(
            super::FunParser::new(local.raw(raw)).parse()
        )
    }
}

