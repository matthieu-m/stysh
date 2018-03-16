//! Syntactic pass, aka parsing.
//!
//! This pass is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

use std::iter;

use basic::mem;

use model::tt;
use model::ast::*;

use pass::syn::com::RawParser;
use pass::syn::{expr, fun, typ};

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
    pub fn new(
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
    )
        -> Parser<'g, 'local>
    {
        Parser { global_arena, local_arena }
    }

    /// Parses a raw slite of bytes into an Abstract Syntax Tree.
    pub fn parse(&mut self, raw: &[u8], interner: &'g mem::Interner)
        -> List<'g>
    {
        ParserImpl::new(RawParser::from_raw(
            raw,
            interner,
            self.global_arena,
            self.local_arena,
        )).parse_all()
    }
}

//
//  Implementation Details
//
struct ParserImpl<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>,
}

impl<'a, 'g, 'local> iter::Iterator for ParserImpl<'a, 'g, 'local> {
    type Item = Node<'g>;

    fn next(&mut self) -> Option<Node<'g>> {
        self.raw.peek()
            .map(|node| {
                match node.front().kind() {
                    tt::Kind::KeywordEnum => Node::Item(self.parse_enum()),
                    tt::Kind::KeywordFun => Node::Item(self.parse_function()),
                    tt::Kind::KeywordRec => Node::Item(self.parse_record()),
                    tt::Kind::KeywordSet => Node::Stmt(self.parse_statement()),
                    tt::Kind::KeywordVar => Node::Stmt(self.parse_statement()),
                    _ => Node::Expr(self.parse_expression()),
                }
            })
    }
}

impl<'a, 'g, 'local> ParserImpl<'a, 'g, 'local> {
    fn new(raw: RawParser<'a, 'g, 'local>) -> ParserImpl<'a, 'g, 'local> {
        ParserImpl { raw: raw }
    }

    fn parse_all(self) -> List<'g> {
        let global_arena = self.raw.global();
        let mut buffer = mem::Array::new(self.raw.local());

        for node in self {
            buffer.push(node);
        }

        global_arena.insert_slice(buffer.into_slice())
    }

    fn parse_expression(&mut self) -> Expression<'g> {
        expr::parse_expression(&mut self.raw)
    }

    fn parse_enum(&mut self) -> Item<'g> {
        Item::Enum(typ::parse_enum(&mut self.raw))
    }

    fn parse_function(&mut self) -> Item<'g> {
        Item::Fun(fun::parse_function(&mut self.raw))
    }

    fn parse_record(&mut self) -> Item<'g> {
        Item::Rec(typ::parse_record(&mut self.raw))
    }

    fn parse_statement(&mut self) -> Statement<'g> {
        expr::parse_statement(&mut self.raw)
    }
}
