//! Syntactic pass, aka parsing.
//!
//! This pass is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

use std::iter;

use basic::mem;

use model::tt;
use model::syn::*;

use pass::syn::com::RawParser;
use pass::syn::{expr, fun};

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
        ParserImpl::new(
            RawParser::from_raw(raw, self.global_arena, self.local_arena)
        ).parse_all()
    }

    /// Transforms a slice of raw Token Trees into an Abstract Syntax Tree.
    pub fn transform(&mut self, nodes: &[tt::Node]) -> List<'g> {
        ParserImpl::new(
            RawParser::new(nodes, self.global_arena, self.local_arena)
        ).parse_all()
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

    fn parse_function(&mut self) -> Item<'g> {
        Item::Fun(fun::parse_function(&mut self.raw))
    }
}
