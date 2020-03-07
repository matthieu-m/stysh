//! Syntactic pass, aka parsing.
//!
//! Body parser, for extensions, interfaces, and implementations.


use crate::model::tt::{Kind, Node};
use crate::model::ast::*;

use super::com::RawParser;
use super::fun;

/// Body.
pub struct Body {
    /// Functions associated with this element.
    pub functions: Id<[FunctionId]>,
    /// Offset of the opening brace.
    pub open: u32,
    /// Offset of the closing brace.
    pub close: u32,
}

/// Parses a body.
pub fn parse_body<'a, 'tree>(
    raw: &mut RawParser<'a, 'tree>,
    name: TypeIdentifier,
)
    -> Body
{
    let mut parser = BodyParser::new(*raw);
    let body = parser.parse_body(name);
    *raw = parser.into_raw();
    body
}

//
//  Implementation Details
//
struct BodyParser<'a, 'tree> {
    raw: RawParser<'a, 'tree>,
}

impl<'a, 'tree> BodyParser<'a, 'tree> {
    fn new(raw: RawParser<'a, 'tree>) -> Self { Self { raw } }

    fn into_raw(self) -> RawParser<'a, 'tree> { self.raw }

    fn parse_body(&mut self, name: TypeIdentifier) -> Body {
        //  Expects:
        //  -   {
        //  -       functions
        //  -   }
        let mut function_ids = vec!();
        let (open, close) = match self.raw.peek() {
            Some(Node::Braced(o, ns, c)) => {
                self.raw.pop_node();
                let mut raw = self.raw.spawn(ns);

                while let Some(node) = raw.peek() {
                    match node.front().kind() {
                        Kind::KeywordFun => {
                            function_ids.push(fun::parse_function(&mut raw));
                        },
                        _ => unimplemented!("parse_body - expected :fun in {:?}, got {:?}", name, node),
                    }
                }

                (o, c)
            }
            _ => unimplemented!("parse_body - unbraced {:?}", name),
        };

        let functions = self.raw.module().borrow_mut().push_function_ids(&function_ids);

        Body {
            functions,
            open: open.offset() as u32,
            close: close.offset() as u32,
        }
    }
}
