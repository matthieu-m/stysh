//! Syntactic pass, aka parsing.
//!
//! This pass is in charge of transforming the Token Tree into the Syn model,
//! aka Abstract Syntax Tree.

use std::cell;

use crate::basic::mem;

use crate::model::tt;
use crate::model::ast::*;

use super::com::RawParser;
use super::{expr, fun, typ};

/// The Stysh parser.
///
/// The responsibility of the parser is to transform raw input into an Abstract
/// Syntax Tree.
pub struct Parser;

impl Parser {
    /// Creates a new instance of the parser.
    pub fn new() -> Parser { Parser }

    /// Parses a raw slite of bytes into an Abstract Syntax Tree.
    pub fn parse(&mut self, raw: &[u8], interner: &mem::Interner)
        -> (Module, Tree)
    {
        let module = cell::RefCell::default();
        let tree = cell::RefCell::default();
        let arena = mem::Arena::default();

        {
            let mut raw = RawParser::from_raw(
                raw,
                &module,
                &tree,
                interner,
                &arena,
            );

            while let Some(node) = raw.peek() {
                match node.front().kind() {
                    tt::Kind::KeywordEnum => { typ::parse_enum(&mut raw); },
                    tt::Kind::KeywordFun => { fun::parse_function(&mut raw); },
                    tt::Kind::KeywordRec => { typ::parse_record(&mut raw); },
                    tt::Kind::KeywordSet => { expr::parse_statement(&mut raw); },
                    tt::Kind::KeywordVar => { expr::parse_statement(&mut raw); },
                    _ => {
                        let id = expr::parse_expression(&mut raw);
                        tree.borrow_mut().set_root(Root::Expression(id));
                    },
                }
            }
        }

        (module.into_inner(), tree.into_inner())
    }
}
