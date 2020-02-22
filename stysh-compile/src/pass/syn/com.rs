//! Syntactic pass, aka parsing.
//!
//! Common helper functions.

use std::{cell, fmt};

use crate::basic::mem;
use crate::basic::com::{Range, Span, MultiStore};
use crate::model::{tt, ast};
use crate::pass::lex;

#[derive(Clone, Copy)]
pub struct RawParser<'a, 'tree> {
    state: ParserState<'a>,
    source: &'a [u8],
    module: &'a cell::RefCell<ast::Module>,
    tree: &'tree cell::RefCell<ast::Tree>,
    interner: &'a mem::Interner,
    arena: &'a mem::Arena,
}

//
//  High-level parsing functions.
//
impl<'a, 'tree> RawParser<'a, 'tree> {
    /// Parses a path, possibly empty.
    pub fn parse_path<S>(&mut self, store: &cell::RefCell<S>) -> ast::Path
        where
            S: MultiStore<ast::Identifier> + MultiStore<u32>,
    {
        let (components, colons) = match self.state.nodes.first().cloned() {
            Some(tt::Node::Run(tokens)) => self.parse_path_impl(tokens),
            _ => return ast::Path::empty(),
        };

        debug_assert!(components.len() == colons.len());

        let components = store.borrow_mut().push_slice(&components);
        let colons = store.borrow_mut().push_slice(&colons);

        ast::Path { components, colons, }
    }

    /// Parses a tuple, whether expression, pattern, or type.
    pub fn parse_tuple<F, S, T: Copy>(
        &mut self,
        store: &cell::RefCell<S>,
        inner_parser: F,
        separator: tt::Kind,
        ns: &'a [tt::Node<'a>],
        o: tt::Token,
        c: tt::Token,
    )
        -> ast::Tuple<T>
        where
            F: Fn(&mut RawParser<'a, 'tree>) -> ast::Id<T>,
            S: MultiStore<ast::Id<T>> + MultiStore<ast::Identifier> + MultiStore<u32>,
    {
        let mut inner = self.spawn(ns);

        let mut fields = vec!();
        let mut commas = vec!();
        let mut names = vec!();
        let mut separators = vec!();

        let mut named = false;

        loop {
            if let Some(n) = inner.pop_kind(tt::Kind::NameField) {
                names.push(ast::Identifier(self.intern_id_of(n), n.span()));
                named = true;
            }

            if let Some(c) = inner.pop_kind(separator) {
                separators.push(c.span().offset() as u32);
            }

            if let Some(t) = inner.peek() {
                fields.push(inner_parser(&mut inner));

                if names.len() < fields.len() {
                    let range = Range::new(t.span().offset(), 0);
                    names.push(ast::Identifier(Default::default(), range));
                }

                if separators.len() < names.len() {
                    separators.push(t.span().offset() as u32);
                }

                if let Some(c) = inner.pop_kind(tt::Kind::SignComma) {
                    commas.push(c.span().offset() as u32)
                } else {
                    commas.push(t.span().end_offset() as u32 - 1)
                }

                continue;
            }

            break;
        }

        assert!(inner.peek().is_none());

        let mut store = store.borrow_mut();

        ast::Tuple {
            fields: store.push_slice(&fields),
            commas: store.push_slice(&commas),
            names: if named { store.push_slice(&names) } else { ast::Id::empty() },
            separators: if named { store.push_slice(&separators) } else { ast::Id::empty() },
            open: o.offset() as u32,
            close: c.offset() as u32,
        }
    }
}

//
//  Low-level parsing functions.
//
impl<'a, 'tree> RawParser<'a, 'tree> {
    pub fn from_raw(
        source: &'a [u8],
        module: &'a cell::RefCell<ast::Module>,
        tree: &'tree cell::RefCell<ast::Tree>,
        interner: &'a mem::Interner,
        arena: &'a mem::Arena,
    )
        -> RawParser<'a, 'tree>
    {
        let local_arena = mem::Arena::default();
        let lexer = lex::Lexer::new(interner, arena, &local_arena);
        RawParser {
            state: ParserState::new(lexer.parse(source)),
            source,
            module,
            tree,
            interner,
            arena,
        }
    }

    pub fn module(&self) -> &'a cell::RefCell<ast::Module> { self.module }

    pub fn tree(&self) -> &'tree cell::RefCell<ast::Tree> { self.tree }

    pub fn rescope<'b>(
        &self,
        tree: &'b cell::RefCell<ast::Tree>
    )
        -> RawParser<'a, 'b>
    {
        RawParser {
            state: self.state,
            source: self.source,
            module: self.module,
            tree,
            interner: self.interner,
            arena: self.arena,
        } 
    }

    pub fn spawn(
        &self,
        nodes: &'a [tt::Node<'a>],
    )
        -> RawParser<'a, 'tree>
    {
        RawParser {
            state: ParserState::new(nodes),
            source: self.source,
            module: self.module,
            tree: self.tree,
            interner: self.interner,
            arena: self.arena,
        }
    }

    pub fn intern_id_of(&self, token: tt::Token) -> mem::InternId {
        let raw = self.source(token);
        if let Some(id) = self.interner.lookup(raw) {
            id
        } else {
            panic!("Unknown token {:?}: {:?}", token, raw);
        }
    }

    pub fn intern_bytes(&self, bytes: &[u8]) -> mem::InternId {
        self.interner.insert(bytes)
    }

    pub fn source(&self, token: tt::Token) -> &'a [u8] {
        if token.kind() == tt::Kind::NameField {
            &self.source[token.span()][1..]
        } else {
            &self.source[token.span()]
        }
    }

    pub fn resolve_identifier(&self, token: tt::Token) -> ast::Identifier {
        debug_assert!(
            token.kind() == tt::Kind::NameField ||
            token.kind() == tt::Kind::NameType ||
            token.kind() == tt::Kind::NameValue,
            "{:?}",
            token
        );

        let id = self.intern_id_of(token);
        ast::Identifier(id, token.span())
    }

    pub fn resolve_type(&self, token: tt::Token) -> ast::TypeIdentifier {
        debug_assert!(token.kind() == tt::Kind::NameType);

        let id = self.intern_id_of(token);
        ast::TypeIdentifier(id, token.span())
    }

    pub fn resolve_variable(&self, token: tt::Token)
        -> ast::VariableIdentifier
    {
        debug_assert!(token.kind() == tt::Kind::NameValue);

        let id = self.intern_id_of(token);
        ast::VariableIdentifier(id, token.span())
    }

    pub fn peek(&self) -> Option<tt::Node<'a>> { self.state.peek() }

    pub fn peek_kind(&self) -> Option<tt::Kind> {
        self.peek_token().map(|tok| tok.kind())
    }

    pub fn peek_token(&self) -> Option<tt::Token> { self.state.peek_token() }

    pub fn pop_node(&mut self) { self.state.pop_node(); }

    pub fn pop_kind(&mut self, kind: tt::Kind) -> Option<tt::Token> {
        if let Some(tok) = self.peek_token() {
            if tok.kind() == kind {
                self.pop_tokens(1);
                return Some(tok);
            }
        }
        None
    }

    pub fn pop_tokens(&mut self, nb: usize) { self.state.pop_tokens(nb); }
}

impl<'a, 'tree> RawParser<'a, 'tree> {
    fn parse_path_impl(&mut self, mut tokens: &[tt::Token])
        -> (Vec<ast::Identifier>, Vec<u32>)
    {
        use self::tt::Kind::*;

        let mut components = vec!();
        let mut colons = vec!();

        while tokens.len() >= 2 {
            let (i, c) = (tokens[0], tokens[1]);

            if c.kind() != SignDoubleColon {
                break;
            }

            if i.kind() != NameModule && i.kind() != NameType {
                break;
            }

            components.push(ast::Identifier(self.intern_id_of(i), i.span()));
            colons.push(c.offset() as u32);
            tokens = &tokens[2..]; 
        }

        self.pop_tokens(colons.len() * 2);

        (components, colons)
    }
}

impl<'a, 'tree> fmt::Debug for RawParser<'a, 'tree>  {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "RawParser {{ {:?} }}", self.state)
    }
}

//
//  Implementation Details
//
#[derive(Clone, Copy)]
struct ParserState<'a> {
    nodes: &'a [tt::Node<'a>],
    run_start: usize,
}

impl<'a> ParserState<'a> {
    fn new(nodes: &'a [tt::Node<'a>]) -> ParserState<'a> {
        ParserState { nodes: nodes, run_start: 0 }
    }

    fn peek(&self) -> Option<tt::Node<'a>> {
        match self.nodes.first().cloned() {
            Some(tt::Node::Run(run)) =>
                Some(tt::Node::Run(&run[self.run_start..self.run_start+1])),
            other => other,
        }
    }

    fn peek_token(&self) -> Option<tt::Token> {
        match self.nodes.first().cloned() {
            Some(tt::Node::Run(run)) => Some(run[self.run_start]),
            Some(tt::Node::Braced(o, _, _)) => Some(o),
            _ => None,
        }
    }

    fn pop_node(&mut self) {
        self.nodes = &self.nodes[1..];
        self.run_start = 0;
    }

    fn pop_tokens(&mut self, nb: usize) {
        if let Some(tt::Node::Run(run)) = self.nodes.first().cloned() {
            debug_assert!(self.run_start + nb <= run.len());

            self.run_start += nb;

            if self.run_start == run.len() {
                self.run_start = 0;
                self.pop_node();
            }
        } else {
            panic!("Unreachable!");
        }
    }
}

impl<'a> fmt::Debug for ParserState<'a>  {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "ParserState {{ [")?;
        match self.nodes.first().cloned() {
            Some(tt::Node::Run(run)) => {
                write!(f, "Run({:?})", &run[self.run_start..])
            },
            other => write!(f, "{:?}", other),
        }?;
        if self.nodes.len() > 1 {
            for n in &self.nodes[1..] {
                write!(f, ", {:?}", n)?;
            }
        }
        write!(f, "] }}")
    }
}

#[cfg(test)]
pub mod tests {
    use std::{cell, rc};
    use crate::basic::mem;
    use crate::model::ast;
    use crate::model::ast::builder::{
        Factory, ExprFactory, ItemFactory, PatternFactory, StmtFactory,
        TypeFactory, RcModule, RcTree,
    };
    use crate::model::ast::interning::Resolver;
    use super::RawParser;

    pub struct Env {
        actual_module: cell::RefCell<ast::Module>,
        actual_tree: cell::RefCell<ast::Tree>,
        expected_module: RcModule,
        expected_tree: RcTree,
        resolver: Resolver,
        interner: rc::Rc<mem::Interner>,
        arena: mem::Arena,
    }

    impl Env {
        pub fn new(source: &[u8]) -> Self {
            let resolver = Resolver::new(source, Default::default());
            let interner = resolver.interner();
            Env {
                actual_module: cell::RefCell::default(),
                actual_tree: cell::RefCell::default(),
                expected_module: RcModule::default(),
                expected_tree: RcTree::default(),
                resolver,
                interner,
                arena: mem::Arena::default(),
            }
        }

        pub fn actual_module(&self) -> &cell::RefCell<ast::Module> {
            &self.actual_module
        }

        pub fn actual_tree(&self) -> &cell::RefCell<ast::Tree> {
            &self.actual_tree
        }

        pub fn expected_module(&self) -> &cell::RefCell<ast::Module> {
            &self.expected_module
        }

        pub fn expected_tree(&self) -> &cell::RefCell<ast::Tree> {
            &self.expected_tree
        }

        pub fn reset_expected_tree(&self) {
            std::mem::take(&mut *self.expected_tree.borrow_mut());
        }

        pub fn factory(&self) -> Factory {
            Factory::new(
                self.expected_module.clone(),
                self.expected_tree.clone(),
                self.resolver.clone(),
            )
        }

        pub fn factories(&self) -> (
            ExprFactory,
            ItemFactory,
            PatternFactory,
            StmtFactory,
            TypeFactory<ast::Module>,
            TypeFactory<ast::Tree>,
        )
        {
            let f = self.factory();
            (f.expr(), f.item(), f.pat(), f.stmt(), f.type_module(), f.type_())
        }

        pub fn raw<'a>(&'a self) -> RawParser<'a, 'a> {
            RawParser::from_raw(
                self.resolver.source(),
                &self.actual_module,
                &self.actual_tree,
                &self.interner,
                &self.arena
            )
        }
    }

    impl Drop for Env {
        fn drop(&mut self) { self.arena.recycle() }
    }
}
