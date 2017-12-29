//! Syntactic pass, aka parsing.
//!
//! Common helper functions.

use std;

use basic::mem;
use model::tt;
use pass::lex;

#[derive(Clone, Copy)]
pub struct RawParser<'a, 'g, 'local> {
    state: ParserState<'a>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> RawParser<'a, 'g, 'local> {
    pub fn new(
        nodes: &'a [tt::Node<'a>],
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> RawParser<'a, 'g, 'local>
    {
        RawParser {
            state: ParserState::new(nodes),
            global_arena: global,
            local_arena: local,
        }
    }

    pub fn from_raw(
        raw: &[u8],
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> RawParser<'local, 'g, 'local>
    {
        let mut lexer = lex::Lexer::new(local, local);
        RawParser {
            state: ParserState::new(lexer.parse(raw)),
            global_arena: global,
            local_arena: local,
        }
    }

    pub fn spawn<'b>(&self, nodes: &'b [tt::Node<'b>])
        -> RawParser<'b, 'g, 'local>
    {
        RawParser {
            state: ParserState::new(nodes),
            global_arena: self.global_arena,
            local_arena: self.local_arena,
        }
    }

    pub fn global(&self) -> &'g mem::Arena { self.global_arena }

    pub fn local(&self) -> &'local mem::Arena { self.local_arena }

    pub fn local_array<T>(&self) -> mem::Array<'local, T> {
        mem::Array::new(self.local_arena)
    }

    pub fn peek(&self) -> Option<tt::Node<'a>> { self.state.peek() }

    pub fn peek_kind(&self) -> Option<tt::Kind> {
        self.state.peek_token().map(|tok| tok.kind())
    }

    pub fn pop_node(&mut self) { self.state.pop_node(); }

    pub fn pop_kind(&mut self, kind: tt::Kind) -> Option<tt::Token> {
        if let Some(tok) = self.state.peek_token() {
            if tok.kind() == kind {
                self.pop_tokens(1);
                return Some(tok);
            }
        }
        None
    }

    pub fn pop_tokens(&mut self, nb: usize) { self.state.pop_tokens(nb); }

    pub fn intern<T: 'g>(&self, t: T) -> &'g T {
        self.global_arena.insert(t)
    }

    pub fn intern_slice<T: 'g>(&self, ts: &[T]) -> &'g [T] {
        self.global_arena.insert_slice(ts)
    }
}

impl<'a, 'g, 'local> std::fmt::Debug for RawParser<'a, 'g, 'local>  {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
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

impl<'a> std::fmt::Debug for ParserState<'a>  {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
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
