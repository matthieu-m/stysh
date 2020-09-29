//! Interning.
//!
//! Test facilities to remove or add the InternId.

use std::rc;

use crate::basic::com::{self, Range};
use crate::basic::mem;

use crate::model::ast::*;

/// Resolver
#[derive(Clone)]
pub struct Resolver {
    source: Vec<u8>,
    interner: rc::Rc<mem::Interner>,
}

//
//  Public interface of Resolver
//
impl Resolver {
    /// Creates an instance.
    pub fn new(
        source: &[u8],
        interner: rc::Rc<mem::Interner>,
    )
        -> Self
    {
        Resolver { source: source.to_vec(), interner }
    }

    /// Returns reference to source.
    pub fn source(&self) -> &[u8] { &self.source }

    /// Returns reference to Interner.
    pub fn interner(&self) -> rc::Rc<mem::Interner> { self.interner.clone() }

    /// Returns resolved Identifier.
    pub fn resolve_identifier(&self, range: Range) -> Identifier {
        Identifier(self.from_range(range), range)
    }

    /// Returns resolved FieldIdentifier.
    pub fn resolve_field_identifier(&self, range: Range) -> FieldIdentifier {
        let identifier = self.resolve_identifier(range.skip_left(1));
        FieldIdentifier::Name(identifier.with_range(range))
    }

    /// Returns resolved TypeIdentifier.
    pub fn resolve_type_identifier(&self, range: Range) -> TypeIdentifier {
        TypeIdentifier(self.from_range(range), range)
    }

    /// Returns resolved VariableIdentifier.
    pub fn resolve_variable_identifier(&self, range: Range) -> VariableIdentifier {
        VariableIdentifier(self.from_range(range), range)
    }

    /// Returns raw mem::InternId.
    ///
    /// This automatically strips leading `.`, if any.
    pub fn resolve_range(&self, range: com::Range) -> mem::InternId {
        let mut raw = self.slice(range);
        if let Some((first, tail)) = raw.split_first() {
            if *first == b'.' {
                raw = tail;
            }
        }
        self.interner.insert(raw)
    }
}

//
//  Implementation of Resolver
//
impl Resolver {
    fn from_range(&self, range: com::Range) -> mem::InternId {
        let raw = self.slice(range);
        self.interner.insert(raw)
    }

    fn slice(&self, range: com::Range) -> &[u8] {
        let slice: &[u8] = &self.source;
        &slice[range]
    }
}
