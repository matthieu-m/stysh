//! Interning.
//!
//! Test facilities to remove or add the InternId.

use std::rc;

use basic::{com, mem};

use model::ast::*;

/// Resolver
#[derive(Clone)]
pub struct Resolver<'g> {
    source: &'g [u8],
    interner: rc::Rc<mem::Interner>,
}

//
//  Public interface of Resolver
//
impl<'g> Resolver<'g> {
    /// Creates an instance.
    pub fn new(
        source: &'g [u8],
        interner: rc::Rc<mem::Interner>,
    )
        -> Self
    {
        Resolver { source, interner }
    }

    /// Returns reference to source.
    pub fn source(&self) -> &'g [u8] { self.source }

    /// Returns reference to Interner.
    pub fn interner(&self) -> rc::Rc<mem::Interner> { self.interner.clone() }

    /// Returns resolved FieldIdentifier.
    pub fn resolve_field_identifier(&self, f: FieldIdentifier) -> FieldIdentifier {
        f.with_id(self.from_range(f.span().skip_left(1)))
    }

    /// Returns resolved TypeIdentifier.
    pub fn resolve_type_identifier(&self, t: TypeIdentifier) -> TypeIdentifier {
        t.with_id(self.from_range(t.span()))
    }

    /// Returns resolved VariableIdentifier.
    pub fn resolve_variable_identifier(&self, v: VariableIdentifier)
        -> VariableIdentifier
    {
        v.with_id(self.from_range(v.span()))
    }

    /// Returns raw mem::InternId.
    ///
    /// This automatically strips leading `.`, if any.
    pub fn resolve_range(&self, range: com::Range) -> mem::InternId {
        let mut raw = &self.source[range];
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
impl<'g> Resolver<'g> {
    fn from_range(&self, range: com::Range) -> mem::InternId {
        let raw = &self.source[range];
        self.interner.insert(raw)
    }
}
