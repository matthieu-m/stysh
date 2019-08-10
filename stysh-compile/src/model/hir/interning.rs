//! Interning.
//!
//! Test facilities to remove the InternId.

use std::rc;

use crate::basic::com::{self, Span};
use crate::basic::mem;

use crate::model::hir::*;

/// Resolver
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
    pub fn interner(&self) -> &mem::Interner { &*self.interner }

    /// Resolves InternId, recursively.
    pub fn resolve_item_id(&self, i: ItemIdentifier) -> ItemIdentifier {
        i.with_id(self.from_range(i.span()))
    }

    /// Resolves InternId, recursively.
    pub fn resolve_value_id(&self, v: ValueIdentifier) -> ValueIdentifier {
        v.with_id(self.from_range(v.span()))
    }

    /// Obtains the InternId of the specified range.
    pub fn from_range(&self, range: com::Range) -> mem::InternId {
        if range == Default::default() {
            Default::default()
        } else {
            let raw = &self.source()[range];
            self.interner.insert(raw)
        }
    }
}
