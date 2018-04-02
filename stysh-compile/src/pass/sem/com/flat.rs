//! Semantic Pass: Common Elements
//!
//! A flat model has one inherent advantage over a graph model for processing:
//! random access to nodes.
//!
//! For iterative passes working off a work-queue, and correlating facts
//! between distant elements, it is much more efficient to jump from one node
//! than to have to traverse the whole graph.

use std::cell;

use model::hir::*;

use super::tbl::Table;

/// ValueContext
///
/// A Flat Model for Patterns and Values.
#[derive(Clone, Debug, Default)]
pub struct ValueContext<'b> {
    children: Table<Gvn, ValueChildren>,
    types: Table<Gvn, Type<'b>>,
}

/// ValueHandle.
///
/// A handle to manipulate the state attached to a given GVN without having
/// to manually thread it in each time.
#[derive(Clone, Copy, Debug)]
pub struct ValueHandle<'a, 'b: 'a> {
    context: &'a cell::RefCell<ValueContext<'b>>,
    gvn: Gvn,
}

/// ValueChildren
///
/// Relationship of this value with its children.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ValueChildren {
    /// A field of another value.
    FieldOf(Field, Gvn),
    /// A leaf value, without children (Default).
    Leaf,
}

//
//  Public interface of ValueContext
//

impl<'b> ValueContext<'b> {
    /// Notifies the ValueContext of the creation of a new value.
    pub fn register(&mut self, gvn: Gvn) {
        debug_assert!(gvn.0 as usize == self.children.len());

        self.children.push(Default::default());
        self.types.push(Default::default());
    }

    /// Clears the existing values.
    pub fn clear(&mut self) {
        self.children.clear();
        self.types.clear();
    }
}

//
//  Public interface of ValueHandle
//

impl<'a, 'b: 'a> ValueHandle<'a, 'b> {
    /// Creates an instance of ValueHandle.
    pub fn new(context: &'a cell::RefCell<ValueContext<'b>>, gvn: Gvn) -> Self {
        assert!(gvn != Default::default(), "A Default GVN is invalid.");
        assert!(
            gvn.0 < context.borrow().children.len() as u32,
            "Unknown GVN {}, next is {}.", gvn.0, context.borrow().children.len()
        );

        ValueHandle { context, gvn }
    }

    /// Returns the GVN of the value.
    pub fn gvn(&self) -> Gvn { self.gvn }

    /// Returns the children of this value.
    pub fn children(&self) -> ValueChildren { self.borrow().children.get(self.gvn) }

    /// Sets the children of a value, inserting or updating as appropriate.
    pub fn set_children(&self, parent: ValueChildren) {
        self.borrow_mut().children.set(self.gvn, parent);
    }

    /// Returns the type of an existing value.
    ///
    /// Note:   Return Type::Unresolved(Default) unless the type was previously
    ///         set.
    pub fn type_(&self) -> Type<'b> {
        self.borrow().types.get(self.gvn)
    }

    /// Sets the type of a GVN, inserting or updating as appropriate.
    ///
    /// Note:   Automatically set when inserting a binding.
    pub fn set_type(&self, ty: Type<'b>) {
        self.borrow_mut().types.set(self.gvn, ty);
    }
}

//
//  Implementation of ValueHandle
//

impl<'a, 'b: 'a> ValueHandle<'a, 'b> {
    fn borrow(&self) -> cell::Ref<'a, ValueContext<'b>> {
        self.context.borrow()
    }

    fn borrow_mut(&self) -> cell::RefMut<'a, ValueContext<'b>> {
        self.context.borrow_mut()
    }
}

//
//  Implementation of traits for ValueChildren
//

impl Default for ValueChildren {
    fn default() -> Self { ValueChildren::Leaf }
}
