//! Semantic Pass: Common Elements

use std::{cell, collections};

use basic::com::{Range, Span};

use model::hir::*;
use super::flat::*;

/// Context.
///
/// An instance of context contains supplementary information about a specific
/// item/value being processed by the semantic passes. Information updated
/// incrementally as each successive pass is altered.
#[derive(Clone, Debug, Default)]
pub struct Context<'g> {
    imp: cell::RefCell<ContextImpl>,
    values: cell::RefCell<ValueContext<'g>>,
}

//
//  Public interface of Context
//
impl<'g> Context<'g> {
    //
    //  General
    //
    
    /// Creates an instance.
    pub fn new() -> Self { Default::default() }

    /// Clears an instance, readying it for reuse.
    pub fn clear(&mut self) {
        self.imp.borrow_mut().clear();
        self.values.borrow_mut().clear();
    }

    /// Returns a fresh GIN.
    pub fn gin(&self) -> Gin {
        let gin = self.imp.borrow_mut().gin();
        //  TODO: register in types.
        gin
    }

    /// Returns a fresh GVN.
    pub fn gvn(&self) -> Gvn {
        let gvn = self.imp.borrow_mut().gvn();
        self.values.borrow_mut().register(gvn);
        gvn
    }

    /// Returns a Value handle.
    ///
    /// Panics: If the GVN is invalid.
    pub fn value<'a>(&'a self, gvn: Gvn) -> ValueHandle<'a, 'g> {
        ValueHandle::new(&self.values, gvn)
    }

    /// Returns the current iteration.
    pub fn iteration(&self) -> usize { self.imp.borrow().iteration() }

    /// Moves on to the next iteration.
    ///
    /// The context contains multiple work queues, for various algorithms. In
    /// a given iteration, only items that were queued prior to the iteration
    /// start can be retrieved.
    ///
    /// This ensures that the work done in a single iteration is bounded, and
    /// that the various passes each get a turn, which is crucial seeing as
    /// they are inter-dependent, and will unlock each other's progress.
    pub fn next_iteration(&self) { self.imp.borrow_mut().next_iteration() }

    //
    //  Bindings
    //

    /// Returns the GIN associated with the item binding.
    pub fn lookup_item(&self, name: ItemIdentifier) -> Option<Gin> {
        self.imp.borrow().lookup_item(name)
    }

    /// Inserts an item binding.
    pub fn insert_item(&self, name: ItemIdentifier) -> Gin {
        let gin = self.gin();
        self.imp.borrow_mut().insert_item(gin, name);
        gin
    }

    /// Returns the GVN associated with the value binding.
    pub fn lookup_value(&self, name: ValueIdentifier) -> Option<Gvn> {
        self.imp.borrow().lookup_value(name)
    }

    /// Inserts a value binding with its currently known type.
    pub fn insert_value(&self, name: ValueIdentifier, ty: Type<'g>) -> Gvn {
        let gvn = self.gvn();
        self.imp.borrow_mut().insert_value(gvn, name);
        self.value(gvn).set_type(ty);
        gvn
    }

    //
    //  Nested Entities to Fetch.
    //

    /// Returns the number of unfetched elements, regardless of their iteration.
    pub fn unfetched(&self) -> usize { self.imp.borrow().unfetched() }

    /// Pops the next unfetched item from the queue, if any remains in this
    /// iteration.
    ///
    /// Note:   If fetching the item is not possible, then it should be put
    ///         back with `push_unfetched` below.
    pub fn pop_unfetched<'a>(&'a self) -> Option<ValueHandle<'a, 'g>> {
        self.imp.borrow_mut().pop_unfetched().map(|g| self.value(g))
    }

    /// Pushes a new unfetched item in the queue, scheduling it for the next
    /// iteration.
    ///
    /// Note:   It is expected that any element added to the HIR item
    ///         associated with this context should be immediately pushed if it
    ///         contains an unfetched item.
    pub fn push_unfetched<'a>(&'a self, h: ValueHandle<'a, 'g>) {
        self.imp.borrow_mut().push_unfetched(h.gvn());
    }

    //
    //  Types to Unify and Propagate.
    //

}

//
//  Implementation Details
//

#[derive(Clone, Debug, Default)]
struct ContextImpl {
    //  Global Item Number.
    gin: u32,
    //  Global Value Number.
    gvn: u32,
    //  Current Iteration Number.
    iteration: u32,
    //  Bindings of a particular function/value.
    items: collections::HashMap<Range, Gin>,
    //  Bindings of a particular function/value.
    values: collections::HashMap<Range, Gvn>,
    //  Nested Entities to Fetch.
    unfetched: WorkQueue<Gvn>,
    //  Types to Unify and Propagate.
}

#[derive(Clone, Debug, Default)]
struct WorkQueue<T>(collections::VecDeque<(u32, T)>);

//
//  Implementation of ContextImpl
//

impl ContextImpl {
    //
    //  General
    //

    fn clear(&mut self) {
        self.gin = BuiltinType::maximum_gin().0;
        self.gvn = 0;
        self.iteration = 0;
        self.values.clear();
        self.unfetched.clear();
    }

    fn gin(&mut self) -> Gin {
        self.gin += 1;

        Gin(self.gin)
    }

    fn gvn(&mut self) -> Gvn {
        self.gvn += 1;

        Gvn(self.gvn)
    }

    fn iteration(&self) -> usize { self.iteration as usize }

    fn next_iteration(&mut self) { self.iteration += 1; }

    //
    //  Bindings
    //

    fn lookup_item(&self, name: ItemIdentifier) -> Option<Gin> {
        self.items.get(&name.span()).cloned()
    }

    fn insert_item(&mut self, gin: Gin, name: ItemIdentifier) {
        debug_assert!(
            !self.items.contains_key(&name.span()),
            "{:?} already contained", name
        );

        self.items.insert(name.span(), gin);
    }

    fn lookup_value(&self, name: ValueIdentifier) -> Option<Gvn> {
        self.values.get(&name.span()).cloned()
    }

    fn insert_value(&mut self, gvn: Gvn, name: ValueIdentifier) {
        debug_assert!(
            !self.values.contains_key(&name.span()),
            "{:?} already contained", name
        );

        self.values.insert(name.span(), gvn);
    }

    //
    //  Nested Entities to Fetch.
    //

    fn unfetched(&self) -> usize { self.unfetched.len() }

    fn pop_unfetched(&mut self) -> Option<Gvn> {
        self.unfetched.pop(self.iteration)
    }

    fn push_unfetched(&mut self, gvn: Gvn) {
        self.unfetched.push(self.iteration, gvn);
    }

    //
    //  Types to Unify and Propagate.
    //

}

//
//  Implementation of WorkQueue
//

impl<T> WorkQueue<T> {
    fn len(&self) -> usize { self.0.len() }

    fn clear(&mut self) { self.0.clear(); }

    fn pop(&mut self, iteration: u32) -> Option<T> {
        if let Some(e) = self.0.pop_front() {
            if e.0 < iteration {
                Some(e.1)
            } else {
                self.0.push_front(e);
                None
            }
        } else {
            None
        }
    }

    fn push(&mut self, iteration: u32, e: T) {
        self.0.push_back((iteration, e));
    }
}
