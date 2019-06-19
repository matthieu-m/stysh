//! Semantic Pass: Common Elements

use std::{cell, collections, fmt, marker};

use basic::com::{Range, Span};
use basic::sea::{self, TableIndex};

use model::hir::*;

use super::Relation;

/// Context.
///
/// An instance of context contains supplementary information about a specific
/// item/value being processed by the semantic passes. Information updated
/// incrementally as each successive pass alters the HIR tree.
#[derive(Clone, Debug, Default)]
pub struct Context {
    imp: cell::RefCell<ContextImpl>,
}

//
//  Public interface of Context
//
impl Context {
    //
    //  General
    //
    
    /// Creates an instance.
    pub fn new() -> Self { Default::default() }

    /// Clears an instance, readying it for reuse.
    pub fn clear(&mut self) {
        self.imp.borrow_mut().clear();
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

    /// Returns the GVN associated with the value binding.
    pub fn lookup_value(&self, name: ValueIdentifier) -> Option<Gvn> {
        self.imp.borrow().lookup_value(name)
    }

    /// Inserts a value binding with its currently known type.
    pub fn insert_value(&self, name: ValueIdentifier, gvn: Gvn) {
        self.imp.borrow_mut().insert_value(gvn, name);
    }

    //
    //  Tracking
    //

    /// Returns the Gvns linked to the one passed as argument.
    pub fn get_gvn_links(&self, gvn: Gvn) -> Vec<Gvn> {
        self.imp.borrow().get_gvn_links(gvn).to_vec()
    }

    /// Links all the Gvns passed together.
    pub fn link_gvns(&self, gvns: &[Gvn]) {
        self.imp.borrow_mut().link_gvns(gvns);
    }

    /// Returns the Relations linked to the TypeId passed as argument.
    pub fn get_type_links(&self, ty: TypeId) -> Vec<Relation<TypeId>> {
        self.imp.borrow().get_type_links(ty).to_vec()
    }

    /// Links two TypeId together.
    pub fn link_types(&self, ty: TypeId, rel: Relation<TypeId>) {
        self.imp.borrow_mut().link_types(ty, rel);
    }

    //
    //  Nested Entities to Fetch.
    //

    /// Returns the number of fetched elements within the current iteration.
    pub fn fetched(&self) -> usize { self.imp.borrow().fetched() }

    /// Returns the number of unfetched elements, regardless of their iteration.
    pub fn unfetched(&self) -> usize { self.imp.borrow().unfetched() }

    /// Pops the next unfetched item from the queue, if any remains in this
    /// iteration.
    ///
    /// Note:   If fetching the item is not possible, then it should be put
    ///         back with `push_unfetched` below, otherwise it should be put
    ///         back with `push_fetched`.
    pub fn pop_unfetched(&self) -> Option<Gvn> {
        self.imp.borrow_mut().pop_unfetched()
    }

    /// Pushes a new fetched item, marking it as fetched.
    ///
    /// Note:   It is unexpected to later push the same item as unfetched!
    pub fn push_fetched(&self, gvn: Gvn) {
        self.imp.borrow_mut().push_fetched(gvn)
    }

    /// Pushes a new unfetched item in the queue, scheduling it for the next
    /// iteration.
    ///
    /// Note:   It is expected that any element added to the HIR item
    ///         associated with this context should be immediately pushed if it
    ///         contains an unfetched item.
    pub fn push_unfetched(&self, gvn: Gvn) {
        self.imp.borrow_mut().push_unfetched(gvn);
    }

    //
    //  Types to Unify and Propagate.
    //

    /// Returns the number of unified elements within the current iteration.
    pub fn unified(&self) -> usize { self.imp.borrow().unified() }

    /// Returns the number of diverging elements, regardless of their iteration.
    pub fn diverging(&self) -> usize { self.imp.borrow().diverging() }

    /// Pops the next diverging item from the queue, if any remains in this
    /// iteration.
    ///
    /// Note:   If unifying the item is not possible, then it should be put
    ///         back with `push_diverging` below, otherwise it should be put
    ///         back with `push_unified`.
    pub fn pop_diverging(&self) -> Option<Gvn> {
        self.imp.borrow_mut().pop_diverging()
    }

    /// Pushes a new unified item, marking it as unified.
    ///
    /// Note:   It is unexpected to later push the same item as diverging!
    pub fn push_unified(&self, gvn: Gvn) {
        self.imp.borrow_mut().push_unified(gvn);
    }

    /// Pushes a new diverging item in the queue, scheduling it for the next
    /// iteration.
    ///
    /// Note:   It is expected that any element added to the HIR item
    ///         associated with this context should be immediately pushed if
    ///         it contains a diverging type.
    pub fn push_diverging(&self, gvn: Gvn) {
        self.imp.borrow_mut().push_diverging(gvn);
    }
}

//
//  Implementation Details
//

#[derive(Clone, Debug, Default)]
struct ContextImpl {
    //  Current Iteration Number.
    iteration: u32,
    //  Number of Entities Fetched during Iteration.
    fetched: u32,
    //  Number of Entities Unified during Iteration.
    unified: u32,
    //  Bindings of a particular function/value.
    values: collections::HashMap<Range, Gvn>,
    //  Tracker of the status of each value.
    tracker: Tracker,
    //  Links of each value.
    gvn_links: GvnLinks,
    //  Links of each type.
    type_links: TypeLinks,
    //  Nested Entities to Fetch.
    unfetched: WorkQueue<Gvn>,
    //  Types to Unify and Propagate.
    diverging: WorkQueue<Gvn>,
}

#[derive(Clone, Debug, Default)]
struct Tracker {
    expressions: TrackerImpl<ExpressionId>,
    patterns: TrackerImpl<PatternId>,
}

#[derive(Clone, Debug, Default)]
struct GvnLinks {
    expressions: GvnLinksImpl<ExpressionId>,
    patterns: GvnLinksImpl<PatternId>,
}

#[derive(Clone, Debug, Default)]
struct TypeLinks {
    links: Vec<Vec<Relation<TypeId>>>,
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
        self.iteration = 0;
        self.fetched = 0;
        self.unified = 0;
        self.values.clear();
        self.tracker.clear();
        self.gvn_links.clear();
        self.type_links.clear();
        self.unfetched.clear();
        self.diverging.clear();
    }

    fn iteration(&self) -> usize { self.iteration as usize }

    fn next_iteration(&mut self) {
        self.fetched = 0;
        self.unified = 0;
        self.iteration += 1;
    }

    //
    //  Bindings
    //

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
    //  Tracking
    //

    fn get_gvn_links(&self, gvn: Gvn) -> &[Gvn] {
        self.gvn_links.get(gvn)
    }

    fn link_gvns(&mut self, gvns: &[Gvn]) {
        for gvn in gvns {
            self.tracker.push(*gvn);
        }
        self.gvn_links.link_gvns(gvns);
    }

    fn push_linked_gvns(&mut self, gvn: Gvn) {
        for linked in self.gvn_links.get(gvn) {
            if !self.tracker.is_fetched(*linked) {
                self.unfetched.push(self.iteration, *linked);
            }
            if !self.tracker.is_unified(*linked) {
                self.diverging.push(self.iteration, *linked);
            }
        }
    }

    fn get_type_links(&self, ty: TypeId) -> &[Relation<TypeId>] {
        self.type_links.get(ty)
    }

    fn link_types(&mut self, ty: TypeId, rel: Relation<TypeId>) {
        self.type_links.link(ty, rel);
        self.type_links.link(*rel.get(), rel.reciprocate(ty));
    }

    //
    //  Nested Entities to Fetch.
    //

    fn fetched(&self) -> usize { self.fetched as usize }

    fn unfetched(&self) -> usize { self.unfetched.len() }

    fn pop_unfetched(&mut self) -> Option<Gvn> {
        self.unfetched.pop(self.iteration)
    }

    fn push_fetched(&mut self, gvn: Gvn) {
        self.fetched += 1;
        self.tracker.mark_fetched(gvn);
        self.push_linked_gvns(gvn);
    }

    fn push_unfetched(&mut self, gvn: Gvn) {
        debug_assert!(!self.tracker.is_fetched(gvn));
        self.unfetched.push(self.iteration, gvn);
    }

    //
    //  Types to Unify and Propagate.
    //

    fn unified(&self) -> usize { self.unified as usize }

    fn diverging(&self) -> usize { self.diverging.len() }

    fn pop_diverging(&mut self) -> Option<Gvn> {
        self.diverging.pop(self.iteration)
    }

    fn push_unified(&mut self, gvn: Gvn) {
        self.unified += 1;
        self.tracker.mark_unified(gvn);
        self.push_linked_gvns(gvn);
    }

    fn push_diverging(&mut self, gvn: Gvn) {
        debug_assert!(!self.tracker.is_unified(gvn));
        self.diverging.push(self.iteration, gvn)
    }
}

//
//  Implementation of Tracker
//

impl Tracker {
    //  General

    fn clear(&mut self) {
        self.expressions.clear();
        self.patterns.clear();
    }

    //  Accessors

    fn is_fetched(&self, gvn: Gvn) -> bool {
        if let Some(e) = gvn.as_expression() {
            self.expressions.is_fetched(&e)
        } else if let Some(p) = gvn.as_pattern() {
            self.patterns.is_fetched(&p)
        } else {
            panic!("Unexpected {:?}", gvn);
        }
    }

    fn is_unified(&self, gvn: Gvn) -> bool {
        if let Some(e) = gvn.as_expression() {
            self.expressions.is_unified(&e)
        } else if let Some(p) = gvn.as_pattern() {
            self.patterns.is_unified(&p)
        } else {
            panic!("Unexpected {:?}", gvn);
        }
    }

    //  Mutators

    fn push(&mut self, gvn: Gvn) {
        if let Some(e) = gvn.as_expression() {
            self.expressions.push(&e)
        } else if let Some(p) = gvn.as_pattern() {
            self.patterns.push(&p)
        } else {
            panic!("Unexpected {:?}", gvn);
        }
    }

    fn mark_fetched(&mut self, gvn: Gvn) {
        if let Some(e) = gvn.as_expression() {
            self.expressions.mark_fetched(&e)
        } else if let Some(p) = gvn.as_pattern() {
            self.patterns.mark_fetched(&p)
        } else {
            panic!("Unexpected {:?}", gvn);
        }
    }

    fn mark_unified(&mut self, gvn: Gvn) {
        if let Some(e) = gvn.as_expression() {
            self.expressions.mark_unified(&e)
        } else if let Some(p) = gvn.as_pattern() {
            self.patterns.mark_unified(&p)
        } else {
            panic!("Unexpected {:?}", gvn);
        }
    }
}

#[derive(Clone, Debug, Default)]
struct TrackerImpl<I: sea::TableIndex> {
    fetched: sea::Table<I, bool>,
    unified: sea::Table<I, bool>,
}

impl<I: sea::TableIndex + fmt::Debug> TrackerImpl<I> {
    //  General

    fn clear(&mut self) {
        self.fetched.clear();
        self.unified.clear();
    }

    //  Accessors

    fn is_fetched(&self, key: &I) -> bool {
        self.fetched.get(key).cloned().unwrap_or(false)
    }

    fn is_unified(&self, key: &I) -> bool {
        self.unified.get(key).cloned().unwrap_or(false)
    }

    //  Mutators

    fn push(&mut self, key: &I) {
        if key.index() < self.fetched.len() {
            return;
        }

        self.fetched.insert(key, false);
        self.unified.insert(key, false);
    }

    fn mark_fetched(&mut self, key: &I) {
        let fetched = self.fetched.at_mut(key);
        debug_assert!(!*fetched);

        *fetched = true;
    }

    fn mark_unified(&mut self, key: &I) {
        let unified = self.unified.at_mut(key);
        debug_assert!(!*unified);

        *unified = true;
    }
}

//
//  Implementation of GvnLinks
//

impl GvnLinks {
    //  General

    fn clear(&mut self) {
        self.expressions.clear();
        self.patterns.clear();
    }

    //  Accessors

    fn get(&self, gvn: Gvn) -> &[Gvn] {
        if let Some(e) = gvn.as_expression() {
            self.expressions.get(&e)
        } else if let Some(p) = gvn.as_pattern() {
            self.patterns.get(&p)
        } else {
            panic!("Unexpected {:?}", gvn);
        }
    }

    //  Mutators

    fn link_gvns(&mut self, gvns: &[Gvn]) {
        for one in gvns {
            for two in gvns {
                self.link_impl(*one, *two);
            }
        }
    }

    fn link_impl(&mut self, one: Gvn, two: Gvn) {
        if one == two { return; }

        if let Some(e) = one.as_expression() {
            self.expressions.link(&e, two)
        } else if let Some(p) = one.as_pattern() {
            self.patterns.link(&p, two)
        } else {
            panic!("Unexpected {:?}", one);
        }
    }
}

#[derive(Clone, Debug, Default)]
struct GvnLinksImpl<I: sea::TableIndex>(Vec<Vec<Gvn>>, marker::PhantomData<*const I>);

impl<I: sea::TableIndex> GvnLinksImpl<I> {
    //  General

    fn clear(&mut self) { self.0.clear(); }

    //  Accessors

    fn get(&self, key: &I) -> &[Gvn] {
        self.0.get(key.index()).map(|v| &**v).unwrap_or(&[])
    }

    //  Mutators

    fn link(&mut self, key: &I, gvn: Gvn) {
        let index = key.index();

        if index >= self.0.len() {
            self.0.resize(index + 1, vec!());
        }

        let links = &mut self.0[index];
        if !links.contains(&gvn) {
            links.push(gvn);
        }
    }
}

//
//  Implementation of TypeLinks
//

impl TypeLinks {
    //  General

    fn clear(&mut self) { self.links.clear() }

    //  Accessors

    fn get(&self, ty: TypeId) -> &[Relation<TypeId>] {
        self.links.get(ty.index()).map(|v| &**v).unwrap_or(&[])
    }

    //  Mutators

    fn link(&mut self, ty: TypeId, rel: Relation<TypeId>) {
        if ty.is_builtin() {
            return;
        }

        let index = ty.index();

        if index >= self.links.len() {
            self.links.resize(index + 1, vec!());
        }

        let links = &mut self.links[index];

        //  Check for duplicates; merge relations if necessary.
        if let Some(e) = links.into_iter().filter(|e| e.get() == rel.get()).next() {
            e.merge(&rel);
            return;
        }

        links.push(rel);
    }
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
}

impl<T: Eq> WorkQueue<T> {
    fn push(&mut self, iteration: u32, e: T) {
        if iteration == 0 || !self.0.iter().any(|item| item.1 == e) {
            self.0.push_back((iteration, e));
        }
    }
}
