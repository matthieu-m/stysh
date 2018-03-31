//! Common Elements.

use std::{cell, cmp, collections};

use basic::com::{self, Span};

use model::hir::*;

/// Context.
///
/// An instance of context contains supplementary information about a specific
/// item/value being processed by the semantic passes. Information updated
/// incrementally as each successive pass is altered.
#[derive(Clone, Debug, Default)]
pub struct Context<'a>(cell::RefCell<ContextImpl<'a>>);

/// Resolution.
///
/// A Resolution represents the outcome of a transformation pass.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Resolution<T> {
    /// Entity transformed.
    pub entity: T,
    /// Number of entities altered in this pass.
    pub altered: u32,
    /// Number of entities introduced in this pass.
    pub introduced: u32,
}

//
//  Public interface of Context
//
impl<'a> Context<'a> {
    //
    //  General
    //
    
    /// Creates an instance.
    pub fn new() -> Self { Default::default() }

    /// Clears an instance, readying it for reuse.
    pub fn clear(&mut self) { self.0.borrow_mut().clear() }

    //
    //  Bindings
    //

    /// Returns a GVN.
    pub fn gvn(&self) -> Gvn { self.0.borrow_mut().gvn() }

    /// Returns the most up-to-date type associated with the binding.
    ///
    /// Panics: if the name is unknown.
    pub fn get_binding(&self, name: ValueIdentifier) -> (Gvn, Type<'a>) {
        self.0.borrow().get_binding(name)
    }

    /// Inserts a binding with its currently known GVN and type.
    ///
    /// Panics: In Debug, if the binding is already inserted.
    pub fn insert_argument(&self, name: ValueIdentifier, gvn: Gvn, ty: Type<'a>) {
        self.0.borrow_mut().insert_binding(name, gvn, ty);
    }

    /// Inserts a binding with its currently known type.
    ///
    /// Panics: In Debug, if the binding is already inserted.
    pub fn insert_binding(&self, name: ValueIdentifier, ty: Type<'a>) -> Gvn {
        let gvn = self.gvn();
        self.0.borrow_mut().insert_binding(name, gvn, ty)
    }

    /// Updates an existing binding with a more up-to-date type.
    ///
    /// Panics: In Debug, if the binding is not already inserted.
    pub fn update_binding(&self, name: ValueIdentifier, ty: Type<'a>) {
        self.0.borrow_mut().update_binding(name, ty);
    }

    //
    //  Names not yet resolved by the Nested Entity Fetcher.
    //

    /// Returns the number of names to fetch.
    pub fn unfetched(&self) -> usize { self.0.borrow().unfetched() }

    /// Mark an ItemIdentifier as unfetched.
    ///
    /// Panics: In Debug, if already marked.
    pub fn mark_unfetched_item(&self, name: ItemIdentifier) {
        self.0.borrow_mut().mark_unfetched_item(name);
    }

    /// Mark an ValueIdentifier as unfetched.
    ///
    /// Panics: In Debug, if already marked.
    pub fn mark_unfetched_value(&self, name: ValueIdentifier) {
        self.0.borrow_mut().mark_unfetched_value(name);
    }

    /// Mark the ItemIdentifier as fetched.
    ///
    /// Panics: In Debug, if unmarked.
    pub fn fetched_item(&self, name: ItemIdentifier) {
        self.0.borrow_mut().fetched_item(name);
    }

    /// Mark the ValueIdentifier as fetched.
    ///
    /// Panics: In Debug, if unmarked.
    pub fn fetched_value(&self, name: ValueIdentifier) {
        self.0.borrow_mut().fetched_value(name);
    }
}

//
//  Public interface of Resolution
//

impl<T> Resolution<T> {
    /// Creates a Resolution with both altered and introduced set to 0.
    pub fn forward(e: T) -> Self {
        Resolution { entity: e, altered: 0, introduced: 0 }
    }

    /// Creates a Resolution with altered set to 1.
    pub fn update(e: T) -> Self { Resolution::forward(e).with_altered(1) }

    /// Sets the number of altered entities.
    pub fn with_altered(mut self, n: u32) -> Self {
        self.altered = n;
        self
    }

    /// Sets the number of introduced entities.
    pub fn with_introduced(mut self, n: u32) -> Self {
        self.introduced = n;
        self
    }

    /// Transforms the entity.
    pub fn map<R, F>(self, f: F) -> Resolution<R>
        where
            F: FnOnce(T) -> R
    {
        let (altered, introduced) = (self.altered, self.introduced);
        let entity = f(self.entity);

        Resolution { entity, altered, introduced }
    }

    /// Transforms into another resolution.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references,
    /// -   introduced: the sum of introduced references.
    pub fn combine<R, F>(self, r: R, f: F) -> Resolution<R>
        where
            F: FnOnce(T) -> R
    {
        let (altered, introduced) = (self.altered, self.introduced);
        let entity = if altered == 0 { r } else { f(self.entity) };

        Resolution { entity, altered, introduced }
    }

    /// Combines with another resolution.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references,
    /// -   introduced: the sum of introduced references.
    pub fn combine2<T1, R, F>(self, r: R, t1: Resolution<T1>, f: F)
        -> Resolution<R>
        where
            F: FnOnce(T, T1) -> R
    {
        let altered = self.altered + t1.altered;
        let introduced = self.introduced + t1.introduced;
        let entity =
            if altered == 0 { r } else { f(self.entity, t1.entity) };

        Resolution { entity, altered, introduced }
    }

    /// Combines with two other resolutions.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references,
    /// -   introduced: the sum of introduced references.
    pub fn combine3<T1, T2, R, F>(
        self,
        r: R,
        t1: Resolution<T1>,
        t2: Resolution<T2>,
        f: F,
    )
        -> Resolution<R>
        where
            F: FnOnce(T, T1, T2) -> R
    {
        let altered = self.altered + t1.altered + t2.altered;
        let introduced = self.introduced + t1.introduced + t2.introduced;
        let entity = if altered == 0 {
            r
        } else {
            f(self.entity, t1.entity, t2.entity)
        };

        Resolution { entity, altered, introduced }
    }
}

//
//  Implementation Details of Context
//

#[derive(Clone, Debug, Default)]
struct ContextImpl<'a> {
    //  Global Value Number.
    gvn: u32,
    //  Bindings of a particular function/value.
    bindings: collections::HashMap<com::Range, (Gvn, Type<'a>)>,
    //  Names not yet resolved ("fetched").
    unfetched_items: collections::HashSet<com::Range>,
    unfetched_values: collections::HashSet<com::Range>,
}

impl<'a> ContextImpl<'a> {
    //
    //  General
    //
    fn clear(&mut self) {
        self.bindings.clear();
        self.unfetched_items.clear();
        self.unfetched_values.clear();
    }

    //
    //  Bindings
    //
    fn gvn(&mut self) -> Gvn {
        self.gvn += 1;
        Gvn(self.gvn)
    }

    fn get_binding(&self, name: ValueIdentifier) -> (Gvn, Type<'a>) {
        match self.bindings.get(&name.span()).cloned() {
            Some(t) => t,
            None => unreachable!(
                "Unknown identifier {:?} in {:?}",
                name, self.bindings
            ),
        }
    }

    fn insert_binding(&mut self, name: ValueIdentifier, gvn: Gvn, ty: Type<'a>)
        -> Gvn
    {
        debug_assert!(
            !self.bindings.contains_key(&name.span()),
            "{:?} already contained", name
        );

        //  When a custom gvn is used, it could overtake the generator.
        self.gvn = cmp::max(self.gvn, gvn.0);

        self.bindings.insert(name.span(), (gvn, ty));
        gvn
    }

    fn update_binding(&mut self, name: ValueIdentifier, ty: Type<'a>) {
        let v = self.bindings.get_mut(&name.span());
        debug_assert!(v.is_some(), "{:?} not already contained", name);

        v.unwrap().1 = ty;
    }

    //
    //  Names not yet resolved by the Nested Entity Fetcher.
    //

    fn unfetched(&self) -> usize {
        self.unfetched_items.len() + self.unfetched_values.len()
    }

    fn mark_unfetched_item(&mut self, name: ItemIdentifier) {
        debug_assert!(
            !self.unfetched_items.contains(&name.span()),
            "{:?} is already marked as unfetched!", name
        );
        self.unfetched_items.insert(name.span());
    }

    fn mark_unfetched_value(&mut self, name: ValueIdentifier) {
        debug_assert!(
            !self.unfetched_values.contains(&name.span()),
            "{:?} is already marked as unfetched!", name
        );
        self.unfetched_values.insert(name.span());
    }

    fn fetched_item(&mut self, name: ItemIdentifier) {
        debug_assert!(
            self.unfetched_items.contains(&name.span()),
            "{:?} is not marked as unfetched!", name
        );
        self.unfetched_items.remove(&name.span());
    }

    fn fetched_value(&mut self, name: ValueIdentifier) {
        debug_assert!(
            self.unfetched_values.contains(&name.span()),
            "{:?} is not marked as unfetched!", name
        );
        self.unfetched_values.remove(&name.span());
    }
}
