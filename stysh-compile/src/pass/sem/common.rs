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

/// Alteration.
///
/// An Alteration represents the outcome of a transformation pass.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Alteration<T> {
    /// Entity altered (or not).
    pub entity: T,
    /// Number of entities altered, including the entity itself, if any.
    pub altered: u32,
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

    /// Returns a fresh GVN.
    pub fn gvn(&self) -> Gvn { self.0.borrow_mut().gvn() }

    //
    //  Bindings
    //

    /// Returns the GVN associated with the binding.
    pub fn lookup_binding(&self, name: ValueIdentifier) -> Option<Gvn> {
        self.0.borrow().lookup_binding(name)
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

    //
    //  Nested Entities to Fetch.
    //

    //
    //  Types to Unify and Propagate.
    //

    //
    //  HIR summary, in table format
    //

    /// Returns the type of an existing value.
    ///
    /// Panics: If the value does not exist.
    pub fn type_of(&self, gvn: Gvn) -> Type<'a> {
        self.0.borrow().type_of(gvn)
    }

    /// Sets the type of a GVN, existing or no.
    ///
    /// Note:   Automatically when inserting a binding.
    pub fn set_type_of(&self, gvn: Gvn, ty: Type<'a>) {
        self.0.borrow_mut().set_type_of(gvn, ty);
    }

}

//
//  Public interface of Alteration
//

impl<T> Alteration<T> {
    /// Creates an Alteration with altered set to 0.
    pub fn forward(e: T) -> Self { Alteration { entity: e, altered: 0 } }

    /// Creates an Alteration with altered set to 1.
    pub fn update(e: T) -> Self { Alteration::forward(e).with_altered(1) }

    /// Sets the number of altered entities.
    pub fn with_altered(mut self, n: u32) -> Self {
        self.altered = n;
        self
    }

    /// Transforms the entity.
    pub fn map<R, F>(self, f: F) -> Alteration<R>
        where
            F: FnOnce(T) -> R
    {
        let altered = self.altered;
        let entity = f(self.entity);

        Alteration { entity, altered }
    }

    /// Transforms into another Alteration.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references.
    pub fn combine<R, F>(self, r: R, f: F) -> Alteration<R>
        where
            F: FnOnce(T) -> R
    {
        let altered = self.altered;
        let entity = if altered == 0 { r } else { f(self.entity) };

        Alteration { entity, altered }
    }

    /// Combines with another Alteration.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references.
    pub fn combine2<T1, R, F>(self, r: R, t1: Alteration<T1>, f: F)
        -> Alteration<R>
        where
            F: FnOnce(T, T1) -> R
    {
        let altered = self.altered + t1.altered;
        let entity =
            if altered == 0 { r } else { f(self.entity, t1.entity) };

        Alteration { entity, altered }
    }

    /// Combines with two other resolutions.
    ///
    /// The resulting combination contains:
    /// -   entity: `r`, if nothing was altered, or the result of `f`,
    /// -   altered: the sum of altered references.
    pub fn combine3<T1, T2, R, F>(
        self,
        r: R,
        t1: Alteration<T1>,
        t2: Alteration<T2>,
        f: F,
    )
        -> Alteration<R>
        where
            F: FnOnce(T, T1, T2) -> R
    {
        let altered = self.altered + t1.altered + t2.altered;
        let entity = if altered == 0 {
            r
        } else {
            f(self.entity, t1.entity, t2.entity)
        };

        Alteration { entity, altered }
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
    bindings: collections::HashMap<com::Range, Gvn>,
    //  Nested Entities to Fetch.
    //  Types to Unify and Propagate.
    //  HIR summary, in table format.
    types: Vec<Type<'a>>,
}

impl<'a> ContextImpl<'a> {
    //
    //  General
    //

    fn clear(&mut self) {
        self.gvn = 0;
        self.bindings.clear();
        self.types.clear();
    }

    fn gvn(&mut self) -> Gvn {
        self.gvn += 1;
        Gvn(self.gvn)
    }

    //
    //  Bindings
    //

    fn lookup_binding(&self, name: ValueIdentifier) -> Option<Gvn> {
        self.bindings.get(&name.span()).cloned()
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

        self.bindings.insert(name.span(), gvn);
        self.set_type_of(gvn, ty);
        gvn
    }

    //
    //  Nested Entities to Fetch.
    //

    //
    //  Types to Unify and Propagate.
    //

    //
    //  HIR summary, in table format
    //

    fn type_of(&self, gvn: Gvn) -> Type<'a> { self.types[self.index_of(gvn)] }

    fn set_type_of(&mut self, gvn: Gvn, ty: Type<'a>) {
        let index = self.index_of(gvn);

        if index >= self.types.len() {
            self.types.resize(index + 1, Type::unresolved());
        }

        self.types[index] = ty;
    }

    fn index_of(&self, gvn: Gvn) -> usize { gvn.0 as usize }
}
