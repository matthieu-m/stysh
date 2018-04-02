//! Core Finalizer.

use basic::mem;

use model::hir::*;

use super::{Alteration, Context};

/// Core Finalizer
#[derive(Clone, Copy, Debug)]
pub struct CoreFinalizer<'a, 'g: 'a> {
    /// Context.
    pub context: &'a Context<'g>,
    /// Arena.
    pub global_arena: &'g mem::Arena,
}

//
//  Public interface of CoreFinalizer
//

impl<'a, 'g: 'a> CoreFinalizer<'a, 'g> {
    /// Creates a new instance.
    pub fn new(
        context: &'a Context<'g>,
        global_arena: &'g mem::Arena,
    )
        -> Self
    {
        CoreFinalizer { context, global_arena }
    }

    /// Inserts into the global_arena.
    pub fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    /// Finalizes an option.
    pub fn finalize_option<T, U, F>(&self, o: Option<T>, f: F)
        -> Alteration<Option<U>>
        where
            F: FnOnce(T) -> Alteration<U>,
    {
        match o {
            None => Alteration::forward(None),
            Some(t) => f(t).map(Some),
        }
    }

    /// Finalizes a slice.
    pub fn finalize_slice<T, F>(&self, slice: &'g [T], mut f: F)
        -> Alteration<&'g [T]>
        where
            T: Copy + 'g,
            F: FnMut(T) -> Alteration<T>,
    {
        let mut found = None;

        for (i, e) in slice.iter().enumerate() {
            let a = f(*e);

            if a.altered > 0 {
                found = Some((i, a));
                break;
            }
        }

        if found.is_none() {
            return Alteration::forward(slice);
        }

        let (index, alteration) = found.unwrap();

        let length = slice.len();
        let mut result = mem::Array::with_capacity(length, self.global_arena);

        for e in &slice[..(index as usize)] {
            result.push(*e);
        }

        let mut altered = alteration.altered;
        result.push(alteration.entity);

        if index as usize + 1 < length {
            for e in &slice[(index as usize + 1)..] {
                let a = f(*e);

                altered += a.altered;

                result.push(a.entity);
            }
        }

        let entity = result.into_slice();
        Alteration { entity, altered }
    }

    /// Finalizes a tuple.
    pub fn finalize_tuple<T, F>(&self, t: Tuple<'g, T>, f: F)
        -> Alteration<Tuple<'g, T>>
        where
            T: Copy + 'g,
            F: FnMut(T) -> Alteration<T>,
    {
        self.finalize_slice(t.fields, f)
            .combine(t, |f| Tuple { fields: f, names: t.names })
    }

}
