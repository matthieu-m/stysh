//! Common Nested Entity Fetcher

use basic::mem;

use model::hir::*;

use super::{Context, Resolution};

/// Core Fetcher
#[derive(Clone, Copy, Debug)]
pub struct CoreFetcher<'a, 'g>
    where 'g: 'a
{
    /// Registry.
    pub registry: &'a Registry<'g>,
    /// Context.
    pub context: &'a Context<'g>,
    /// Arena.
    pub global_arena: &'g mem::Arena,
}

//
//  Public interface of CoreFetcher
//

impl<'a, 'g> CoreFetcher<'a, 'g>
    where 'g: 'a
{
    /// Creates a new instance.
    pub fn new(
        registry: &'a Registry<'g>,
        context: &'a Context<'g>,
        global_arena: &'g mem::Arena,
    )
        -> Self
    {
        CoreFetcher { registry, context, global_arena }
    }

    /// Inserts into the global_arena.
    pub fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    /// Fetches an option.
    pub fn fetch_option<T, F>(&self, o: Option<T>, f: F)
        -> Resolution<Option<T>>
        where
            F: FnOnce(T) -> Resolution<T>,
    {
        match o {
            None => Resolution::forward(None),
            Some(t) => f(t).map(Some),
        }
    }

    /// Fetches a slice.
    pub fn fetch_slice<T, F>(&self, slice: &'g [T], mut f: F)
        -> Resolution<&'g [T]>
        where
            T: Copy + 'g,
            F: FnMut(T) -> Resolution<T>,
    {
        let mut found = None;

        for (i, e) in slice.iter().enumerate() {
            let r = f(*e);

            if r.altered > 0 {
                found = Some((i, r));
                break;
            }
        }

        if found.is_none() {
            return Resolution::forward(slice);
        }

        let found = found.unwrap();

        let length = slice.len();
        let mut result = mem::Array::with_capacity(length, self.global_arena);

        for e in &slice[..(found.0 as usize)] {
            result.push(*e);
        }

        let mut altered = found.1.altered;
        result.push(found.1.entity);

        if found.0 as usize + 1 < length {
            for e in &slice[(found.0 as usize + 1)..] {
                let r = f(*e);

                altered += r.altered;

                result.push(r.entity);
            }
        }

        let (entity, introduced) = (result.into_slice(), 0);
        Resolution { entity, altered, introduced }
    }
    

    /// Fetches a tuple.
    pub fn fetch_tuple<T, F>(&self, t: Tuple<'g, T>, f: F)
        -> Resolution<Tuple<'g, T>>
        where
            T: Copy + 'g,
            F: FnMut(T) -> Resolution<T>,
    {
        let fields = self.fetch_slice(t.fields, f);
        fields.combine(t, |f| Tuple { fields: f, names: t.names })
    }
}

#[cfg(test)]
pub mod tests {
    use std::rc;

    use basic::mem;

    use model::hir::*;
    use model::hir::builder::{
        Factory, ItemFactory, PatternFactory, PrototypeFactory, StmtFactory,
        TypeFactory, ValueFactory,
    };
    use model::hir::interning::{Resolver, Scrubber};

    use super::{Context, CoreFetcher};

    #[derive(Default)]
    pub struct Env {
        global_arena: mem::Arena,
    }

    pub struct LocalEnv<'g> {
        global_arena: &'g mem::Arena,
        registry: mocks::MockRegistry<'g>,
        context: Context<'g>,
        resolver: Resolver<'g>,
    }

    impl Env {
        pub fn local<'g>(&'g self, raw: &'g [u8]) -> LocalEnv<'g> {
            let interner = rc::Rc::new(mem::Interner::new());
            LocalEnv {
                global_arena: &self.global_arena,
                registry: mocks::MockRegistry::new(&self.global_arena),
                context: Context::default(),
                resolver: Resolver::new(raw, interner, &self.global_arena),
            }
        }

        pub fn factories<'g>(&'g self) -> (
            ItemFactory<'g>,
            PatternFactory<'g>,
            PrototypeFactory<'g>,
            StmtFactory<'g>,
            TypeFactory<'g>,
            ValueFactory<'g>
        )
        {
            let f = Factory::new(&self.global_arena);
            (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.value())
        }
    }

    impl<'g> LocalEnv<'g> {
        pub fn core<'a>(&'a self) -> CoreFetcher<'a, 'g> {
            CoreFetcher::new(&self.registry, &self.context, self.global_arena)
        }

        pub fn mark_unfetched_items(&self, items: &[ItemIdentifier]) {
            for i in items { self.context.mark_unfetched_item(*i); }
        }

        pub fn resolver(&self) -> &Resolver<'g> { &self.resolver }

        pub fn scrubber(&self) -> Scrubber<'g> {
            Scrubber::new(self.global_arena)
        }

        pub fn insert_enum(&mut self, e: Enum) {
            let e = self.resolver.resolve_enum(e);
            self.registry.insert_enum(e);
        }

        #[allow(dead_code)]
        pub fn insert_record(&mut self, r: Record) {
            let r = self.resolver.resolve_record(r);
            self.registry.insert_record(r);
        }
    }
}
