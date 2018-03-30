//! Common Type Unifier.

use basic::mem;

use model::hir::*;

use super::{Context, Resolution};

/// Core Unifier
#[derive(Clone, Copy, Debug)]
pub struct CoreUnifier<'a, 'g>
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
//  Public interface of CoreUnifier
//

impl<'a, 'g> CoreUnifier<'a, 'g>
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
        CoreUnifier { registry, context, global_arena }
    }

    /// Returns the fields associated with the type, if any.
    pub fn fields_of(&self, c: Type<'g>) -> Option<Tuple<'g, Type<'g>>> {
        use self::Type::*;

        match c {
            Builtin(_) | Enum(..) | Unresolved(..) => None,
            Rec(r, _) => self.registry.lookup_record(r.name).map(|r| r.definition),
            Tuple(t) => Some(t),
        }
    }

    /// Inserts into the global_arena.
    pub fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    /// Returns the type of a known binding.
    ///
    /// Panics: If the binding is unknown.
    pub fn type_of(&self, name: ValueIdentifier) -> Type<'g> {
        self.context.get_binding(name).1
    }

    /// Unifies an option.
    pub fn unify_option<T, U, F>(&self, o: Option<T>, f: F)
        -> Resolution<Option<U>>
        where
            F: FnOnce(T) -> Resolution<U>,
    {
        match o {
            None => Resolution::forward(None),
            Some(t) => f(t).map(Some),
        }
    }

    /// Unifies a slice.
    pub fn unify_slice<T, F>(&self, slice: &'g [T], mut f: F)
        -> Resolution<&'g [T]>
        where
            T: Copy + 'g,
            F: FnMut(usize, T) -> Resolution<T>,
    {
        let mut found = None;
        let mut introduced = 0;

        for (i, e) in slice.iter().enumerate() {
            let r = f(i, *e);

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
            for (i, e) in slice[(found.0 as usize + 1)..].iter().enumerate() {
                let r = f(i + found.0 as usize, *e);

                altered += r.altered;
                introduced += r.introduced;

                result.push(r.entity);
            }
        }

        let entity = result.into_slice();
        Resolution { entity, altered, introduced }
    }

    /// Unifies a tuple.
    pub fn unify_tuple<T, F>(&self, t: Tuple<'g, T>, ty: Type<'g>, mut f: F)
        -> Resolution<Tuple<'g, T>>
        where
            T: Copy + 'g,
            F: FnMut(T, Type<'g>) -> Resolution<T>,
    {
        let fields = if let Type::Tuple(ty) = ty {
            self.unify_slice(t.fields, |i, e| {
                ty.fields.get(i).map(|ty| f(e, *ty))
                    .unwrap_or(Resolution::forward(e))
            })
        } else {
            self.unify_slice(t.fields, |_, e| f(e, Type::unresolved()))
        };
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

    use super::{Context, CoreUnifier};

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
        pub fn core<'a>(&'a self) -> CoreUnifier<'a, 'g> {
            CoreUnifier::new(&self.registry, &self.context, self.global_arena)
        }

        pub fn resolver(&self) -> &Resolver<'g> { &self.resolver }

        pub fn scrubber(&self) -> Scrubber<'g> {
            Scrubber::new(self.global_arena)
        }

        #[allow(dead_code)]
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
