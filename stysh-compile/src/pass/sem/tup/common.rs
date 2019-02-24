//! Common Type Unifier.

use basic::mem::DynArray;

use model::hir::*;

use super::{Alteration, Context};

/// Core Unifier
#[derive(Clone, Copy, Debug)]
pub struct CoreUnifier<'a> {
    /// Context.
    pub context: &'a Context,
    /// Registry.
    pub registry: &'a Registry,
}

//
//  Public interface of CoreUnifier
//

impl<'a> CoreUnifier<'a> {
    /// Creates a new instance.
    pub fn new(context: &'a Context, registry: &'a Registry) -> Self {
        CoreUnifier { registry, context }
    }

    /// Returns the type of a known GVN.
    ///
    /// Panics: If the GVN is unknown.
    pub fn type_of(&self, gvn: Gvn) -> Type {
        self.context.value(gvn).type_()
    }

    /// Unifies an option.
    pub fn unify_option<T, U, F>(&self, o: Option<T>, fun: F)
        -> Alteration<Option<U>>
        where
            F: FnOnce(T) -> Alteration<U>,
    {
        match o {
            None => Alteration::forward(None),
            Some(t) => fun(t).map(Some),
        }
    }

    /// Unifies an array.
    pub fn unify_array<T, F>(&self, array: DynArray<T>, mut fun: F)
        -> Alteration<DynArray<T>>
        where
            T: Default,
            F: FnMut(usize, T) -> Alteration<T>,
    {
        let mut altered = 0;

        for i in 0..array.len() {
            let element = array.replace_with_default(i);

            let alteration = fun(i, element);

            altered += alteration.altered;
            array.replace(i, alteration.entity);
        }

        Alteration { entity: array, altered }
    }

    /// Unifies a tuple.
    pub fn unify_tuple<T, F>(&self, t: Tuple<T>, ty: Type, mut fun: F)
        -> Alteration<Tuple<T>>
        where
            T: Default,
            F: FnMut(T, Type) -> Alteration<T>,
    {
        let fields = if let Type::Tuple(ty, ..) = ty {
            self.unify_array(t.fields.clone(), |i, e| {
                if let Some(ty) = ty.fields.get(i) {
                    fun(e, ty)
                } else {
                    Alteration::forward(e)
                }
            })
        } else {
            self.unify_array(t.fields.clone(), |_, e| fun(e, Type::unresolved()))
        };
        fields.combine(t, |t, f| Tuple { fields: f, names: t.names })
    }
}

#[cfg(test)]
pub mod tests {
    use std::rc;

    use basic::mem;

    use model::hir::*;
    use model::hir::gn::GlobalNumberer;
    use model::hir::builder::{
        Factory, ItemFactory, PatternFactory, PrototypeFactory, StmtFactory,
        TypeFactory, ValueFactory,
    };
    use model::hir::interning::{Resolver, Scrubber};

    use super::{Context, CoreUnifier};

    #[derive(Default)]
    pub struct Env;

    pub struct LocalEnv<'g> {
        registry: mocks::MockRegistry,
        context: Context,
        resolver: Resolver<'g>,
    }

    impl Env {
        pub fn local<'g>(&self, raw: &'g [u8]) -> LocalEnv<'g> {
            let interner = rc::Rc::new(mem::Interner::new());
            LocalEnv {
                registry: mocks::MockRegistry::new(),
                context: Context::default(),
                resolver: Resolver::new(raw, interner),
            }
        }

        pub fn factories(&self) -> (
            ItemFactory,
            PatternFactory,
            PrototypeFactory,
            StmtFactory,
            TypeFactory,
            ValueFactory
        )
        {
            let f = Factory::new();
            (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.value())
        }
    }

    impl<'g> LocalEnv<'g> {
        pub fn core<'a>(&'a self) -> CoreUnifier<'a> {
            CoreUnifier::new(&self.context, &self.registry)
        }

        pub fn numberer(&self) -> GlobalNumberer {
            GlobalNumberer::new()
        }

        pub fn resolver(&self) -> &Resolver<'g> { &self.resolver }

        pub fn scrubber(&self) -> Scrubber {
            Scrubber::new()
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
