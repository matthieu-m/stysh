//! Common Type Unifier.

use std::cell;

use model::hir::*;

use super::Context;

/// Core Unifier
#[derive(Clone, Copy, Debug)]
pub struct CoreUnifier<'a> {
    /// Context.
    pub context: &'a Context,
    /// Registry.
    pub registry: &'a Registry,
    /// Tree.
    pub tree: &'a cell::RefCell<Tree>,
}

/// Status
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Status {
    /// Diverging, still, though progress may have been made.
    Diverging,
    /// Unified, whether already unified or freshly unified.
    Unified,
}

//
//  Public interface of CoreUnifier
//

impl<'a> CoreUnifier<'a> {
    /// Creates a new instance.
    pub fn new(
        context: &'a Context,
        registry: &'a Registry,
        tree: &'a cell::RefCell<Tree>,
    )
        -> Self 
    {
        CoreUnifier { context, registry, tree }
    }

    /// Returns a reference to the Tree.
    pub fn tree(&self) -> cell::Ref<'a, Tree> { self.tree.borrow() }

    /// Returns a mutable reference to the Tree.
    pub fn tree_mut(&self) -> cell::RefMut<'a, Tree> { self.tree.borrow_mut() }
}

#[cfg(test)]
pub mod tests {
    use std::{cell, rc};

    use basic::{com, mem};

    use model::hir::*;
    use model::hir::builder::{
        Factory, ItemFactory, PatternFactory, PrototypeFactory, StmtFactory,
        TypeFactory, TypeIdFactory, ValueFactory, RcTree,
    };
    use model::hir::interning::Resolver;

    use super::{Context, CoreUnifier};
    use super::super::Relation;

    #[derive(Default)]
    pub struct Env {
        source: RcTree,
        target: RcTree,
    }

    pub struct LocalEnv {
        registry: mocks::MockRegistry,
        context: Context,
        source: RcTree,
        target: RcTree,
        resolver: Resolver,
    }

    impl Env {
        pub fn local(&self, raw: &[u8]) -> LocalEnv {
            let interner = rc::Rc::new(mem::Interner::new());
            LocalEnv {
                registry: mocks::MockRegistry::new(),
                context: Context::default(),
                source: self.source.clone(),
                target: self.target.clone(),
                resolver: Resolver::new(raw, interner),
            }
        }

        pub fn source_factories(&self) -> (
            ItemFactory,
            PatternFactory,
            PrototypeFactory,
            StmtFactory,
            TypeFactory,
            TypeIdFactory,
            ValueFactory
        )
        {
            let f = Factory::new(self.source.clone());
            (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.type_id(), f.value())
        }

        pub fn target_factories(&self) -> (
            ItemFactory,
            PatternFactory,
            PrototypeFactory,
            StmtFactory,
            TypeFactory,
            TypeIdFactory,
            ValueFactory
        )
        {
            let f = Factory::new(self.target.clone());
            (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.type_id(), f.value())
        }
    }

    impl LocalEnv {
        pub fn core<'a>(&'a self) -> CoreUnifier<'a> {
            CoreUnifier::new(&self.context, &self.registry, &*self.source)
        }

        pub fn source(&self) -> &cell::RefCell<Tree> { &*self.source }

        pub fn target(&self) -> &cell::RefCell<Tree> { &*self.target }

        pub fn item_id(&self, pos: usize, len: usize) -> ItemIdentifier {
            let id = ItemIdentifier(Default::default(), com::Range::new(pos, len));
            self.resolver.resolve_item_id(id)
        }

        pub fn value_id(&self, pos: usize, len: usize) -> ValueIdentifier {
            let id = ValueIdentifier(Default::default(), com::Range::new(pos, len));
            self.resolver.resolve_value_id(id)
        }

        pub fn link_types(&self, ty: TypeId, rel: Relation<TypeId>) {
            self.context.link_types(ty, rel);
        }

        pub fn link_types_of(&self, gvn: Gvn, grel: Relation<Gvn>) {
            let ty = self.type_of(gvn);
            let rel = grel.map(|g| self.type_of(g));

            println!("link_types_of {:?} ({:?}) to {:?} ({:?})",
                gvn, ty, grel, rel);
            self.context.link_types(ty, rel);
        }

        fn type_of(&self, gvn: Gvn) -> TypeId {
            if let Some(e) = gvn.as_expression() {
                self.source.borrow().get_expression_type_id(e)
            } else if let Some(p) = gvn.as_pattern() {
                self.source.borrow().get_pattern_type_id(p)
            } else {
                unreachable!()
            }
        }
    }
}
