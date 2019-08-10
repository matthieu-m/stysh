//! Common Type Unifier.

use std::cell;

use crate::model::hir::*;

use super::{Context, RegRef};

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

    /// Returns a reference to the unified Registry.
    pub fn registry(&self) -> RegRef<'a> {
        RegRef::new(self.registry, self.tree())
    }

    /// Returns a reference to the Tree.
    pub fn tree(&self) -> cell::Ref<'a, Tree> { self.tree.borrow() }

    /// Returns a mutable reference to the Tree.
    pub fn tree_mut(&self) -> cell::RefMut<'a, Tree> { self.tree.borrow_mut() }
}

#[cfg(test)]
pub mod tests {
    use std::{cell, rc};

    use crate::basic::{com, mem};

    use crate::model::hir::*;
    use crate::model::hir::builder::{
        Factory, ItemFactory, PatternFactory, StatementFactory,
        TypeFactory, TypeIdFactory, ExpressionFactory, RcModule, RcTree,
    };
    use crate::model::hir::interning::Resolver;

    use super::Context;
    use super::super::Relation;

    #[derive(Default)]
    pub struct Env {
        module: RcModule,
        source_tree: RcTree,
        target_tree: RcTree,
    }

    pub struct LocalEnv {
        context: Context,
        module: RcModule,
        source_tree: RcTree,
        target_tree: RcTree,
        resolver: Resolver,
    }

    impl Env {
        pub fn local(&self, raw: &[u8]) -> LocalEnv {
            let interner = rc::Rc::new(mem::Interner::new());
            LocalEnv {
                context: Context::default(),
                module: self.module.clone(),
                source_tree: self.source_tree.clone(),
                target_tree: self.target_tree.clone(),
                resolver: Resolver::new(raw, interner),
            }
        }

        pub fn source_factory(&self) -> Factory {
            Factory::new(self.module.clone(), self.source_tree.clone())
        }

        pub fn source_factories(&self) -> (
            ItemFactory,
            PatternFactory,
            StatementFactory,
            TypeFactory<Tree>,
            TypeIdFactory<Tree>,
            ExpressionFactory,
        )
        {
            let f = self.source_factory();
            (f.item(), f.pat(), f.stmt(), f.type_(), f.type_id(), f.value())
        }

        pub fn target_factories(&self) -> (
            ItemFactory,
            PatternFactory,
            StatementFactory,
            TypeFactory<Tree>,
            TypeIdFactory<Tree>,
            ExpressionFactory,
        )
        {
            let f = Factory::new(self.module.clone(), self.target_tree.clone());
            (f.item(), f.pat(), f.stmt(), f.type_(), f.type_id(), f.value())
        }
    }

    impl LocalEnv {
        pub fn context(&self) -> &Context { &self.context }

        pub fn module(&self) -> &cell::RefCell<Module> { &*self.module }

        pub fn source(&self) -> &cell::RefCell<Tree> { &*self.source_tree }

        pub fn target(&self) -> &cell::RefCell<Tree> { &*self.target_tree }

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
                self.source_tree.borrow().get_expression_type_id(e)
            } else if let Some(p) = gvn.as_pattern() {
                self.source_tree.borrow().get_pattern_type_id(p)
            } else {
                unreachable!()
            }
        }
    }
}
