//! Semantic pass: Type Mapping.
//!
//! Mappings:
//! -   maps variable to binding,
//! -   maps function call to callable (binding or function),
//! -   maps type or module to definition.

use std::cell;

use crate::basic::com::{Id, Span, Store, MultiStore};

use crate::model::{ast, hir};

use super::scp::Scope;

/// The Type Mapper.
///
/// For each top-level reference to a symbol, resolves the symbol.
pub struct TypeMapper<'a, A, H> {
    scope: &'a dyn Scope,
    ast_store: &'a A,
    hir_store: &'a cell::RefCell<H>,
}

impl<'a, A, H> TypeMapper<'a, A, H> {
    /// Creates a new instance.
    pub fn new(scope: &'a dyn Scope, ast_store: &'a A, hir_store: &'a cell::RefCell<H>) -> Self {
        TypeMapper { scope, ast_store, hir_store, }
    }
}

impl<'a, A, H> TypeMapper<'a, A, H>
    where
        A: Store<ast::Type> + MultiStore<ast::TypeId> + MultiStore<ast::Identifier>,
        H: Store<hir::Type, hir::TypeId> + MultiStore<hir::ValueIdentifier>
            + MultiStore<hir::PathComponent> + MultiStore<hir::TypeId>,
{
    /// Translates a type into... a type!
    pub fn type_of(&self, t: ast::TypeId) -> hir::TypeId {
        use self::ast::Type;

        match self.ast_store.get(t) {
            Type::Missing(_) => unimplemented!(),
            Type::Nested(t, p) => self.type_of_nested(t, p),
            Type::Simple(t) => self.type_of_simple(t),
            Type::Tuple(t) => self.type_of_tuple(t),
        }
    }

    /// Translates a tuple of types into a type.
    pub fn tuple_of(&self, tup: ast::Tuple<ast::Type>) -> hir::Tuple<hir::TypeId> {
        let fields = self.ast_store.get_slice(tup.fields);
        let names = self.ast_store.get_slice(tup.names);

        debug_assert!(names.is_empty() || names.len() == fields.len());

        let fields = self.array_of(fields, |&t| self.type_of(t));
        let names = self.array_of(names, |&id| id.into());

        hir::Tuple { fields, names }
    }
}

//
//  Implementation Details
//
impl<'a, A, H> TypeMapper<'a, A, H>
    where
        A: Store<ast::Type> + MultiStore<ast::TypeId> + MultiStore<ast::Identifier>,
        H: Store<hir::Type, hir::TypeId> + MultiStore<hir::ValueIdentifier>
            + MultiStore<hir::PathComponent> + MultiStore<hir::TypeId>,
{
    fn type_of_nested(&self, t: ast::TypeIdentifier, p: ast::Path)
        -> hir::TypeId
    {
        let components = self.ast_store.get_slice(p.components);
        let range = if let Some(first) = components.first() {
            first.span().extend(t.span())
        } else {
            t.span()
        };

        let mut path = Vec::with_capacity(components.len());
        for &c in components {
            path.push(hir::PathComponent::Unresolved(c.into()));
        }

        let path = self.hir_store.borrow_mut().push_slice(&path);

        self.hir_store.borrow_mut().push(hir::Type::Unresolved(t.into(), path), range)
    }

    fn type_of_simple(&self, t: ast::TypeIdentifier) -> hir::TypeId {
        let type_ = self.scope.lookup_type(t.into());
        let range = t.span();

        self.hir_store.borrow_mut().push(type_, range)
    }

    /// Translates a tuple of types into a type.
    pub fn type_of_tuple(&self, t: ast::Tuple<ast::Type>) -> hir::TypeId {
        let type_ = hir::Type::Tuple(self.tuple_of(t));
        let range = t.span();
        self.hir_store.borrow_mut().push(type_, range)
    }

    fn array_of<'b, T: 'b, U, F: FnMut(&'b T) -> U>(
        &self,
        input: &'b [T],
        mut transformer: F,
    )
        -> Id<[U]>
        where
            H: MultiStore<U>,
    {
        let mut result = Vec::with_capacity(input.len());

        for i in input {
            result.push(transformer(i));
        }

        self.hir_store.borrow_mut().push_slice(&result)
    }
}
