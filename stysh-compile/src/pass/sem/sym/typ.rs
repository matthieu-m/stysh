//! Semantic pass: Type Mapping.
//!
//! Mappings:
//! -   maps variable to binding,
//! -   maps function call to callable (binding or function),
//! -   maps type or module to definition.

use basic::mem;
use basic::com::{Store, MultiStore};

use model::{ast, hir};

use super::scp::Scope;

/// The Type Mapper.
///
/// For each top-level reference to a symbol, resolves the symbol.
pub struct TypeMapper<'a, S> {
    scope: &'a Scope,
    store: &'a S,
}

impl<'a, S> TypeMapper<'a, S> {
    /// Creates a new instance.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(scope: &'a Scope, store: &'a S) -> Self {
        TypeMapper { scope, store, }
    }
}

impl<'a, S> TypeMapper<'a, S>
    where
        S: Store<ast::Type> + MultiStore<ast::TypeId> + MultiStore<ast::Identifier>,
{
    /// Translates a type into... a type!
    pub fn type_of(&self, t: ast::TypeId) -> hir::TypeDefinition {
        use model::ast::Type;

        match self.store.get(t) {
            Type::Missing(_) => unimplemented!(),
            Type::Nested(t, p) => self.type_of_nested(t, p),
            Type::Simple(t) => self.type_of_simple(t),
            Type::Tuple(t) => self.type_of_tuple(t),
        }
    }

    /// Translates a tuple of types into a type.
    pub fn tuple_of(&self, tup: ast::Tuple<ast::Type>) -> hir::DynTuple<hir::TypeDefinition> {
        let fields = self.store.get_slice(tup.fields);
        let names = self.store.get_slice(tup.names);

        debug_assert!(names.is_empty() || names.len() == fields.len());

        let fields = self.array_of(fields, |&t| self.type_of(t));
        let names = self.array_of(names, |&id| id.into());

        hir::DynTuple { fields, names }
    }
}

//
//  Implementation Details
//
impl<'a, S> TypeMapper<'a, S>
    where
        S: Store<ast::Type> + MultiStore<ast::TypeId> + MultiStore<ast::Identifier>,
{
    fn type_of_nested(&self, t: ast::TypeIdentifier, p: ast::Path)
        -> hir::TypeDefinition
    {
        let components = self.store.get_slice(p.components);
        let path = mem::DynArray::with_capacity(components.len());
        for &c in components {
            path.push(hir::TypeDefinition::Unresolved(
                c.into(),
                Default::default(),
            ));
        }

        hir::TypeDefinition::Unresolved(
            t.into(),
            hir::Path { components: path },
        )
    }

    fn type_of_simple(&self, t: ast::TypeIdentifier) -> hir::TypeDefinition {
        self.scope.lookup_type(t.into())
    }

    /// Translates a tuple of types into a type.
    pub fn type_of_tuple(&self, t: ast::Tuple<ast::Type>) -> hir::TypeDefinition {
        hir::TypeDefinition::Tuple(self.tuple_of(t))
    }

    fn array_of<'b, T: 'b, U, F: FnMut(&'b T) -> U>(
        &self,
        input: &'b [T],
        mut transformer: F,
    )
        -> mem::DynArray<U>
    {
        let result = mem::DynArray::with_capacity(input.len());

        for i in input {
            result.push(transformer(i));
        }

        result
    }
}
