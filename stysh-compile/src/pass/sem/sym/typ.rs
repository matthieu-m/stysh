//! Semantic pass: Type Mapping.
//!
//! Mappings:
//! -   maps variable to binding,
//! -   maps function call to callable (binding or function),
//! -   maps type or module to definition.

use basic::mem;

use model::{ast, hir};

use super::scp::Scope;

/// The Type Mapper.
///
/// For each top-level reference to a symbol, resolves the symbol.
pub struct TypeMapper<'a> {
    scope: &'a Scope,
}

impl<'a> TypeMapper<'a> {
    /// Creates a new instance.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(scope: &'a Scope) -> Self {
        TypeMapper { scope }
    }

    /// Translates a type into... a type!
    pub fn type_of(&self, t: &ast::Type) -> hir::TypeDefinition {
        use model::ast::Type;

        match *t {
            Type::Missing(_) => unimplemented!(),
            Type::Nested(t, p) => self.type_of_nested(t, p),
            Type::Simple(t) => self.type_of_simple(t),
            Type::Tuple(ref t) => self.type_of_tuple(t),
        }
    }

    /// Translates a tuple of types into a type.
    pub fn tuple_of(&self, tup: &ast::Tuple<ast::Type>) -> hir::DynTuple<hir::TypeDefinition> {
        debug_assert!(
            tup.names.is_empty() || tup.names.len() == tup.fields.len()
        );

        let fields = self.array_of(tup.fields, |t| self.type_of(t));
        let names = self.array_of(tup.names, |&(i, r)| hir::ValueIdentifier(i, r));

        hir::DynTuple { fields, names }
    }
}

//
//  Implementation Details
//
impl<'a> TypeMapper<'a> {
    fn type_of_nested(&self, t: ast::TypeIdentifier, p: ast::Path)
        -> hir::TypeDefinition
    {
        let path = mem::DynArray::with_capacity(p.components.len());
        for &c in p.components {
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
    pub fn type_of_tuple(&self, t: &ast::Tuple<ast::Type>) -> hir::TypeDefinition {
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
