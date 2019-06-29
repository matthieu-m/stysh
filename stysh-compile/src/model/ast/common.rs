//! Common types.

use basic::{com, mem};

use model::ast::*;

pub use basic::com::Id;

/// A Constructor.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<T> {
    /// Type of the constructor.
    pub type_: TypeId,
    /// Arguments of the constructor.
    pub arguments: Tuple<T>,
}

/// An Identifier.
///
/// Used either for field names or variables names.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Identifier(pub mem::InternId, pub com::Range);

/// A Tuple, either type or value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<T> {
    /// Fields of the tuple.
    pub fields: Id<[Id<T>]>,
    /// Offsets of the commas separating the fields, an absent comma is placed
    /// at the offset of the last character of the field it would have followed.
    pub commas: Id<[u32]>,
    /// Ranges of the names of the fields:
    /// -   if the tuple is unnanmed, the slice is empty,
    /// -   otherwise, the slice has the same size as the fields.
    /// An absent name is placed at the offset of the first character of the
    /// field it would have preceeded, with a length of 0.
    pub names: Id<[Identifier]>,
    /// Offsets of the separators between names and fields, an absent separator
    /// is placed at the offset of the first field it would have preceeded.
    /// Note: The separator is ':' for types and patterns and ':=' for values.
    pub separators: Id<[u32]>,
    /// Offset of the opening parenthesis.
    pub open: u32,
    /// Offset of the closing parenthesis, an absent parenthesis is placed at
    /// at the offset of the last character of the field it would have followed.
    pub close: u32,
}

//
//  Implementations
//

impl Identifier {
    /// Sets the InternId of the Identifier.
    pub fn with_id(self, id: mem::InternId) -> Self {
        Identifier(id, self.1)
    }

    /// Sets the range spanned by the Identifier.
    pub fn with_range(self, range: com::Range) -> Self {
        Identifier(self.0, range)
    }
}

impl<T> Tuple<T> {
    /// Returns a unit tuple.
    pub fn unit() -> Tuple<T> { Tuple::default() }

    /// Returns whether the tuple is empty.
    pub fn is_empty(&self) -> bool { self.fields.is_empty() }
}

//
//  Implementations of Span
//

impl com::Span for Identifier {
    /// Returns the range spanned by the identifier.
    fn span(&self) -> com::Range { self.1 }
}

impl<T> com::Span for Tuple<T> {
    /// Returns the range spanned by the tuple.
    fn span(&self) -> com::Range {
        com::Range::new(self.open as usize, (self.close + 1 - self.open) as usize)
    }
}

//
//  Implementations of Default
//

impl<T> Default for Tuple<T> {
    fn default() -> Tuple<T> {
        Tuple {
            fields: Id::empty(),
            commas: Id::empty(),
            names: Id::empty(),
            separators: Id::empty(),
            open: 0,
            close: 0
        }
    }
}

#[cfg(test)]
pub mod tests {
    use std::cell;

    use model::ast::{Module, Tree};
    use model::ast::builder::{Factory, RcModule, RcTree};
    use model::ast::interning::Resolver;

    pub struct Env {
        module: RcModule,
        tree: RcTree,
        resolver: Resolver,
    }

    impl Env {
        pub fn new(source: &[u8]) -> Self {
            Env {
                module: RcModule::default(),
                tree: RcTree::default(),
                resolver: Resolver::new(source, Default::default()),
            }
        }

        pub fn module(&self) -> &cell::RefCell<Module> { &self.module }

        pub fn tree(&self) -> &cell::RefCell<Tree> { &self.tree }

        pub fn factory(&self) -> Factory {
            Factory::new(self.module.clone(), self.tree.clone(), self.resolver.clone())
        }
    }
}
