//! Common types.

use crate::basic::com::{Range, Span, MultiStore};
use crate::basic::mem::InternId;

use crate::model::ast::*;

pub use crate::basic::com::Id;

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
pub struct Identifier(pub InternId, pub Range);

impl Identifier {
    /// Sets the InternId of the Identifier.
    pub fn with_id(self, id: InternId) -> Self {
        Identifier(id, self.1)
    }

    /// Sets the range spanned by the Identifier.
    pub fn with_range(self, range: Range) -> Self {
        Identifier(self.0, range)
    }
}

/// A Path.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Path {
    /// Components of the path.
    pub components: Id<[Identifier]>,
    /// Offsets of the double colons separating the arguments, an absent double
    /// colon is placed at the offset of the last character of the field it
    /// would have followed.
    pub colons: Id<[u32]>,
}

impl Path {
    /// Creates an empty path.
    pub fn empty() -> Path {
        Path { components: Id::empty(), colons: Id::empty(), }
    }

    /// Returns whether the path is empty.
    pub fn is_empty(&self) -> bool { self.components == Id::empty() }

    /// Returns the range of the path.
    pub fn range<S>(&self, store: &S) -> Option<Range>
        where
            S: MultiStore<Identifier> + MultiStore<u32>,
    {
        if self.is_empty() {
            return None;
        }

        let components = store.get_slice(self.components);
        let colons = store.get_slice(self.colons);

        if let (Some(&i), Some(&c)) = (components.first(), colons.last()) {
            Some(i.span().extend(Range::new(c as usize, 2)))
        } else {
            unreachable!("A non-empty path should have both a component and a colon");
        }
    }

    /// Replicates from the Source to the Target store.
    pub fn replicate<Source, Target>(
        &self,
        source: &Source,
        target: &mut Target
    )
        -> Path
        where
            Source: MultiStore<Identifier> + MultiStore<u32>,
            Target: MultiStore<Identifier> + MultiStore<u32>,
    {
        let components = target.push_slice(source.get_slice(self.components));
        let colons = target.push_slice(source.get_slice(self.colons));
        Path { components, colons }
    }
}

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
    /// the offset of the last character of the field it would have followed.
    pub close: u32,
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

impl Span for Identifier {
    /// Returns the range spanned by the identifier.
    fn span(&self) -> Range { self.1 }
}

impl<T> Span for Tuple<T> {
    /// Returns the range spanned by the tuple.
    fn span(&self) -> Range {
        Range::new(self.open as usize, (self.close + 1 - self.open) as usize)
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

    use crate::model::ast::{Module, Tree};
    use crate::model::ast::builder::{Factory, RcModule, RcTree};
    use crate::model::ast::interning::Resolver;

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
