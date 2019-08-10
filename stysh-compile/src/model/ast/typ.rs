//! Types

use std::convert;

use crate::basic::com::{Range, Span, Store, MultiStore};
use crate::basic::mem::InternId;

use crate::model::ast::*;

/// A TypeId.
pub type TypeId = Id<Type>;

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A missing type.
    Missing(Range),
    /// A nested nominal type.
    Nested(TypeIdentifier, Path),
    /// A simple nominal type.
    Simple(TypeIdentifier),
    /// A tuple.
    Tuple(Tuple<Type>),
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

/// A Type Identifier.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeIdentifier(pub InternId, pub Range);

//
//  Implementations
//

impl Type {
    /// Returns the name of the type.
    pub fn name(&self) -> Option<TypeIdentifier> {
        use self::Type::*;

        match *self {
            Nested(t, _) | Simple(t) => Some(t),
            Missing(_) | Tuple(_) => None,
        }
    }
}

impl TypeIdentifier {
    /// Returns the InternId.
    pub fn id(&self) -> InternId { self.0 }

    /// Sets the InternId of the TypeIdentifier.
    pub fn with_id(self, id: InternId) -> Self {
        TypeIdentifier(id, self.1)
    }
}

/// Recursively copies the given TypeId from source to target.
pub fn replicate_type<Source, Target>(
    id: TypeId,
    source: &Source,
    target: &mut Target
)
    -> TypeId
    where
        Source: Store<Type> + MultiStore<TypeId> + MultiStore<Identifier> + MultiStore<u32>,
        Target: Store<Type> + MultiStore<TypeId> + MultiStore<Identifier> + MultiStore<u32>,
{
    use self::Type::*;

    let ty = source.get(id);
    let range = source.get_range(id);

    match ty {
        Missing(_) | Simple(_) => target.push(ty, range),
        Nested(name, path) => {
            let path = replicate_path(path, source, target);
            target.push(Nested(name, path), range)
        },
        Tuple(tuple) => {
            let tuple = replicate_tuple(tuple, source, target);
            target.push(Tuple(tuple), range)
        }
    }
}

//
//  Implementations of Span
//

impl Span for TypeIdentifier {
    /// Returns the range spanned by the type identifier.
    fn span(&self) -> Range { self.1 }
}

//
//  Implementations of From
//

impl convert::From<Tuple<Type>> for Type {
    fn from(t: Tuple<Type>) -> Type { Type::Tuple(t) }
}

//
//  Private Functions
//

fn replicate_path<Source, Target>(
    path: Path,
    source: &Source,
    target: &mut Target
)
    -> Path
    where
        Source: MultiStore<Identifier> + MultiStore<u32>,
        Target: MultiStore<Identifier> + MultiStore<u32>,
{
    let components = target.push_slice(source.get_slice(path.components));
    let colons = target.push_slice(source.get_slice(path.colons));
    Path { components, colons }
}

fn replicate_tuple<Source, Target>(
    tuple: Tuple<Type>,
    source: &Source,
    target: &mut Target
)
    -> Tuple<Type>
    where
        Source: Store<Type> + MultiStore<TypeId> + MultiStore<Identifier> + MultiStore<u32>,
        Target: Store<Type> + MultiStore<TypeId> + MultiStore<Identifier> + MultiStore<u32>,
{
    let fields: Vec<_> = source.get_slice(tuple.fields)
        .iter()
        .map(|&id| replicate_type(id, source, target))
        .collect();
    let fields = target.push_slice(&fields);

    let commas = target.push_slice(source.get_slice(tuple.commas));
    let names = target.push_slice(source.get_slice(tuple.names));
    let separators = target.push_slice(source.get_slice(tuple.separators));
    let (open, close) = (tuple.open, tuple.close);

    Tuple { fields, commas, names, separators, open, close, }
}
