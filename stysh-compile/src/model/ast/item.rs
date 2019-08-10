//! Items

use std::convert;

use crate::basic::com::{Range, Span};

use crate::model::ast::*;

/// An EnumId.
pub type EnumId = Id<Enum>;

/// A FunctionId.
pub type FunctionId = Id<Function>;

/// A RecordId.
pub type RecordId = Id<Record>;

/// An Item.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item {
    /// An enum.
    Enum(EnumId),
    /// A function.
    Fun(FunctionId),
    /// A record.
    Rec(RecordId),
}

/// An Argument.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Argument {
    /// Name of the argument.
    pub name: VariableIdentifier,
    /// Type of the argument.
    pub type_: TypeId,
    /// Offset of the colon.
    pub colon: u32,
    /// Offset of the comma, if any.
    pub comma: u32,
}

/// An Enum.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum {
    /// Name of the enum.
    pub name: TypeIdentifier,
    /// Variants of the enum.
    pub variants: Id<[InnerRecord]>,
    /// Offset of the `:enum` keyword.
    pub keyword: u32,
    /// Offset of the opening brace.
    pub open: u32,
    /// Offset of the closing brace.
    pub close: u32,
    /// Offsets of the comma separating the variants, an absent comma is placed
    /// at the offset of the last character of the field it would have followed.
    pub commas: Id<[u32]>,
}

/// A Function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function {
    /// Name of the function.
    pub name: VariableIdentifier,
    /// List of arguments of the function.
    pub arguments: Id<[Argument]>,
    /// Return type of the function.
    pub result: TypeId,
    /// Offset of the ":fun" keyword.
    pub keyword: u32,
    /// Offset of the "(" token.
    pub open: u32,
    /// Offset of the ")" token.
    pub close: u32,
    /// Offset of the "->" token, if any.
    pub arrow: u32,
}

/// An InnerRecord.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum InnerRecord {
    /// A missing record, when a semi-colon immediately appears for example.
    Missing(Range),
    /// A tuple record, where fields are identified by index.
    Tuple(TypeIdentifier, Tuple<Type>),
    /// An unexpected range of tokens.
    Unexpected(Range),
    /// A unit record, with no argument.
    Unit(TypeIdentifier),
}

/// A Record.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record {
    /// Inner representation of the record.
    pub inner: InnerRecord,
    /// Offset of the `:rec` keyword.
    pub keyword: u32,
    /// Offset of the semi-colon, for unit records.
    pub semi_colon: u32,
}

//
//  Implementations
//

impl InnerRecord {
    /// Returns the name of the InnerRecord, if any.
    pub fn name(&self) -> Option<TypeIdentifier> {
        use self::InnerRecord::*;

        match *self {
            Tuple(name, _) | Unit(name) => Some(name),
            _ => None,
        }
    }
}

impl Record {
    /// Returns the name of the Record.
    pub fn name(&self) -> TypeIdentifier { self.inner.name().expect("Name") }
}

//
//  Implementations of Span
//

impl Span for Argument {
    /// Returns the range spanned by the argument.
    fn span(&self) -> Range {
        let offset = self.name.span().offset();
        let end_offset = self.comma as usize + 1;
        Range::new(offset, end_offset - offset)
    }
}

impl Span for Enum {
    /// Returns the range spanned by the enum.
    fn span(&self) -> Range {
        Range::new(self.keyword as usize, (self.close + 1 - self.keyword) as usize)
    }
}

impl Span for InnerRecord {
    /// Returns the range spanned by the inner record.
    fn span(&self) -> Range {
        use self::InnerRecord::*;

        match *self {
            Missing(r) | Unexpected(r) => r,
            Tuple(name, tuple) => name.1.extend(tuple.span()),
            Unit(name) => name.1,
        }
    }
}

impl Span for Record {
    /// Returns the range spanned by the record.
    fn span(&self) -> Range {
        Range::new(self.keyword as usize, (self.semi_colon + 1 - self.keyword) as usize)
    }
}

//
//  Implementations of From
//

impl convert::From<EnumId> for Item {
    fn from(e: EnumId) -> Item { Item::Enum(e) }
}

impl convert::From<FunctionId> for Item {
    fn from(f: FunctionId) -> Item { Item::Fun(f) }
}

impl convert::From<RecordId> for Item {
    fn from(r: RecordId) -> Item { Item::Rec(r) }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use crate::basic::com::Range;
    use super::super::common::tests::Env;

    #[test]
    fn range_enum_empty() {
        let env = Env::new(b" :enum Empty { }");
        let item = env.factory().item();

        let e = item.enum_(7, 5).build();
        assert_eq!(env.module().borrow().get_enum_range(e), range(1, 15));
    }

    #[test]
    fn range_enum_minimal() {
        let env = Env::new(b":enum Minimal");
        let item = env.factory().item();

        let e = item.enum_(6, 7).braces(12, 12).build();
        assert_eq!(env.module().borrow().get_enum_range(e), range(0, 13));
    }

    #[test]
    fn range_enum_simple() {
        let env = Env::new(b":enum Simple { One, Two }");
        let item = env.factory().item();

        let e =
            item.enum_(6, 6)
                .push_unit(15, 3)
                .push_unit(20, 3)
                .build();
        assert_eq!(env.module().borrow().get_enum_range(e), range(0, 25));
    }

    #[test]
    fn range_fun() {
        let env = Env::new(b"   :fun add() -> Int { 1 + 1 }");
        let f = env.factory();
        let e = f.expr();

        let item =
            f.item()
                .function(
                    8,
                    3,
                    f.type_().simple(16, 3),
                    e.block(e.bin_op(e.int(1, 23), e.int(1, 27)).build())
                        .build(),
                ).build();
        assert_eq!(env.module().borrow().get_function_range(item), range(3, 27));
    }

    fn range(offset: usize, length: usize) -> Range {
        Range::new(offset, length)
    }
}
