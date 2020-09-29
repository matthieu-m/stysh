//! Items

use std::convert;

use crate::basic::com::{Range, Span};

use crate::model::ast::*;

/// An EnumId.
pub type EnumId = Id<Enum>;

/// An ExtensionId.
pub type ExtensionId = Id<Extension>;

/// A FunctionId.
pub type FunctionId = Id<Function>;

/// An ImplementationId.
pub type ImplementationId = Id<Implementation>;

/// An InterfaceId.
pub type InterfaceId = Id<Interface>;

/// A RecordId.
pub type RecordId = Id<Record>;

/// An Item.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item {
    /// An enum.
    Enum(EnumId),
    /// An extension.
    Ext(ExtensionId),
    /// A function.
    Fun(FunctionId),
    /// An implementation.
    Imp(ImplementationId),
    /// An interface.
    Int(InterfaceId),
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
///
/// ```text
/// :enum <name> <parameters>? { <variant>, ... }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum {
    /// Name of the enum.
    pub name: TypeIdentifier,
    /// Generic parameters, if any.
    pub parameters: Option<Id<GenericParameterPack>>,
    /// Variants of the enum.
    pub variants: Id<[RecordId]>,
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

/// An Extension.
///
/// ```text
/// :ext <parameters>? <extended-type> { <function> ... }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Extension {
    /// Extended type.
    pub extended: TypeId,
    /// Generic parameters, if any.
    pub parameters: Option<Id<GenericParameterPack>>,
    /// Functions.
    pub functions: Id<[FunctionId]>,
    /// Offset of the `:ext` keyword.
    pub keyword: u32,
    /// Offset of the opening brace.
    pub open: u32,
    /// Offset of the closing brace.
    pub close: u32,
}

/// A Function.
///
/// ```text
/// :fun <name> <parameters>? (<argument>, ...) ->? <result>? ;
/// ```
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function {
    /// Name of the function.
    pub name: VariableIdentifier,
    /// Generic parameters, if any.
    pub parameters: Option<Id<GenericParameterPack>>,
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
    /// Offset of the ";" token, if any.
    pub semi_colon: u32,
}

/// An Implementation.
///
/// ```text
/// :imp <parameters>? <implemented-interface> :for <extended-type> { <function> ... }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Implementation {
    /// Implemented interface.
    pub implemented: TypeId,
    /// Extended type.
    pub extended: TypeId,
    /// Generic parameters, if any.
    pub parameters: Option<Id<GenericParameterPack>>,
    /// Functions.
    pub functions: Id<[FunctionId]>,
    /// Offset of the `:imp` keyword.
    pub keyword: u32,
    /// Offset of the `:for` keyword.
    pub for_: u32,
    /// Offset of the opening brace.
    pub open: u32,
    /// Offset of the closing brace.
    pub close: u32,
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

/// An Interface.
///
/// ```text
/// :int <interface> <parameters>? { <function> ... }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Interface {
    /// Name of the enum.
    pub name: TypeIdentifier,
    /// Generic parameters, if any.
    pub parameters: Option<Id<GenericParameterPack>>,
    /// Functions.
    pub functions: Id<[FunctionId]>,
    /// Offset of the `:int` keyword.
    pub keyword: u32,
    /// Offset of the opening brace.
    pub open: u32,
    /// Offset of the closing brace.
    pub close: u32,
}

/// A Record.
///
/// ```text
/// :rec <inner.type-identifier> <parameters>? <inner.tuple>? ;
/// ```
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record {
    /// Inner representation of the record.
    pub inner: InnerRecord,
    /// Generic parameters, if any.
    pub parameters: Option<Id<GenericParameterPack>>,
    /// Offset of the `:rec` keyword.
    pub keyword: u32,
    /// Offset of the semi-colon, for unit records.
    pub semi_colon: u32,
}

impl Record {
    /// Returns the name of the Record.
    pub fn name(&self) -> TypeIdentifier { self.inner.name().expect("Name") }
}

/// A Scope, for associated items.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Scope {
    /// Module scope, the default.
    Module,
    /// Extension scope.
    Ext(ExtensionId),
    /// Implementation scope.
    Imp(ImplementationId),
    /// Interface scope.
    Int(InterfaceId),
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

impl Span for Extension {
    /// Returns the range spanned by the extension.
    fn span(&self) -> Range {
        Range::new(self.keyword as usize, (self.close + 1 - self.keyword) as usize)
    }
}

impl Span for Implementation {
    /// Returns the range spanned by the implementation.
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

impl Span for Interface {
    /// Returns the range spanned by the interface.
    fn span(&self) -> Range {
        Range::new(self.keyword as usize, (self.close + 1 - self.keyword) as usize)
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

impl convert::From<ExtensionId> for Item {
    fn from(e: ExtensionId) -> Item { Item::Ext(e) }
}

impl convert::From<FunctionId> for Item {
    fn from(f: FunctionId) -> Item { Item::Fun(f) }
}

impl convert::From<ImplementationId> for Item {
    fn from(i: ImplementationId) -> Item { Item::Imp(i) }
}

impl convert::From<InterfaceId> for Item {
    fn from(i: InterfaceId) -> Item { Item::Int(i) }
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
    fn range_extension_minimal() {
        let env = Env::new(b":ext Simple { }");
        let item = env.factory().item();

        let e = item.extension(5, 6).build();
        assert_eq!(env.module().borrow().get_extension_range(e), range(0, 15));
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
                    f.type_module().simple(16, 3),
                ).build();
        e.block(e.bin_op(e.int(1, 23), e.int(1, 27)).build()).build_body(item);

        assert_eq!(env.module().borrow().get_function_range(item), range(3, 27));
    }

    #[test]
    fn range_implementation_minimal() {
        let env = Env::new(b":imp Interface :for Simple { }");
        let item = env.factory().item();

        let i = item.implementation(5, 9, 6).build();
        assert_eq!(env.module().borrow().get_implementation_range(i), range(0, 30));
    }

    #[test]
    fn range_interface_minimal() {
        let env = Env::new(b":int Simple { }");
        let item = env.factory().item();

        let e = item.interface(5, 6).build();
        assert_eq!(env.module().borrow().get_interface_range(e), range(0, 15));
    }

    fn range(offset: usize, length: usize) -> Range {
        Range::new(offset, length)
    }
}
