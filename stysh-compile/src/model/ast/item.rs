//! Items

use std::convert;

use basic::com::{self, Span};

use model::ast::*;
use model::tt;

/// An Item.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item<'a> {
    /// An enum.
    Enum(Enum<'a>),
    /// A function.
    Fun(Function<'a>),
    /// A record.
    Rec(Record<'a>),
}

/// An Argument.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Argument<'a> {
    /// Name of the argument.
    pub name: VariableIdentifier,
    /// Type of the argument.
    pub type_: Type<'a>,
    /// Offset of the colon.
    pub colon: u32,
    /// Offset of the comma, if any.
    pub comma: u32,
}

/// An Enum.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum<'a> {
    /// Name of the enum.
    pub name: TypeIdentifier,
    /// Variants of the enum.
    pub variants: &'a [InnerRecord<'a>],
    /// Offset of the `:enum` keyword.
    pub keyword: u32,
    /// Offset of the opening brace.
    pub open: u32,
    /// Offset of the closing brace.
    pub close: u32,
    /// Offsets of the comma separating the variants, an absent comma is placed
    /// at the offset of the last character of the field it would have followed.
    pub commas: &'a [u32],
}

/// A Function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function<'a> {
    /// Name of the function.
    pub name: VariableIdentifier,
    /// List of arguments of the function.
    pub arguments: &'a [Argument<'a>],
    /// Return type of the function.
    pub result: Type<'a>,
    /// Body of the function.
    pub body: Block<'a>,
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
pub enum InnerRecord<'a> {
    /// A missing record, when a semi-colon immediately appears for example.
    Missing(com::Range),
    /// A tuple record, where fields are identified by index.
    Tuple(TypeIdentifier, Tuple<'a, Type<'a>>),
    /// An unexpected range of tokens.
    Unexpected(com::Range),
    /// A unit record, with no argument.
    Unit(TypeIdentifier),
}

/// A Record.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record<'a> {
    /// Inner representation of the record.
    pub inner: InnerRecord<'a>,
    /// Offset of the `:rec` keyword.
    pub keyword: u32,
    /// Offset of the semi-colon, for unit records.
    pub semi_colon: u32,
}

//
//  Implementations
//
impl<'a> Enum<'a> {
    /// Returns the `:enum` token.
    pub fn keyword(&self) -> tt::Token {
        tt::Token::new(tt::Kind::KeywordEnum, self.keyword as usize, 5)
    }

    /// Returns the `{` token.
    pub fn brace_open(&self) -> tt::Token {
        if self.open == 0 {
            let start = self.name.span().end_offset();
            tt::Token::new(tt::Kind::BraceOpen, start, 0)
        } else {
            tt::Token::new(tt::Kind::BraceOpen, self.open as usize, 1)
        }
    }

    /// Returns the `}` token.
    pub fn brace_close(&self) -> tt::Token {
        if self.close == 0 {
            let last_comma = self.comma(self.commas.len().wrapping_sub(1));
            let last = last_comma.unwrap_or_else(|| self.brace_open());
            tt::Token::new(tt::Kind::BraceClose, last.span().end_offset(), 0)
        } else {
            tt::Token::new(tt::Kind::BraceClose, self.close as usize, 1)
        }
    }

    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        let make_semi = |&o| if o == 0 {
            let position = self.variants[i].span().end_offset();
            tt::Token::new(tt::Kind::SignComma, position, 0)
        } else {
            tt::Token::new(tt::Kind::SignComma, o as usize, 1)
        };
        self.commas.get(i).map(make_semi)
    }
}

impl<'a> InnerRecord<'a> {
    /// Returns the name of the inner record.
    pub fn name(&self) -> TypeIdentifier {
        use self::InnerRecord::*;

        match *self {
            Missing(r) | Unexpected(r)
                => TypeIdentifier(Default::default(), r),
            Tuple(t, _) | Unit(t) => t,
        }
    }
}

impl<'a> Record<'a> {
    /// Returns the `:rec` token.
    pub fn keyword(&self) -> tt::Token {
        tt::Token::new(tt::Kind::KeywordRec, self.keyword as usize, 4)
    }

    /// Returns the name of the record.
    pub fn name(&self) -> TypeIdentifier { self.inner.name() }

    /// Returns the token of the semi-colon following the record declaration,
    /// if there is no such semi-colon, the position it would have been at is
    /// faked.
    pub fn semi_colon(&self) -> tt::Token {
        let (offset, range) = if self.semi_colon == 0 {
            (self.inner.span().end_offset(), 0)
        } else {
            (self.semi_colon as usize, 1)
        };
        tt::Token::new(tt::Kind::SignSemiColon, offset, range)
    }
}

//
//  Implementations of Span
//
impl<'a> Span for Item<'a> {
    /// Returns the range spanned by the item.
    fn span(&self) -> com::Range {
        use self::Item::*;

        match *self {
            Enum(e) => e.span(),
            Fun(fun) => fun.span(),
            Rec(r) => r.span(),
        }
    }
}

impl<'a> Span for Argument<'a> {
    /// Returns the range spanned by the argument.
    fn span(&self) -> com::Range {
        let offset = self.name.span().offset();
        let end_offset = self.type_.span().end_offset();
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a> Span for Enum<'a> {
    /// Returns the range spanned by the enum.
    fn span(&self) -> com::Range {
        self.keyword().span().extend(self.brace_close().span())
    }
}

impl<'a> Span for InnerRecord<'a> {
    /// Returns the range spanned by the inner record.
    fn span(&self) -> com::Range {
        use self::InnerRecord::*;

        match *self {
            Missing(r) | Unexpected(r) => r,
            Tuple(t, tup) => t.span().extend(tup.span()),
            Unit(t) => t.span(),
        }
    }
}

impl<'a> Span for Function<'a> {
    /// Returns the range spanned by the function.
    fn span(&self) -> com::Range {
        com::Range::new(self.keyword as usize, 4).extend(self.body.span())
    }
}

impl<'a> Span for Record<'a> {
    /// Returns the range spanned by the record.
    fn span(&self) -> com::Range {
        self.keyword().span().extend(self.semi_colon().span())
    }
}

//
//  Implementations of From
//
impl<'a> convert::From<Enum<'a>> for Item<'a> {
    fn from(e: Enum<'a>) -> Item<'a> { Item::Enum(e) }
}

impl<'a> convert::From<Function<'a>> for Item<'a> {
    fn from(f: Function<'a>) -> Item<'a> { Item::Fun(f) }
}

impl<'a> convert::From<Record<'a>> for Item<'a> {
    fn from(r: Record<'a>) -> Item<'a> { Item::Rec(r) }
}
//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use super::*;
    use model::ast::builder::Factory;

    #[test]
    fn range_enum_empty() {
        let global_arena = mem::Arena::new();
        let item = Factory::new(&global_arena).item();

        //  " :enum Empty { }"
        let e: Enum = item.enum_(7, 5).build();
        assert_eq!(e.span(), range(1, 15));
    }

    #[test]
    fn range_enum_minimal() {
        let global_arena = mem::Arena::new();
        let item = Factory::new(&global_arena).item();

        //  ":enum Minimal"
        let e: Enum = item.enum_(6, 7).braces(12, 12).build();
        assert_eq!(e.span(), range(0, 13));
    }

    #[test]
    fn range_enum_simple() {
        let global_arena = mem::Arena::new();
        let item = Factory::new(&global_arena).item();

        //  ":enum Simple { One, Two }"
        let e: Enum =
            item.enum_(6, 6)
                .push_unit(15, 3)
                .push_unit(20, 3)
                .build();
        assert_eq!(e.span(), range(0, 25));
    }

    #[test]
    fn range_fun() {
        let global_arena = mem::Arena::new();
        let f = Factory::new(&global_arena);
        let e = f.expr();

        //  "   :fun add() -> Int { 1 + 1 }"
        let item: Item =
            f.item()
                .function(
                    8,
                    3,
                    f.type_().simple(16, 3),
                    e.block(e.bin_op(e.int(1, 23), e.int(1, 27)).build())
                        .build(),
                ).build();
        assert_eq!(item.span(), range(3, 27), "{:?}", item);
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
