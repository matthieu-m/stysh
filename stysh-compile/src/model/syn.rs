//! Syntactic model, aka AST.
//!
//! This is the model describing the syntax of the language, with a 1-to-1
//! mapping to the actual textual representation (modulo whitespace).
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

use std::convert;

use basic::com;

use model::tt;
pub use model::tt::StringFragment;

/// A Range trait.
pub trait Range {
    /// Returns the range spanned by self.
    fn range(&self) -> com::Range;
}

/// A List of AST nodes.
pub type List<'a> = &'a [Node<'a>];

/// An AST node, the building piece of the graph.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Node<'a> {
    /// An expression.
    Expr(Expression<'a>),
    /// An item.
    Item(Item<'a>),
    /// A statement.
    Stmt(Statement<'a>),
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expression<'a> {
    /// A binary operation.
    BinOp(BinaryOperator, u32, &'a Expression<'a>, &'a Expression<'a>),
    /// A block expression.
    Block(&'a [Statement<'a>], &'a Expression<'a>, com::Range),
    /// A constructor expression.
    Constructor(Constructor<'a, Expression<'a>>),
    /// A field access expression.
    FieldAccess(FieldAccess<'a>),
    /// A function call expression.
    FunctionCall(FunctionCall<'a>),
    /// A if expression.
    If(IfElse<'a>),
    /// A literal.
    Lit(Literal<'a>),
    /// A prefix unary operation.
    PreOp(PrefixOperator, u32, &'a Expression<'a>),
    /// A tuple.
    Tuple(Tuple<'a, Expression<'a>>),
    /// A variable identifier.
    Var(VariableIdentifier),
}

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

/// A Pattern.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern<'a> {
    /// An ignored binding, always '_'.
    Ignored(VariableIdentifier),
    /// A tuple.
    Tuple(Tuple<'a, Pattern<'a>>),
    /// A variable identifier.
    Var(VariableIdentifier),
}

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Statement<'a> {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A variable re-binding.
    Set(VariableReBinding<'a>),
    /// A variable definition.
    Var(VariableBinding<'a>),
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
    pub body: Expression<'a>,
    /// Offset of the ":fun" keyword.
    pub keyword: u32,
    /// Offset of the "(" token.
    pub open: u32,
    /// Offset of the ")" token.
    pub close: u32,
    /// Offset of the "->" token, if any.
    pub arrow: u32,
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

/// A Binary Operator such as `+` or `*`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BinaryOperator {
    /// The `:and` operator.
    And,
    /// The `!=` operator.
    Different,
    /// The `==` operator.
    Equal,
    /// The `//` operator.
    FloorBy,
    /// The `>` operator.
    GreaterThan,
    /// The `>=` operator.
    GreaterThanOrEqual,
    /// The `<` operator.
    LessThan,
    /// The `<=` operator.
    LessThanOrEqual,
    /// The `-` operator.
    Minus,
    /// The `:or` operator.
    Or,
    /// The `+` operator.
    Plus,
    /// The `*` operator.
    Times,
    /// The `:xor` operator.
    Xor,
}

/// A Prefix Unary Operator such as `:not`.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum PrefixOperator {
    /// The `:not` operator.
    Not,
}

/// A Constructor.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<'a, T: 'a> {
    /// Type of the constructor.
    pub type_: Type<'a>,
    /// Arguments of the constructor.
    pub arguments: Tuple<'a, T>,
}

/// A Field Access such .0 or .name.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FieldAccess<'a> {
    /// Record or Tuple accessed.
    pub accessed: &'a Expression<'a>,
    /// Field identifier.
    pub field: FieldIdentifier,
}

/// A function call expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionCall<'a> {
    /// Function called.
    pub function: &'a Expression<'a>,
    /// Arguments.
    pub arguments: Tuple<'a, Expression<'a>>,
}

/// A if-else expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct IfElse<'a> {
    /// Condition.
    pub condition: &'a Expression<'a>,
    /// Expression evaluated if condition evaluates to true.
    pub true_expr: &'a Expression<'a>,
    /// Expression evaluated if condition evaluates to false.
    pub false_expr: &'a Expression<'a>,
    /// Offset of :if.
    pub if_: u32,
    /// Offset of :else.
    pub else_: u32,
}

/// A Literal value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Literal<'a> {
    /// A boolean value.
    Bool(bool, com::Range),
    /// A bytes value.
    Bytes(&'a [StringFragment], com::Range),
    /// An integral value.
    Integral(com::Range),
    /// A string value.
    String(&'a [StringFragment], com::Range),
}

/// A variable binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableBinding<'a> {
    /// Name of the binding.
    pub pattern: Pattern<'a>,
    /// Type of the binding, if specified.
    pub type_: Option<Type<'a>>,
    /// Expression being bound.
    pub expr: Expression<'a>,
    /// Offset of the :var keyword.
    pub var: u32,
    /// Offset of the : sign, or 0 if none.
    pub colon: u32,
    /// Offset of the := sign.
    pub bind: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

/// A variable re-binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableReBinding<'a> {
    /// Left-hand side identifying a binding.
    pub left: Expression<'a>,
    /// Expression being bound.
    pub expr: Expression<'a>,
    /// Offset of the :set keyword.
    pub set: u32,
    /// Offset of the := sign.
    pub bind: u32,
    /// Offset of the ; sign.
    pub semi: u32,
}

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type<'a> {
    /// A missing type.
    Missing(com::Range),
    /// A nested nominal type.
    Nested(TypeIdentifier, Path<'a>),
    /// A simple nominal type.
    Simple(TypeIdentifier),
    /// A tuple.
    Tuple(Tuple<'a, Type<'a>>),
}

/// A Path.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Path<'a> {
    /// Components of the path.
    pub components: &'a [TypeIdentifier],
    /// Offsets of the double colons separating the arguments, an absent double
    /// colon is placed at the offset of the last character of the field it
    /// would have followed.
    pub colons: &'a [u32],
}

/// A Type Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeIdentifier(pub com::Range);

/// A Tuple, either type or value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<'a, T: 'a> {
    /// Fields of the tuple.
    pub fields: &'a [T],
    /// Offsets of the commas separating the fields, an absent comma is placed
    /// at the offset of the last character of the field it would have followed.
    pub commas: &'a [u32],
    /// Offset of the opening parenthesis.
    pub open: u32,
    /// Offset of the closing parenthesis, an absent parenthesis is placed at
    /// at the offset of the last character of the field it would have followed.
    pub close: u32,
}

/// A Field Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FieldIdentifier(pub com::Range);

/// A Value Identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct VariableIdentifier(pub com::Range);

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
            tt::Token::new(tt::Kind::BraceOpen, self.name.0.end_offset(), 0)
        } else {
            tt::Token::new(tt::Kind::BraceOpen, self.open as usize, 1)
        }
    }

    /// Returns the `}` token.
    pub fn brace_close(&self) -> tt::Token {
        if self.close == 0 {
            let last_comma = self.comma(self.commas.len().wrapping_sub(1));
            let last = last_comma.unwrap_or_else(|| self.brace_open());
            tt::Token::new(tt::Kind::BraceClose, last.range().end_offset(), 0)
        } else {
            tt::Token::new(tt::Kind::BraceClose, self.close as usize, 1)
        }
    }

    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        let make_semi = |&o| if o == 0 {
            let position = self.variants[i].range().end_offset();
            tt::Token::new(tt::Kind::SignComma, position, 0)
        } else {
            tt::Token::new(tt::Kind::SignComma, o as usize, 1)
        };
        self.commas.get(i).map(make_semi)
    }
}

impl<'a> Record<'a> {
    /// Returns the `:rec` token.
    pub fn keyword(&self) -> tt::Token {
        tt::Token::new(tt::Kind::KeywordRec, self.keyword as usize, 4)
    }

    /// Returns the name of the record.
    pub fn name(&self) -> TypeIdentifier {
        self.inner.name()
    }

    /// Returns the token of the semi-colon following the record declaration,
    /// if there is no such semi-colon, the position it would have been at is
    /// faked.
    pub fn semi_colon(&self) -> tt::Token {
        let (offset, range) = if self.semi_colon == 0 {
            (self.inner.range().end_offset(), 0)
        } else {
            (self.semi_colon as usize, 1)
        };
        tt::Token::new(tt::Kind::SignSemiColon, offset, range)
    }
}

impl<'a> InnerRecord<'a> {
    /// Returns the name of the inner record.
    pub fn name(&self) -> TypeIdentifier {
        use self::InnerRecord::*;

        match *self {
            Missing(r) | Unexpected(r) => TypeIdentifier(r),
            Tuple(t, _) | Unit(t) => t,
        }
    }
}

impl<'a> FunctionCall<'a> {
    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        self.arguments.comma(i)
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        self.arguments.parenthesis_open()
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        self.arguments.parenthesis_close()
    }
}

impl<'a> Type<'a> {
    /// Returns the name of the type.
    pub fn name(&self) -> Option<TypeIdentifier> {
        use self::Type::*;

        match *self {
            Nested(t, _) | Simple(t) => Some(t),
            Missing(_) | Tuple(_) => None,
        }
    }
}

impl<'a> Path<'a> {
    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn double_colon(&self, i: usize) -> Option<tt::Token> {
        self.colons
            .get(i)
            .map(|&o| tt::Token::new(tt::Kind::SignDoubleColon, o as usize, 2))
    }
}

impl<'a, T: 'a> Tuple<'a, T> {
    /// Returns whether the tuple is empty.
    pub fn is_empty(&self) -> bool { self.fields.is_empty() }

    /// Returns the number of fields of the tuple.
    pub fn len(&self) -> usize { self.fields.len() }

    /// Returns the token of the comma following the i-th field, if there is no
    /// such comma the position it would have been at is faked.
    pub fn comma(&self, i: usize) -> Option<tt::Token> {
        self.commas
            .get(i)
            .map(|&o| tt::Token::new(tt::Kind::SignComma, o as usize, 1))
    }

    /// Returns the token of the opening parenthesis.
    pub fn parenthesis_open(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisOpen, self.open as usize, 1)
    }

    /// Returns the token of the closing parenthesis.
    pub fn parenthesis_close(&self) -> tt::Token {
        tt::Token::new(tt::Kind::ParenthesisClose, self.close as usize, 1)
    }
}

impl<'a, T: 'a + Clone> Tuple<'a, T> {
    /// Returns the field at index i.
    pub fn field(&self, i: usize) -> Option<T> {
        self.fields.get(i).cloned()
    }
}

//
//  Trait Implementations
//
impl<'a> Range for Node<'a> {
    /// Returns the range spanned by the node.
    fn range(&self) -> com::Range {
        use self::Node::*;

        match *self {
            Expr(expr) => expr.range(),
            Item(item) => item.range(),
            Stmt(stmt) => stmt.range(),
        }
    }
}

impl<'a> Range for Expression<'a> {
    /// Returns the range spanned by the expression.
    fn range(&self) -> com::Range {
        use self::Expression::*;

        match *self {
            BinOp(_, _, left, right) => left.range().extend(right.range()),
            Block(_, _, range) => range,
            Constructor(c) => c.range(),
            FieldAccess(f) => f.range(),
            FunctionCall(fun) => fun.range(),
            If(if_else) => if_else.range(),
            Lit(lit) => lit.range(),
            PreOp(_, pos, expr)
                => com::Range::new(pos as usize, 0).extend(expr.range()),
            Tuple(t) => t.range(),
            Var(VariableIdentifier(range)) => range,
        }
    }
}

impl<'a> Range for Item<'a> {
    /// Returns the range spanned by the item.
    fn range(&self) -> com::Range {
        use self::Item::*;

        match *self {
            Enum(e) => e.range(),
            Fun(fun) => fun.range(),
            Rec(r) => r.range(),
        }
    }
}

impl<'a> Range for Pattern<'a> {
    /// Returns the range spanned by the item.
    fn range(&self) -> com::Range {
        use self::Pattern::*;

        match *self {
            Ignored(i) => i.range(),
            Tuple(t) => t.range(),
            Var(v) => v.range(),
        }
    }
}

impl<'a> Range for Enum<'a> {
    /// Returns the range spanned by the enum.
    fn range(&self) -> com::Range {
        self.keyword().range().extend(self.brace_close().range())
    }
}

impl<'a> Range for Record<'a> {
    /// Returns the range spanned by the record.
    fn range(&self) -> com::Range {
        self.keyword().range().extend(self.semi_colon().range())
    }
}

impl<'a> Range for InnerRecord<'a> {
    /// Returns the range spanned by the inner record.
    fn range(&self) -> com::Range {
        use self::InnerRecord::*;

        match *self {
            Missing(r) | Unexpected(r) => r,
            Tuple(t, tup) => t.0.extend(tup.range()),
            Unit(t) => t.0,
        }
    }
}

impl<'a> Range for Function<'a> {
    /// Returns the range spanned by the function.
    fn range(&self) -> com::Range {
        com::Range::new(self.keyword as usize, 4).extend(self.body.range())
    }
}

impl<'a> Range for Argument<'a> {
    /// Returns the range spanned by the argument.
    fn range(&self) -> com::Range {
        let offset = self.name.0.offset();
        let end_offset = self.comma as usize + 1;
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a, T: 'a> Range for Constructor<'a, T> {
    /// Returns the range spanned by the constructor.
    fn range(&self) -> com::Range {
        if self.arguments.close == 0 {
            self.type_.range()
        } else {
            self.type_.range().extend(self.arguments.range())
        }
    }
}

impl<'a> Range for FieldAccess<'a> {
    /// Returns the range spanned by the constructor.
    fn range(&self) -> com::Range {
        self.accessed.range().extend(self.field.0)
    }
}

impl<'a> Range for FunctionCall<'a> {
    /// Returns the range spanned by the function call.
    fn range(&self) -> com::Range {
        self.function.range().extend(self.parenthesis_close().range())
    }
}

impl<'a> Range for IfElse<'a> {
    /// Returns the range spanned by the argument.
    fn range(&self) -> com::Range {
        let offset = self.if_ as usize;
        let end_offset = self.false_expr.range().end_offset();
        com::Range::new(offset, end_offset - offset)
    }
}

impl<'a> Range for Statement<'a> {
    /// Returns the range spanned by the statement.
    fn range(&self) -> com::Range {
        use self::Statement::*;

        match *self {
            Set(set) => set.range(),
            Var(var) => var.range(),
        }
    }
}

impl<'a> Range for Literal<'a> {
    /// Returns the range spanned by the literal.
    fn range(&self) -> com::Range {
        use self::Literal::*;

        match *self {
            Bool(_, r) => r,
            Bytes(_, r) => r,
            Integral(r) => r,
            String(_, r) => r,
        }
    }
}

impl<'a> Range for VariableBinding<'a> {
    /// Returns the range spanned by the binding.
    fn range(&self) -> com::Range {
        debug_assert!(
            self.semi as usize >= self.expr.range().end_offset() - 1,
            "{} should occur after {}", self.semi, self.expr.range()
        );
        com::Range::new(
            self.var as usize,
            (self.semi + 1 - self.var) as usize
        )
    }
}

impl<'a> Range for VariableReBinding<'a> {
    /// Returns the range spanned by the binding.
    fn range(&self) -> com::Range {
        debug_assert!(
            self.semi as usize >= self.expr.range().end_offset() - 1,
            "{} should occur after {}", self.semi, self.expr.range()
        );
        com::Range::new(
            self.set as usize,
            (self.semi + 1 - self.set) as usize
        )
    } 
}

impl<'a> Range for Type<'a> {
    /// Returns the range spanned by the type.
    fn range(&self) -> com::Range {
        use self::Type::*;

        match *self {
            Missing(r) => r,
            Nested(t, p) => p.range().extend(t.range()),
            Simple(t) => t.range(),
            Tuple(t) => t.range(),
        }
    }
}

impl<'a> Range for Path<'a> {
    /// Returns the range spanned by the path.
    fn range(&self) -> com::Range {
        self.components[0]
            .range()
            .extend(self.double_colon(self.colons.len() - 1).unwrap().range())
    }
}

impl<'a, T: 'a> Range for Tuple<'a, T> {
    /// Returns the range spanned by the tuple.
    fn range(&self) -> com::Range {
        self.parenthesis_open().range().extend(self.parenthesis_close().range())
    }
}

impl Range for VariableIdentifier {
    /// Returns the range spanned by the variable identifier.
    fn range(&self) -> com::Range {
        self.0
    }
}

impl Range for TypeIdentifier {
    /// Returns the range spanned by the type identifier.
    fn range(&self) -> com::Range {
        self.0
    }
}

impl<'a> convert::From<Constructor<'a, Expression<'a>>> for Expression<'a> {
    fn from(c: Constructor<'a, Expression<'a>>) -> Expression<'a> {
        Expression::Constructor(c)
    }
}

impl<'a> convert::From<FieldAccess<'a>> for Expression<'a> {
    fn from(f: FieldAccess<'a>) -> Expression<'a> {
        Expression::FieldAccess(f)
    }
}

impl<'a> convert::From<FunctionCall<'a>> for Expression<'a> {
    fn from(f: FunctionCall<'a>) -> Expression<'a> {
        Expression::FunctionCall(f)
    }
}

impl<'a> convert::From<IfElse<'a>> for Expression<'a> {
    fn from(i: IfElse<'a>) -> Expression<'a> {
        Expression::If(i)
    }
}

impl<'a> convert::From<Literal<'a>> for Expression<'a> {
    fn from(l: Literal<'a>) -> Expression<'a> {
        Expression::Lit(l)
    }
}

impl<'a> convert::From<Tuple<'a, Expression<'a>>> for Expression<'a> {
    fn from(t: Tuple<'a, Expression<'a>>) -> Expression<'a> {
        Expression::Tuple(t)
    }
}

impl convert::From<VariableIdentifier> for Expression<'static> {
    fn from(v: VariableIdentifier) -> Expression<'static> {
        Expression::Var(v)
    }
}

impl<'a> convert::From<Enum<'a>> for Item<'a> {
    fn from(e: Enum<'a>) -> Item<'a> { Item::Enum(e) }
}

impl<'a> convert::From<Function<'a>> for Item<'a> {
    fn from(f: Function<'a>) -> Item<'a> { Item::Fun(f) }
}

impl<'a> convert::From<Record<'a>> for Item<'a> {
    fn from(r: Record<'a>) -> Item<'a> { Item::Rec(r) }
}

impl<'a> convert::From<Tuple<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(t: Tuple<'a, Pattern<'a>>) -> Pattern<'a> {
        Pattern::Tuple(t)
    }
}

impl<'a> convert::From<VariableBinding<'a>> for Statement<'a> {
    fn from(v: VariableBinding<'a>) -> Statement<'a> { Statement::Var(v) }
}

impl<'a> convert::From<VariableReBinding<'a>> for Statement<'a> {
    fn from(v: VariableReBinding<'a>) -> Statement<'a> { Statement::Set(v) }
}

impl<'a> convert::From<Tuple<'a, Type<'a>>> for Type<'a> {
    fn from(t: Tuple<'a, Type<'a>>) -> Type<'a> { Type::Tuple(t) }
}

impl<'a, T: 'a> Default for Tuple<'a, T> {
    fn default() -> Tuple<'a, T> {
        Tuple { fields: &[], commas: &[], open: 0, close: 0 }
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use super::*;
    use model::syn_builder::Factory;

    #[test]
    fn range_enum_empty() {
        let global_arena = mem::Arena::new();
        let item = Factory::new(&global_arena).item();

        //  " :enum Empty { }"
        let e: Enum = item.enum_(7, 5).build();
        assert_eq!(e.range(), range(1, 15));
    }

    #[test]
    fn range_enum_minimal() {
        let global_arena = mem::Arena::new();
        let item = Factory::new(&global_arena).item();

        //  ":enum Minimal"
        let e: Enum = item.enum_(6, 7).braces(12, 12).build();
        assert_eq!(e.range(), range(0, 13));
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
        assert_eq!(e.range(), range(0, 25));
    }

    #[test]
    fn range_expression_literal() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  "   1"
        assert_eq!(e.int(3, 4).range(), range(3, 4));
    }

    #[test]
    fn range_expression_binary_operator() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  "   1 + 1"
        assert_eq!(
            e.bin_op(e.int(3, 1), e.int(7, 1)).build().range(),
            range(3, 5)
        );
    }

    #[test]
    fn range_item_fun() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let e = syn.expr();

        //  "   :fun add() -> Int 1 + 1"
        let item: Item =
            syn.item()
                .function(
                    8,
                    3,
                    syn.type_().simple(16, 3),
                    e.bin_op(e.int(21, 1), e.int(25, 1)).build(),
                ).build();
        assert_eq!(item.range(), range(3, 23));
    }

    #[test]
    fn range_node_binary_operator() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  "   1 + 1"
        let node = Node::Expr(e.bin_op(e.int(3, 1), e.int(7, 1)).build());

        assert_eq!(node.range(), range(3, 5));
    }

    #[test]
    fn range_node_prefix_unary_operator() {
        let global_arena = mem::Arena::new();
        let e = Factory::new(&global_arena).expr();

        //  " !     1"
        let node = Node::Expr(e.pre_op(e.int(7, 2)).offset(1).build());

        assert_eq!(node.range(), range(1, 8));
    }

    #[test]
    fn range_stmt_variable_binding() {
        let global_arena = mem::Arena::new();
        let syn = Factory::new(&global_arena);
        let (e, p, s) = (syn.expr(), syn.pat(), syn.stmt());

        //  "     :var fool := 1234;"
        let mut var = s.var(p.var(10, 4), e.int(18, 4));

        let with_semi: Statement = var.build();
        assert_eq!(with_semi.range(), range(5, 18));

        let without_semi: Statement = var.semi_colon(21).build();
        assert_eq!(without_semi.range(), range(5, 17));
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }
}
