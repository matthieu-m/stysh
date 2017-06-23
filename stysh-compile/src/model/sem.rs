//! Semantic model, also confusingly named AST.
//!
//! This is the model describing the semantics of the language. It abstracts
//! over syntactic sugar ("0xa", "1_0" and "0b1010" are all an integral of value
//! "10") and resolves the names and types to their declaration.
//!
//! When produced by the sem pass, it is also expected that the model be
//! type-checked, that is that if an variable of type T is passed to a function
//! expecting an argument of type U, it has been checked that T be compatible to
//! U *and* that transformation has been made explicit.
//!
//! The structures are parameterized by the lifetime of the arena providing the
//! memory for their members.

use std;

use basic::{com, mem};
use basic::mem::CloneInto;

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type<'a> {
    /// A built-in type.
    Builtin(BuiltinType),
    /// A tuple type.
    Tuple(Tuple<'a, Type<'a>>),
    /// An unresolved type.
    Unresolved(ItemIdentifier),
}

/// A built-in Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinType {
    /// A boolean.
    Bool,
    /// A 64-bits signed integer.
    Int,
    /// A String.
    String,
}

/// A Value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Value<'a> {
    /// Type of the value.
    pub type_: Type<'a>,
    /// Range of the expression evaluating to the value.
    pub range: com::Range,
    /// Expression evaluating to the value.
    pub expr: Expr<'a>,
}

/// A binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Binding<'a> {
    /// A function argument.
    Argument(ValueIdentifier, Type<'a>, com::Range),
    /// A variable declaration.
    Variable(ValueIdentifier, Value<'a>, com::Range),
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr<'a> {
    /// A reference to an existing argument binding.
    ArgumentRef(ValueIdentifier),
    /// A block expression.
    Block(&'a [Stmt<'a>], &'a Value<'a>),
    /// A built-in value.
    BuiltinVal(BuiltinValue<'a>),
    /// A function call.
    Call(Callable<'a>, &'a [Value<'a>]),
    /// A if expression (condition, true-branch, false-branch).
    If(&'a Value<'a>, &'a Value<'a>, &'a Value<'a>),
    /// A tuple.
    Tuple(Tuple<'a, Value<'a>>),
    /// An unresolved reference.
    UnresolvedRef(ValueIdentifier),
    /// A reference to an existing variable binding.
    VariableRef(ValueIdentifier),
}

/// A built-in value, the type is implicit.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinValue<'a> {
    /// A boolean.
    Bool(bool),
    /// An integral.
    Int(i64),
    /// A String.
    String(&'a [u8]),
}

/// A Statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Stmt<'a> {
    //  FIXME(matthieum): expressions of unit type sequenced with a semi-colon?
    /// A variable binding.
    Var(Binding<'a>),
}

/// A Callable.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Callable<'a> {
    /// A built-in function.
    Builtin(BuiltinFunction),
    /// A static user-defined function.
    Function(FunctionProto<'a>),
    /// An unknown callable binding.
    Unknown(ValueIdentifier),
    /// An unresolved callable binding.
    ///
    /// Note: this variant only contains possible resolutions.
    /// Note: this variant contains at least two possible resolutions.
    Unresolved(&'a [Callable<'a>]),
}

/// A built-in function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinFunction {
    /// An addition.
    Add,
    /// An non-equality comparison.
    Differ,
    /// An equality comparison.
    Equal,
    /// A floor division.
    FloorDivide,
    /// A greater than comparison.
    GreaterThan,
    /// A greater than or equal comparison.
    GreaterThanOrEqual,
    /// A less than comparison.
    LessThan,
    /// A less than or equal comparison.
    LessThanOrEqual,
    /// A multiplication.
    Multiply,
    /// A substraction.
    Substract,
}

/// A tuple.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<'a, T: 'a> {
    /// The tuple fields.
    pub fields: &'a [T],
}

/// An annotated prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Prototype<'a> {
    /// A function prototype.
    Fun(FunctionProto<'a>),
}

/// A function prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionProto<'a> {
    /// The function's identifier.
    pub name: ItemIdentifier,
    /// The function's prototype range.
    pub range: com::Range,
    /// The function's arguments (always arguments).
    pub arguments: &'a [Binding<'a>],
    /// The return type of the function.
    pub result: Type<'a>,
}

/// A function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Function<'a> {
    /// The prototype.
    pub prototype: &'a FunctionProto<'a>,
    /// The body.
    pub body: Value<'a>,
}

/// An full-fledged item.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Item<'a> {
    /// A full-fledged function definition.
    Fun(Function<'a>),
}

/// An item identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ItemIdentifier(pub com::Range);

/// A value identifier.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ValueIdentifier(pub com::Range);

impl<'a> Type<'a> {
    /// Returns an unresolved type.
    pub fn unresolved() -> Type<'a> {
        Type::Unresolved(ItemIdentifier::unresolved())
    }
}

impl<'a> Prototype<'a> {
    /// Returns the range spanned by the prototype.
    pub fn range(&self) -> com::Range {
        match *self {
            Prototype::Fun(fun) => fun.range,
        }
    }
}

impl<'a> Callable<'a> {
    /// Returns the type of the result of the function.
    pub fn result_type(&self) -> Type<'a> {
        use self::Callable::*;

        match *self {
            Builtin(fun) => fun.result_type(),
            Function(fun) => fun.result,
            Unknown(_) | Unresolved(_) => Type::unresolved(),
        }
    }
}

impl BuiltinFunction {
    /// Returns the type of the result of the function.
    pub fn result_type(&self) -> Type<'static> {
        use self::BuiltinFunction::*;

        let type_ = match *self {
            Add | FloorDivide | Multiply | Substract => BuiltinType::Int,
            Differ | Equal | GreaterThan | GreaterThanOrEqual |
            LessThan | LessThanOrEqual => BuiltinType::Bool,
        };
        Type::Builtin(type_)
    }
}

impl ItemIdentifier {
    /// Returns a sentinel instance of ItemIdentifier.
    pub fn unresolved() -> ItemIdentifier {
        ItemIdentifier(com::Range::new(0, 0))
    }
}

impl ValueIdentifier {
    /// Returns a sentinel instance of ValueIdentifier.
    pub fn unresolved() -> ItemIdentifier {
        ItemIdentifier(com::Range::new(0, 0))
    }
}

//
//  CloneInto implementations
//
impl<'a, 'target> CloneInto<'target> for Type<'a> {
    type Output = Type<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        match *self {
            Type::Tuple(t) => Type::Tuple(arena.intern(&t)),
            Type::Builtin(t) => Type::Builtin(t),
            Type::Unresolved(n) => Type::Unresolved(n),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Value<'a> {
    type Output = Value<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Value {
            type_: arena.intern(&self.type_),
            range: self.range,
            expr: arena.intern(&self.expr),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Expr<'a> {
    type Output = Expr<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        match *self {
            Expr::BuiltinVal(v) => Expr::BuiltinVal(arena.intern(&v)),
            Expr::Tuple(t) => Expr::Tuple(arena.intern(&t)),
            _ => unimplemented!(),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Binding<'a> {
    type Output = Binding<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Binding::*;

        match *self {
            Argument(id, type_, range)
                => Argument(id, arena.intern(&type_), range),
            Variable(id, value, range)
                => Variable(id, arena.intern(&value), range),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Prototype<'a> {
    type Output = Prototype<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        match *self {
            Prototype::Fun(fun)
                => Prototype::Fun(arena.intern(&fun)),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for FunctionProto<'a> {
    type Output = FunctionProto<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        FunctionProto {
            name: self.name,
            range: self.range,
            arguments: CloneInto::clone_into(self.arguments, arena),
            result: arena.intern(&self.result),
        }
    }
}

impl<'a, 'target, T> CloneInto<'target> for Tuple<'a, T>
    where T: CloneInto<'target> + Copy + 'a
{
    type Output = Tuple<'target, <T as CloneInto<'target>>::Output>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Tuple { fields: CloneInto::clone_into(self.fields, arena) }
    }
}

impl<'a, 'target> CloneInto<'target> for BuiltinValue<'a> {
    type Output = BuiltinValue<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        match *self {
            BuiltinValue::Bool(b) => BuiltinValue::Bool(b),
            BuiltinValue::Int(i) => BuiltinValue::Int(i),
            BuiltinValue::String(s) =>
                BuiltinValue::String(arena.insert_slice(s)),
        }
    }
}

impl<'a> std::convert::From<BuiltinValue<'a>> for bool {
    fn from(value: BuiltinValue<'a>) -> bool {
        match value {
            BuiltinValue::Bool(b) => b,
            _ => panic!("{} is not a boolean", value),
        }
    }
}

impl<'a> std::convert::From<BuiltinValue<'a>> for i64 {
    fn from(value: BuiltinValue<'a>) -> i64 {
        match value {
            BuiltinValue::Int(i) => i,
            _ => panic!("{} is not an integer", value),
        }
    }
}


//
//  Implementation Details
//
impl std::fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl<'a> std::fmt::Display for BuiltinValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            BuiltinValue::Bool(b) =>
                write!(f, "{}", if b { "true" } else { "false" }),
            BuiltinValue::Int(i) => write!(f, "{:x}", i),
            BuiltinValue::String(s) => write!(f, "{}", com::Slice(s)),
        }
    }
}

impl std::fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use self::BuiltinFunction::*;

        match *self {
            Add => write!(f, "__add__"),
            Differ => write!(f, "__ne__"),
            Equal => write!(f, "__eq__"),
            FloorDivide => write!(f, "__fdiv__"),
            GreaterThan => write!(f, "__gt__"),
            GreaterThanOrEqual => write!(f, "__gte__"),
            LessThan => write!(f, "__lt__"),
            LessThanOrEqual => write!(f, "__lte__"),
            Multiply => write!(f, "__mul__"),
            Substract => write!(f, "__sub__"),
        }
    }
}

impl<'a> std::fmt::Display for Callable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use self::Callable::*;

        match *self {
            Builtin(b) => write!(f, "{}", b),
            Function(fun) => write!(f, "<{}>", fun.name.0),
            Unknown(_) => write!(f, "<unknown>"),
            Unresolved(funs) => {
                write!(f, "<unresolved: ")?;
                for (i, e) in funs.iter().enumerate() {
                    if i != 0 { write!(f, ", ")? }
                    write!(f, "{}", e)?;
                }
                write!(f, ">")
            },
        }
    }
}

impl<'a, T> std::fmt::Display for Tuple<'a, T>
    where
        T: std::fmt::Display
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "(")?;
        for (i, e) in self.fields.iter().enumerate() {
            if i != 0 { write!(f, ", ")? }
            write!(f, "{}", e)?;
        }
        write!(f, ")")
    }
}

impl<'a> std::fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Type::Builtin(t) => write!(f, "{}", t),
            Type::Tuple(t) => write!(f, "{}", t),
            Type::Unresolved(i) => write!(f, "<{}>", i.0),
        }
    }
}
