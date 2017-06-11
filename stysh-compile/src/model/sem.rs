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
    /// A built-in function call.
    BuiltinCall(BuiltinFunction, &'a [Value<'a>]),
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
pub struct Prototype<'a> {
    /// The prototype name.
    pub name: ItemIdentifier,
    /// The prototype range.
    pub range: com::Range,
    /// The prototype itself.
    pub proto: Proto<'a>,
}

/// A generic prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Proto<'a> {
    /// A function prototype.
    Fun(FunctionProto<'a>),
}

/// A function prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionProto<'a> {
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

//
//  CloneInto implementations
//
impl<'a, 'target> CloneInto<'target> for Type<'a> {
    type Output = Type<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        match *self {
            Type::Tuple(t) => Type::Tuple(CloneInto::clone_into(&t, arena)),
            Type::Builtin(t) => Type::Builtin(t),
            Type::Unresolved(n) => Type::Unresolved(n),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Value<'a> {
    type Output = Value<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Value {
            type_: CloneInto::clone_into(&self.type_, arena),
            range: self.range,
            expr: CloneInto::clone_into(&self.expr, arena),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Expr<'a> {
    type Output = Expr<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        match *self {
            Expr::BuiltinVal(v) =>
                Expr::BuiltinVal(CloneInto::clone_into(&v, arena)),
            Expr::Tuple(t) => Expr::Tuple(CloneInto::clone_into(&t, arena)),
            _ => unimplemented!(),
        }
    }
}

impl<'a, 'target, T> CloneInto<'target> for Tuple<'a, T>
    where T: CloneInto<'target> + 'a
{
    type Output = Tuple<'target, <T as CloneInto<'target>>::Output>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        let mut fields = mem::Array::with_capacity(self.fields.len(), arena);
        for f in self.fields {
            fields.push(CloneInto::clone_into(f, arena));
        }
        Tuple { fields: fields.into_slice() }
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
            Add => write!(f, "add"),
            Differ => write!(f, "ne"),
            Equal => write!(f, "eq"),
            FloorDivide => write!(f, "fdiv"),
            GreaterThan => write!(f, "gt"),
            GreaterThanOrEqual => write!(f, "gte"),
            LessThan => write!(f, "lt"),
            LessThanOrEqual => write!(f, "lte"),
            Multiply => write!(f, "mul"),
            Substract => write!(f, "sub"),
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
