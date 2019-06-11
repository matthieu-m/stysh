//! Values.

use std::{convert, fmt};

use basic::mem;

use model::hir::*;

//
//  Public Types
//

/// A (simplified) Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type, possibly nested.
    Enum(ItemIdentifier, PathId, Id<[TypeId]>),
    /// A record type, possibly nested.
    Rec(ItemIdentifier, PathId, Tuple<TypeId>),
    /// A tuple type.
    Tuple(Tuple<TypeId>),
    /// An unresolved type, possibly nested.
    Unresolved(ItemIdentifier, PathId),
}

//
//  Public Values
//

/// A built-in function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinFunction {
    /// An addition.
    Add,
    /// A boolean and.
    And,
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
    /// A boolean not.
    Not,
    /// A boolean or.
    Or,
    /// A substraction.
    Substract,
    /// A boolean xor.
    Xor,
}

/// A built-in value, the type is implicit.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinValue {
    /// A boolean.
    Bool(bool),
    /// An integral.
    Int(i64),
    /// A String.
    String(mem::InternId),
}

/// A Callable.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Callable {
    /// A built-in function.
    Builtin(BuiltinFunction),
    /// A static user-defined function.
    Function(ItemIdentifier, Tuple<TypeId>, TypeId),
    /// An unknown callable binding.
    Unknown(ValueIdentifier),
    /// An unresolved callable binding.
    ///
    /// Note: this variant only contains possible resolutions.
    /// Note: this variant contains at least two possible resolutions.
    Unresolved(Id<[Callable]>),
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr {
    /// A block expression.
    Block(Id<[Stmt]>, Option<ExpressionId>),
    /// A built-in value.
    BuiltinVal(BuiltinValue),
    /// A function call.
    Call(Callable, Tuple<ExpressionId>),
    /// A constructor call.
    Constructor(Tuple<ExpressionId>),
    /// A field access.
    FieldAccess(ExpressionId, Field),
    /// A if expression (condition, true-branch, false-branch).
    If(ExpressionId, ExpressionId, ExpressionId),
    /// An implicit cast (variant to enum, anonymous to named, ...).
    Implicit(Implicit),
    /// A loop.
    Loop(Id<[Stmt]>),
    /// A reference to an existing binding.
    Ref(ValueIdentifier, Gvn),
    /// A tuple.
    Tuple(Tuple<ExpressionId>),
    /// An unresolved reference.
    UnresolvedRef(ValueIdentifier),
}

/// An Implicit cast.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Implicit {
    /// An enumerator to enum cast.
    ToEnum(ItemIdentifier, ExpressionId),
}

//
//  Public interface
//

impl BuiltinFunction {
    /// Returns the number of arguments expected.
    pub fn number_arguments(&self) -> u8 {
        if *self == BuiltinFunction::Not {
            1
        } else {
            2
        }
    }

    /// Returns the type of the result of the function.
    pub fn result_type(&self) -> Type {
        use self::BuiltinFunction::*;

        match *self {
            Add | FloorDivide | Multiply | Substract => Type::int(),
            And | Differ | Equal | GreaterThan | GreaterThanOrEqual |
            LessThan | LessThanOrEqual | Not | Or | Xor => Type::bool_(),
        }
    }
}

impl BuiltinValue {
    /// Returns the type of the built-in value.
    pub fn result_type(&self) -> Type {
        use self::BuiltinValue::*;

        match *self {
            Bool(_) => Type::bool_(),
            Int(_) => Type::int(),
            String(_) => Type::string(),
        }
    }
}

impl Expr {
    /// Returns a builtin Bool expr.
    pub fn bool_(b: bool) -> Self { Expr::BuiltinVal(BuiltinValue::Bool(b)) }

    /// Returns a builtin Int expr.
    pub fn int(i: i64) -> Self { Expr::BuiltinVal(BuiltinValue::Int(i)) }

    /// Returns a Unit expr.
    pub fn unit() -> Self { Expr::Tuple(Tuple::unit()) }
}

impl Type {
    /// Returns a Bool type.
    pub fn bool_() -> Self { Type::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { Type::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { Type::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { Type::Builtin(BuiltinType::Void) }

    /// Returns a Unit type.
    pub fn unit() -> Self { Type::Tuple(Tuple::unit()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        Type::Unresolved(ItemIdentifier::unresolved(), PathId::empty())
    }

    /// Replaces the Path.
    pub fn with_path(self, path: PathId) -> Self {
        use self::Type::*;

        match self {
            Enum(name, _, vars) => Enum(name, path, vars),
            Rec(name, _, definition) => Rec(name, path, definition),
            Unresolved(name, _) => Unresolved(name, path),
            _ => panic!("{:?} has no path!", self),
        }
    }

    /// Returns the name, if any.
    pub fn name(&self) -> ItemIdentifier {
        use self::Type::*;

        match *self {
            Enum(name, ..) | Rec(name, ..) | Unresolved(name, ..) => name,
            _ => panic!("{:?} has no name!", self),
        }
    }
}

//
//  Default Implementations
//

impl Default for Callable {
    fn default() -> Callable { Callable::Unknown(Default::default()) }
}

impl Default for Expr {
    fn default() -> Expr { Expr::UnresolvedRef(Default::default()) }
}

//
//  From Implementations
//

impl convert::From<BuiltinValue> for bool {
    fn from(value: BuiltinValue) -> Self {
        match value {
            BuiltinValue::Bool(b) => b,
            _ => panic!("{} is not a boolean", value),
        }
    }
}

impl convert::From<BuiltinValue> for i64 {
    fn from(value: BuiltinValue) -> Self {
        match value {
            BuiltinValue::Int(i) => i,
            _ => panic!("{} is not an integer", value),
        }
    }
}

//
//  Implementation Details
//

impl fmt::Display for BuiltinValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            BuiltinValue::Bool(b) =>
                write!(f, "{}", if *b { "true" } else { "false" }),
            BuiltinValue::Int(i) => write!(f, "{:x}", i),
            BuiltinValue::String(i) => write!(f, "{:?}", i),
        }
    }
}

impl fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::BuiltinFunction::*;

        match *self {
            And => write!(f, "__and__"),
            Add => write!(f, "__add__"),
            Differ => write!(f, "__ne__"),
            Equal => write!(f, "__eq__"),
            FloorDivide => write!(f, "__fdiv__"),
            GreaterThan => write!(f, "__gt__"),
            GreaterThanOrEqual => write!(f, "__gte__"),
            LessThan => write!(f, "__lt__"),
            LessThanOrEqual => write!(f, "__lte__"),
            Multiply => write!(f, "__mul__"),
            Not => write!(f, "__not__"),
            Or => write!(f, "__or__"),
            Substract => write!(f, "__sub__"),
            Xor => write!(f, "__xor__"),
        }
    }
}
