//! Values.

use std::{convert, fmt};

use basic::com::{self, Span};
use basic::mem::{self, DynArray, Ptr};

use model::ast;
use model::hir::*;

/// A Value.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Value {
    /// Type of the value.
    pub type_: Type,
    /// Range of the expression evaluating to the value.
    pub range: com::Range,
    /// Expression evaluating to the value.
    pub expr: Expr,
    /// Unique identifier of this value within the context;
    /// or at least, it is unique *after* the GVN pass.
    pub gvn: Gvn
}

/// An Expression.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr {
    /// A block expression.
    Block(DynArray<Stmt>, Option<Ptr<Value>>),
    /// A built-in value.
    BuiltinVal(BuiltinValue),
    /// A function call.
    Call(Callable, DynArray<Value>),
    /// A constructor call.
    Constructor(Constructor<Value>),
    /// A field access.
    FieldAccess(Ptr<Value>, Field),
    /// A if expression (condition, true-branch, false-branch).
    If(Ptr<Value>, Ptr<Value>, Ptr<Value>),
    /// An implicit cast (variant to enum, anonymous to named, ...).
    Implicit(Implicit),
    /// A loop.
    Loop(DynArray<Stmt>),
    /// A reference to an existing binding.
    Ref(ValueIdentifier, Gvn),
    /// A tuple.
    Tuple(Tuple<Value>),
    /// An unresolved reference.
    UnresolvedRef(ValueIdentifier),
}

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
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Callable {
    /// A built-in function.
    Builtin(BuiltinFunction),
    /// A static user-defined function.
    Function(FunctionProto),
    /// An unknown callable binding.
    Unknown(ValueIdentifier),
    /// An unresolved callable binding.
    ///
    /// Note: this variant only contains possible resolutions.
    /// Note: this variant contains at least two possible resolutions.
    Unresolved(DynArray<Callable>),
}

/// A global value number.
///
/// Defaults to 0, which is considered an invalid value.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Gvn(pub u32);

/// An Implicit cast.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Implicit {
    /// An enumerator to enum cast.
    ToEnum(EnumProto, Ptr<Value>),
}

/// A value identifier.
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ValueIdentifier(pub mem::InternId, pub com::Range);

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

        let type_ = match *self {
            Add | FloorDivide | Multiply | Substract => BuiltinType::Int,
            And | Differ | Equal | GreaterThan | GreaterThanOrEqual |
            LessThan | LessThanOrEqual | Not | Or | Xor => BuiltinType::Bool,
        };
        Type::Builtin(type_)
    }
}

impl BuiltinValue {
    /// Returns the type of the built-in value.
    pub fn result_type(&self) -> Type {
        use self::BuiltinValue::*;

        Type::Builtin(match *self {
            Bool(_) => BuiltinType::Bool,
            Int(_) => BuiltinType::Int,
            String(_) => BuiltinType::String,
        })
    }
}

impl Callable {
    /// Returns the type of the result of the function.
    pub fn result_type(&self) -> Type {
        use self::Callable::*;

        match self {
            Builtin(fun) => fun.result_type(),
            Function(fun) => fun.result.clone(),
            Unknown(_) | Unresolved(_) => Type::unresolved(),
        }
    }
}

impl Expr {
    /// Returns a builtin Bool expr.
    pub fn bool_(b: bool) -> Self { Expr::BuiltinVal(BuiltinValue::Bool(b)) }

    /// Returns a builtin Int expr.
    pub fn int(i: i64) -> Self { Expr::BuiltinVal(BuiltinValue::Int(i)) }
}

impl Value {
    /// Sets the gvn the value refers to.
    pub fn ref_gvn<G: convert::Into<Gvn>>(mut self, gvn: G) -> Value {
        use self::Expr::*;

        self.expr = match self.expr {
            Ref(id, _) => Ref(id, gvn.into()),
            _ => panic!("Cannot specify reference GVN in {:?}", self.expr),
        };

        self
    }

    /// Sets the expression of the value.
    pub fn with_expr(mut self, expr: Expr) -> Value {
        self.expr = expr;
        self
    }

    /// Sets the gin of the value type..
    pub fn with_gin<G: convert::Into<Gin>>(mut self, gin: G) -> Value {
        self.type_ = self.type_.with_gin(gin.into());
        self
    }

    /// Sets the gvn of the value.
    pub fn with_gvn<G: convert::Into<Gvn>>(mut self, gvn: G) -> Value {
        self.gvn = gvn.into();
        self
    }

    /// Sets the range.
    pub fn with_range(mut self, pos: usize, len: usize) -> Value {
        self.range = com::Range::new(pos, len);
        self
    }

    /// Sets the type.
    pub fn with_type(mut self, t: Type) -> Value {
        self.type_ = t;
        self
    }

    /// Strips the type.
    pub fn without_type(mut self) -> Value {
        self.type_ = Type::unresolved();
        self
    }
}

impl Value {
    /// Returns a Bool Value.
    pub fn bool_(b: bool) -> Self {
        Value {
            type_: Type::bool_(),
            range: Default::default(),
            expr: Expr::bool_(b),
            gvn: Default::default(),
        }
    }

    /// Returns an Int Value.
    pub fn int(i: i64) -> Self {
        Value {
            type_: Type::int(),
            range: Default::default(),
            expr: Expr::int(i),
            gvn: Default::default(),
        }
    }

    /// Returns a unit value.
    pub fn unit() -> Self {
        Value {
            type_: Type::unit(),
            range: Default::default(),
            expr: Expr::Tuple(Tuple::unit()),
            gvn: Default::default(),
        }
    }
}

impl ValueIdentifier {
    /// Returns the InternId.
    pub fn id(&self) -> mem::InternId { self.0 }

    /// Sets the InternId.
    pub fn with_id(self, id: mem::InternId) -> Self {
        ValueIdentifier(id, self.1)
    }

    /// Sets the Range.
    pub fn with_range(self, range: com::Range) -> Self {
        ValueIdentifier(self.0, range)
    }
}

//
//  Span Implementations
//

impl Span for Value {
    /// Returns the range spanned by the value.
    fn span(&self) -> com::Range { self.range }
}

impl Span for ValueIdentifier {
    /// Returns the range spanned by the ValueIdentifier.
    fn span(&self) -> com::Range { self.1 }
}

//
//  Debug Implementations
//

impl std::fmt::Debug for Gvn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Gvn({})", self.0)
    }
}

impl std::fmt::Debug for ValueIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "ValueIdentifier({:?}, {})", self.0, self.1)
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

impl convert::From<Constructor<Value>> for Expr {
    fn from(c: Constructor<Value>) -> Self { Expr::Constructor(c) }
}

impl convert::From<Tuple<Value>> for Expr {
    fn from(t: Tuple<Value>) -> Self { Expr::Tuple(t) }
}

impl convert::From<u32> for Gvn {
    fn from(v: u32) -> Gvn { Gvn(v) }
}

impl convert::From<Constructor<Value>> for Value {
    fn from(c: Constructor<Value>) -> Self {
        Value {
            type_: c.type_.clone(),
            range: c.span(),
            expr: Expr::Constructor(c),
            gvn: Default::default(),
        }
    }
}

impl convert::From<ast::VariableIdentifier> for ValueIdentifier {
    fn from(value: ast::VariableIdentifier) -> Self {
        ValueIdentifier(value.id(), value.span())
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

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Callable::*;

        match self {
            Builtin(b) => write!(f, "{}", b),
            Function(fun) => write!(f, "{}", fun.name),
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
