//! Values.

use std::{convert, fmt};

use basic::{com, mem};
use basic::com::Span;
use basic::mem::CloneInto;

use model::ast;
use model::hir::*;

/// A Value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Value<'a> {
    /// Type of the value.
    pub type_: Type<'a>,
    /// Range of the expression evaluating to the value.
    pub range: com::Range,
    /// Expression evaluating to the value.
    pub expr: Expr<'a>,
    /// Unique identifier of this value within the context;
    /// or at least, it is unique *after* the GVN pass.
    pub gvn: Gvn,
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr<'a> {
    /// A reference to an existing argument binding.
    ArgumentRef(ValueIdentifier, Gvn),
    /// A block expression.
    Block(&'a [Stmt<'a>], Option<&'a Value<'a>>),
    /// A built-in value.
    BuiltinVal(BuiltinValue<'a>),
    /// A function call.
    Call(Callable<'a>, &'a [Value<'a>]),
    /// A constructor call.
    Constructor(Constructor<'a , Value<'a>>),
    /// A field access.
    FieldAccess(&'a Value<'a>, u16),
    /// A if expression (condition, true-branch, false-branch).
    If(&'a Value<'a>, &'a Value<'a>, &'a Value<'a>),
    /// An implicit cast (variant to enum, anonymous to named, ...).
    Implicit(Implicit<'a>),
    /// A loop.
    Loop(&'a [Stmt<'a>]),
    /// A tuple.
    Tuple(Tuple<'a, Value<'a>>),
    /// An unresolved field access.
    UnresolvedField(&'a Value<'a>, ValueIdentifier),
    /// An unresolved reference.
    UnresolvedRef(ValueIdentifier),
    /// A reference to an existing variable binding.
    VariableRef(ValueIdentifier, Gvn),
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
pub enum BuiltinValue<'a> {
    /// A boolean.
    Bool(bool),
    /// An integral.
    Int(i64),
    /// A String.
    String(&'a [u8]),
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

/// A global value number.
///
/// Defaults to 0, which is considered an invalid value.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Gvn(pub u32);

/// An Implicit cast.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Implicit<'a> {
    /// An enumerator to enum cast.
    ToEnum(EnumProto, &'a Value<'a>),
}

/// A value identifier.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
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
    pub fn result_type(&self) -> Type<'static> {
        use self::BuiltinFunction::*;

        let type_ = match *self {
            Add | FloorDivide | Multiply | Substract => BuiltinType::Int,
            And | Differ | Equal | GreaterThan | GreaterThanOrEqual |
            LessThan | LessThanOrEqual | Not | Or | Xor => BuiltinType::Bool,
        };
        Type::Builtin(type_)
    }
}

impl<'a> BuiltinValue<'a> {
    /// Returns the type of the built-in value.
    pub fn result_type(&self) -> Type<'static> {
        use self::BuiltinValue::*;

        Type::Builtin(match *self {
            Bool(_) => BuiltinType::Bool,
            Int(_) => BuiltinType::Int,
            String(_) => BuiltinType::String,
        })
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

impl Expr<'static> {
    /// Returns a builtin Bool expr.
    pub fn bool_(b: bool) -> Self { Expr::BuiltinVal(BuiltinValue::Bool(b)) }

    /// Returns a builtin Int expr.
    pub fn int(i: i64) -> Self { Expr::BuiltinVal(BuiltinValue::Int(i)) }
}

impl<'a> Value<'a> {
    /// Sets the gvn the value refers to.
    pub fn ref_gvn<G: convert::Into<Gvn>>(mut self, gvn: G) -> Value<'a> {
        use self::Expr::*;

        self.expr = match self.expr {
            ArgumentRef(id, _) => ArgumentRef(id, gvn.into()),
            VariableRef(id, _) => VariableRef(id, gvn.into()),
            _ => panic!("Cannot specify reference GVN in {:?}", self.expr),
        };

        self
    }

    /// Sets the gvn of the value.
    pub fn with_gvn<G: convert::Into<Gvn>>(mut self, gvn: G) -> Value<'a> {
        self.gvn = gvn.into();
        self
    }

    /// Sets the range.
    pub fn with_range(mut self, pos: usize, len: usize) -> Value<'a> {
        self.range = com::Range::new(pos, len);
        self
    }
}

impl Value<'static> {
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
//  CloneInto implementations
//

impl<'a, 'target> CloneInto<'target> for BuiltinValue<'a> {
    type Output = BuiltinValue<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::BuiltinValue::*;

        match *self {
            Bool(b) => Bool(b),
            Int(i) => Int(i),
            String(s) => String(arena.insert_slice(s)),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Callable<'a> {
    type Output = Callable<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Callable::*;

        match *self {
            Builtin(f) => Builtin(f),
            Function(ref f) => Function(arena.intern(f)),
            Unknown(v) => Unknown(v),
            Unresolved(c) => Unresolved(CloneInto::clone_into(c, arena)),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Expr<'a> {
    type Output = Expr<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Expr::*;

        match *self {
            ArgumentRef(v, gvn) => ArgumentRef(v, gvn),
            Block(stmts, v) => Block(
                CloneInto::clone_into(stmts, arena),
                v.map(|v| arena.intern_ref(v)),
            ),
            BuiltinVal(v) => BuiltinVal(arena.intern(&v)),
            Call(c, args) => Call(
                arena.intern(&c),
                CloneInto::clone_into(args, arena),
            ),
            Constructor(c) => Constructor(arena.intern(&c)),
            If(c, t, f) => If(
                arena.intern_ref(c),
                arena.intern_ref(t),
                arena.intern_ref(f),
            ),
            Implicit(i) => Implicit(arena.intern(&i)),
            Loop(stmts) => Loop(CloneInto::clone_into(stmts, arena)),
            Tuple(t) => Tuple(arena.intern(&t)),
            VariableRef(v, gvn) => VariableRef(v, gvn),
            _ => panic!("not yet implement for {:?}", self),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Implicit<'a> {
    type Output = Implicit<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Implicit::*;

        match *self {
            ToEnum(e, v) => ToEnum(e, arena.intern_ref(v)),
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
            gvn: Default::default(),
        }
    }
}

//
//  Span Implementations
//

impl<'a> Span for Value<'a> {
    /// Returns the range spanned by the value.
    fn span(&self) -> com::Range { self.range }
}

impl Span for ValueIdentifier {
    /// Returns the range spanned by the ValueIdentifier.
    fn span(&self) -> com::Range { self.1 }
}

//
//  From Implementations
//

impl<'a> convert::From<BuiltinValue<'a>> for bool {
    fn from(value: BuiltinValue<'a>) -> Self {
        match value {
            BuiltinValue::Bool(b) => b,
            _ => panic!("{} is not a boolean", value),
        }
    }
}

impl<'a> convert::From<BuiltinValue<'a>> for i64 {
    fn from(value: BuiltinValue<'a>) -> Self {
        match value {
            BuiltinValue::Int(i) => i,
            _ => panic!("{} is not an integer", value),
        }
    }
}

impl<'a> convert::From<Constructor<'a, Value<'a>>> for Expr<'a> {
    fn from(c: Constructor<'a, Value<'a>>) -> Self { Expr::Constructor(c) }
}

impl<'a> convert::From<Tuple<'a, Value<'a>>> for Expr<'a> {
    fn from(t: Tuple<'a, Value<'a>>) -> Self { Expr::Tuple(t) }
}

impl convert::From<u32> for Gvn {
    fn from(v: u32) -> Gvn { Gvn(v) }
}

impl<'a> convert::From<Constructor<'a, Value<'a>>> for Value<'a> {
    fn from(c: Constructor<'a, Value<'a>>) -> Self {
        Value {
            type_: Type::Rec(c.type_),
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

impl<'a> fmt::Display for BuiltinValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            BuiltinValue::Bool(b) =>
                write!(f, "{}", if b { "true" } else { "false" }),
            BuiltinValue::Int(i) => write!(f, "{:x}", i),
            BuiltinValue::String(s) => write!(f, "{}", com::Slice(s)),
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

impl<'a> fmt::Display for Callable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Callable::*;

        match *self {
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
