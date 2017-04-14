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

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An unresolved type.
    Unresolved,
}

/// A built-in Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinType {
    /// A 64-bits signed integer.
    Int,
    /// A String.
    String,
}

/// A Value.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Value<'a> {
    /// Type of the value.
    pub type_: Type,
    /// Range of the expression evaluating to the value.
    pub range: com::Range,
    /// Expression evaluating to the value.
    pub expr: Expr<'a>,
}

/// A binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Binding<'a> {
    /// A function argument.
    Argument(ValueIdentifier, Type, com::Range),
    /// A variable declaration.
    Variable(ValueIdentifier, Value<'a>, com::Range),
}

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr<'a> {
    /// A reference to an existing argument binding.
    ArgumentRef(ValueIdentifier),
    /// A block expression.
    Block(&'a [Stmt<'a>], &'a Expr<'a>),
    /// A built-in value.
    BuiltinVal(BuiltinValue<'a>),
    /// A built-in function call.
    BuiltinCall(BuiltinFunction, &'a [Value<'a>]),
    /// An unresolved reference.
    UnresolvedRef(ValueIdentifier),
    /// A reference to an existing variable binding.
    VariableRef(ValueIdentifier),
}

/// A built-in value, the type is implicit.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinValue<'a> {
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
    pub result: Type,
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

impl<'a, 'target> Value<'a> {
    /// Duplicates the value into the target arena.
    pub fn duplicate(&self, arena: &'target mem::Arena) -> Value<'target> {
        let e = match self.expr {
            Expr::BuiltinVal(v) => Expr::BuiltinVal(v.duplicate(arena)),
            _ => unimplemented!(),
        };

        Value {
            type_: self.type_,
            range: self.range,
            expr: e,
        }
    }
}

impl<'a, 'target> BuiltinValue<'a> {
    /// Duplicates the built-in value into the target arena.
    pub fn duplicate(&self, arena: &'target mem::Arena) -> BuiltinValue<'target> {
        match *self {
            BuiltinValue::Int(i) => BuiltinValue::Int(i),
            BuiltinValue::String(s) =>
                BuiltinValue::String(arena.insert_slice(s)),
        }
    }
}

//
//  Implementation Details
//
impl<'a> std::fmt::Display for BuiltinValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            BuiltinValue::Int(i) => write!(f, "{:x}", i),
            BuiltinValue::String(s) => write!(f, "{}", com::Slice(s)),
        }
    }
}

impl std::fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            BuiltinFunction::Add => write!(f, "add"),
        }
    }
}
