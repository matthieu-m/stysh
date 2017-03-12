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

use basic::com;

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type {
    /// A built-in type.
    Builtin(BuiltinType),
}

/// A built-in Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinType {
    /// A 64-bits signed integer.
    Int,
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

/// An Expression.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr<'a> {
    /// A built-in value.
    BuiltinVal(BuiltinValue),
    /// A built-in function call.
    BuiltinCall(BuiltinFunction, &'a [Value<'a>]),
}

/// A built-in value, the type is implicit.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinValue {
    /// An integral.
    Int(i64),
}

/// A built-in function.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum BuiltinFunction {
    /// An addition.
    Add,
}

//
//  Implementation Details
//
impl std::fmt::Display for BuiltinValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            BuiltinValue::Int(i) => write!(f, "{:x}", i),
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
