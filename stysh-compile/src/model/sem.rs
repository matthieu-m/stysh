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

use model::syn;

/// A registry of the definitions
pub trait Registry<'a> {
    /// Get the definition of the enum.
    fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'a>>;
}

/// A Type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Type<'a> {
    /// A built-in type.
    Builtin(BuiltinType),
    /// An enum type.
    Enum(EnumProto),
    /// A record type.
    Rec(RecordProto),
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
    /// An implicit cast (variant to enum, anonymous to named, ...).
    Implicit(Implicit<'a>),
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
    /// A record constructor.
    ConstructorRec(RecordProto),
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

/// An Implicit cast.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Implicit<'a> {
    /// An enumerator to enum cast.
    ToEnum(EnumProto, &'a Value<'a>),
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

/// A tuple.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Tuple<'a, T: 'a> {
    /// The tuple fields.
    pub fields: &'a [T],
}

/// An annotated prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Prototype<'a> {
    /// An enum prototype.
    Enum(EnumProto),
    /// A function prototype.
    Fun(FunctionProto<'a>),
    /// A record prototype.
    Rec(RecordProto),
}

/// An enum prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EnumProto {
    /// The enum identifier.
    pub name: ItemIdentifier,
    /// The enum range.
    pub range: com::Range,
}

/// A record prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RecordProto {
    /// The record identifier.
    pub name: ItemIdentifier,
    /// The record range.
    pub range: com::Range,
    /// The enum this record is a part of, or undefined.
    pub enum_: ItemIdentifier,
}

/// An enum.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Enum<'a> {
    /// The prototype.
    pub prototype: &'a EnumProto,
    /// The variants.
    pub variants: &'a [Record<'a>],
}

/// A record.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Record<'a> {
    /// The prototype.
    pub prototype: &'a RecordProto,
    /// The fields.
    pub fields: &'a [Type<'a>],
}

/// A function prototype.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionProto<'a> {
    /// The function identifier.
    pub name: ItemIdentifier,
    /// The function prototype range.
    pub range: com::Range,
    /// The function arguments (always arguments).
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
    /// A full-fledged enum definition.
    Enum(Enum<'a>),
    /// A full-fledged function definition.
    Fun(Function<'a>),
    /// A full-fledged record definition.
    Rec(Record<'a>),
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
            ConstructorRec(rec) => Type::Rec(rec),
            Function(fun) => fun.result,
            Unknown(_) | Unresolved(_) => Type::unresolved(),
        }
    }
}

impl<'a> Prototype<'a> {
    /// Returns the range spanned by the prototype.
    pub fn range(&self) -> com::Range {
        use self::Prototype::*;

        match *self {
            Enum(e) => e.range,
            Fun(fun) => fun.range,
            Rec(r) => r.range,
        }
    }
}

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
        use self::Type::*;

        match *self {
            Builtin(t) => Builtin(t),
            Enum(e) => Enum(e),
            Rec(r) => Rec(r),
            Tuple(t) => Tuple(arena.intern(&t)),
            Unresolved(n) => Unresolved(n),
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
        use self::Expr::*;

        match *self {
            ArgumentRef(v) => ArgumentRef(v),
            Block(stmts, v) => Block(
                CloneInto::clone_into(stmts, arena),
                arena.intern_ref(v),
            ),
            BuiltinVal(v) => BuiltinVal(arena.intern(&v)),
            Call(c, args) => Call(
                arena.intern(&c),
                CloneInto::clone_into(args, arena),
            ),
            If(c, t, f) => If(
                arena.intern_ref(c),
                arena.intern_ref(t),
                arena.intern_ref(f),
            ),
            Implicit(i) => Implicit(arena.intern(&i)),
            Tuple(t) => Tuple(arena.intern(&t)),
            VariableRef(v) => VariableRef(v),
            _ => panic!("not yet implement for {:?}", self),
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

impl<'a, 'target> CloneInto<'target> for Stmt<'a> {
    type Output = Stmt<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Stmt::*;

        match *self {
            Var(b) => Var(arena.intern(&b)),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Callable<'a> {
    type Output = Callable<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Callable::*;

        match *self {
            Builtin(f) => Builtin(f),
            ConstructorRec(r) => ConstructorRec(r),
            Function(ref f) => Function(arena.intern(f)),
            Unknown(v) => Unknown(v),
            Unresolved(c) => Unresolved(CloneInto::clone_into(c, arena)),
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

impl<'a, 'target> CloneInto<'target> for Prototype<'a> {
    type Output = Prototype<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Prototype::*;

        match *self {
            Enum(e) => Enum(e),
            Rec(r) => Rec(r),
            Fun(fun) => Fun(arena.intern(&fun)),
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

impl<'target> CloneInto<'target> for EnumProto {
    type Output = EnumProto;

    fn clone_into(&self, _: &'target mem::Arena) -> Self::Output {
        *self
    }
}

impl<'a, 'target> CloneInto<'target> for Enum<'a> {
    type Output = Enum<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Enum {
            prototype: arena.intern_ref(self.prototype),
            variants: CloneInto::clone_into(self.variants, arena),
        }
    }
}

impl<'target> CloneInto<'target> for RecordProto {
    type Output = RecordProto;

    fn clone_into(&self, _: &'target mem::Arena) -> Self::Output {
        *self
    }
}

impl<'a, 'target> CloneInto<'target> for Record<'a> {
    type Output = Record<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Record {
            prototype: arena.intern_ref(self.prototype),
            fields: CloneInto::clone_into(self.fields, arena),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Function<'a> {
    type Output = Function<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Function {
            prototype: arena.intern_ref(self.prototype),
            body: arena.intern(&self.body),
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
        use self::BuiltinValue::*;

        match *self {
            Bool(b) => Bool(b),
            Int(i) => Int(i),
            String(s) => String(arena.insert_slice(s)),
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

impl std::convert::From<syn::VariableIdentifier> for ValueIdentifier {
    fn from(value: syn::VariableIdentifier) -> ValueIdentifier {
        ValueIdentifier(value.range())
    }
}

impl std::convert::From<syn::TypeIdentifier> for ItemIdentifier {
    fn from(value: syn::TypeIdentifier) -> ItemIdentifier {
        ItemIdentifier(value.range())
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

impl<'a> std::fmt::Display for Callable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use self::Callable::*;

        match *self {
            Builtin(b) => write!(f, "{}", b),
            ConstructorRec(rec) => write!(f, "{}", rec.name),
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

impl<'a> std::fmt::Display for ItemIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "<{}>", self.0)
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
            Type::Enum(e) => write!(f, "{}", e.name),
            Type::Rec(r) => write!(f, "{}", r.name),
            Type::Tuple(t) => write!(f, "{}", t),
            Type::Unresolved(i) => write!(f, "{}", i),
        }
    }
}

/// Mocks for the traits.
pub mod mocks {
    use basic::mem;
    use super::{Enum, ItemIdentifier, Registry};

    /// A mock for the Regitry trait.
    #[derive(Debug)]
    pub struct MockRegistry<'g> {
        /// Map of enums to be returned from lookup_enum.
        pub enums: mem::ArrayMap<'g, ItemIdentifier, Enum<'g>>,
    }

    impl<'g> MockRegistry<'g> {
        /// Creates a new instance of MockRegistry.
        pub fn new(arena: &'g mem::Arena) -> MockRegistry<'g> {
            MockRegistry { 
                enums: mem::ArrayMap::new(arena)
            }
        }
    }

    impl<'g> Registry<'g> for MockRegistry<'g> {
        fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'g>> {
            self.enums.get(&id).cloned()
        }
    }
}
