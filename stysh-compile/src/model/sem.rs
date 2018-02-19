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

use std::{convert, fmt};

use basic::{com, mem};
use basic::mem::CloneInto;

use model::syn::{self, Range};

/// A registry of the definitions
pub trait Registry<'a> {
    /// Get the definition of the enum.
    fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'a>>;

    /// Get the definition of the record.
    fn lookup_record(&self, id: ItemIdentifier) -> Option<Record<'a>>;
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
    /// An uninhabited type.
    Void,
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
    /// Unique identifier of this value within the context;
    /// or at least, it is unique *after* the GVN pass.
    pub gvn: Gvn,
}

/// A binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Binding<'a> {
    /// A function argument.
    Argument(ValueIdentifier, Gvn, Type<'a>, com::Range),
    /// A variable declaration.
    Variable(Pattern<'a>, Value<'a>, com::Range),
}

/// A re-binding.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ReBinding<'a> {
    /// The left-hand side value.
    pub left: Value<'a>,
    /// The right-hand side value.
    pub right: Value<'a>,
    /// The range of the re-binding statement.
    pub range: com::Range,
}

/// A return statement.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Return<'a> {
    /// The returned value.
    pub value: Value<'a>,
    /// The range of the return statement.
    pub range: com::Range,
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

/// A Pattern
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Pattern<'a> {
    /// A constructor.
    Constructor(Constructor<'a, Pattern<'a>>),
    /// An ignored binding, '_'.
    Ignored(com::Range),
    /// A tuple.
    Tuple(Tuple<'a, Pattern<'a>>, com::Range),
    /// A variable.
    Var(ValueIdentifier, Gvn),
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
    /// A return statement.
    Return(Return<'a>),
    /// A variable re-binding.
    Set(ReBinding<'a>),
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

/// A constructor.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Constructor<'a, T: 'a> {
    /// The type.
    pub type_: RecordProto,
    /// The arguments.
    pub arguments: &'a [T],
    /// The range.
    pub range: com::Range,
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

/// A full-fledged item.
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

/// A global value number.
///
/// Defaults to 0, which is considered an invalid value.
#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Gvn(pub u32);

impl Expr<'static> {
    /// Returns a builtin Bool expr.
    pub fn bool_(b: bool) -> Self { Expr::BuiltinVal(BuiltinValue::Bool(b)) }

    /// Returns a builtin Int expr.
    pub fn int(i: i64) -> Self { Expr::BuiltinVal(BuiltinValue::Int(i)) }
}

impl<'a> Stmt<'a> {
    /// Range spanned by the statement.
    pub fn range(&self) -> com::Range {
        use self::Stmt::*;

        match *self {
            Return(r) => r.range(),
            Set(r) => r.range(),
            Var(b) => b.range(),
        }
    }

    /// Result type of the statement.
    ///
    /// Not to be mistaken for the type of the value assigned to or returned.
    pub fn result_type(&self) -> Type<'static> {
        use self::Stmt::*;

        match *self {
            Return(_) => Type::void(),
            Set(_) | Var(_) => Type::unit(),
        }
    }
}

impl<'a> Type<'a> {
    /// Returns the range this type would cover if it was anchored at 0.
    pub fn range(&self) -> com::Range {
        use self::Type::*;
        use self::BuiltinType::*;

        fn len(i: ItemIdentifier) -> usize { i.range().length() }

        let len = match *self {
            Builtin(Bool) => 4,
            Builtin(Int) => 3,
            Builtin(String) => 6,
            Builtin(Void) => 4,
            Enum(p) => len(p.name),
            Rec(r) => len(r.name)
                + if len(r.enum_) > 0 { 2 + len(r.enum_) } else { 0 },
            Tuple(t) => t.fields.iter().map(|t| t.range().length()).sum(),
            Unresolved(i) => len(i),
        };

        com::Range::new(0, len)
    }
}

impl Type<'static> {
    /// Returns a Bool type.
    pub fn bool_() -> Self { Type::Builtin(BuiltinType::Bool) }

    /// Returns an Int type.
    pub fn int() -> Self { Type::Builtin(BuiltinType::Int) }

    /// Returns a String type.
    pub fn string() -> Self { Type::Builtin(BuiltinType::String) }

    /// Returns a Void type.
    pub fn void() -> Self { Type::Builtin(BuiltinType::Void) }

    /// Returns a unit type.
    pub fn unit() -> Self { Type::Tuple(Tuple::unit()) }

    /// Returns an unresolved type.
    pub fn unresolved() -> Self {
        Type::Unresolved(ItemIdentifier::unresolved())
    }
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

impl<'a> Binding<'a> {
    /// Range spanned by the binding.
    pub fn range(&self) -> com::Range {
        use self::Binding::*;

        match *self {
            Argument(_, _, _, r) | Variable(_, _, r) => r,
        }
    }

    /// Sets the gvn of the binding.
    pub fn with_gvn<G: convert::Into<Gvn>>(self, gvn: G) -> Binding<'a> {
        use self::Binding::*;

        match self {
            Argument(id, _, t, r) => Argument(id, gvn.into(), t, r),
            Variable(..) => panic!("Cannot specify GVN in {:?}", self),
        }
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

impl<'a> Pattern<'a> {
    /// Returns the range spanned by the pattern.
    pub fn range(&self) -> com::Range {
        use self::Pattern::*;

        match *self {
            Constructor(c) => c.range(),
            Ignored(r) => r,
            Tuple(_, r) => r,
            Var(v, _) => v.0,
        }
    }

    /// Sets the gvn of the pattern.
    ///
    /// Panics: if the pattern is not a Var.
    pub fn with_gvn<G: convert::Into<Gvn>>(self, gvn: G) -> Pattern<'a> {
        use self::Pattern::*;

        match self {
            Var(v, _) => Var(v, gvn.into()),
            _ => panic!("Cannot specify GVN in {:?}", self),
        }
    }
}

impl<'a, T: 'a> Constructor<'a, T> {
    /// Returns the range spanned by the constructor.
    pub fn range(&self) -> com::Range { self.range }
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

impl<'a> ReBinding<'a> {
    /// Range spanned by the re-binding.
    pub fn range(&self) -> com::Range { self.range }
}

impl<'a> Return<'a> {
    /// Range spanned by the return statement.
    pub fn range(&self) -> com::Range { self.range }
}

impl<T: 'static> Tuple<'static, T> {
    /// Returns a unit tuple.
    pub fn unit() -> Self { Tuple { fields: &[] } }
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
    /// Returns the range spanned by the ItemIdentifier.
    pub fn range(&self) -> com::Range { self.0 }

    /// Returns a sentinel instance of ItemIdentifier.
    pub fn unresolved() -> ItemIdentifier {
        ItemIdentifier(com::Range::new(0, 0))
    }
}

impl ValueIdentifier {
    /// Returns the range spanned by the ValueIdentifier.
    pub fn range(&self) -> com::Range { self.0 }

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
            gvn: Default::default(),
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

impl<'a, 'target> CloneInto<'target> for Pattern<'a> {
    type Output = Pattern<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Pattern::*;

        match *self {
            Constructor(c) => Constructor(arena.intern(&c)),
            Ignored(r) => Ignored(r),
            Tuple(t, r) => Tuple(arena.intern(&t), r),
            Var(v, gvn) => Var(v, gvn),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Binding<'a> {
    type Output = Binding<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Binding::*;

        match *self {
            Argument(id, gvn, type_, range)
                => Argument(id, gvn, arena.intern(&type_), range),
            Variable(pat, value, range)
                => Variable(arena.intern(&pat), arena.intern(&value), range),
        }
    }
}

impl<'a, 'target> CloneInto<'target> for ReBinding<'a> {
    type Output = ReBinding<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        ReBinding {
            left: arena.intern(&self.left),
            right: arena.intern(&self.right),
            range: self.range,
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Return<'a> {
    type Output = Return<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Return {
            value: arena.intern(&self.value),
            range: self.range,
        }
    }
}

impl<'a, 'target> CloneInto<'target> for Stmt<'a> {
    type Output = Stmt<'target>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        use self::Stmt::*;

        match *self {
            Return(r) => Return(arena.intern(&r)),
            Set(b) => Set(arena.intern(&b)),
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

impl<'a, 'target, T> CloneInto<'target> for Constructor<'a, T>
    where T: CloneInto<'target> + Copy + 'a
{
    type Output = Constructor<'target, <T as CloneInto<'target>>::Output>;

    fn clone_into(&self, arena: &'target mem::Arena) -> Self::Output {
        Constructor {
            type_: self.type_,
            arguments: CloneInto::clone_into(self.arguments, arena),
            range: self.range,
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

impl convert::From<u32> for Gvn {
    fn from(v: u32) -> Gvn { Gvn(v) }
}

impl convert::From<syn::VariableIdentifier> for ValueIdentifier {
    fn from(value: syn::VariableIdentifier) -> Self {
        ValueIdentifier(value.range())
    }
}

impl convert::From<syn::TypeIdentifier> for ItemIdentifier {
    fn from(value: syn::TypeIdentifier) -> Self {
        ItemIdentifier(value.range())
    }
}

impl<'a> convert::From<Constructor<'a, Value<'a>>> for Expr<'a> {
    fn from(c: Constructor<'a, Value<'a>>) -> Self { Expr::Constructor(c) }
}

impl<'a> convert::From<Tuple<'a, Value<'a>>> for Expr<'a> {
    fn from(t: Tuple<'a, Value<'a>>) -> Self { Expr::Tuple(t) }
}

impl<'a> convert::From<Enum<'a>> for Item<'a> {
    fn from(e: Enum<'a>) -> Self { Item::Enum(e) }
}

impl<'a> convert::From<Function<'a>> for Item<'a> {
    fn from(f: Function<'a>) -> Self { Item::Fun(f) }
}

impl<'a> convert::From<Record<'a>> for Item<'a> {
    fn from(r: Record<'a>) -> Self { Item::Rec(r) }
}

impl<'a> convert::From<Constructor<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(c: Constructor<'a, Pattern<'a>>) -> Self {
        Pattern::Constructor(c)
    }
}

impl<'a> convert::From<Tuple<'a, Pattern<'a>>> for Pattern<'a> {
    fn from(t: Tuple<'a, Pattern<'a>>) -> Self {
        let f = &t.fields;
        let off = f.first().map(|p| p.range().offset() - 1).unwrap_or(0);
        let end = f.last().map(|p| p.range().end_offset() + 1).unwrap_or(0);

        Pattern::Tuple(t, com::Range::new(off, end - off))
    }
}

impl convert::From<EnumProto> for Prototype<'static> {
    fn from(e: EnumProto) -> Self { Prototype::Enum(e) }
}

impl<'a> convert::From<FunctionProto<'a>> for Prototype<'a> {
    fn from(f: FunctionProto<'a>) -> Self { Prototype::Fun(f) }
}

impl convert::From<RecordProto> for Prototype<'static> {
    fn from(r: RecordProto) -> Self { Prototype::Rec(r) }
}

impl<'a> convert::From<ReBinding<'a>> for Stmt<'a> {
    fn from(r: ReBinding<'a>) -> Self { Stmt::Set(r) }
}

impl<'a> convert::From<Binding<'a>> for Stmt<'a> {
    fn from(b: Binding<'a>) -> Self { Stmt::Var(b) }
}

impl convert::From<EnumProto> for Type<'static> {
    fn from(e: EnumProto) -> Self { Type::Enum(e) }
}

impl convert::From<RecordProto> for Type<'static> {
    fn from(r: RecordProto) -> Self { Type::Rec(r) }
}

impl<'a> convert::From<Tuple<'a, Type<'a>>> for Type<'a> {
    fn from(t: Tuple<'a, Type<'a>>) -> Self { Type::Tuple(t) }
}

impl<'a> convert::From<Constructor<'a, Value<'a>>> for Value<'a> {
    fn from(c: Constructor<'a, Value<'a>>) -> Self {
        Value {
            type_: Type::Rec(c.type_),
            range: c.range(),
            expr: Expr::Constructor(c),
            gvn: Default::default(),
        }
    }
}


//
//  Implementation Details
//
impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

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

impl<'a> fmt::Display for ItemIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "<{}>", self.0)
    }
}

impl<'a, T> fmt::Display for Tuple<'a, T>
    where
        T: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "(")?;
        for (i, e) in self.fields.iter().enumerate() {
            if i != 0 { write!(f, ", ")? }
            write!(f, "{}", e)?;
        }
        write!(f, ")")
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
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
    use super::{Enum, ItemIdentifier, Record, Registry};

    /// A mock for the Regitry trait.
    #[derive(Debug)]
    pub struct MockRegistry<'g> {
        /// Map of enums to be returned from lookup_enum.
        pub enums: mem::ArrayMap<'g, ItemIdentifier, Enum<'g>>,
        /// Map of records to be returned from lookup_record.
        pub records: mem::ArrayMap<'g, ItemIdentifier, Record<'g>>,
    }

    impl<'g> MockRegistry<'g> {
        /// Creates a new instance of MockRegistry.
        pub fn new(arena: &'g mem::Arena) -> MockRegistry<'g> {
            MockRegistry { 
                enums: mem::ArrayMap::new(arena),
                records: mem::ArrayMap::new(arena),
            }
        }
    }

    impl<'g> Registry<'g> for MockRegistry<'g> {
        fn lookup_enum(&self, id: ItemIdentifier) -> Option<Enum<'g>> {
            self.enums.get(&id).cloned()
        }

        fn lookup_record(&self, id: ItemIdentifier) -> Option<Record<'g>> {
            self.records.get(&id).cloned()
        }
    }
}
