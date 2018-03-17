//! Builder for the semantic model (aka AST).

use std::{self, convert, marker};

use basic::{com, mem};
use basic::com::Span;

use model::hir::*;

//
//  High-Level Builders
//

/// Factory
#[derive(Clone, Copy)]
pub struct Factory<'a> {
    arena: &'a mem::Arena,
}

/// ItemFactory
#[derive(Clone, Copy)]
pub struct ItemFactory<'a> {
    arena: &'a mem::Arena,
}

/// PatternFactory
#[derive(Clone, Copy)]
pub struct PatternFactory<'a> {
    arena: &'a mem::Arena,
}

/// PrototypeFactory
#[derive(Clone, Copy)]
pub struct PrototypeFactory<'a> {
    arena: &'a mem::Arena,
}

/// StmtFactory
#[derive(Clone, Copy)]
pub struct StmtFactory<'a>(marker::PhantomData<&'a ()>);

/// TypeFactory
#[derive(Clone, Copy)]
pub struct TypeFactory<'a> {
    arena: &'a mem::Arena,
}

/// ValueFactory
#[derive(Clone, Copy)]
pub struct ValueFactory<'a> {
    arena: &'a mem::Arena,
}

//
//  Item Builders
//

/// EnumBuilder
#[derive(Clone)]
pub struct EnumBuilder<'a> {
    prototype: &'a EnumProto,
    variants: mem::Array<'a, Record<'a>>,
}

/// RecordBuilder
#[derive(Clone)]
pub struct RecordBuilder<'a> {
    prototype: &'a RecordProto,
    definition: TupleBuilder<'a, Type<'a>>,
}

//
//  Pattern Builders
//

//
//  Prototype Builders
//
#[derive(Clone)]
pub struct FunctionProtoBuilder<'a> {
    name: ItemIdentifier,
    range: com::Range,
    arguments: mem::Array<'a, Binding<'a>>,
    result: Type<'a>,
    with_gvn: bool,
}

//
//  Statement Builders
//

//
//  Type Builders
//
#[derive(Clone, Copy)]
pub struct BuiltinTypeBuilder;

//
//  Value Builders
//
#[derive(Clone)]
pub struct BlockBuilder<'a> {
    value: Option<&'a Value<'a>>,
    statements: mem::Array<'a, Stmt<'a>>,
    range: com::Range,
}

#[derive(Clone, Copy)]
pub struct BuiltinValueBuilder;

#[derive(Clone)]
pub struct CallBuilder<'a> {
    callable: Callable<'a>,
    unresolved: mem::Array<'a, Callable<'a>>,
    arguments: mem::Array<'a, Value<'a>>,
}

#[derive(Clone, Copy)]
pub struct FieldAccessBuilder<'a> {
    index: u16,
    type_: Option<Type<'a>>,
    value: &'a Value<'a>,
}

#[derive(Clone, Copy)]
pub struct IfBuilder<'a> {
    type_: Option<Type<'a>>,
    condition: &'a Value<'a>,
    true_: &'a Value<'a>,
    false_: &'a Value<'a>,
}

#[derive(Clone, Copy)]
pub struct ImplicitBuilder<'a> {
    arena: &'a mem::Arena,
}

#[derive(Clone)]
pub struct LoopBuilder<'a>{
    statements: mem::Array<'a, Stmt<'a>>,
    range: com::Range,
}

#[derive(Clone)]
pub struct ValueTupleBuilder<'a> {
    type_: TupleBuilder<'a, Type<'a>>,
    expr: TupleBuilder<'a, Value<'a>>,
}

//
//  Low-Level Builders
//

#[derive(Clone)]
pub struct ConstructorBuilder<'a, T: 'a> {
    record: RecordProto,
    arguments: mem::Array<'a, T>,
    range: com::Range,
}

#[derive(Clone, Copy)]
pub struct EnumProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
}

#[derive(Clone, Copy)]
pub struct RecordProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
    enum_: ItemIdentifier,
}

/// TupleBuilder
#[derive(Clone)]
pub struct TupleBuilder<'a, T: 'a> {
    fields: mem::Array<'a, T>,
}

//
//  Implementation of Factory
//
impl<'a> Factory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        Factory { arena: arena }
    }

    /// Creates a ItemFactory.
    pub fn item(&self) -> ItemFactory<'a> { ItemFactory::new(self.arena) }

    /// Creates a PatternFactory.
    pub fn pat(&self) -> PatternFactory<'a> { PatternFactory::new(self.arena) }

    /// Creates a PrototypeFactory.
    pub fn proto(&self) -> PrototypeFactory<'a> {
        PrototypeFactory::new(self.arena)
    }

    /// Creates a StmtFactory.
    pub fn stmt(&self) -> StmtFactory<'a> { StmtFactory::new(self.arena) }

    /// Creates an TypeFactory.
    pub fn type_(&self) -> TypeFactory<'a> { TypeFactory::new(self.arena) }

    /// Creates a ValueFactory.
    pub fn value(&self) -> ValueFactory<'a> { ValueFactory::new(self.arena) }
}

//
//  Implementation Details (Item)
//
impl<'a> ItemFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self { ItemFactory { arena } }

    /// Creates an ItemIdentifier.
    pub fn id(&self, pos: usize, len: usize) -> ItemIdentifier {
        ItemIdentifier(Default::default(), range(pos, len))
    }

    /// Creates an EnumBuilder.
    pub fn enum_(&self, p: EnumProto) -> EnumBuilder<'a> {
        EnumBuilder::new(self.arena, p)
    }

    /// Creates a Function.
    pub fn fun(&self, p: FunctionProto<'a>, body: Value<'a>) -> Function<'a> {
        Function {
            prototype: self.arena.insert(p),
            body: body,
        }
    }

    /// Creates a RecordBuilder.
    pub fn rec(&self, r: RecordProto) -> RecordBuilder<'a> {
        RecordBuilder::new(self.arena, r)
    }

    /// Shortcut: Creates a Unit Record.
    pub fn unit(&self, pos: usize, len: usize) -> Record<'a> {
        let proto = RecordProtoBuilder::new(self.id(pos, len), pos).build();
        self.rec(proto).build()
    }
}

impl<'a> EnumBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena, p: EnumProto) -> Self {
        EnumBuilder {
            prototype: arena.insert(p),
            variants: mem::Array::new(arena),
        }
    }

    /// Pushes a variant.
    pub fn push(&mut self, r: Record<'a>) -> &mut Self {
        let proto = self.variants.arena().insert(RecordProto {
            enum_: self.prototype.name,
            ..*r.prototype
        });
        self.variants.push(Record {
            prototype: proto,
            definition: r.definition,
        });
        self
    }

    /// Creates an Enum.
    pub fn build(&self) -> Enum<'a> {
        Enum {
            prototype: self.prototype,
            variants: self.variants.clone().into_slice(),
        }
    }
}

impl<'a> RecordBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena, p: RecordProto) -> Self {
        RecordBuilder {
            prototype: arena.insert(p),
            definition: TupleBuilder::new(arena),
        }
    }

    /// Pushes a field.
    pub fn push(&mut self, t: Type<'a>) -> &mut Self {
        self.definition.push(t);
        self
    }

    /// Creates an Record.
    pub fn build(&self) -> Record<'a> {
        Record {
            prototype: self.prototype,
            definition: self.definition.build(),
        }
    }
}

//
//  Implementation Details (Pattern)
//
impl<'a> PatternFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self { PatternFactory { arena } }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, record: RecordProto, pos: usize, len: usize)
        -> ConstructorBuilder<'a, Pattern<'a>>
    {
        ConstructorBuilder::new(self.arena, record, pos, len)
    }

    /// Creates an ignored Pattern.
    pub fn ignored(&self, pos: usize) -> Pattern<'static> {
        Pattern::Ignored(range(pos, 1))
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<'a, Pattern<'a>> {
        TupleBuilder::new(self.arena)
    }

    /// Creates a var Pattern.
    pub fn var(&self, id: ValueIdentifier) -> Pattern<'static> {
        Pattern::Var(id, Default::default())
    }
}

//
//  Implementation Details (Prototype)
//
impl<'a> PrototypeFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self { PrototypeFactory { arena } }

    /// Creates an EnumProtoBuilder.
    pub fn enum_(&self, name: ItemIdentifier) -> EnumProtoBuilder {
        let mut e = EnumProtoBuilder::new(name, 0);
        e.range(name.span().offset() - 6, name.span().length() + 6);
        e
    }

    /// Creates a FunctionProtoBuilder.
    pub fn fun(
        &self,
        name: ItemIdentifier,
        result: Type<'a>,
    )
        -> FunctionProtoBuilder<'a>
    {
        FunctionProtoBuilder::new(self.arena, name, result)
    }

    /// Creates a RecordProtoBuilder.
    pub fn rec(&self, name: ItemIdentifier, pos: usize) -> RecordProtoBuilder {
        let mut r = RecordProtoBuilder::new(name, pos);
        if pos != name.span().offset() {
            r.range(name.span().offset() - 5, name.span().length() + 5);
        }
        r
    }
}

impl<'a> FunctionProtoBuilder<'a> {
    /// Creates an instance.
    pub fn new(
        arena: &'a mem::Arena,
        name: ItemIdentifier,
        result: Type<'a>,
    )
        -> Self
    {
        FunctionProtoBuilder {
            name: name,
            range: range(0, 0),
            arguments: mem::Array::new(arena),
            result: result,
            with_gvn: false,
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, name: ValueIdentifier, type_: Type<'a>) -> &mut Self
    {
        let gvn = Default::default();
        let len = name.span().length() + 2 + type_.span().length();
        let range = range(name.span().offset(), len);
        self.arguments.push(Binding::Argument(name, gvn, type_, range));
        self
    }

    /// Sets the range of the last argument.
    pub fn arg_range(&mut self, pos: usize, len: usize) -> &mut Self {
        if let Some(&mut Binding::Argument(_, _, _, ref mut r))
            = self.arguments.last_mut() {
            *r = range(pos, len);
        }
        self
    }

    /// Toggle GVN generation.
    pub fn with_gvn(&mut self) -> &mut Self {
        self.with_gvn = true;
        self
    }

    /// Creates a FunctionProto.
    pub fn build(&self) -> FunctionProto<'a> {
        let arguments = self.arguments.clone().into_slice();

        for (i, a) in arguments.iter_mut().enumerate() {
            if let &mut Binding::Argument(_, ref mut g, _, ref mut r) = a {
                *g = Gvn(if self.with_gvn { i as u32 + 1 } else { 0 });

                if i + 1 == self.arguments.len() { continue; }
                *r = range(r.offset(), r.length() + 1);
            }
        }

        FunctionProto {
            name: self.name,
            range: self.range,
            arguments: arguments,
            result: self.result
        }
    }
}

//
//  Implementation Details (Stmt)
//
impl<'a> StmtFactory<'a> {
    /// Creates an instance.
    pub fn new(_: &'a mem::Arena) -> Self { StmtFactory(marker::PhantomData) }

    /// Creates a return Stmt.
    pub fn ret(&self, value: Value<'a>) -> Stmt<'a> {
        let off = value.range.offset() - 8;
        let end = value.range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Return(Return { value, range })
    }

    /// Shortcut: Creates an empty return statement.
    pub fn ret_unit(&self, pos: usize, len: usize) -> Stmt<'a> {
        let value = Value::unit().with_range(pos + len - 3, 2);
        let range = range(pos, len);

        Stmt::Return(Return { value, range })
    }

    /// Creates a re-binding Stmt.
    pub fn set(&self, left: Value<'a>, right: Value<'a>) -> Stmt<'a> {
        let off = left.range.offset() - 5;
        let end = right.range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Set(ReBinding { left, right, range })
    }

    /// Creates a binding Stmt.
    pub fn var(&self, pattern: Pattern<'a>, value: Value<'a>) -> Stmt<'a> {
        let off = pattern.span().offset() - 5;
        let end = value.range.end_offset() + 1;

        Stmt::Var(Binding::Variable(pattern, value, range(off, end - off)))
    }

    /// Shortcut: Creates a simple binding Stmt.
    pub fn var_id(&self, id: ValueIdentifier, value: Value<'a>) -> Stmt<'a> {
        self.var(Pattern::Var(id, Default::default()), value)
    }
}

//
//  Implementation Details (Type)
//
impl<'a> TypeFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self { TypeFactory { arena } }

    /// Creates a BuiltinTypeBuilder.
    pub fn builtin(&self) -> BuiltinTypeBuilder { BuiltinTypeBuilder::new() }

    /// Shortcut: creates a Bool Type.
    pub fn bool_(&self) -> Type<'a> { Type::Builtin(self.builtin().bool_()) }

    /// Shortcut: creates a Int Type.
    pub fn int(&self) -> Type<'a> { Type::Builtin(self.builtin().int()) }

    /// Shortcut: creates a String Type.
    pub fn string(&self) -> Type<'a> { Type::Builtin(self.builtin().string()) }

    /// Shortcut: creates a Void Type.
    pub fn void(&self) -> Type<'a> { Type::Builtin(self.builtin().void()) }

    /// Creates an EnumProtoBuilder.
    pub fn enum_(&self, name: ItemIdentifier, pos: usize)
        -> EnumProtoBuilder
    {
        EnumProtoBuilder::new(name, pos)
    }

    /// Creates a RecordProtoBuilder.
    pub fn record(&self, name: ItemIdentifier, pos: usize)
        -> RecordProtoBuilder
    {
        RecordProtoBuilder::new(name, pos)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<'a, Type<'a>> {
        TupleBuilder::new(self.arena)
    }

    /// Creates an unresolved type.
    pub fn unresolved(&self, id: ItemIdentifier) -> Type<'a> {
        Type::Unresolved(id)
    }
}

impl BuiltinTypeBuilder {
    /// Creates an instance.
    pub fn new() -> Self { BuiltinTypeBuilder }

    /// Creates a Bool BuiltinType.
    pub fn bool_(&self) -> BuiltinType { BuiltinType::Bool }

    /// Creates a Int BuiltinType.
    pub fn int(&self) -> BuiltinType { BuiltinType::Int }

    /// Creates a String BuiltinType.
    pub fn string(&self) -> BuiltinType { BuiltinType::String }

    /// Creates a Void BuiltinType.
    pub fn void(&self) -> BuiltinType { BuiltinType::Void }
}

//
//  Implementation Details (Value)
//
impl<'a> ValueFactory<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self { ValueFactory { arena } }

    /// Creates an ValueIdentifier.
    pub fn id(&self, pos: usize, len: usize) -> ValueIdentifier {
        ValueIdentifier(Default::default(), range(pos, len))
    }

    /// Creates an ArgumentRef Value.
    pub fn arg_ref(&self, type_: Type<'a>, name: ValueIdentifier, pos: usize)
        -> Value<'a>
    {
        value(type_, Expr::ArgumentRef(name, Default::default()))
            .with_range(pos, name.span().length())
    }

    /// Creates a BlockBuilder.
    pub fn block(&self, value: Value<'a>) -> BlockBuilder<'a> {
        BlockBuilder::new(self.arena, value)
    }

    /// Creates a diverging BlockBuilder.
    pub fn block_div(&self) -> BlockBuilder<'a> {
        BlockBuilder::diverging(self.arena)
    }

    /// Creates a BuiltinValueBuilder.
    pub fn builtin(&self) -> BuiltinValueBuilder { BuiltinValueBuilder::new() }

    /// Shortcut: creates a boolean Value.
    pub fn bool_(&self, b: bool, pos: usize) -> Value<'a> {
        value(
            Type::Builtin(BuiltinType::Bool),
            Expr::BuiltinVal(self.builtin().bool_(b))
        ).with_range(pos, if b { 4 } else { 5 })
    }

    /// Shortcut: creates an integral Value.
    pub fn int(&self, i: i64, pos: usize) -> Value<'a> {
        value(
            Type::Builtin(BuiltinType::Int),
            Expr::BuiltinVal(self.builtin().int(i))
        ).with_range(pos, count_characters(i))
    }

    /// Shortcut: creates a string Value.
    pub fn string(&self, s: &'static str, pos: usize) -> Value<'a> {
        value(
            Type::Builtin(BuiltinType::String),
            Expr::BuiltinVal(self.builtin().string(s))
        ).with_range(pos, s.len() + 2)
    }

    /// Creates a CallBuilder.
    pub fn call(&self) -> CallBuilder<'a> { CallBuilder::new(self.arena) }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, record: RecordProto, pos: usize, len: usize)
        -> ConstructorBuilder<'a, Value<'a>>
    {
        ConstructorBuilder::new(self.arena, record, pos, len)
    }

    /// Creates a FieldAccessBuilder.
    pub fn field_access(&self, index: u16, accessed: Value<'a>)
        -> FieldAccessBuilder<'a>
    {
        FieldAccessBuilder::new(self.arena, index, accessed)
    }

    /// Creates an IfBuilder.
    pub fn if_(&self, cond: Value<'a>, true_: Value<'a>, false_: Value<'a>)
        -> IfBuilder<'a>
    {
        IfBuilder::new(self.arena, cond, true_, false_)
    }

    /// Creates an ImplicitBuilder.
    pub fn implicit(&self) -> ImplicitBuilder<'a> {
        ImplicitBuilder::new(self.arena)
    }

    /// Creates a LoopBuilder.
    pub fn loop_(&self) -> LoopBuilder<'a> { LoopBuilder::new(self.arena) }

    /// Creates a ValueTupleBuilder.
    pub fn tuple(&self) -> ValueTupleBuilder<'a> {
        ValueTupleBuilder::new(self.arena)
    }

    /// Creates an unresolved field Value.
    pub fn unresolved_field(&self, id: ValueIdentifier, v: Value<'a>)
        -> Value<'a>
    {
        value(
            Type::unresolved(),
            Expr::UnresolvedField(self.arena.insert(v), id),
        )
    }

    /// Creates an unresolved ref Value.
    pub fn unresolved_ref(&self, name: ValueIdentifier) -> Value<'a> {
        value(Type::unresolved(), Expr::UnresolvedRef(name))
            .with_range(name.span().offset(), name.span().length())
    }

    /// Creates a variable ref Value.
    pub fn var_ref(&self, type_: Type<'a>, name: ValueIdentifier, pos: usize)
        -> Value<'a>
    {
        value(type_, Expr::VariableRef(name, Default::default()))
            .with_range(pos, name.span().length())
    }

    /// Shortcut: creates a Bool ref.
    pub fn bool_ref(&self, name: ValueIdentifier, pos: usize) -> Value<'a> {
        self.var_ref(Type::Builtin(BuiltinType::Bool), name, pos)
    }

    /// Shortcut: creates a Int ref.
    pub fn int_ref(&self, name: ValueIdentifier, pos: usize) -> Value<'a> {
        self.var_ref(Type::Builtin(BuiltinType::Int), name, pos)
    }

    /// Shortcut: creates a String ref.
    pub fn string_ref(&self, name: ValueIdentifier, pos: usize) -> Value<'a> {
        self.var_ref(Type::Builtin(BuiltinType::String), name, pos)
    }
}

impl<'a> BlockBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena, value: Value<'a>) -> Self {
        BlockBuilder {
            value: Some(arena.insert(value)),
            statements: mem::Array::new(arena),
            range: Default::default(),
        }
    }

    /// Creates an instance.
    pub fn diverging(arena: &'a mem::Arena) -> Self {
        BlockBuilder {
            value: None,
            statements: mem::Array::new(arena),
            range: Default::default(),
        }
    }

    /// Push a statement.
    pub fn push(&mut self, stmt: Stmt<'a>) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Sets a range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = com::Range::new(pos, len);
        self
    }

    /// Creates a Block Value.
    pub fn build(&self) -> Value<'a> {
        let (first, last) = (self.statements.first(), self.statements.last());
        let value_range = self.value.map(|v| v.range);

        let off = if self.range == Default::default() {
            first.map(|s| s.span().offset())
                .unwrap_or_else(|| value_range.unwrap().offset()) - 2
        } else {
            self.range.offset()
        };

        let end = if self.range == Default::default() {
            value_range.map(|v| v.end_offset())
                .unwrap_or_else(|| last.unwrap().span().end_offset()) + 2
        } else {
            self.range.end_offset()
        };

        let expr =
            Expr::Block(self.statements.clone().into_slice(), self.value);
        let type_ =
            self.value.map(|v| v.type_)
                .or_else(|| last.map(|s| s.result_type()))
                .unwrap_or(Type::unit());
        value(type_, expr).with_range(off, end - off)
    }
}

impl BuiltinValueBuilder {
    /// Creates an instance.
    pub fn new() -> Self { BuiltinValueBuilder }

    /// Creates a Bool.
    pub fn bool_(&self, value: bool) -> BuiltinValue<'static> {
        BuiltinValue::Bool(value)
    }

    /// Creates an Int.
    pub fn int(&self, value: i64) -> BuiltinValue<'static> {
        BuiltinValue::Int(value)
    }

    /// Creates a string.
    pub fn string(&self, value: &'static str) -> BuiltinValue<'static> {
        BuiltinValue::String(value.as_bytes())
    }
}

impl<'a> CallBuilder<'a> {
    /// Creates an instance, defaults to Add.
    pub fn new(arena: &'a mem::Arena) -> Self {
        CallBuilder {
            callable: Callable::Builtin(BuiltinFunction::Add),
            unresolved: mem::Array::new(arena),
            arguments: mem::Array::new(arena),
        }
    }

    /// Sets built-in function.
    pub fn builtin(&mut self, b: BuiltinFunction) -> &mut Self {
        self.callable = Callable::Builtin(b);
        self
    }

    /// Sets a user-defined function.
    pub fn function(&mut self, f: FunctionProto<'a>) -> &mut Self {
        self.callable = Callable::Function(f);
        self
    }

    /// Sets an unknown binding.
    pub fn unknown(&mut self, name: ValueIdentifier) -> &mut Self {
        self.callable = Callable::Unknown(name);
        self
    }

    /// Pushes an unresolved builtin.
    pub fn push_builtin(&mut self, b: BuiltinFunction) -> &mut Self {
        self.unresolved.push(Callable::Builtin(b));
        self
    }

    /// Pushes an unresolved user-defined function.
    pub fn push_function(&mut self, f: FunctionProto<'a>) -> &mut Self {
        self.unresolved.push(Callable::Function(f));
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, argument: Value<'a>) -> &mut Self {
        self.arguments.push(argument);
        self
    }

    /// Creates a Call Value.
    pub fn build(&self) -> Value<'a> {
        use self::Callable::*;

        let callable = if self.unresolved.len() == 0 {
            self.callable
        } else {
            Callable::Unresolved(self.unresolved.clone().into_slice())
        };

        let args = if self.arguments.len() == 0 {
            None
        } else {
            let off = self.arguments.first().unwrap().range.offset();
            let end = self.arguments.last().unwrap().range.end_offset();
            Some((off, end - off))
        };

        let (off, len) = args.map(|(off, len)| {
            match callable {
                Builtin(BuiltinFunction::Not) => (off - 5, len + 5),
                Builtin(_) => (off, len),
                Function(p) => {
                    let n = p.name.span().length();
                    (off - 1 - n, len + 2 + n)
                },
                Unknown(n) => {
                    let n = n.span().length();
                    (off - 1 - n, len + 2 + n)
                },
                Unresolved(_) => (0, 0),
            }
        }).unwrap_or((0, 0));

        value(
            callable.result_type(),
            Expr::Call(callable, self.arguments.clone().into_slice()),
        ).with_range(off, len)
    }
}

impl<'a> FieldAccessBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena, index: u16, value: Value<'a>) -> Self {
        FieldAccessBuilder {
            index: index,
            type_: None,
            value: arena.insert(value),
        }
    }

    /// Sets the type.
    pub fn type_(&mut self, type_: Type<'a>) -> &mut Self {
        self.type_ = Some(type_);
        self
    }

    /// Creates a field access Value.
    pub fn build(&self) -> Value<'a> {
        let i = self.index as i64;
        let len = self.value.range.length() + 1 + count_characters(i);

        let type_ = self.type_.unwrap_or_else(|| {
            let fields = if let Type::Tuple(t) = self.value.type_ {
                t.fields
            } else {
                &[]
            };
            fields.get(i as usize)
                .cloned()
                .unwrap_or(Type::Builtin(BuiltinType::Int))
        });

        value(type_, Expr::FieldAccess(self.value, self.index))
            .with_range(self.value.range.offset(), len)
    }
}

impl<'a> IfBuilder<'a> {
    /// Creates an instance.
    pub fn new(
        arena: &'a mem::Arena,
        condition: Value<'a>,
        true_: Value<'a>,
        false_: Value<'a>
    )
        -> Self
    {
        IfBuilder {
            type_: None,
            condition: arena.insert(condition),
            true_: arena.insert(true_),
            false_: arena.insert(false_),
        }
    }

    /// Sets a type.
    pub fn type_(&mut self, type_: Type<'a>) -> &mut Self {
        self.type_ = Some(type_);
        self
    }

    /// Creates an if-else Value.
    pub fn build(&self) -> Value<'a> {
        let off = self.condition.range.offset() - 4;
        let end = self.false_.range.end_offset();

        let true_ = self.true_.type_;
        let false_ = self.false_.type_;

        let type_ = self.type_.unwrap_or_else(|| {
            if true_ == false_ || false_ == Type::void() {
                true_
            } else if true_ == Type::void() {
                false_
            } else {
                Type::unresolved()
            }
        });

        value(
            type_,
            Expr::If(self.condition, self.true_, self.false_)
        ).with_range(off, end - off)
    }
}

impl<'a> ImplicitBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        ImplicitBuilder { arena }
    }

    /// Creates a to-enum implicit Value.
    pub fn enum_(&self, e: EnumProto, v: Value<'a>) -> Value<'a> {
        value(
            Type::Enum(e),
            Expr::Implicit(Implicit::ToEnum(e, self.arena.insert(v))),
        ).with_range(v.range.offset(), v.range.length())
    }
}

impl<'a> LoopBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        LoopBuilder {
            statements: mem::Array::new(arena),
            range: Default::default(),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, offset: usize, length: usize) -> &mut Self {
        self.range = com::Range::new(offset, length);
        self
    }

    /// Push a statement.
    pub fn push(&mut self, stmt: Stmt<'a>) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Creates a Loop Value.
    pub fn build(&self) -> Value<'a> {
        let (off, len) = if self.range == Default::default() {
            let first = self.statements.first().expect("statements");
            let last = self.statements.last().expect("statements");

            let offset = first.span().offset() - 8;
            let end_offset = last.span().end_offset() + 2;
            (offset, end_offset - offset)
        } else {
            (self.range.offset(), self.range.length())
        };

        let expr = Expr::Loop(self.statements.clone().into_slice());
        value(Type::Builtin(BuiltinType::Void), expr).with_range(off, len)
    }
}

impl<'a> ValueTupleBuilder<'a> {
    /// Creates an instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        ValueTupleBuilder {
            type_: TupleBuilder::new(arena),
            expr: TupleBuilder::new(arena),
        }
    }

    /// Push a value.
    pub fn push(&mut self, value: Value<'a>) -> &mut Self {
        self.type_.push(value.type_);
        self.expr.push(value);
        self
    }

    /// Creates a Tuple Value.
    pub fn build(&self) -> Value<'a> {
        let values = &self.expr.fields;
        let (off, len) = if values.len() == 0 {
            (0, 0)
        } else {
            let off = values.first().unwrap().range.offset() - 1;
            let end = values.last().unwrap().range.end_offset() + 1;
            (off, end - off)
        };
        value(self.type_.build(), self.expr.build()).with_range(off, len)
    }
}

//
//  Implementations of Low-Level builders
//
impl EnumProtoBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier, pos: usize) -> Self {
        EnumProtoBuilder {
            name: name,
            range: range(pos, name.span().length()),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Creates a Record.
    pub fn build<U: convert::From<EnumProto>>(&self) -> U {
        EnumProto {
            name: self.name,
            range: self.range,
        }.into()
    }
}

impl RecordProtoBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier, pos: usize) -> Self {
        RecordProtoBuilder {
            name: name,
            range: range(pos, name.span().length()),
            enum_: ItemIdentifier::unresolved(),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Sets an enum.
    pub fn enum_(&mut self, name: ItemIdentifier) -> &mut Self {
        self.enum_ = name;
        self
    }

    /// Creates a Record.
    pub fn build<U: convert::From<RecordProto>>(&self) -> U {
        RecordProto {
            name: self.name,
            range: self.range,
            enum_: self.enum_,
        }.into()
    }
}

impl<'a, T> ConstructorBuilder<'a, T> {
    /// Creates an instance.
    pub fn new(
        arena: &'a mem::Arena,
        record: RecordProto,
        pos: usize,
        len: usize
    )
        -> Self
    {
        ConstructorBuilder {
            record: record,
            arguments: mem::Array::new(arena),
            range: range(pos, len),
        }
    }

    /// Push an argument.
    pub fn push(&mut self, argument: T) -> &mut Self {
        self.arguments.push(argument);
        self
    }

}

impl<'a, T: 'a + Clone> ConstructorBuilder<'a, T> {
    /// Creates a Constructor.
    pub fn build<U: convert::From<Constructor<'a, T>>>(&self) -> U {
        Constructor {
            type_: self.record,
            arguments: self.arguments.clone().into_slice(),
            range: self.range,
        }.into()
    }
}

impl<'a> ConstructorBuilder<'a, Value<'a>> {
    /// Shortcut: creates a Value.
    pub fn build_value(&self) -> Value<'a> {
        self.build::<Value<'a>>()
    }
}

impl<'a, T: 'a> TupleBuilder<'a, T> {
    /// Creates a new instance.
    pub fn new(arena: &'a mem::Arena) -> Self {
        TupleBuilder { fields: mem::Array::new(arena) }
    }

    /// Appends a field.
    pub fn push(&mut self, field: T) -> &mut Self {
        self.fields.push(field);
        self
    }
}

impl<'a, T: 'a + Clone> TupleBuilder<'a, T> {
    /// Creates a new Tuple instance.
    pub fn build<U: convert::From<Tuple<'a, T>>>(&self) -> U {
        Tuple {
            fields: self.fields.clone().into_slice(),
        }.into()
    }
}

//
//  Implementation Details
//
fn count_characters(i: i64) -> usize {
    if i == std::i64::MIN { 20 }
    else if i < 0 { 1 + count_characters(i * -1) }
    else if i < 10 { 1 }
    else { 1 + count_characters(i / 10) }
}

fn range(pos: usize, len: usize) -> com::Range { com::Range::new(pos, len) }

fn value<'a>(type_: Type<'a>, expr: Expr<'a>) -> Value<'a> {
    Value {
        type_: type_,
        range: range(0, 0),
        expr: expr,
        gvn: Default::default(),
    }
}
