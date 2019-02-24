//! Builder for the semantic model (aka AST).

use std::{self, convert};

use basic::com;
use basic::com::Span;
use basic::mem::{DynArray, Ptr};

use model::hir::*;

//
//  High-Level Builders
//

/// Factory
#[derive(Clone, Copy, Debug)]
pub struct Factory;

/// ItemFactory
#[derive(Clone, Copy, Debug)]
pub struct ItemFactory;

/// PatternFactory
#[derive(Clone, Copy, Debug)]
pub struct PatternFactory;

/// PrototypeFactory
#[derive(Clone, Copy, Debug)]
pub struct PrototypeFactory;

/// StmtFactory
#[derive(Clone, Copy, Debug)]
pub struct StmtFactory;

/// TypeFactory
#[derive(Clone, Copy, Debug)]
pub struct TypeFactory;

/// ValueFactory
#[derive(Clone, Copy, Debug)]
pub struct ValueFactory;

//
//  Item Builders
//

/// EnumBuilder
#[derive(Clone, Debug)]
pub struct EnumBuilder {
    prototype: EnumProto,
    variants: DynArray<Record>,
}

/// RecordBuilder
#[derive(Clone, Debug)]
pub struct RecordBuilder {
    prototype: RecordProto,
    definition: TupleBuilder<Type>,
}

//
//  Pattern Builders
//

//
//  Prototype Builders
//
#[derive(Clone, Debug)]
pub struct FunctionProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
    arguments: DynArray<Argument>,
    result: Type,
    with_gvn: bool,
}

//
//  Statement Builders
//

//
//  Type Builders
//
#[derive(Clone, Copy, Debug)]
pub struct BuiltinTypeBuilder;

#[derive(Clone, Debug)]
pub struct TypeEnumBuilder {
    enum_: Enum,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeRecordBuilder {
    record: Record,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeUnresolvedBuilder {
    name: ItemIdentifier,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeUnresolvedEnumBuilder {
    proto: EnumProtoBuilder,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeUnresolvedRecordBuilder {
    proto: RecordProtoBuilder,
    path: PathBuilder,
}

//
//  Value Builders
//
#[derive(Clone, Debug)]
pub struct BlockBuilder {
    value: Option<Value>,
    statements: DynArray<Stmt>,
    range: com::Range,
}

#[derive(Clone, Copy, Debug)]
pub struct BuiltinValueBuilder;

#[derive(Clone, Debug)]
pub struct CallBuilder {
    callable: Callable,
    unresolved: DynArray<Callable>,
    arguments: DynArray<Value>,
}

#[derive(Clone, Debug)]
pub struct FieldAccessBuilder {
    field: Field,
    type_: Option<Type>,
    value: Value,
}

#[derive(Clone, Debug)]
pub struct IfBuilder {
    type_: Option<Type>,
    condition: Value,
    true_: Value,
    false_: Value,
}

#[derive(Clone, Copy, Debug)]
pub struct ImplicitBuilder;

#[derive(Clone, Debug)]
pub struct LoopBuilder {
    statements: DynArray<Stmt>,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct ValueTupleBuilder {
    type_: TupleBuilder<Type>,
    expr: TupleBuilder<Value>,
}

//
//  Low-Level Builders
//

#[derive(Clone, Debug)]
pub struct ConstructorBuilder<T> {
    type_: Type,
    arguments: TupleBuilder<T>,
    range: com::Range,
}

#[derive(Clone, Copy, Debug)]
pub struct EnumProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct PathBuilder {
    components: DynArray<Type>,
}

#[derive(Clone, Copy, Debug)]
pub struct RecordProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
    enum_: ItemIdentifier,
}

/// TupleBuilder
#[derive(Clone, Debug)]
pub struct TupleBuilder<T> {
    fields: DynArray<T>,
    names: DynArray<ValueIdentifier>,
}

//
//  Implementation of Factory
//
impl Factory {
    /// Creates an instance.
    pub fn new() -> Self { Factory }

    /// Creates a ItemFactory.
    pub fn item(&self) -> ItemFactory { ItemFactory::new() }

    /// Creates a PatternFactory.
    pub fn pat(&self) -> PatternFactory { PatternFactory::new() }

    /// Creates a PrototypeFactory.
    pub fn proto(&self) -> PrototypeFactory { PrototypeFactory::new() }

    /// Creates a StmtFactory.
    pub fn stmt(&self) -> StmtFactory { StmtFactory::new() }

    /// Creates an TypeFactory.
    pub fn type_(&self) -> TypeFactory { TypeFactory::new() }

    /// Creates a ValueFactory.
    pub fn value(&self) -> ValueFactory { ValueFactory::new() }
}

//
//  Implementation Details (Item)
//
impl ItemFactory {
    /// Creates an instance.
    pub fn new() -> Self { ItemFactory }

    /// Creates an ItemIdentifier.
    pub fn id(&self, pos: usize, len: usize) -> ItemIdentifier {
        ItemIdentifier(Default::default(), range(pos, len))
    }

    /// Creates an EnumBuilder.
    pub fn enum_(&self, p: EnumProto) -> EnumBuilder { EnumBuilder::new(p) }

    /// Creates a Function.
    pub fn fun(&self, prototype: FunctionProto, body: Value) -> Function {
        Function { prototype, body }
    }

    /// Creates a RecordBuilder.
    pub fn rec(&self, r: RecordProto) -> RecordBuilder {
        RecordBuilder::new(r)
    }

    /// Shortcut: Creates a Unit Record.
    pub fn unit(&self, pos: usize, len: usize) -> Record {
        let proto = RecordProtoBuilder::new(self.id(pos, len), pos).build();
        self.rec(proto).build()
    }
}

impl EnumBuilder {
    /// Creates an instance.
    pub fn new(p: EnumProto) -> Self {
        EnumBuilder {
            prototype: p,
            variants: DynArray::default(),
        }
    }

    /// Pushes a variant.
    pub fn push(&mut self, r: Record) -> &mut Self {
        let proto = RecordProto {
            enum_: self.prototype.name,
            ..r.prototype
        };
        self.variants.push(Record {
            prototype: proto,
            definition: r.definition,
        });
        self
    }

    /// Creates an Enum.
    pub fn build(&self) -> Enum {
        Enum {
            prototype: self.prototype,
            variants: self.variants.clone(),
        }
    }
}

impl RecordBuilder {
    /// Creates an instance.
    pub fn new(p: RecordProto) -> Self {
        RecordBuilder {
            prototype: p,
            definition: TupleBuilder::new(),
        }
    }

    /// Pushes a field.
    pub fn push(&mut self, t: Type) -> &mut Self {
        self.definition.push(t);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.definition.name(name);
        self
    }

    /// Creates an Record.
    pub fn build(&self) -> Record {
        Record {
            prototype: self.prototype,
            definition: self.definition.build(),
        }
    }
}

//
//  Implementation Details (Pattern)
//
impl PatternFactory {
    /// Creates an instance.
    pub fn new() -> Self { PatternFactory }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, type_: Type, pos: usize, len: usize)
        -> ConstructorBuilder<Pattern>
    {
        ConstructorBuilder::new(type_, pos, len)
    }

    /// Creates an ignored Pattern.
    pub fn ignored(&self, pos: usize) -> Pattern {
        Pattern::Ignored(range(pos, 1))
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<Pattern> {
        TupleBuilder::new()
    }

    /// Creates a var Pattern.
    pub fn var(&self, id: ValueIdentifier) -> Pattern {
        Pattern::Var(id, Default::default())
    }
}

//
//  Implementation Details (Prototype)
//
impl PrototypeFactory {
    /// Creates an instance.
    pub fn new() -> Self { PrototypeFactory }

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
        result: Type,
    )
        -> FunctionProtoBuilder
    {
        FunctionProtoBuilder::new(name, result)
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

impl FunctionProtoBuilder {
    /// Creates an instance.
    pub fn new(
        name: ItemIdentifier,
        result: Type,
    )
        -> Self
    {
        FunctionProtoBuilder {
            name: name,
            range: range(0, 0),
            arguments: DynArray::default(),
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
    pub fn push(&mut self, name: ValueIdentifier, type_: Type) -> &mut Self
    {
        let gvn = Default::default();
        let len = name.span().length() + 2 + type_.span().length();
        let range = range(name.span().offset(), len);
        self.arguments.push(Argument { name, type_, range, gvn });
        self
    }

    /// Sets the range of the last argument.
    pub fn arg_range(&mut self, pos: usize, len: usize) -> &mut Self {
        if let Some(mut a) = self.arguments.last() {
            a.range = range(pos, len);
            self.arguments.replace(self.arguments.len() - 1, a);
        }
        self
    }

    /// Toggle GVN generation.
    pub fn with_gvn(&mut self) -> &mut Self {
        self.with_gvn = true;
        self
    }

    /// Creates a FunctionProto.
    pub fn build(&self) -> FunctionProto {
        let mut arguments = self.arguments.get_array();
        arguments.iter_mut().enumerate().for_each(|(i, a)| {
            a.gvn = Gvn(if self.with_gvn { i as u32 + 1 } else { 0 });

            if i + 1 == self.arguments.len() { return; }
            a.range = range(a.range.offset(), a.range.length() + 1);
        });

        FunctionProto {
            name: self.name,
            range: self.range,
            arguments: DynArray::new(arguments),
            result: self.result.clone()
        }
    }
}

//
//  Implementation Details (Stmt)
//
impl StmtFactory {
    /// Creates an instance.
    pub fn new() -> Self { StmtFactory }

    /// Creates a return Stmt.
    pub fn ret(&self, value: Value) -> Stmt {
        let off = value.range.offset() - 8;
        let end = value.range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Return(Return { value, range })
    }

    /// Shortcut: Creates an empty return statement.
    pub fn ret_unit(&self, pos: usize, len: usize) -> Stmt {
        let value = Value::unit().with_range(pos + len - 3, 2);
        let range = range(pos, len);

        Stmt::Return(Return { value, range })
    }

    /// Creates a re-binding Stmt.
    pub fn set(&self, left: Value, right: Value) -> Stmt {
        let off = left.range.offset() - 5;
        let end = right.range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Set(ReBinding { left, right, range })
    }

    /// Creates a binding Stmt.
    pub fn var(&self, left: Pattern, right: Value) -> Stmt {
        let off = left.span().offset() - 5;
        let end = right.range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Var(Binding { left, right, range })
    }

    /// Shortcut: Creates a simple binding Stmt.
    pub fn var_id(&self, id: ValueIdentifier, value: Value) -> Stmt {
        self.var(Pattern::Var(id, Default::default()), value)
    }
}

//
//  Implementation Details (Type)
//
impl TypeFactory {
    /// Creates an instance.
    pub fn new() -> Self { TypeFactory }

    /// Creates a BuiltinTypeBuilder.
    pub fn builtin(&self) -> BuiltinTypeBuilder { BuiltinTypeBuilder::new() }

    /// Shortcut: creates a Bool Type.
    pub fn bool_(&self) -> Type { Type::Builtin(self.builtin().bool_()) }

    /// Shortcut: creates a Int Type.
    pub fn int(&self) -> Type { Type::Builtin(self.builtin().int()) }

    /// Shortcut: creates a String Type.
    pub fn string(&self) -> Type { Type::Builtin(self.builtin().string()) }

    /// Shortcut: creates a Void Type.
    pub fn void(&self) -> Type { Type::Builtin(self.builtin().void()) }

    /// Creates a TypeEnumBuilder.
    pub fn enum_(&self, e: Enum) -> TypeEnumBuilder {
        TypeEnumBuilder::new(e)
    }

    /// Creates a TypeRecordBuilder.
    pub fn record(&self, r: Record) -> TypeRecordBuilder {
        TypeRecordBuilder::new(r)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TupleBuilder<Type> {
        TupleBuilder::new()
    }

    /// Creates a TypeUnresolvedBuilder.
    pub fn unresolved(&self, id: ItemIdentifier) -> TypeUnresolvedBuilder {
        TypeUnresolvedBuilder::new(id)
    }

    /// Creates a TypeUnresolvedEnumBuilder.
    pub fn unresolved_enum(&self, name: ItemIdentifier, pos: usize)
        -> TypeUnresolvedEnumBuilder
    {
        TypeUnresolvedEnumBuilder::new(name, pos)
    }

    /// Creates a TypeUnresolvedRecordBuilder.
    pub fn unresolved_record(&self, name: ItemIdentifier, pos: usize)
        -> TypeUnresolvedRecordBuilder
    {
        TypeUnresolvedRecordBuilder::new(name, pos)
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

impl TypeEnumBuilder {
    /// Creates an instance.
    pub fn new(enum_: Enum) -> Self {
        TypeEnumBuilder {
            enum_: enum_,
            path: PathBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: Type) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates a Enum Type.
    pub fn build(&self) -> Type {
        Type::Enum(self.enum_.clone(), self.path.build(), Gin::default())
    }
}

impl TypeRecordBuilder {
    /// Creates an instance.
    pub fn new(record: Record) -> Self {
        TypeRecordBuilder {
            record: record,
            path: PathBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: Type) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates a Record Type.
    pub fn build(&self) -> Type {
        Type::Rec(self.record.clone(), self.path.build(), Gin::default())
    }
}

impl TypeUnresolvedBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier) -> Self {
        TypeUnresolvedBuilder {
            name: name,
            path: PathBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: Type) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates an Unresolved Type.
    pub fn build(&self) -> Type {
        Type::Unresolved(self.name, self.path.build(), Gin::default())
    }
}

impl TypeUnresolvedEnumBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier, pos: usize) -> Self {
        TypeUnresolvedEnumBuilder {
            proto: EnumProtoBuilder::new(name, pos),
            path: PathBuilder::new(),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.proto.range(pos, len);
        self
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: Type) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates an UnresolvedEnum Type.
    pub fn build(&self) -> Type {
        Type::UnresolvedEnum(self.proto.build(), self.path.build(), Gin::default())
    }
}

impl TypeUnresolvedRecordBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier, pos: usize) -> Self {
        TypeUnresolvedRecordBuilder {
            proto: RecordProtoBuilder::new(name, pos),
            path: PathBuilder::new(),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.proto.range(pos, len);
        self
    }

    /// Sets an enum.
    pub fn enum_(&mut self, name: ItemIdentifier) -> &mut Self {
        self.proto.enum_(name);
        self
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: Type) -> &mut Self {
        if let Type::UnresolvedEnum(e, ..) = component {
            self.proto.enum_(e.name);
        }
        self.path.push(component);
        self
    }

    /// Creates an UnresolvedRecord Type.
    pub fn build(&self) -> Type {
        Type::UnresolvedRec(self.proto.build(), self.path.build(), Gin::default())
    }
}

//
//  Implementation Details (Value)
//
impl ValueFactory {
    /// Creates an instance.
    pub fn new() -> Self { ValueFactory }

    /// Creates an ValueIdentifier.
    pub fn id(&self, pos: usize, len: usize) -> ValueIdentifier {
        ValueIdentifier(Default::default(), range(pos, len))
    }

    /// Creates a BlockBuilder.
    pub fn block(&self, value: Value) -> BlockBuilder {
        BlockBuilder::new(value)
    }

    /// Creates a diverging BlockBuilder.
    pub fn block_div(&self) -> BlockBuilder {
        BlockBuilder::diverging()
    }

    /// Creates a BuiltinValueBuilder.
    pub fn builtin(&self) -> BuiltinValueBuilder { BuiltinValueBuilder::new() }

    /// Shortcut: creates a boolean Value.
    pub fn bool_(&self, b: bool, pos: usize) -> Value {
        value(
            Type::Builtin(BuiltinType::Bool),
            Expr::BuiltinVal(self.builtin().bool_(b))
        ).with_range(pos, if b { 4 } else { 5 })
    }

    /// Shortcut: creates an integral Value.
    pub fn int(&self, i: i64, pos: usize) -> Value {
        value(
            Type::Builtin(BuiltinType::Int),
            Expr::BuiltinVal(self.builtin().int(i))
        ).with_range(pos, count_characters(i))
    }

    /// Shortcut: creates a string Value.
    pub fn string(&self, s: &'static str, pos: usize) -> Value {
        value(
            Type::Builtin(BuiltinType::String),
            Expr::BuiltinVal(self.builtin().string(s))
        ).with_range(pos, s.len() + 2)
    }

    /// Creates a CallBuilder.
    pub fn call(&self) -> CallBuilder { CallBuilder::new() }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, type_: Type, pos: usize, len: usize)
        -> ConstructorBuilder<Value>
    {
        ConstructorBuilder::new(type_, pos, len)
    }

    /// Creates a FieldAccessBuilder.
    pub fn field_access(&self, accessed: Value)
        -> FieldAccessBuilder
    {
        FieldAccessBuilder::new(accessed)
    }

    /// Creates an IfBuilder.
    pub fn if_(&self, cond: Value, true_: Value, false_: Value)
        -> IfBuilder
    {
        IfBuilder::new(cond, true_, false_)
    }

    /// Creates an ImplicitBuilder.
    pub fn implicit(&self) -> ImplicitBuilder {
        ImplicitBuilder::new()
    }

    /// Creates a LoopBuilder.
    pub fn loop_(&self) -> LoopBuilder { LoopBuilder::new() }

    /// Creates an Ref Value.
    pub fn name_ref(&self, name: ValueIdentifier, pos: usize) -> Value {
        value(Type::unresolved(), Expr::Ref(name, Default::default()))
            .with_range(pos, name.span().length())
    }

    /// Creates a ValueTupleBuilder.
    pub fn tuple(&self) -> ValueTupleBuilder {
        ValueTupleBuilder::new()
    }

    /// Creates an unresolved ref Value.
    pub fn unresolved_ref(&self, name: ValueIdentifier) -> Value {
        value(Type::unresolved(), Expr::UnresolvedRef(name))
            .with_range(name.span().offset(), name.span().length())
    }

    /// Shortcut: creates a Bool ref.
    pub fn bool_ref(&self, name: ValueIdentifier, pos: usize) -> Value {
        self.name_ref(name, pos).with_type(Type::Builtin(BuiltinType::Bool))
    }

    /// Shortcut: creates a Int ref.
    pub fn int_ref(&self, name: ValueIdentifier, pos: usize) -> Value {
        self.name_ref(name, pos).with_type(Type::Builtin(BuiltinType::Int))
    }

    /// Shortcut: creates a String ref.
    pub fn string_ref(&self, name: ValueIdentifier, pos: usize) -> Value {
        self.name_ref(name, pos).with_type(Type::Builtin(BuiltinType::String))
    }
}

impl BlockBuilder {
    /// Creates an instance.
    pub fn new(value: Value) -> Self {
        BlockBuilder {
            value: Some(value),
            statements: DynArray::default(),
            range: Default::default(),
        }
    }

    /// Creates an instance.
    pub fn diverging() -> Self {
        BlockBuilder {
            value: None,
            statements: DynArray::default(),
            range: Default::default(),
        }
    }

    /// Push a statement.
    pub fn push(&mut self, stmt: Stmt) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Sets a range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = com::Range::new(pos, len);
        self
    }

    /// Creates a Block Value.
    pub fn build(&self) -> Value {
        let (first, last) = (self.statements.first(), self.statements.last());
        let value_range = self.value.as_ref().map(|v| v.range);

        let off = if self.range == Default::default() {
            first.map(|s| s.span().offset())
                .unwrap_or_else(|| value_range.unwrap().offset()) - 2
        } else {
            self.range.offset()
        };

        let end = if self.range == Default::default() {
            value_range.map(|v| v.end_offset())
                .unwrap_or_else(|| last.as_ref().unwrap().span().end_offset()) + 2
        } else {
            self.range.end_offset()
        };

        let expr = Expr::Block(
            self.statements.clone(),
            self.value.as_ref().map(|v| Ptr::new(v.clone())),
        );
        let type_ =
            self.value.as_ref().map(|v| v.type_.clone())
                .or_else(|| last.map(|s| s.result_type()))
                .unwrap_or(Type::unit());
        value(type_, expr).with_range(off, end - off)
    }
}

impl BuiltinValueBuilder {
    /// Creates an instance.
    pub fn new() -> Self { BuiltinValueBuilder }

    /// Creates a Bool.
    pub fn bool_(&self, value: bool) -> BuiltinValue {
        BuiltinValue::Bool(value)
    }

    /// Creates an Int.
    pub fn int(&self, value: i64) -> BuiltinValue {
        BuiltinValue::Int(value)
    }

    /// Creates a string.
    pub fn string(&self, value: &'static str) -> BuiltinValue {
        BuiltinValue::String(value.as_bytes().iter().cloned().collect())
    }
}

impl CallBuilder {
    /// Creates an instance, defaults to Add.
    pub fn new() -> Self {
        CallBuilder {
            callable: Callable::Builtin(BuiltinFunction::Add),
            unresolved: DynArray::default(),
            arguments: DynArray::default(),
        }
    }

    /// Sets built-in function.
    pub fn builtin(&mut self, b: BuiltinFunction) -> &mut Self {
        self.callable = Callable::Builtin(b);
        self
    }

    /// Sets a user-defined function.
    pub fn function(&mut self, f: FunctionProto) -> &mut Self {
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
    pub fn push_function(&mut self, f: FunctionProto) -> &mut Self {
        self.unresolved.push(Callable::Function(f));
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, argument: Value) -> &mut Self {
        self.arguments.push(argument);
        self
    }

    /// Creates a Call Value.
    pub fn build(&self) -> Value {
        use self::Callable::*;

        let callable = if self.unresolved.len() == 0 {
            self.callable.clone()
        } else {
            Callable::Unresolved(self.unresolved.clone())
        };

        let args = if self.arguments.len() == 0 {
            None
        } else {
            let off = self.arguments.first().unwrap().range.offset();
            let end = self.arguments.last().unwrap().range.end_offset();
            Some((off, end - off))
        };

        let (off, len) = args.map(|(off, len)| {
            match &callable {
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
            Type::unresolved(),
            Expr::Call(callable, self.arguments.clone()),
        ).with_range(off, len)
    }
}

impl FieldAccessBuilder {
    /// Creates an instance.
    pub fn new(value: Value) -> Self {
        FieldAccessBuilder {
            field: Field::Index(0, Default::default()),
            type_: None,
            value: value,
        }
    }

    /// Sets the index.
    pub fn index(&mut self, index: u16) -> &mut Self {
        self.field = Field::Index(index, Default::default());
        self
    }

    /// Sets the unresolved name.
    pub fn unresolved(&mut self, name: ValueIdentifier) -> &mut Self {
        self.field = Field::Unresolved(name);
        self
    }

    /// Sets the type.
    pub fn type_(&mut self, type_: Type) -> &mut Self {
        self.type_ = Some(type_);
        self
    }

    /// Creates a field access Value.
    pub fn build(&self) -> Value {
        let field = if let Field::Index(i, r) = self.field {
            let range = if r == Default::default() {
                range(self.value.span().end_offset(), 1 + count_characters(i as i64))
            } else {
                r
            };
            Field::Index(i, range)
        } else {
            self.field
        };

        let type_ = self.type_.clone().unwrap_or_else(|| {
            self.value.type_.field(self.field)
        });

        let offset = self.value.range.offset();
        let length = self.value.range.extend(field.span()).length();

        value(type_, Expr::FieldAccess(Ptr::new(self.value.clone()), field))
            .with_range(offset, length)
    }
}

impl IfBuilder {
    /// Creates an instance.
    pub fn new(
        condition: Value,
        true_: Value,
        false_: Value
    )
        -> Self
    {
        IfBuilder {
            type_: None,
            condition: condition,
            true_: true_,
            false_: false_,
        }
    }

    /// Sets a type.
    pub fn type_(&mut self, type_: Type) -> &mut Self {
        self.type_ = Some(type_);
        self
    }

    /// Creates an if-else Value.
    pub fn build(&self) -> Value {
        let off = self.condition.range.offset() - 4;
        let end = self.false_.range.end_offset();

        let true_ = self.true_.type_.clone();
        let false_ = self.false_.type_.clone();

        let type_ = self.type_.clone().unwrap_or_else(|| {
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
            Expr::If(
                Ptr::new(self.condition.clone()),
                Ptr::new(self.true_.clone()),
                Ptr::new(self.false_.clone()),
            )
        ).with_range(off, end - off)
    }
}

impl ImplicitBuilder {
    /// Creates an instance.
    pub fn new() -> Self {
        ImplicitBuilder
    }

    /// Creates a to-enum implicit Value.
    pub fn enum_(&self, e: EnumProto, v: Value) -> Value {
        let r = v.range;
        value(
            Type::UnresolvedEnum(e, Path::default(), Gin::default()),
            Expr::Implicit(Implicit::ToEnum(e, Ptr::new(v))),
        ).with_range(r.offset(), r.length())
    }
}

impl LoopBuilder {
    /// Creates an instance.
    pub fn new() -> Self {
        LoopBuilder {
            statements: DynArray::default(),
            range: Default::default(),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, offset: usize, length: usize) -> &mut Self {
        self.range = com::Range::new(offset, length);
        self
    }

    /// Push a statement.
    pub fn push(&mut self, stmt: Stmt) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Creates a Loop Value.
    pub fn build(&self) -> Value {
        let (off, len) = if self.range == Default::default() {
            let first = self.statements.first().expect("statements");
            let last = self.statements.last().expect("statements");

            let offset = first.span().offset() - 8;
            let end_offset = last.span().end_offset() + 2;
            (offset, end_offset - offset)
        } else {
            (self.range.offset(), self.range.length())
        };

        let expr = Expr::Loop(self.statements.clone());
        value(Type::Builtin(BuiltinType::Void), expr).with_range(off, len)
    }
}

impl ValueTupleBuilder {
    /// Creates an instance.
    pub fn new() -> Self {
        ValueTupleBuilder {
            type_: TupleBuilder::new(),
            expr: TupleBuilder::new(),
        }
    }

    /// Push a value.
    pub fn push(&mut self, value: Value) -> &mut Self {
        self.type_.push(value.type_.clone());
        self.expr.push(value);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.type_.name(name);
        self.expr.name(name);
        self
    }

    /// Creates a Tuple Value.
    pub fn build(&self) -> Value {
        let range = self.expr.span();
        value(self.type_.build(), self.expr.build())
            .with_range(range.offset(), range.length())
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

impl PathBuilder {
    /// Creates an instance.
    pub fn new() -> Self {
        PathBuilder { components: DynArray::default() }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: Type) -> &mut Self {
        self.components.push(component);
        self
    }

    /// Builds a Path.
    pub fn build(&self) -> Path {
        Path { components: self.components.clone() }
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

impl<T> ConstructorBuilder<T> {
    /// Creates an instance.
    pub fn new(
        type_: Type,
        pos: usize,
        len: usize
    )
        -> Self
    {
        ConstructorBuilder {
            type_: type_,
            arguments: TupleBuilder::new(),
            range: range(pos, len),
        }
    }

    /// Push an argument.
    pub fn push(&mut self, argument: T) -> &mut Self {
        self.arguments.push(argument);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.arguments.name(name);
        self
    }
}

impl<T: Clone> ConstructorBuilder<T> {
    /// Creates a Constructor.
    pub fn build(&self) -> Constructor<T> {
        Constructor {
            type_: self.type_.clone(),
            arguments: self.arguments.build(),
            range: self.range,
        }
    }
}

impl ConstructorBuilder<Pattern> {
    /// Shortcut: creates a Pattern.
    pub fn build_pattern(&self) -> Pattern {
        self.build().into()
    }
}

impl ConstructorBuilder<Value> {
    /// Shortcut: creates a Value.
    pub fn build_value(&self) -> Value {
        self.build().into()
    }
}

impl<T> TupleBuilder<T> {
    /// Creates a new instance.
    pub fn new() -> Self {
        TupleBuilder {
            fields: DynArray::default(),
            names: DynArray::default(),
        }
    }

    /// Appends a field.
    pub fn push(&mut self, field: T) -> &mut Self {
        self.fields.push(field);
        self.names.push(Default::default());
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        if !self.names.is_empty() {
            self.names.replace(self.names.len() - 1, name);
        }
        self
    }

    fn names(&self) -> DynArray<ValueIdentifier> {
        if let Some(first) = self.names.first() {
            if first != Default::default() {
                return self.names.clone();
            }
        }

        DynArray::default()
    }
}

impl<T: Clone> TupleBuilder<T> {
    /// Creates a new Tuple instance.
    pub fn build<U: convert::From<Tuple<T>>>(&self) -> U {
        Tuple {
            fields: self.fields.clone(),
            names: self.names(),
        }.into()
    }
}

impl<T: Clone + Span> Span for TupleBuilder<T> {
    /// Computes the range spanned by the tuple.
    fn span(&self) -> com::Range {
        let off =
            self.names()
                .first().map(|v| v.span().offset() - 1)
                .or_else(|| self.fields.first().map(|f| f.span().offset() - 1))
                .unwrap_or(0);

        let end =
            self.fields
                .last().map(|f| f.span().end_offset() + 1)
                .unwrap_or(0);

        com::Range::new(off, end - off)
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

fn value(type_: Type, expr: Expr) -> Value {
    Value {
        type_: type_,
        range: range(0, 0),
        expr: expr,
        gvn: Default::default(),
    }
}
