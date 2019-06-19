//! Builder for the semantic model (aka AST).

use std::{self, cell, convert, rc};

use basic::com;
use basic::com::Span;
use basic::mem::{self, DynArray};

use model::hir::*;

//
//  High-Level Builders
//

/// Ref-counted Tree.
pub type RcTree = rc::Rc<cell::RefCell<Tree>>;

/// Factory
#[derive(Clone, Debug)]
pub struct Factory(RcTree);

/// ItemFactory
#[derive(Clone, Copy, Debug)]
pub struct ItemFactory;

/// PatternFactory
#[derive(Clone, Debug)]
pub struct PatternFactory(RcTree);

/// PrototypeFactory
#[derive(Clone, Copy, Debug)]
pub struct PrototypeFactory;

/// StmtFactory
#[derive(Clone, Debug)]
pub struct StmtFactory(RcTree);

/// TypeDefinitionFactory
#[derive(Clone, Copy, Debug)]
pub struct TypeDefinitionFactory;

/// TypeFactory
#[derive(Clone, Debug)]
pub struct TypeFactory(RcTree);

/// TypeIdFactory
#[derive(Clone, Debug)]
pub struct TypeIdFactory(RcTree);

/// ValueFactory
#[derive(Clone, Debug)]
pub struct ValueFactory(RcTree);

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
    definition: DynTupleBuilder<TypeDefinition>,
}

//
//  Pattern Builders
//

#[derive(Clone, Debug)]
pub struct PatConstructorBuilder {
    tree: RcTree,
    arguments: TupleBuilder<PatternId>,
    typ: Type,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct PatSimpleBuilder {
    tree: RcTree,
    name: ValueIdentifier,
    typ: Type,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct PatTupleBuilder {
    tree: RcTree,
    arguments: TupleBuilder<PatternId>,
    typ: TupleBuilder<TypeId>,
    range: com::Range,
}

//
//  Prototype Builders
//
#[derive(Clone, Debug)]
pub struct FunctionProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
    arguments: DynArray<Argument>,
    result: TypeDefinition,
}

//
//  Statement Builders
//

//
//  TypeDefinition Builders
//
#[derive(Clone, Copy, Debug)]
pub struct BuiltinTypeBuilder;

#[derive(Clone, Debug)]
pub struct TypeDefinitionEnumBuilder {
    enum_: Enum,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeDefinitionRecordBuilder {
    record: Record,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeDefinitionUnresolvedBuilder {
    name: ItemIdentifier,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeDefinitionUnresolvedEnumBuilder {
    proto: EnumProtoBuilder,
    path: PathBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeDefinitionUnresolvedRecordBuilder {
    proto: RecordProtoBuilder,
    path: PathBuilder,
}

//
//  Type Builders
//
#[derive(Clone, Debug)]
pub struct TypeEnumBuilder {
    tree: RcTree,
    name: ItemIdentifier,
    path: Vec<ItemIdentifier>,
    records: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeRecordBuilder {
    tree: RcTree,
    name: ItemIdentifier,
    path: Vec<ItemIdentifier>,
    fields: TupleBuilder<TypeId>,
}

#[derive(Clone, Debug)]
pub struct TypeTupleBuilder {
    tree: RcTree,
    tuple: TupleBuilder<TypeId>,
}

#[derive(Clone, Debug)]
pub struct TypeUnresolvedBuilder {
    tree: RcTree,
    name: ItemIdentifier,
    path: Vec<ItemIdentifier>,
}

//
//  TypeId Builders
//
#[derive(Clone, Debug)]
pub struct TypeIdEnumBuilder {
    builder: TypeEnumBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeIdRecordBuilder {
    builder: TypeRecordBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeIdTupleBuilder {
    builder: TypeTupleBuilder,
}

#[derive(Clone, Debug)]
pub struct TypeIdUnresolvedBuilder {
    builder: TypeUnresolvedBuilder,
}

//
//  Value Builders
//
#[derive(Clone, Debug)]
pub struct BlockBuilder {
    tree: RcTree,
    expr: Option<ExpressionId>,
    statements: Vec<Stmt>,
    typ: Type,
    range: com::Range,
}

#[derive(Clone, Copy, Debug)]
pub struct BuiltinValueBuilder;

#[derive(Clone, Debug)]
pub struct CallBuilder {
    tree: RcTree,
    callable: Callable,
    unresolved: Vec<Callable>,
    arguments: TupleBuilder<ExpressionId>,
    typ: Type,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct ExprConstructorBuilder {
    tree: RcTree,
    arguments: TupleBuilder<ExpressionId>,
    typ: Type,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct ExprTupleBuilder {
    tree: RcTree,
    arguments: TupleBuilder<ExpressionId>,
    typ: TupleBuilder<TypeId>,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct FieldAccessBuilder {
    tree: RcTree,
    field: Field,
    expr: ExpressionId,
    typ: Option<Type>,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct IfBuilder {
    tree: RcTree,
    condition: ExpressionId,
    true_: ExpressionId,
    false_: ExpressionId,
    typ: Option<Type>,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct ImplicitBuilder {
    tree: RcTree,
    expr: ExpressionId,
    typ: Type,
}

#[derive(Clone, Debug)]
pub struct LoopBuilder {
    tree: RcTree,
    statements: Vec<Stmt>,
    typ: Type,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct RefBuilder {
    tree: RcTree,
    typ: Type,
    name: ValueIdentifier,
    gvn: Gvn,
    range: com::Range,
}

//
//  Low-Level Builders
//

#[derive(Clone, Debug)]
pub struct DynTupleBuilder<T> {
    fields: DynArray<T>,
    names: DynArray<ValueIdentifier>,
}

#[derive(Clone, Copy, Debug)]
pub struct EnumProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
}

#[derive(Clone, Debug)]
pub struct PathBuilder {
    components: DynArray<TypeDefinition>,
}

#[derive(Clone, Copy, Debug)]
pub struct RecordProtoBuilder {
    name: ItemIdentifier,
    range: com::Range,
    enum_: ItemIdentifier,
}

#[derive(Clone, Debug)]
pub struct TupleBuilder<T> {
    fields: Vec<T>,
    names: Vec<ValueIdentifier>,
}

//
//  Implementation of Factory
//
impl Factory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { Factory(tree) }

    /// Creates a ItemFactory.
    pub fn item(&self) -> ItemFactory { ItemFactory::new() }

    /// Creates a PatternFactory.
    pub fn pat(&self) -> PatternFactory { PatternFactory::new(self.0.clone()) }

    /// Creates a PrototypeFactory.
    pub fn proto(&self) -> PrototypeFactory { PrototypeFactory::new() }

    /// Creates a StmtFactory.
    pub fn stmt(&self) -> StmtFactory { StmtFactory::new(self.0.clone()) }

    /// Creates a TypeDefinitionFactory.
    pub fn type_definition(&self) -> TypeDefinitionFactory {
        TypeDefinitionFactory::new()
    }

    /// Creates an TypeFactory.
    pub fn type_(&self) -> TypeFactory { TypeFactory::new(self.0.clone()) }

    /// Creates an TypeIdFactory.
    pub fn type_id(&self) -> TypeIdFactory { TypeIdFactory::new(self.0.clone()) }

    /// Creates a ValueFactory.
    pub fn value(&self) -> ValueFactory { ValueFactory::new(self.0.clone()) }
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
    pub fn fun(&self, prototype: FunctionProto, body: Tree) -> Function {
        Function { prototype, body }
    }

    /// Creates a RecordBuilder.
    pub fn rec(&self, r: RecordProto) -> RecordBuilder {
        RecordBuilder::new(r)
    }

    /// Shortcut: Creates a Unit Record.
    pub fn unit(&self, id: ItemIdentifier) -> Record {
        let proto = RecordProtoBuilder::new(id, id.1.offset()).build();
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
            definition: DynTupleBuilder::new(),
        }
    }

    /// Pushes a field.
    pub fn push(&mut self, t: TypeDefinition) -> &mut Self {
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
    pub fn new(tree: RcTree) -> Self { PatternFactory(tree) }

    /// Creates a PatConstructorBuilder.
    pub fn constructor(&self, typ: Type) -> PatConstructorBuilder {
        PatConstructorBuilder::new(self.0.clone(), typ)
    }

    /// Creates a PatSimpleBuilder.
    pub fn simple(&self) -> PatSimpleBuilder {
        PatSimpleBuilder::new(self.0.clone())
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> PatTupleBuilder {
        PatTupleBuilder::new(self.0.clone())
    }

    /// Shortcut: creates an ignored Pattern.
    pub fn ignored(&self, pos: usize) -> PatternId {
        self.simple().range(pos, 1).build()
    }

    /// Shortcut: creates a var Pattern.
    pub fn var(&self, id: ValueIdentifier) -> PatternId {
        self.var_typed(id, Type::unresolved())
    }

    /// Shortcut: creates a typed var Pattern.
    pub fn var_typed(&self, id: ValueIdentifier, typ: Type) -> PatternId {
        self.simple().var(id).type_(typ).build()
    }
}

impl PatConstructorBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, typ: Type) -> PatConstructorBuilder {
        PatConstructorBuilder {
            tree,
            arguments: TupleBuilder::new(),
            typ,
            range: Default::default(),
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, argument: PatternId) -> &mut Self {
        self.arguments.push(argument);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.arguments.name(name);
        self
    }

    /// Specifies the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Builds a Constructor.
    pub fn build(&self) -> PatternId {
        let typ = self.typ;
        let range = self.range;

        let pattern = Pattern::Constructor(self.arguments.build(&self.tree));

        self.tree.borrow_mut().push_pattern(typ, pattern, range)
    }
}

impl PatSimpleBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> PatSimpleBuilder {
        PatSimpleBuilder {
            tree,
            name: Default::default(),
            typ: Type::unresolved(),
            range: Default::default(),
        }
    }

    /// Sets the name.
    pub fn var(&mut self, name: ValueIdentifier) -> &mut Self {
        self.name = name;
        self
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Sets the type.
    pub fn type_(&mut self, typ: Type) -> &mut Self {
        self.typ = typ;
        self
    }

    /// Builds a Var pattern.
    pub fn build(&self) -> PatternId {
        let range = if self.range == Default::default() {
            self.name.1
        } else {
            self.range
        };

        let pattern = if self.name == Default::default() {
            Pattern::Ignored(range)
        } else {
            Pattern::Var(self.name)
        };

        self.tree.borrow_mut().push_pattern(self.typ, pattern, range)
    }
}

impl PatTupleBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> PatTupleBuilder {
        PatTupleBuilder {
            tree,
            arguments: TupleBuilder::new(),
            typ: TupleBuilder::new(),
            range: Default::default(),
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, argument: PatternId) -> &mut Self {
        self.arguments.push(argument);
        self.typ.push(self.tree.borrow().get_pattern_type_id(argument));
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.typ.name(name);
        self.arguments.name(name);
        self
    }

    /// Specifies the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Builds a Constructor.
    pub fn build(&self) -> PatternId {
        let range = self.range;

        let typ = Type::Tuple(self.typ.build(&self.tree));
        let pattern = Pattern::Tuple(self.arguments.build(&self.tree));

        self.tree.borrow_mut().push_pattern(typ, pattern, range)
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
        result: TypeDefinition,
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
        result: TypeDefinition,
    )
        -> Self
    {
        FunctionProtoBuilder {
            name,
            range: range(0, 0),
            arguments: DynArray::default(),
            result,
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, name: ValueIdentifier, type_: TypeDefinition) -> &mut Self
    {
        let len = name.span().length() + 2 + type_.span().length();
        let range = range(name.span().offset(), len);
        self.arguments.push(Argument { name, type_, range });
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

    /// Creates a FunctionProto.
    pub fn build(&self) -> FunctionProto {
        FunctionProto {
            name: self.name,
            range: self.range,
            arguments: self.arguments.clone(),
            result: self.result.clone()
        }
    }
}

//
//  Implementation Details (Stmt)
//
impl StmtFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { StmtFactory(tree) }

    /// Creates a return Stmt.
    pub fn ret(&self, expr: ExpressionId) -> Stmt {
        let r = self.0.borrow().get_expression_range(expr);

        let off = r.offset() - 8;
        let end = r.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Return(Return { value: expr, range })
    }

    /// Shortcut: Creates an empty return statement.
    pub fn ret_unit(&self, pos: usize, len: usize) -> Stmt {
        let typ = Type::unit();
        let expr = Expr::Tuple(Tuple::unit());
        let rng = range(pos + len - 3, 2);

        let expr = self.0.borrow_mut().push_expression(typ, expr, rng);

        Stmt::Return(Return { value: expr, range: range(pos, len) })
    }

    /// Creates a re-binding Stmt.
    pub fn set(&self, left: ExpressionId, right: ExpressionId) -> Stmt {
        let left_range = self.0.borrow().get_expression_range(left);
        let right_range = self.0.borrow().get_expression_range(right);

        let off = left_range.offset() - 5;
        let end = right_range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Set(ReBinding { left, right, range })
    }

    /// Creates a binding Stmt.
    pub fn var(&self, left: PatternId, right: ExpressionId) -> Stmt {
        let left_range = self.0.borrow().get_pattern_range(left);
        let right_range = self.0.borrow().get_expression_range(right);

        let off = left_range.offset() - 5;
        let end = right_range.end_offset() + 1;
        let range = range(off, end - off);

        Stmt::Var(Binding { left, right, range })
    }

    /// Shortcut: Creates a simple binding Stmt.
    pub fn var_id(&self, id: ValueIdentifier, expr: ExpressionId) -> Stmt {
        let typ = self.0.borrow().get_expression_type(expr);
        let range = id.1;

        let pattern = self.0.borrow_mut().push_pattern(typ, Pattern::Var(id), range);
        self.var(pattern, expr)
    }
}

//
//  Implementation Details (TypeDefinition)
//

impl TypeDefinitionFactory {
    /// Creates an instance.
    pub fn new() -> Self { TypeDefinitionFactory }

    /// Creates a BuiltinTypeBuilder.
    pub fn builtin(&self) -> BuiltinTypeBuilder { BuiltinTypeBuilder::new() }

    /// Shortcut: creates a Bool TypeDefinition.
    pub fn bool_(&self) -> TypeDefinition {
        TypeDefinition::Builtin(self.builtin().bool_())
    }

    /// Shortcut: creates a Int TypeDefinition.
    pub fn int(&self) -> TypeDefinition {
        TypeDefinition::Builtin(self.builtin().int())
    }

    /// Shortcut: creates a String TypeDefinition.
    pub fn string(&self) -> TypeDefinition {
        TypeDefinition::Builtin(self.builtin().string())
    }

    /// Shortcut: creates a Void TypeDefinition.
    pub fn void(&self) -> TypeDefinition {
        TypeDefinition::Builtin(self.builtin().void())
    }

    /// Creates a TypeDefinitionEnumBuilder.
    pub fn enum_(&self, e: Enum) -> TypeDefinitionEnumBuilder {
        TypeDefinitionEnumBuilder::new(e)
    }

    /// Creates a TypeDefinitionRecordBuilder.
    pub fn record(&self, r: Record) -> TypeDefinitionRecordBuilder {
        TypeDefinitionRecordBuilder::new(r)
    }

    /// Creates a DynTupleBuilder.
    pub fn tuple(&self) -> DynTupleBuilder<TypeDefinition> {
        DynTupleBuilder::new()
    }

    /// Creates a TypeDefinitionUnresolvedBuilder.
    pub fn unresolved(&self, id: ItemIdentifier) -> TypeDefinitionUnresolvedBuilder {
        TypeDefinitionUnresolvedBuilder::new(id)
    }

    /// Creates a TypeDefinitionUnresolvedEnumBuilder.
    pub fn unresolved_enum(&self, name: ItemIdentifier, pos: usize)
        -> TypeDefinitionUnresolvedEnumBuilder
    {
        TypeDefinitionUnresolvedEnumBuilder::new(name, pos)
    }

    /// Creates a TypeDefinitionUnresolvedRecordBuilder.
    pub fn unresolved_record(&self, name: ItemIdentifier, pos: usize)
        -> TypeDefinitionUnresolvedRecordBuilder
    {
        TypeDefinitionUnresolvedRecordBuilder::new(name, pos)
    }
}

impl TypeDefinitionEnumBuilder {
    /// Creates an instance.
    pub fn new(enum_: Enum) -> Self {
        TypeDefinitionEnumBuilder {
            enum_: enum_,
            path: PathBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: TypeDefinition) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates a Enum Type.
    pub fn build(&self) -> TypeDefinition {
        TypeDefinition::Enum(self.enum_.clone(), self.path.build())
    }
}

impl TypeDefinitionRecordBuilder {
    /// Creates an instance.
    pub fn new(record: Record) -> Self {
        TypeDefinitionRecordBuilder {
            record: record,
            path: PathBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: TypeDefinition) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates a Record Type.
    pub fn build(&self) -> TypeDefinition {
        TypeDefinition::Rec(self.record.clone(), self.path.build())
    }
}

impl TypeDefinitionUnresolvedBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier) -> Self {
        TypeDefinitionUnresolvedBuilder {
            name: name,
            path: PathBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: TypeDefinition) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates an Unresolved Type.
    pub fn build(&self) -> TypeDefinition {
        TypeDefinition::Unresolved(self.name, self.path.build())
    }
}

impl TypeDefinitionUnresolvedEnumBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier, pos: usize) -> Self {
        TypeDefinitionUnresolvedEnumBuilder {
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
    pub fn push(&mut self, component: TypeDefinition) -> &mut Self {
        self.path.push(component);
        self
    }

    /// Creates an UnresolvedEnum Type.
    pub fn build(&self) -> TypeDefinition {
        TypeDefinition::UnresolvedEnum(self.proto.build(), self.path.build())
    }
}

impl TypeDefinitionUnresolvedRecordBuilder {
    /// Creates an instance.
    pub fn new(name: ItemIdentifier, pos: usize) -> Self {
        TypeDefinitionUnresolvedRecordBuilder {
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
    pub fn push(&mut self, component: TypeDefinition) -> &mut Self {
        if let TypeDefinition::UnresolvedEnum(e, ..) = component {
            self.proto.enum_(e.name);
        }
        self.path.push(component);
        self
    }

    /// Creates an UnresolvedRecord Type.
    pub fn build(&self) -> TypeDefinition {
        TypeDefinition::UnresolvedRec(self.proto.build(), self.path.build())
    }
}

//
//  Implementation Details (Type)
//

impl TypeFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { TypeFactory(tree) }

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
    pub fn enum_(&self, name: ItemIdentifier) -> TypeEnumBuilder {
        TypeEnumBuilder::new(self.0.clone(), name)
    }

    /// Creates a TypeRecordBuilder.
    pub fn record(&self, name: ItemIdentifier) -> TypeRecordBuilder {
        TypeRecordBuilder::new(self.0.clone(), name)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TypeTupleBuilder {
        TypeTupleBuilder::new(self.0.clone())
    }

    /// Creates a TypeUnresolvedBuilder.
    pub fn unresolved(&self, name: ItemIdentifier) -> TypeUnresolvedBuilder {
        TypeUnresolvedBuilder::new(self.0.clone(), name)
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
    /// Creates a new instance.
    pub fn new(tree: RcTree, name: ItemIdentifier) -> TypeEnumBuilder {
        TypeEnumBuilder {
            tree,
            name,
            path: vec!(),
            records: vec!(),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: ItemIdentifier) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Appends a record to the enum.
    pub fn push(&mut self, record: Type) -> &mut Self {
        if let Type::Rec(..) = record {
            self.records.push(record);
            return self;
        }
        panic!("Only expects records, not {:?}", record);
    }

    /// Builds a Type::Enum.
    pub fn build(&self) -> Type {
        let path = self.tree.borrow_mut().push_path(&self.path);
        let records = self.tree.borrow_mut().push_types(&self.records);

        Type::Enum(self.name, path, records)
    }
}

impl TypeRecordBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, name: ItemIdentifier) -> TypeRecordBuilder {
        TypeRecordBuilder {
            tree,
            name,
            path: vec!(),
            fields: TupleBuilder::new(),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: ItemIdentifier) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Appends a field.
    pub fn push(&mut self, typ: Type) -> &mut Self {
        let ty = self.tree.borrow_mut().push_type(typ);
        self.fields.push(ty);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.fields.name(name);
        self
    }

    /// Builds a Type::Rec.
    pub fn build(&self) -> Type {
        let path = self.tree.borrow_mut().push_path(&self.path);
        let fields = self.fields.build(&self.tree);

        Type::Rec(self.name, path, fields)
    }
}

impl TypeTupleBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> TypeTupleBuilder {
        TypeTupleBuilder { tree, tuple: TupleBuilder::new() }
    }

    /// Appends a field.
    pub fn push(&mut self, typ: Type) -> &mut Self {
        let ty = self.tree.borrow_mut().push_type(typ);
        self.tuple.push(ty);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.tuple.name(name);
        self
    }

    /// Builds a Type::Tuple.
    pub fn build(&self) -> Type {
        Type::Tuple(self.tuple.build(&self.tree))
    }
}

impl TypeUnresolvedBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, name: ItemIdentifier) -> TypeUnresolvedBuilder {
        TypeUnresolvedBuilder { tree, name, path: vec!() }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: ItemIdentifier) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a Type::Unresolved.
    pub fn build(&self) -> Type {
        let path = self.tree.borrow_mut().push_path(&self.path);

        Type::Unresolved(self.name, path)
    }
}

//
//  Implementation Details (Type)
//

impl TypeIdFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { TypeIdFactory(tree) }

    /// Shortcut: creates a Bool Type.
    pub fn bool_(&self) -> TypeId {
        let ty = Type::Builtin(self.builtin().bool_());
        self.0.borrow_mut().push_type(ty)
    }

    /// Shortcut: creates a Int Type.
    pub fn int(&self) -> TypeId {
        let ty = Type::Builtin(self.builtin().int());
        self.0.borrow_mut().push_type(ty)
    }

    /// Shortcut: creates a String Type.
    pub fn string(&self) -> TypeId {
        let ty = Type::Builtin(self.builtin().string());    
        self.0.borrow_mut().push_type(ty)
    }

    /// Shortcut: creates a Void Type.
    pub fn void(&self) -> TypeId {
        let ty = Type::Builtin(self.builtin().void());
        self.0.borrow_mut().push_type(ty)
    }

    /// Creates a TypeIdEnumBuilder.
    pub fn enum_(&self, name: ItemIdentifier) -> TypeIdEnumBuilder {
        TypeIdEnumBuilder::new(self.0.clone(), name)
    }

    /// Creates a TypeIdRecordBuilder.
    pub fn record(&self, name: ItemIdentifier) -> TypeIdRecordBuilder {
        TypeIdRecordBuilder::new(self.0.clone(), name)
    }

    /// Creates a TypeIdTupleBuilder.
    pub fn tuple(&self) -> TypeIdTupleBuilder {
        TypeIdTupleBuilder::new(self.0.clone())
    }

    /// Shortcut: creates an unnamed Unresolved Type.
    pub fn unresolved(&self) -> TypeId {
        self.unresolved_named(ItemIdentifier::unresolved()).build()
    }

    /// Creates a TypeIdUnresolvedBuilder.
    pub fn unresolved_named(&self, name: ItemIdentifier)
        -> TypeIdUnresolvedBuilder
    {
        TypeIdUnresolvedBuilder::new(self.0.clone(), name)
    }

    fn builtin(&self) -> BuiltinTypeBuilder { BuiltinTypeBuilder::new() }
}

impl TypeIdEnumBuilder {
    /// Creates a new instance.
    pub fn new(tree: RcTree, name: ItemIdentifier) -> TypeIdEnumBuilder {
        TypeIdEnumBuilder { builder: TypeEnumBuilder::new(tree, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: ItemIdentifier) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Appends a record to the enum.
    pub fn push(&mut self, record: Type) -> &mut Self {
        self.builder.push(record);
        self
    }

    /// Builds a Type::Enum.
    pub fn build(&self) -> TypeId {
        let ty = self.builder.build();
        self.builder.tree.borrow_mut().push_type(ty)
    }
}

impl TypeIdRecordBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, name: ItemIdentifier) -> TypeIdRecordBuilder {
        TypeIdRecordBuilder { builder: TypeRecordBuilder::new(tree, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: ItemIdentifier) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Appends a field.
    pub fn push(&mut self, typ: Type) -> &mut Self {
        self.builder.push(typ);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.builder.name(name);
        self
    }

    /// Builds a Type::Rec.
    pub fn build(&self) -> TypeId {
        let ty = self.builder.build();
        self.builder.tree.borrow_mut().push_type(ty)
    }
}

impl TypeIdTupleBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> TypeIdTupleBuilder {
        TypeIdTupleBuilder { builder: TypeTupleBuilder::new(tree) }
    }

    /// Appends a field.
    pub fn push(&mut self, typ: Type) -> &mut Self {
        self.builder.push(typ);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.builder.name(name);
        self
    }

    /// Builds a Type::Tuple.
    pub fn build(&self) -> TypeId {
        let ty = self.builder.build();
        self.builder.tree.borrow_mut().push_type(ty)
    }
}

impl TypeIdUnresolvedBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, name: ItemIdentifier) -> TypeIdUnresolvedBuilder {
        TypeIdUnresolvedBuilder { builder: TypeUnresolvedBuilder::new(tree, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: ItemIdentifier) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a Type::Unresolved.
    pub fn build(&self) -> TypeId {
        let ty = self.builder.build();
        self.builder.tree.borrow_mut().push_type(ty)
    }
}

//
//  Implementation Details (Value)
//
impl ValueFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { ValueFactory(tree) }

    /// Creates an ValueIdentifier.
    pub fn id(&self, pos: usize, len: usize) -> ValueIdentifier {
        ValueIdentifier(Default::default(), range(pos, len))
    }

    /// Creates a BlockBuilder.
    pub fn block(&self, expr: ExpressionId) -> BlockBuilder {
        BlockBuilder::new(self.0.clone(), expr)
    }

    /// Creates an expression-less BlockBuilder.
    pub fn block_expression_less(&self) -> BlockBuilder {
        BlockBuilder::expression_less(self.0.clone())
    }

    /// Creates a BuiltinValueBuilder.
    pub fn builtin(&self) -> BuiltinValueBuilder { BuiltinValueBuilder::new() }

    /// Shortcut: creates a boolean Value.
    pub fn bool_(&self, b: bool, pos: usize) -> ExpressionId {
        let typ = Type::Builtin(BuiltinType::Bool);
        let expr = Expr::BuiltinVal(self.builtin().bool_(b));
        let range = range(pos, if b { 4 } else { 5 });

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Shortcut: creates an integral Value.
    pub fn int(&self, i: i64, pos: usize) -> ExpressionId {
        let typ = Type::Builtin(BuiltinType::Int);
        let expr = Expr::BuiltinVal(self.builtin().int(i));
        let range = range(pos, count_characters(i));

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Shortcut: creates a string Value.
    pub fn string(&self, s: mem::InternId, pos: usize, len: usize) -> ExpressionId {
        let typ = Type::Builtin(BuiltinType::String);
        let expr = Expr::BuiltinVal(self.builtin().string(s));
        let range = range(pos, len);

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Creates a CallBuilder.
    pub fn call(&self) -> CallBuilder {
        CallBuilder::new(self.0.clone())
    }

    /// Creates an ExprConstructorBuilder.
    pub fn constructor(&self, typ: Type) -> ExprConstructorBuilder {
        ExprConstructorBuilder::new(self.0.clone(), typ)
    }

    /// Creates a FieldAccessBuilder.
    pub fn field_access(&self, accessed: ExpressionId) -> FieldAccessBuilder {
        FieldAccessBuilder::new(self.0.clone(), accessed)
    }

    /// Creates an IfBuilder.
    pub fn if_(
        &self,
        cond: ExpressionId,
        true_: ExpressionId,
        false_: ExpressionId,
    )
        -> IfBuilder
    {
        IfBuilder::new(self.0.clone(), cond, true_, false_)
    }

    /// Creates an ImplicitBuilder.
    pub fn implicit(&self, of: ExpressionId) -> ImplicitBuilder {
        ImplicitBuilder::new(self.0.clone(), of)
    }

    /// Creates a LoopBuilder.
    pub fn loop_(&self) -> LoopBuilder { LoopBuilder::new(self.0.clone()) }

    /// Creates a RefBuilder
    pub fn name_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        let range = range(pos, name.span().length());
        RefBuilder::new(self.0.clone(), name, range)
    }

    /// Creates an ExprTupleBuilder.
    pub fn tuple(&self) -> ExprTupleBuilder {
        ExprTupleBuilder::new(self.0.clone())
    }

    /// Creates an unresolved ref Value.
    pub fn unresolved_ref(&self, name: ValueIdentifier) -> ExpressionId {
        let typ = Type::unresolved();
        let expr = Expr::UnresolvedRef(name);
        let range = range(name.span().offset(), name.span().length());

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Shortcut: creates a RefBuilder for a boolean.
    pub fn bool_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        self.quick_ref(Type::Builtin(BuiltinType::Bool), name, pos)
    }

    /// Shortcut: creates a RefBuilder for an Int.
    pub fn int_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        self.quick_ref(Type::Builtin(BuiltinType::Int), name, pos)
    }

    /// Shortcut: creates a RefBuilder for a String.
    pub fn string_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        self.quick_ref(Type::Builtin(BuiltinType::String), name, pos)
    }

    fn quick_ref(&self, typ: Type, name: ValueIdentifier, pos: usize) -> RefBuilder {
        let mut builder = self.name_ref(name, pos);
        builder.type_(typ);
        builder
    }
}

impl BlockBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, expr: ExpressionId) -> Self {
        BlockBuilder {
            tree,
            expr: Some(expr),
            statements: vec!(),
            typ: Type::unresolved(),
            range: Default::default(),
        }
    }

    /// Creates an instance.
    pub fn expression_less(tree: RcTree) -> Self {
        BlockBuilder {
            tree,
            expr: None,
            statements: vec!(),
            typ: Type::void(),
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
    pub fn build(&self) -> ExpressionId {
        let typ = Type::unresolved();
        let range = self.compute_range();

        let stmts = self.tree.borrow_mut().push_statements(&self.statements);
        let expr = Expr::Block(stmts, self.expr.clone());

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    /// Creates a Block Value.
    pub fn build_with_type(&self) -> ExpressionId {
        let typ = self.compute_type();
        let range = self.compute_range();

        let stmts = self.tree.borrow_mut().push_statements(&self.statements);
        let expr = Expr::Block(stmts, self.expr.clone());

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_type(&self) -> Type {
        if let Some(expr) = self.expr {
            self.tree.borrow().get_expression_type(expr)
        } else if let Some(stmt) = self.statements.last() {
            //  Possibly void, if ending with break/continue/return.
            stmt.result_type()
        } else {
            Type::unit()
        }
    }

    fn compute_range(&self) -> com::Range {
        if self.range != Default::default() {
            return self.range;
        }

        let offset = if let Some(stmt) = self.statements.first() {
            stmt.span().offset() - 2
        } else if let Some(expr) = self.expr {
            self.tree.borrow().get_expression_range(expr).offset() - 2
        } else {
            panic!("Cannot compute range for an empty block!");
        };

        let end = if let Some(expr) = self.expr {
            self.tree.borrow().get_expression_range(expr).end_offset() + 2
        } else if let Some(stmt) = self.statements.last() {
            stmt.span().end_offset() + 2
        } else {
            panic!("Cannot compute range for an empty block!");
        };

        com::Range::new(offset, end - offset)
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
    pub fn string(&self, id: mem::InternId) -> BuiltinValue {
        BuiltinValue::String(id)
    }
}

impl CallBuilder {
    /// Creates an instance, defaults to Add.
    pub fn new(tree: RcTree) -> Self {
        CallBuilder {
            tree,
            callable: Callable::Builtin(BuiltinFunction::Add),
            unresolved: vec!(),
            arguments: TupleBuilder::new(),
            typ: Type::unresolved(),
            range: Default::default(),
        }
    }

    /// Sets a callable.
    pub fn callable(&mut self, callable: Callable) -> &mut Self {
        self.callable = callable;
        self
    }

    /// Sets the result type.
    pub fn type_(&mut self, typ: Type) -> &mut Self {
        self.typ = typ;
        self
    }

    /// Shortcut: sets built-in function.
    pub fn builtin(&mut self, b: BuiltinFunction, typ: Type) -> &mut Self {
        self.callable = Callable::Builtin(b);
        self.typ = typ;
        self
    }

    /// Shortcut: sets a user-defined function.
    pub fn function(
        &mut self,
        name: ItemIdentifier,
        arguments: Tuple<TypeId>,
        result: TypeId,
    )
        -> &mut Self
    {
        self.callable = Callable::Function(name, arguments, result);
        self
    }

    /// Shortcut: sets an unknown binding.
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
    pub fn push_function(
        &mut self,
        name: ItemIdentifier,
        arguments: Tuple<TypeId>,
        result: TypeId,
    )
        -> &mut Self
    {
        self.unresolved.push(Callable::Function(name, arguments, result));
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, argument: ExpressionId) -> &mut Self {
        self.arguments.push(argument);
        self
    }

    /// Names the last argument pushed.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.arguments.name(name);
        self
    }

    /// Creates a Call Value.
    pub fn build(&self) -> ExpressionId {
        let callable = if self.unresolved.len() == 0 {
            self.callable.clone()
        } else {
            let unresolved =
                self.tree.borrow_mut().push_callables(&self.unresolved);
            Callable::Unresolved(unresolved)
        };

        let typ = self.typ;
        let range = self.compute_range(&callable);

        let args = self.arguments.build(&self.tree);
        let expr = Expr::Call(callable, args);

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_range(&self, callable: &Callable) -> com::Range {
        use self::Callable::*;

        if self.range != Default::default() {
            return self.range;
        }

        let args = if let (Some(first), Some(last)) =
            (self.arguments.fields.first(), self.arguments.fields.last())
        {
            let off = self.tree.borrow().get_expression_range(*first).offset();
            let end = self.tree.borrow().get_expression_range(*last).end_offset();
            Some((off, end - off))
        } else {
            None
        };

        let (off, len) = args.map(|(off, len)| {
            match callable {
                Builtin(BuiltinFunction::Not) => (off - 5, len + 5),
                Builtin(_) => (off, len),
                Function(n, _, _) => {
                    let n = n.span().length();
                    (off - 1 - n, len + 2 + n)
                },
                Unknown(n) => {
                    let n = n.span().length();
                    (off - 1 - n, len + 2 + n)
                },
                Unresolved(_) => (0, 0),
            }
        }).unwrap_or((0, 0));

        range(off, len)
    }
}

impl ExprConstructorBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, typ: Type) -> ExprConstructorBuilder {
        ExprConstructorBuilder {
            tree,
            arguments: TupleBuilder::new(),
            typ,
            range: Default::default(),
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, argument: ExpressionId) -> &mut Self {
        self.arguments.push(argument);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.arguments.name(name);
        self
    }

    /// Specifies the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Builds a Constructor.
    pub fn build(&self) -> ExpressionId {
        let typ = self.typ;
        let range = self.range;

        let expr = Expr::Constructor(self.arguments.build(&self.tree));

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }
}

impl ExprTupleBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> ExprTupleBuilder {
        ExprTupleBuilder {
            tree,
            arguments: TupleBuilder::new(),
            typ: TupleBuilder::new(),
            range: Default::default(),
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, argument: ExpressionId) -> &mut Self {
        self.arguments.push(argument);
        self.typ.push(self.tree.borrow().get_expression_type_id(argument));
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.arguments.name(name);
        self.typ.name(name);
        self
    }

    /// Specifies the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Builds a Tuple.
    pub fn build(&self) -> ExpressionId {
        let range = self.range;

        let typ = Type::Tuple(self.typ.build(&self.tree));
        let expr = Expr::Tuple(self.arguments.build(&self.tree));

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }
}

impl FieldAccessBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, expr: ExpressionId) -> Self {
        FieldAccessBuilder {
            tree,
            field: Field::Index(0, Default::default()),
            expr,
            typ: None,
            range: Default::default(),
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
    pub fn type_(&mut self, typ: Type) -> &mut Self {
        self.typ = Some(typ);
        self
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Creates a FieldAccess Value.
    pub fn build(&self) -> ExpressionId {
        let typ = self.compute_type();
        let field = self.compute_field();
        let range = self.compute_range(&field);
        let expr = Expr::FieldAccess(self.expr, field);

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_field(&self) -> Field {
        if let Field::Index(i, _) = self.field {
            let length = 1 + count_characters(i as i64);
            let accessed = self.tree.borrow().get_expression_range(self.expr);
            Field::Index(i, range(accessed.end_offset(), length))
        } else {
            self.field
        }
    }

    fn compute_type(&self) -> Type {
        if let Some(typ) = self.typ {
            return typ;
        }

        if let Field::Index(i, _) = self.field {
            let accessed = self.tree.borrow().get_expression_type(self.expr);

            return match accessed {
                Type::Rec(_, _, tup) | Type::Tuple(tup) => {
                    self.tree.borrow()
                        .get_type_ids(tup.fields)
                        .get(i as usize)
                        .map(|ty| self.tree.borrow().get_type(*ty))
                        .unwrap_or(Type::unresolved())
                },
                _ => Type::unresolved(),
            };
        }

        return Type::unresolved();
    }

    fn compute_range(&self, field: &Field) -> com::Range {
        if self.range != Default::default() {
            return self.range;
        }

        let accessed = self.tree.borrow().get_expression_range(self.expr);
        accessed.extend(field.span())
    }
}

impl IfBuilder {
    /// Creates an instance.
    pub fn new(
        tree: RcTree,
        condition: ExpressionId,
        true_: ExpressionId,
        false_: ExpressionId,
    )
        -> Self
    {
        IfBuilder {
            tree,
            condition,
            true_,
            false_,
            typ: None,
            range: Default::default(),
        }
    }

    /// Sets a type.
    pub fn type_(&mut self, typ: Type) -> &mut Self {
        self.typ = Some(typ);
        self
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Creates an If Value.
    pub fn build(&self) -> ExpressionId {
        let typ = self.compute_type();
        let range = self.compute_range();

        let expr = Expr::If(self.condition, self.true_, self.false_);

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_type(&self) -> Type {
        if let Some(typ) = self.typ {
            return typ;
        }

        let true_ = self.tree.borrow().get_expression_type(self.true_);
        let false_ = self.tree.borrow().get_expression_type(self.false_);

        if true_ == false_ || false_ == Type::void() {
            true_
        } else if true_ == Type::void() {
            false_
        } else {
            Type::unresolved()
        }
    }

    fn compute_range(&self) -> com::Range {
        if self.range != Default::default() {
            return self.range;
        }

        let off = self.tree.borrow().get_expression_range(self.condition).offset() - 4;
        let end = self.tree.borrow().get_expression_range(self.false_).end_offset();

        range(off, end - off)
    }
}

impl ImplicitBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, expr: ExpressionId) -> Self {
        ImplicitBuilder {
            tree,
            expr,
            typ: Type::unresolved(),
        }
    }

    /// Sets a type.
    pub fn type_(&mut self, typ: Type) -> &mut Self {
        self.typ = typ;
        self
    }

    /// Builds an Implicit Value.
    pub fn build(&self) -> ExpressionId {
        let typ = self.typ;
        let range = self.tree.borrow().get_expression_range(self.expr);

        let expr = Expr::Implicit(self.compute_implicit());

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_implicit(&self) -> Implicit {
        match self.typ {
            Type::Enum(name, ..) => Implicit::ToEnum(name, self.expr),
            _ => panic!("Cannot compute implicit for {:?}", self.typ),
        }
    }
}

impl LoopBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self {
        LoopBuilder {
            tree,
            statements: vec!(),
            typ: Type::void(),
            range: Default::default(),
        }
    }

    /// Push a statement.
    pub fn push(&mut self, stmt: Stmt) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Sets the range.
    pub fn range(&mut self, offset: usize, length: usize) -> &mut Self {
        self.range = com::Range::new(offset, length);
        self
    }

    /// Creates a Loop Value.
    pub fn build(&self) -> ExpressionId {
        let typ = self.typ;
        let range = self.compute_range();

        let statements = self.tree.borrow_mut().push_statements(&self.statements);
        let expr = Expr::Loop(statements);

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_range(&self) -> com::Range {
        if self.range != Default::default() {
            return self.range;
        }

        if let (Some(first), Some(last)) =
            (self.statements.first(), self.statements.last())
        {
            let off = first.span().offset() - 8;
            let end = last.span().end_offset() + 2;

            return range(off, end - off);
        }

        panic!("Cannot compute range of empty loop!");
    }
}

impl RefBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree, name: ValueIdentifier, range: com::Range) -> RefBuilder {
        RefBuilder {
            tree: tree,
            typ: Type::unresolved(),
            name: name,
            gvn: Gvn::default(),
            range: range,
        }
    }

    /// Sets the type.
    pub fn type_(&mut self, typ: Type) -> &mut Self {
        self.typ = typ;
        self
    }

    /// Sets the GVN.
    pub fn gvn<I: Into<Gvn>>(&mut self, gvn: I) -> &mut Self {
        self.gvn = gvn.into();
        self
    }

    /// Shortcut: sets the GVN as PatternId.
    pub fn pattern(&mut self, id: u32) -> &mut Self {
        self.gvn(PatternId::new(id))
    }

    /// Creates a Ref Value.
    pub fn build(&self) -> ExpressionId {
        let expr = Expr::Ref(self.name, self.gvn);

        self.tree.borrow_mut().push_expression(self.typ, expr, self.range)
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
    pub fn push(&mut self, component: TypeDefinition) -> &mut Self {
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

impl<T> DynTupleBuilder<T> {
    /// Creates a new instance.
    pub fn new() -> Self {
        DynTupleBuilder {
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

impl<T: Clone> DynTupleBuilder<T> {
    /// Creates a new Tuple instance.
    pub fn build<U: convert::From<DynTuple<T>>>(&self) -> U {
        DynTuple {
            fields: self.fields.clone(),
            names: self.names(),
        }.into()
    }
}

impl<T: Clone + Span> Span for DynTupleBuilder<T> {
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

impl<T> TupleBuilder<T> {
    /// Creates a new instance.
    fn new() -> Self {
        TupleBuilder {
            fields: vec!(),
            names: vec!(),
        }
    }

    /// Appends a field.
    fn push(&mut self, field: T) -> &mut Self {
        self.fields.push(field);
        self
    }

    /// Appends the name of the last field, if any.
    fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        if self.names.len() == self.fields.len() - 1 {
            self.names.push(name);
        }
        self
    }
}

impl TupleBuilder<ExpressionId> {
    /// Builds a Tuple Value.
    fn build(&self, tree: &RcTree) -> Tuple<ExpressionId> {
        let fields = tree.borrow_mut().push_expressions(&self.fields);
        let names = tree.borrow_mut().push_names(&self.names);

        Tuple { fields, names }
    }
}

impl TupleBuilder<PatternId> {
    /// Builds a Tuple Value.
    fn build(&self, tree: &RcTree) -> Tuple<PatternId> {
        let fields = tree.borrow_mut().push_patterns(&self.fields);
        let names = tree.borrow_mut().push_names(&self.names);

        Tuple { fields, names }
    }
}

impl TupleBuilder<TypeId> {
    /// Builds a Tuple Value.
    fn build(&self, tree: &RcTree) -> Tuple<TypeId> {
        let fields = tree.borrow_mut().push_type_ids(&self.fields);
        let names = tree.borrow_mut().push_names(&self.names);

        Tuple { fields, names }
    }
}

fn count_characters(i: i64) -> usize {
    if i == std::i64::MIN { 20 }
    else if i < 0 { 1 + count_characters(i * -1) }
    else if i < 10 { 1 }
    else { 1 + count_characters(i / 10) }
}

fn range(pos: usize, len: usize) -> com::Range { com::Range::new(pos, len) }
