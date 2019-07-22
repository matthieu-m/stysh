//! Builder for the semantic model (aka AST).

use std::{self, cell, rc};

use basic::mem;
use basic::com::{Range, Span, Store, MultiStore};

use model::hir::*;

//
//  High-Level Builders
//

/// Ref-counted Module.
pub type RcModule = Rc<Module>;
/// Ref-counted Tree.
pub type RcTree = Rc<Tree>;

//
//  Factory
//

#[derive(Clone, Debug)]
pub struct Factory(RcModule, RcTree);

impl Factory {
    /// Creates an instance.
    pub fn new(module: RcModule, tree: RcTree) -> Self { Factory(module, tree) }

    /// Creates a ItemFactory.
    pub fn item(&self) -> ItemFactory { ItemFactory::new(self.0.clone()) }

    /// Creates a PatternFactory.
    pub fn pat(&self) -> PatternFactory { PatternFactory::new(self.1.clone()) }

    /// Creates a PrototypeFactory.
    pub fn proto(&self) -> PrototypeFactory { PrototypeFactory::new(self.0.clone()) }

    /// Creates a StatementFactory.
    pub fn stmt(&self) -> StatementFactory { StatementFactory::new(self.1.clone()) }

    /// Creates an TypeFactory.
    pub fn type_(&self) -> TypeFactory<Tree> { TypeFactory::new(self.1.clone()) }

    /// Creates an TypeIdFactory.
    pub fn type_id(&self) -> TypeIdFactory<Tree> { TypeIdFactory::new(self.1.clone()) }

    /// Creates an TypeIdFactory, backed by modules.
    pub fn type_module(&self) -> TypeIdFactory<Module> { TypeIdFactory::new(self.0.clone()) }

    /// Creates a ExpressionFactory.
    pub fn value(&self) -> ExpressionFactory { ExpressionFactory::new(self.1.clone()) }
}


//
//  Items
//

#[derive(Clone, Debug)]
pub struct ItemFactory(RcModule);

#[derive(Clone, Debug)]
pub struct EnumBuilder {
    module: RcModule,
    prototype: EnumPrototype,
    variants: Vec<RecordId>,
}

#[derive(Clone, Debug)]
pub struct RecordBuilder {
    prototype: RecordPrototype,
    definition: TupleBuilder<Module, TypeId>,
}

impl ItemFactory {
    /// Creates an instance.
    pub fn new(module: RcModule) -> Self { ItemFactory(module) }

    /// Creates an EnumBuilder.
    pub fn enum_(&self, p: EnumPrototype) -> EnumBuilder {
        EnumBuilder::new(self.0.clone(), p)
    }

    /// Creates a Function.
    pub fn fun(&self, prototype: FunctionPrototype, body: Tree) -> FunctionId {
        let name = prototype.name;

        let id = if let Some(id) = self.0.borrow().lookup_function(name) {
            id
        } else {
            self.0.borrow_mut().push_function_name(name)
        };

        let function = Function { prototype, body };

        let mut module = self.0.borrow_mut();
        module.set_function_prototype(id, prototype);
        module.set_function(id, function);

        id
    }

    /// Creates a RecordBuilder.
    pub fn rec(&self, r: RecordPrototype) -> RecordBuilder {
        RecordBuilder::new(self.0.clone(), r)
    }

    /// Shortcut: Creates a Unit Record.
    pub fn unit(&self, id: ItemIdentifier) -> RecordId {
        let proto = RecordPrototypeBuilder::new(self.0.clone(), id, id.1.offset()).build();
        self.rec(proto).build()
    }

    /// Shortcut: Creates a Unit Record, with an EnumId.
    pub fn unit_of_enum(&self, id: ItemIdentifier, enum_: EnumId) -> RecordId {
        let proto = RecordPrototypeBuilder::new(self.0.clone(), id, id.1.offset())
            .enum_(enum_)
            .build();
        self.rec(proto).build()
    }
}

impl EnumBuilder {
    /// Creates an instance.
    pub fn new(module: RcModule, prototype: EnumPrototype) -> Self {
        EnumBuilder {
            module,
            prototype,
            variants: vec!(),
        }
    }

    /// Pushes a variant.
    pub fn push(&mut self, r: RecordId) -> &mut Self {
        debug_assert!(
            self.module.borrow().get_record_prototype(r).enum_ ==
            self.module.borrow().lookup_enum(self.prototype.name)
        );
        self.variants.push(r);
        self
    }

    /// Creates an Enum.
    pub fn build(&self) -> EnumId {
        let name = self.prototype.name;

        let id = if let Some(id) = self.module.borrow().lookup_enum(name) {
            id
        } else {
            self.module.borrow_mut().push_enum_name(name)
        };

        let mut module = self.module.borrow_mut();

        let enum_ = Enum {
            prototype: self.prototype,
            variants: module.push_record_ids(self.variants.iter().cloned()),
        };

        module.set_enum_prototype(id, enum_.prototype);
        module.set_enum(id, enum_);
        id
    }
}

impl RecordBuilder {
    /// Creates an instance.
    pub fn new(module: RcModule, prototype: RecordPrototype) -> Self {
        RecordBuilder {
            prototype,
            definition: TupleBuilder::new(module),
        }
    }

    /// Pushes a field.
    pub fn push(&mut self, t: TypeId) -> &mut Self {
        self.definition.push(t);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.definition.name(name);
        self
    }

    /// Creates a Record.
    pub fn build(&self) -> RecordId {
        let module = self.definition.store.clone();
        let name = self.prototype.name;

        let id = if let Some(id) = module.borrow().lookup_record(name) {
            id
        } else {
            module.borrow_mut().push_record_name(name)
        };

        let record = Record {
            prototype: self.prototype,
            definition: self.definition.build(),
        };

        let mut module = module.borrow_mut();
        module.set_record_prototype(id, record.prototype);
        module.set_record(id, record);
        id
    }
}


//
//  Pattern
//

#[derive(Clone, Debug)]
pub struct PatternFactory(RcTree);

#[derive(Clone, Debug)]
pub struct PatternSimpleBuilder {
    tree: RcTree,
    name: ValueIdentifier,
    typ: Type,
    range: Range,
}

impl PatternFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { PatternFactory(tree) }

    /// Creates a ConstructorBuilder.
    pub fn constructor(&self, ty: Type) -> ConstructorBuilder<Pattern, PatternId> {
        ConstructorBuilder::new(self.0.clone(), ty, Box::new(Pattern::Constructor))
    }

    /// Creates a PatternSimpleBuilder.
    pub fn simple(&self) -> PatternSimpleBuilder {
        PatternSimpleBuilder::new(self.0.clone())
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TypedTupleBuilder<PatternId> {
        TypedTupleBuilder::new(self.0.clone())
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

impl PatternSimpleBuilder {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> PatternSimpleBuilder {
        PatternSimpleBuilder {
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


//
//  Prototype
//

#[derive(Clone, Debug)]
pub struct PrototypeFactory(RcModule);

#[derive(Clone, Debug)]
pub struct EnumPrototypeBuilder {
    module: RcModule,
    name: ItemIdentifier,
    range: Range,
}

#[derive(Clone, Debug)]
pub struct FunctionPrototypeBuilder {
    module: RcModule,
    name: ItemIdentifier,
    range: Range,
    arguments: TupleBuilder<Module, TypeId>,
    result: TypeId,
}

#[derive(Clone, Debug)]
pub struct RecordPrototypeBuilder {
    module: RcModule,
    name: ItemIdentifier,
    range: Range,
    enum_: Option<EnumId>,
}

impl PrototypeFactory {
    /// Creates an instance.
    pub fn new(module: RcModule) -> Self { PrototypeFactory(module) }

    /// Creates an EnumPrototypeBuilder.
    pub fn enum_(&self, name: ItemIdentifier) -> EnumPrototypeBuilder {
        let mut e = EnumPrototypeBuilder::new(self.0.clone(), name, 0);
        e.range(name.span().offset() - 6, name.span().length() + 6);
        e
    }

    /// Creates a FunctionPrototypeBuilder.
    pub fn fun(
        &self,
        name: ItemIdentifier,
        result: TypeId,
    )
        -> FunctionPrototypeBuilder
    {
        FunctionPrototypeBuilder::new(self.0.clone(), name, result)
    }

    /// Creates a RecordPrototypeBuilder.
    pub fn rec(&self, name: ItemIdentifier, pos: usize) -> RecordPrototypeBuilder {
        let mut r = RecordPrototypeBuilder::new(self.0.clone(), name, pos);
        if pos != name.span().offset() {
            r.range(name.span().offset() - 5, name.span().length() + 5);
        }
        r
    }
}

impl EnumPrototypeBuilder {
    /// Creates an instance.
    pub fn new(module: RcModule, name: ItemIdentifier, pos: usize) -> Self {
        EnumPrototypeBuilder {
            module, 
            name,
            range: range(pos, name.span().length()),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Creates a Record.
    pub fn build(&self) -> EnumPrototype {
        let name = self.name;
        let prototype = EnumPrototype { name: self.name, range: self.range, };

        let id = self.module.borrow_mut().push_enum_name(name);
        self.module.borrow_mut().set_enum_prototype(id, prototype);

        prototype
    }
}

impl FunctionPrototypeBuilder {
    /// Creates an instance.
    pub fn new(
        module: RcModule,
        name: ItemIdentifier,
        result: TypeId,
    )
        -> Self
    {
        FunctionPrototypeBuilder {
            module: module.clone(),
            name,
            range: range(0, 0),
            arguments: TupleBuilder::new(module),
            result,
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, name: ValueIdentifier, type_: TypeId) -> &mut Self
    {
        self.arguments.push(type_);
        self.arguments.name(name);
        self
    }

    /// Creates a FunctionPrototype.
    pub fn build(&self) -> FunctionPrototype {
        let arguments = self.arguments.build();
        let prototype = FunctionPrototype {
            name: self.name,
            range: self.range,
            arguments,
            result: self.result,
        };

        let id = self.module.borrow_mut().push_function_name(self.name);
        self.module.borrow_mut().set_function_prototype(id, prototype);

        prototype
    }
}

impl RecordPrototypeBuilder {
    /// Creates an instance.
    pub fn new(module: RcModule, name: ItemIdentifier, pos: usize) -> Self {
        RecordPrototypeBuilder {
            module,
            name,
            range: range(pos, name.span().length()),
            enum_: None,
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Sets an enum.
    pub fn enum_(&mut self, e: EnumId) -> &mut Self {
        self.enum_ = Some(e);
        self
    }

    /// Creates a Record.
    pub fn build(&self) -> RecordPrototype {
        let name = self.name;
        let prototype = RecordPrototype { name: self.name, range: self.range, enum_: self.enum_, };

        let id = self.module.borrow_mut().push_record_name(name);
        self.module.borrow_mut().set_record_prototype(id, prototype);

        prototype
    }
}

//
//  Statement
//

#[derive(Clone, Debug)]
pub struct StatementFactory(RcTree);

impl StatementFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { StatementFactory(tree) }

    /// Creates a return Statement.
    pub fn ret(&self, expr: ExpressionId) -> Statement {
        let r = self.0.borrow().get_expression_range(expr);

        let off = r.offset() - 8;
        let end = r.end_offset() + 1;
        let range = range(off, end - off);

        Statement::Return(Return { value: expr, range })
    }

    /// Shortcut: Creates an empty return statement.
    pub fn ret_unit(&self, pos: usize, len: usize) -> Statement {
        let typ = Type::unit();
        let expr = Expression::Tuple(Tuple::unit());
        let rng = range(pos + len - 3, 2);

        let expr = self.0.borrow_mut().push_expression(typ, expr, rng);

        Statement::Return(Return { value: expr, range: range(pos, len) })
    }

    /// Creates a re-binding Statement.
    pub fn set(&self, left: ExpressionId, right: ExpressionId) -> Statement {
        let left_range = self.0.borrow().get_expression_range(left);
        let right_range = self.0.borrow().get_expression_range(right);

        let off = left_range.offset() - 5;
        let end = right_range.end_offset() + 1;
        let range = range(off, end - off);

        Statement::Set(ReBinding { left, right, range })
    }

    /// Creates a binding Statement.
    pub fn var(&self, left: PatternId, right: ExpressionId) -> Statement {
        let left_range = self.0.borrow().get_pattern_range(left);
        let right_range = self.0.borrow().get_expression_range(right);

        let off = left_range.offset() - 5;
        let end = right_range.end_offset() + 1;
        let range = range(off, end - off);

        Statement::Var(Binding { left, right, range })
    }

    /// Shortcut: Creates a simple binding Statement.
    pub fn var_id(&self, id: ValueIdentifier, expr: ExpressionId) -> Statement {
        let typ = self.0.borrow().get_expression_type(expr);
        let range = id.1;

        let pattern = self.0.borrow_mut().push_pattern(typ, Pattern::Var(id), range);
        self.var(pattern, expr)
    }
}


//
//  Type
//

#[derive(Clone, Debug)]
pub struct TypeFactory<S>(Rc<S>);

#[derive(Clone, Debug)]
pub struct BuiltinTypeBuilder;

#[derive(Clone, Debug)]
pub struct TypeEnumBuilder<S> {
    name: EnumId,
    path: PathBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct TypeRecordBuilder<S> {
    name: RecordId,
    path: PathBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct TypeTupleBuilder<S> {
    tuple: TupleBuilder<S, TypeId>,
}

#[derive(Clone, Debug)]
pub struct TypeUnresolvedBuilder<S> {
    name: ItemIdentifier,
    path: PathBuilder<S>,
}

impl<S> TypeFactory<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self { TypeFactory(store) }

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
    pub fn enum_(&self, name: EnumId) -> TypeEnumBuilder<S> {
        TypeEnumBuilder::new(self.0.clone(), name)
    }

    /// Creates a TypeRecordBuilder.
    pub fn record(&self, name: RecordId) -> TypeRecordBuilder<S> {
        TypeRecordBuilder::new(self.0.clone(), name)
    }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TypeTupleBuilder<S> {
        TypeTupleBuilder::new(self.0.clone())
    }

    /// Creates a TypeUnresolvedBuilder.
    pub fn unresolved(&self, name: ItemIdentifier) -> TypeUnresolvedBuilder<S> {
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

impl<S> TypeEnumBuilder<S> {
    /// Creates a new instance.
    pub fn new(store: Rc<S>, name: EnumId) -> Self {
        TypeEnumBuilder {
            name,
            path: PathBuilder::new(store),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a Type::Enum.
    pub fn build(&self) -> Type
        where
            S: MultiStore<PathComponent>
    {
        Type::Enum(self.name, self.path.build())
    }
}

impl<S> TypeRecordBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: RecordId) -> Self {
        TypeRecordBuilder {
            name,
            path: PathBuilder::new(store),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a Type::Rec.
    pub fn build(&self) -> Type
        where
            S: MultiStore<PathComponent>
    {
        Type::Rec(self.name, self.path.build())
    }
}

impl<S> TypeTupleBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self {
        TypeTupleBuilder { tuple: TupleBuilder::new(store) }
    }

    /// Appends a field.
    pub fn push(&mut self, ty: TypeId) -> &mut Self {
        self.tuple.push(ty);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.tuple.name(name);
        self
    }

    /// Builds a Type::Tuple.
    pub fn build(&self) -> Type
        where
            S: MultiStore<ValueIdentifier> + MultiStore<TypeId>,
    {
        Type::Tuple(self.tuple.build())
    }
}

impl<S> TypeUnresolvedBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: ItemIdentifier) -> Self {
        TypeUnresolvedBuilder {
            name,
            path: PathBuilder::new(store),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a Type::Unresolved.
    pub fn build(&self) -> Type
        where
            S: MultiStore<PathComponent>
    {
        Type::Unresolved(self.name, self.path.build())
    }
}

//
//  Implementation Details (Type)
//

#[derive(Clone, Debug)]
pub struct TypeIdFactory<S>(Rc<S>);

#[derive(Clone, Debug)]
pub struct TypeIdEnumBuilder<S>{
    builder: TypeEnumBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct TypeIdRecordBuilder<S> {
    builder: TypeRecordBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct TypeIdTupleBuilder<S> {
    builder: TypeTupleBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct TypeIdUnresolvedBuilder<S> {
    builder: TypeUnresolvedBuilder<S>,
}

impl<S> TypeIdFactory<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self { TypeIdFactory(store) }

    /// Shortcut: creates a Bool Type.
    pub fn bool_(&self) -> TypeId
        where
            S: Store<Type, TypeId>,
    {
        let ty = Type::Builtin(self.builtin().bool_());
        self.0.borrow_mut().push(ty, Range::default())
    }

    /// Shortcut: creates a Int Type.
    pub fn int(&self) -> TypeId
        where
            S: Store<Type, TypeId>,
    {
        let ty = Type::Builtin(self.builtin().int());
        self.0.borrow_mut().push(ty, Range::default())
    }

    /// Shortcut: creates a String Type.
    pub fn string(&self) -> TypeId
        where
            S: Store<Type, TypeId>,
    {
        let ty = Type::Builtin(self.builtin().string());    
        self.0.borrow_mut().push(ty, Range::default())
    }

    /// Shortcut: creates a Void Type.
    pub fn void(&self) -> TypeId
        where
            S: Store<Type, TypeId>,
    {
        let ty = Type::Builtin(self.builtin().void());
        self.0.borrow_mut().push(ty, Range::default())
    }

    /// Creates a TypeIdEnumBuilder.
    pub fn enum_(&self, name: EnumId) -> TypeIdEnumBuilder<S> {
        TypeIdEnumBuilder::new(self.0.clone(), name)
    }

    /// Creates a TypeIdRecordBuilder.
    pub fn record(&self, name: RecordId) -> TypeIdRecordBuilder<S> {
        TypeIdRecordBuilder::new(self.0.clone(), name)
    }

    /// Creates a TypeIdTupleBuilder.
    pub fn tuple(&self) -> TypeIdTupleBuilder<S> {
        TypeIdTupleBuilder::new(self.0.clone())
    }

    /// Shortcut: creates an unnamed Unresolved Type.
    pub fn unresolved(&self) -> TypeId
        where
            S: Store<Type, TypeId> + MultiStore<PathComponent>
    {
        self.unresolved_named(ItemIdentifier::unresolved()).build()
    }

    /// Creates a TypeIdUnresolvedBuilder.
    pub fn unresolved_named(&self, name: ItemIdentifier)
        -> TypeIdUnresolvedBuilder<S>
    {
        TypeIdUnresolvedBuilder::new(self.0.clone(), name)
    }

    fn builtin(&self) -> BuiltinTypeBuilder { BuiltinTypeBuilder::new() }
}

impl<S> TypeIdEnumBuilder<S> {
    /// Creates a new instance.
    pub fn new(store: Rc<S>, name: EnumId) -> Self {
        TypeIdEnumBuilder { builder: TypeEnumBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a Type::Enum.
    pub fn build(&self) -> TypeId
        where
            S: Store<Type, TypeId> + MultiStore<PathComponent>,
    {
        let ty = self.builder.build();
        self.builder.path.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> TypeIdRecordBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: RecordId) -> Self {
        TypeIdRecordBuilder { builder: TypeRecordBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a Type::Rec.
    pub fn build(&self) -> TypeId
        where
            S: Store<Type, TypeId> + MultiStore<PathComponent>,
    {
        let ty = self.builder.build();
        self.builder.path.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> TypeIdTupleBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self {
        TypeIdTupleBuilder { builder: TypeTupleBuilder::new(store) }
    }

    /// Appends a field.
    pub fn push(&mut self, typ: TypeId) -> &mut Self {
        self.builder.push(typ);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.builder.name(name);
        self
    }

    /// Builds a Type::Tuple.
    pub fn build(&self) -> TypeId
        where
            S: Store<Type, TypeId> + MultiStore<TypeId> + MultiStore<ValueIdentifier>,
    {
        let ty = self.builder.build();
        self.builder.tuple.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> TypeIdUnresolvedBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: ItemIdentifier) -> Self {
        TypeIdUnresolvedBuilder { builder: TypeUnresolvedBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a Type::Unresolved.
    pub fn build(&self) -> TypeId
        where
            S: Store<Type, TypeId> + MultiStore<PathComponent>
    {
        let ty = self.builder.build();
        self.builder.path.store.borrow_mut().push(ty, Range::default())
    }
}

//
//  Implementation Details (Value)
//

#[derive(Clone, Debug)]
pub struct ExpressionFactory(RcTree);

#[derive(Clone, Debug)]
pub struct BlockBuilder {
    tree: RcTree,
    expr: Option<ExpressionId>,
    statements: Vec<Statement>,
    typ: Type,
    range: Range,
}

#[derive(Clone, Copy, Debug)]
pub struct BuiltinValueBuilder;

#[derive(Clone, Debug)]
pub struct CallBuilder {
    tree: RcTree,
    callable: Callable,
    unresolved: Vec<Callable>,
    arguments: TupleBuilder<Tree, ExpressionId>,
    typ: Type,
    range: Range,
}

#[derive(Clone, Debug)]
pub struct FieldAccessBuilder {
    tree: RcTree,
    field: Field,
    expr: ExpressionId,
    typ: Option<Type>,
    range: Range,
}

#[derive(Clone, Debug)]
pub struct IfBuilder {
    tree: RcTree,
    condition: ExpressionId,
    true_: ExpressionId,
    false_: ExpressionId,
    typ: Option<Type>,
    range: Range,
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
    statements: Vec<Statement>,
    typ: Type,
    range: Range,
}

#[derive(Clone, Debug)]
pub struct RefBuilder {
    tree: RcTree,
    typ: Type,
    name: ValueIdentifier,
    gvn: Gvn,
    range: Range,
}

impl ExpressionFactory {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self { ExpressionFactory(tree) }

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
        let expr = Expression::BuiltinVal(self.builtin().bool_(b));
        let range = range(pos, if b { 4 } else { 5 });

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Shortcut: creates an integral Value.
    pub fn int(&self, i: i64, pos: usize) -> ExpressionId {
        let typ = Type::Builtin(BuiltinType::Int);
        let expr = Expression::BuiltinVal(self.builtin().int(i));
        let range = range(pos, count_characters(i));

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Shortcut: creates a string Value.
    pub fn string(&self, s: mem::InternId, pos: usize, len: usize) -> ExpressionId {
        let typ = Type::Builtin(BuiltinType::String);
        let expr = Expression::BuiltinVal(self.builtin().string(s));
        let range = range(pos, len);

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Creates a CallBuilder.
    pub fn call(&self) -> CallBuilder {
        CallBuilder::new(self.0.clone())
    }

    /// Creates an ConstructorBuilder.
    pub fn constructor(&self, ty: Type) -> ConstructorBuilder<Expression, ExpressionId> {
        ConstructorBuilder::new(self.0.clone(), ty, Box::new(Expression::Constructor))
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

    /// Creates a TypedTupleBuilder.
    pub fn tuple(&self) -> TypedTupleBuilder<ExpressionId> {
        TypedTupleBuilder::new(self.0.clone())
    }

    /// Creates an unresolved ref Value.
    pub fn unresolved_ref(&self, name: ValueIdentifier) -> ExpressionId {
        let typ = Type::unresolved();
        let expr = Expression::UnresolvedRef(name);
        let range = range(name.span().offset(), name.span().length());

        self.0.borrow_mut().push_expression(typ, expr, range)
    }

    /// Shortcut: creates a RefBuilder for a boolean.
    pub fn bool_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        self.quick_ref(Type::bool_(), name, pos)
    }

    /// Shortcut: creates a RefBuilder for an Int.
    pub fn int_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        self.quick_ref(Type::int(), name, pos)
    }

    /// Shortcut: creates a RefBuilder for a String.
    pub fn string_ref(&self, name: ValueIdentifier, pos: usize) -> RefBuilder {
        self.quick_ref(Type::string(), name, pos)
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
    pub fn push(&mut self, stmt: Statement) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Sets a range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = Range::new(pos, len);
        self
    }

    /// Creates a Block Value.
    pub fn build(&self) -> ExpressionId {
        let typ = Type::unresolved();
        let range = self.compute_range();

        let stmts = self.tree.borrow_mut()
            .push_statements(self.statements.iter().cloned());
        let expr = Expression::Block(stmts, self.expr.clone());

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    /// Creates a Block Value.
    pub fn build_with_type(&self) -> ExpressionId {
        let typ = self.compute_type();
        let range = self.compute_range();

        let stmts = self.tree.borrow_mut()
            .push_statements(self.statements.iter().cloned());
        let expr = Expression::Block(stmts, self.expr.clone());

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

    fn compute_range(&self) -> Range {
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

        Range::new(offset, end - offset)
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
            tree: tree.clone(),
            callable: Callable::Builtin(BuiltinFunction::Add),
            unresolved: vec!(),
            arguments: TupleBuilder::new(tree),
            typ: Type::unresolved(),
            range: Default::default(),
        }
    }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
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
    pub fn function(&mut self, function: FunctionId) -> &mut Self {
        self.callable = Callable::Function(function);
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
    pub fn push_function(&mut self, function: FunctionId) -> &mut Self {
        self.unresolved.push(Callable::Function(function));
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
            let unresolved = self.tree.borrow_mut()
                .push_callables(self.unresolved.iter().cloned());
            Callable::Unresolved(unresolved)
        };

        let typ = self.typ;
        let range = self.compute_range(&callable);

        let args = self.arguments.build();
        let expr = Expression::Call(callable, args);

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_range(&self, callable: &Callable) -> Range {
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
                Unknown(n) => {
                    let n = n.span().length();
                    (off - 1 - n, len + 2 + n)
                },
                Function(_) | Unresolved(_) => (0, 0),
            }
        }).unwrap_or((0, 0));

        range(off, len)
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
        let expr = Expression::FieldAccess(self.expr, field);

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
                Type::Tuple(tup) => {
                    self.tree.borrow()
                        .get_type_ids(tup.fields)
                        .get(i as usize)
                        .map(|ty| self.tree.borrow().get_type(*ty))
                        .unwrap_or(Type::unresolved())
                },
                _ => Type::unresolved(),
            };
        }

        Type::unresolved()
    }

    fn compute_range(&self, field: &Field) -> Range {
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

        let expr = Expression::If(self.condition, self.true_, self.false_);

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

    fn compute_range(&self) -> Range {
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

        let expr = Expression::Implicit(self.compute_implicit());

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
    pub fn push(&mut self, stmt: Statement) -> &mut Self {
        self.statements.push(stmt);
        self
    }

    /// Sets the range.
    pub fn range(&mut self, offset: usize, length: usize) -> &mut Self {
        self.range = Range::new(offset, length);
        self
    }

    /// Creates a Loop Value.
    pub fn build(&self) -> ExpressionId {
        let typ = self.typ;
        let range = self.compute_range();

        let statements = self.tree.borrow_mut()
            .push_statements(self.statements.iter().cloned());
        let expr = Expression::Loop(statements);

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_range(&self) -> Range {
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
    pub fn new(tree: RcTree, name: ValueIdentifier, range: Range) -> RefBuilder {
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
        let expr = Expression::Ref(self.name, self.gvn);

        self.tree.borrow_mut().push_expression(self.typ, expr, self.range)
    }
}

//
//  Implementations of Low-Level builders
//

pub struct ConstructorBuilder<E, Id> {
    type_: Type,
    tuple: TupleBuilder<Tree, Id>,
    transformer: Box<Fn(Tuple<Id>) -> E>,
    range: Range,
}

#[derive(Clone, Debug)]
pub struct PathBuilder<S> {
    store: Rc<S>,
    components: Vec<PathComponent>,
}

#[derive(Clone, Debug)]
pub struct TupleBuilder<S, T> {
    store: Rc<S>,
    fields: Vec<T>,
    names: Vec<ValueIdentifier>,
}

#[derive(Clone, Debug)]
pub struct TypedTupleBuilder<T> {
    arguments: TupleBuilder<Tree, T>,
    typ: TupleBuilder<Tree, TypeId>,
    range: Range,
}

impl<E, Id> ConstructorBuilder<E, Id> {
    /// Creates an instance.
    pub fn new(
        tree: RcTree,
        type_: Type,
        transformer: Box<Fn(Tuple<Id>) -> E>,
    )
        -> Self
    {
        ConstructorBuilder {
            type_,
            tuple: TupleBuilder::new(tree),
            transformer,
            range: Range::default(),
        }
    }

    /// Appends an argument.
    pub fn push(&mut self, argument: Id) -> &mut Self {
        self.tuple.push(argument);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.tuple.name(name);
        self
    }

    /// Specifies the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Builds a Constructor.
    pub fn build(&self) -> Id
        where
            Tree: TypedStore<Id, Element = E> + MultiStore<Id>,
    {
        use std::ops::DerefMut;

        let ty = self.type_;
        let range = self.range;

        let element: E = (self.transformer)(self.tuple.build());

        TypedStore::push(self.tuple.store.borrow_mut().deref_mut(), ty, element, range)
    }
}

impl<S> PathBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self {
        PathBuilder { store, components: vec!(), }
    }

    /// Appends a component to the path.
    pub fn push(&mut self, component: PathComponent) -> &mut Self {
        self.components.push(component);
        self
    }

    /// Builds a Path.
    pub fn build(&self) -> PathId
        where
            S: MultiStore<PathComponent>
    {
        self.store.borrow_mut().push_slice(&self.components)
    }
}

impl<S, T> TupleBuilder<S, T> {
    /// Creates a new instance.
    fn new(store: Rc<S>) -> Self {
        TupleBuilder {
            store,
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

    fn build(&self) -> Tuple<T>
        where
            S: MultiStore<T> + MultiStore<ValueIdentifier>,
    {
        let names = self.store.borrow_mut().push_slice(&self.names);
        self.build_named(names)
    }

    fn build_named(&self, names: Id<[ValueIdentifier]>) -> Tuple<T>
        where
            S: MultiStore<T> + MultiStore<ValueIdentifier>,
    {
        let fields = self.store.borrow_mut().push_slice(&self.fields);
        Tuple { fields, names, }
    }
}

impl<Id> TypedTupleBuilder<Id> {
    /// Creates an instance.
    pub fn new(tree: RcTree) -> Self {
        TypedTupleBuilder {
            arguments: TupleBuilder::new(tree.clone()),
            typ: TupleBuilder::new(tree),
            range: Default::default(),
        }
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

    /// Appends an argument.
    pub fn push(&mut self, argument: Id) -> &mut Self
        where
            Id: Copy,
            Tree: TypedStore<Id>,
    {
        use std::ops::Deref;

        self.arguments.push(argument);
        let typ = TypedStore::get_type(self.arguments.store.borrow().deref(), argument);
        self.typ.push(typ);
        self
    }

    /// Builds a Tuple.
    pub fn build(&self) -> Id
        where
            Tree: TypedStore<Id> + MultiStore<Id>,
            Tuple<Id>: Into<<Tree as TypedStore<Id>>::Element>,
    {
        use std::ops::DerefMut;

        let range = self.range;

        let typ = self.typ.build();
        let element: <Tree as TypedStore<Id>>::Element =
            self.arguments.build_named(typ.names).into();

        TypedStore::push(self.arguments.store.borrow_mut().deref_mut(), Type::Tuple(typ), element, range)
    }
}


//
//  Implementation Details
//

type Rc<S> = rc::Rc<cell::RefCell<S>>;

fn count_characters(i: i64) -> usize {
    if i == std::i64::MIN { 20 }
    else if i < 0 { 1 + count_characters(i * -1) }
    else if i < 10 { 1 }
    else { 1 + count_characters(i / 10) }
}

fn range(pos: usize, len: usize) -> Range { Range::new(pos, len) }
