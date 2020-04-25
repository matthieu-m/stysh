//! Builder for the semantic model (aka AST).

use std::{self, cell, rc};

use crate::basic::mem;
use crate::basic::com::{Range, Span, Store, MultiStore};

use crate::model::ast;
use crate::model::hir::*;

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

    /// Creates a StatementFactory.
    pub fn stmt(&self) -> StatementFactory { StatementFactory::new(self.1.clone()) }

    /// Creates an TypeFactory.
    pub fn type_(&self) -> TypeFactory<Tree> { TypeFactory::new(self.1.clone()) }

    /// Creates an TypeIdFactory.
    pub fn type_id(&self) -> TypeIdFactory<Tree> { TypeIdFactory::new(self.1.clone()) }

    /// Creates an TypeIdFactory, backed by modules.
    pub fn type_module(&self) -> TypeIdFactory<Module> { TypeIdFactory::new(self.0.clone()) }

    /// Creates an ElaboraetTypeFactory.
    pub fn elaborate_type(&self) -> ElaborateTypeFactory<Tree> {
        ElaborateTypeFactory::new(self.1.clone())
    }

    /// Creates an ElaborateTypeIdFactory.
    pub fn elaborate_type_id(&self) -> ElaborateTypeIdFactory<Tree> {
        ElaborateTypeIdFactory::new(self.1.clone())
    }

    /// Creates an ElaborateTypeIdFactory, backed by modules.
    pub fn elaborate_type_module(&self) -> ElaborateTypeIdFactory<Module> {
        ElaborateTypeIdFactory::new(self.0.clone())
    }

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
    id: EnumId,
    name: ItemIdentifier,
    range: Range,
    variants: Vec<RecordId>,
}

#[derive(Clone, Debug)]
pub struct ExtensionBuilder {
    module: RcModule,
    id: ExtensionId,
    name: ItemIdentifier,
    range: Range,
    extended: TypeId,
    elaborate_extended: ElaborateTypeId,
}

#[derive(Clone, Debug)]
pub struct FunctionSignatureBuilder {
    module: RcModule,
    id: FunctionId,
    name: ItemIdentifier,
    range: Range,
    scope: Scope,
    arguments: Vec<ValueIdentifier>,
    argument_types: Vec<TypeId>,
    result: TypeId,
    elaborate_argument_types: Vec<ElaborateTypeId>,
    elaborate_result: ElaborateTypeId,
}

#[derive(Clone, Debug)]
pub struct ImplementationBuilder {
    module: RcModule,
    id: ImplementationId,
    implemented_name: ItemIdentifier,
    extended_name: ItemIdentifier,
    range: Range,
    implemented: InterfaceId,
    extended: TypeId,
    elaborate_implemented: ElaborateTypeId,
    elaborate_extended: ElaborateTypeId,
}

#[derive(Clone, Debug)]
pub struct InterfaceBuilder {
    module: RcModule,
    id: InterfaceId,
    name: ItemIdentifier,
    range: Range,
}

#[derive(Clone, Debug)]
pub struct RecordBuilder {
    id: RecordId,
    name: ItemIdentifier,
    range: Range,
    enum_: Option<EnumId>,
    definition: TupleBuilder<Module, TypeId>,
    elaborate_definition: TupleBuilder<Module, ElaborateTypeId>,
}

impl ItemFactory {
    /// Creates an instance.
    pub fn new(module: RcModule) -> Self { ItemFactory(module) }

    /// Creates an EnumBuilder.
    pub fn enum_(&self, name: ItemIdentifier) -> EnumBuilder {
        let mut e = EnumBuilder::new(self.0.clone(), name);
        if name.span().offset() >= 6 {
            e.range(name.span().offset() - 6, name.span().length() + 6);
        }
        e
    }

    /// Creates an ExtensionBuilder.
    pub fn ext(&self, name: ItemIdentifier, extended: TypeId)
        ->  ExtensionBuilder
    {
        ExtensionBuilder::new(self.0.clone(), name, extended)
    }

    /// Creates a FunctionSignatureBuilder.
    pub fn fun(
        &self,
        name: ItemIdentifier,
        result: TypeId,
    )
        -> FunctionSignatureBuilder
    {
        FunctionSignatureBuilder::new(self.0.clone(), name, result)
    }

    /// Creates an ImplementationBuilder.
    pub fn impl_(
        &self,
        implemented_name: ItemIdentifier,
        extended_name: ItemIdentifier,
        implemented: InterfaceId,
        extended: TypeId
    )
        -> ImplementationBuilder
    {
        ImplementationBuilder::new(
            self.0.clone(),
            implemented_name,
            extended_name,
            implemented,
            extended,
        )
    }

    /// Creates an InterfaceBuilder.
    pub fn int(&self, name: ItemIdentifier) -> InterfaceBuilder {
        InterfaceBuilder::new(self.0.clone(), name)
    }

    /// Creates a RecordBuilder.
    pub fn rec(&self, name: ItemIdentifier) -> RecordBuilder {
        let mut r = RecordBuilder::new(self.0.clone(), name);
        if name.span().offset() >= 5 {
            r.range(name.span().offset() - 5, name.span().length() + 5);
        }
        r
    }

    /// Shortcut: Creates a Unit Record.
    pub fn unit(&self, id: ItemIdentifier) -> RecordId {
        RecordBuilder::new(self.0.clone(), id).build()
    }

    /// Shortcut: Creates a Unit Record, with an EnumId.
    pub fn unit_of_enum(&self, id: ItemIdentifier, enum_: EnumId) -> RecordId {
        RecordBuilder::new(self.0.clone(), id)
            .enum_(enum_)
            .build()
    }
}

impl EnumBuilder {
    /// Creates an instance.
    pub fn new(module: RcModule, name: ItemIdentifier) -> Self {
        let id = module.borrow().enums().len();
        let id = ast::EnumId::new(id as u32);
        let id = module.borrow_mut().push_enum_name(id);

        EnumBuilder {
            module,
            id,
            name,
            range: Default::default(),
            variants: vec!(),
        }
    }

    /// Gets the id.
    pub fn id(&self) -> EnumId { self.id }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Pushes a variant.
    pub fn push(&mut self, r: RecordId) -> &mut Self {
        self.variants.push(r);
        self
    }

    /// Creates an Enum.
    pub fn build(&self) -> EnumId {
        let name = self.name;
        let range = self.range;

        let mut module = self.module.borrow_mut();
        let variants = module.push_record_ids(self.variants.iter().copied());

        let enum_ = Enum { name, range, variants, };

        module.set_enum(self.id, enum_);
        self.id
    }
}

impl ExtensionBuilder {
    /// Creates an instance.
    pub fn new(
        module: RcModule,
        name: ItemIdentifier,
        extended: TypeId,
    )
        -> Self
    {
        let id = module.borrow().extensions().len();
        let id = ast::ExtensionId::new(id as u32);
        let id = module.borrow_mut().push_extension_name(id);

        ExtensionBuilder {
            module,
            id,
            name,
            range: Default::default(),
            extended,
            elaborate_extended: Default::default(),
        }
    }

    /// Gets the id.
    pub fn id(&self) -> ExtensionId { self.id }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Sets the elaborate type.
    pub fn elaborate_extended(&mut self, e: ElaborateTypeId) -> &mut Self {
        self.elaborate_extended = e;
        self
    }

    /// Creates an Extension.
    pub fn build(&self) -> ExtensionId {
        let extension = Extension {
            name: self.name,
            range: self.range,
            extended: self.extended,
            elaborate_extended: self.elaborate_extended,
        };

        self.module.borrow_mut().set_extension(self.id, extension);

        self.id
    }
}

impl FunctionSignatureBuilder {
    /// Creates an instance.
    pub fn new(
        module: RcModule,
        name: ItemIdentifier,
        result: TypeId,
    )
        -> Self
    {
        let id = module.borrow().functions().len();
        let id = ast::FunctionId::new(id as u32);
        let id = module.borrow_mut().push_function_name(id);

        FunctionSignatureBuilder {
            module: module.clone(),
            id,
            name,
            range: Default::default(),
            scope: Default::default(),
            arguments: Default::default(),
            argument_types: Default::default(),
            result,
            elaborate_argument_types: Default::default(),
            elaborate_result: Default::default(),
        }
    }

    /// Gets the id.
    pub fn id(&self) -> FunctionId { self.id }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Sets the elaborate result.
    pub fn elaborate_result(&mut self, r: ElaborateTypeId) -> &mut Self {
        self.elaborate_result = r;
        self
    }

    /// Sets the extension.
    pub fn extension(&mut self, ext: ExtensionId) -> &mut Self {
        self.scope = Scope::Ext(ext);
        self
    }

    /// Sets the interface.
    pub fn interface(&mut self, int: InterfaceId) -> &mut Self {
        self.scope = Scope::Int(int);
        self
    }

    /// Pushes an argument.
    pub fn push(&mut self, name: ValueIdentifier, type_: TypeId) -> &mut Self
    {
        self.arguments.push(name);
        self.argument_types.push(type_);
        self
    }

    /// Pushes an elaborate argument.
    pub fn push_elaborate(&mut self, type_: ElaborateTypeId) -> &mut Self
    {
        self.elaborate_argument_types.push(type_);
        self
    }

    /// Creates a FunctionId.
    pub fn build(&self) -> FunctionId {
        let elaborate_result = self.build_elaborate_result(self.result);

        let arguments = self.build_arguments();
        let argument_types = self.build_argument_types();
        let elaborate_argument_types = self.build_elaborate_argument_types();

        let signature = FunctionSignature {
            name: self.name,
            range: self.range,
            scope: self.scope,
            arguments,
            argument_types,
            result: self.result,
            elaborate_argument_types,
            elaborate_result,
        };

        self.module.borrow_mut().set_function(self.id, signature);

        self.id
    }

    fn build_elaborate_result(&self, result: TypeId) -> ElaborateTypeId {
        if self.elaborate_result != ElaborateTypeId::default() {
            return self.elaborate_result;
        }

        self.convert_type(result)
    }

    fn build_arguments(&self) -> Id<[ValueIdentifier]> {
        self.module.borrow_mut()
            .push_arguments(self.arguments.iter().copied())
    }

    fn build_argument_types(&self) -> Id<[TypeId]> {
        self.module.borrow_mut()
            .push_type_ids(self.argument_types.iter().copied())
    }

    fn build_elaborate_argument_types(&self) -> Id<[ElaborateTypeId]> {
        let elaborate = if !self.elaborate_argument_types.is_empty() {
            self.elaborate_argument_types.clone()
        } else {
            self.argument_types.iter().map(|&ty| self.convert_type(ty)).collect()
        };

        self.module.borrow_mut().push_elaborate_type_ids(elaborate.into_iter())
    }

    fn convert_type(&self, ty: TypeId) -> ElaborateTypeId {
        use std::convert::TryInto;

        if let Some(b) = ty.builtin() {
            return ElaborateTypeId::from(b);
        }

        let type_ = self.module.borrow().get_type(ty);

        let elaborate = if let Type::Tuple(tup) = type_ {
            ElaborateType::Tuple(self.convert_tuple(tup))
        } else {
            type_.try_into().expect("Type")
        };
        self.module.borrow_mut().push_elaborate_type(elaborate)
    }

    fn convert_tuple(&self, tup: Tuple<TypeId>) -> Tuple<ElaborateTypeId> {
        if tup.fields == Id::empty() {
            return Tuple::unit();
        }

        let tys: Vec<_> = self.module.borrow()
            .get_type_ids(tup.fields)
            .iter()
            .copied()
            .collect();

        let mut elaborate = TupleBuilder::new(self.module.clone());
        for ty in tys {
            elaborate.push(self.convert_type(ty));
        }
        let mut elaborate = elaborate.build();
        elaborate.names = tup.names;
        elaborate
    }
}

impl ImplementationBuilder {
    /// Creates an instance.
    pub fn new(
        module: RcModule,
        implemented_name: ItemIdentifier,
        extended_name: ItemIdentifier,
        implemented: InterfaceId,
        extended: TypeId,
    )
        -> Self
    {
        let id = module.borrow().implementations().len();
        let id = ast::ImplementationId::new(id as u32);
        let id = module.borrow_mut().push_implementation_name(id);

        ImplementationBuilder {
            module,
            id,
            implemented_name,
            extended_name,
            range: Default::default(),
            implemented,
            extended,
            elaborate_implemented: Default::default(),
            elaborate_extended: Default::default(),
        }
    }

    /// Gets the id.
    pub fn id(&self) -> ImplementationId { self.id }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Sets the elaborate implemented interface.
    pub fn elaborate_implemented(&mut self, i: ElaborateTypeId) -> &mut Self {
        self.elaborate_implemented = i;
        self
    }

    /// Sets the elaborate extended type.
    pub fn elaborate_extended(&mut self, t: ElaborateTypeId) -> &mut Self {
        self.elaborate_extended = t;
        self
    }

    /// Creates an Implementation.
    pub fn build(&self) -> ImplementationId {
        let implementation = Implementation {
            implemented_name: self.implemented_name,
            extended_name: self.extended_name,
            range: self.range,
            implemented: self.implemented,
            extended: self.extended,
            elaborate_implemented: self.elaborate_implemented,
            elaborate_extended: self.elaborate_extended,
        };

        self.module.borrow_mut().set_implementation(self.id, implementation);

        self.id
    }
}

impl InterfaceBuilder {
    /// Creates an instance.
    pub fn new(
        module: RcModule,
        name: ItemIdentifier,
    )
        -> Self
    {
        let id = module.borrow().interfaces().len();
        let id = ast::InterfaceId::new(id as u32);
        let id = module.borrow_mut().push_interface_name(id);

        InterfaceBuilder {
            id,
            module,
            name,
            range: Default::default(),
        }
    }

    /// Gets the id.
    pub fn id(&self) -> InterfaceId { self.id }

    /// Sets the range.
    pub fn range(&mut self, pos: usize, len: usize) -> &mut Self {
        self.range = range(pos, len);
        self
    }

    /// Creates an Interface.
    pub fn build(&self) -> InterfaceId {
        let interface = Interface {
            name: self.name,
            range: self.range,
        };

        self.module.borrow_mut().set_interface(self.id, interface);

        self.id
    }
}

impl RecordBuilder {
    /// Creates an instance.
    pub fn new(module: RcModule, name: ItemIdentifier) -> Self {
        let id = module.borrow().records().len();
        let id = ast::RecordId::new(id as u32);
        let id = module.borrow_mut().push_record_name(id);

        RecordBuilder {
            id,
            name,
            range: Default::default(),
            enum_: None,
            definition: TupleBuilder::new(module.clone()),
            elaborate_definition: TupleBuilder::new(module),
        }
    }

    /// Gets the id.
    pub fn id(&self) -> RecordId { self.id }

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

    /// Pushes a field.
    pub fn push(&mut self, t: TypeId) -> &mut Self {
        self.definition.push(t);
        self
    }

    /// Pushes an elaborate field.
    pub fn push_elaborate(&mut self, t: ElaborateTypeId) -> &mut Self {
        self.elaborate_definition.push(t);
        self
    }

    /// Overrides the name of the last field, if any.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.definition.name(name);
        self.elaborate_definition.name(name);
        self
    }

    /// Creates a Record.
    pub fn build(&self) -> RecordId {
        let module = self.definition.store.clone();
        let name = self.name;
        let mut range = self.range;
        let enum_ = self.enum_;
        let definition = self.definition.build();
        let elaborate_definition = self.elaborate_definition.build();

        if range == Default::default() {
            range = name.1;
        }

        let record = Record { name, range, enum_, definition, elaborate_definition, };

        let mut module = module.borrow_mut();
        module.set_record(self.id, record);
        self.id
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
    pub fn int(&self, id: ValueIdentifier) -> PatternId {
        self.var_typed(id, Type::int())
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
pub struct TypeTupleBuilder<S> {
    tuple: TupleBuilder<S, TypeId>,
}

impl<S> TypeFactory<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self { TypeFactory(store) }

    /// Shortcut: creates a Bool Type.
    pub fn bool_(&self) -> Type { Type::Builtin(BuiltinType::Bool) }

    /// Shortcut: creates a Int Type.
    pub fn int(&self) -> Type { Type::Builtin(BuiltinType::Int) }

    /// Shortcut: creates a String Type.
    pub fn string(&self) -> Type { Type::Builtin(BuiltinType::String) }

    /// Shortcut: creates a Void Type.
    pub fn void(&self) -> Type { Type::Builtin(BuiltinType::Void) }

    /// Shortcut: creates an Enum Type.
    pub fn enum_(&self, e: EnumId) -> Type { Type::Enum(e) }

    /// Creates a TypeInterfaceBuilder.
    pub fn interface(&self, i: InterfaceId) -> Type { Type::Int(i) }

    /// Creates a TypeRecordBuilder.
    pub fn record(&self, r: RecordId) -> Type { Type::Rec(r) }

    /// Creates a TupleBuilder.
    pub fn tuple(&self) -> TypeTupleBuilder<S> {
        TypeTupleBuilder::new(self.0.clone())
    }

    /// Creates a TypeUnresolvedBuilder.
    pub fn unresolved(&self) -> Type { Type::Unresolved }
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
            S: MultiStore<Identifier> + MultiStore<TypeId>,
    {
        Type::Tuple(self.tuple.build())
    }
}


//
//  Elaborate Type
//

#[derive(Clone, Debug)]
pub struct ElaborateTypeFactory<S>(Rc<S>);

#[derive(Clone, Debug)]
pub struct ElaborateTypeEnumBuilder<S> {
    name: EnumId,
    path: PathBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeInterfaceBuilder<S> {
    name: InterfaceId,
    path: PathBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeRecordBuilder<S> {
    name: RecordId,
    path: PathBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeTupleBuilder<S> {
    tuple: TupleBuilder<S, ElaborateTypeId>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeUnresolvedBuilder<S> {
    name: ItemIdentifier,
    path: PathBuilder<S>,
}

impl<S> ElaborateTypeFactory<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self { ElaborateTypeFactory(store) }

    /// Shortcut: creates a Bool Elaborate.
    pub fn bool_(&self) -> ElaborateType { ElaborateType::Builtin(BuiltinType::Bool) }

    /// Shortcut: creates a Int ElaborateType.
    pub fn int(&self) -> ElaborateType { ElaborateType::Builtin(BuiltinType::Int) }

    /// Shortcut: creates a String ElaborateType.
    pub fn string(&self) -> ElaborateType { ElaborateType::Builtin(BuiltinType::String) }

    /// Shortcut: creates a Void ElaborateType.
    pub fn void(&self) -> ElaborateType { ElaborateType::Builtin(BuiltinType::Void) }

    /// Creates a ElaborateTypeEnumBuilder.
    pub fn enum_(&self, name: EnumId) -> ElaborateTypeEnumBuilder<S> {
        ElaborateTypeEnumBuilder::new(self.0.clone(), name)
    }

    /// Creates a ElaborateTypeInterfaceBuilder.
    pub fn interface(&self, name: InterfaceId) -> ElaborateTypeInterfaceBuilder<S> {
        ElaborateTypeInterfaceBuilder::new(self.0.clone(), name)
    }

    /// Creates a ElaborateTypeRecordBuilder.
    pub fn record(&self, name: RecordId) -> ElaborateTypeRecordBuilder<S> {
        ElaborateTypeRecordBuilder::new(self.0.clone(), name)
    }

    /// Creates a ElaborateTypeTupleBuilder.
    pub fn tuple(&self) -> ElaborateTypeTupleBuilder<S> {
        ElaborateTypeTupleBuilder::new(self.0.clone())
    }

    /// Creates a ElaborateTypeUnresolvedBuilder.
    pub fn unresolved(&self, name: ItemIdentifier) -> ElaborateTypeUnresolvedBuilder<S> {
        ElaborateTypeUnresolvedBuilder::new(self.0.clone(), name)
    }
}

impl<S> ElaborateTypeEnumBuilder<S> {
    /// Creates a new instance.
    pub fn new(store: Rc<S>, name: EnumId) -> Self {
        ElaborateTypeEnumBuilder {
            name,
            path: PathBuilder::new(store),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a ElaborateType::Enum.
    pub fn build(&self) -> ElaborateType
        where
            S: MultiStore<PathComponent>
    {
        ElaborateType::Enum(self.name, self.path.build())
    }
}

impl<S> ElaborateTypeInterfaceBuilder<S> {
    /// Creates a new instance.
    pub fn new(store: Rc<S>, name: InterfaceId) -> Self {
        ElaborateTypeInterfaceBuilder {
            name,
            path: PathBuilder::new(store),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a ElaborateType::Int.
    pub fn build(&self) -> ElaborateType
        where
            S: MultiStore<PathComponent>
    {
        ElaborateType::Int(self.name, self.path.build())
    }
}

impl<S> ElaborateTypeRecordBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: RecordId) -> Self {
        ElaborateTypeRecordBuilder {
            name,
            path: PathBuilder::new(store),
        }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a ElaborateType::Rec.
    pub fn build(&self) -> ElaborateType
        where
            S: MultiStore<PathComponent>
    {
        ElaborateType::Rec(self.name, self.path.build())
    }
}

impl<S> ElaborateTypeTupleBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self {
        ElaborateTypeTupleBuilder { tuple: TupleBuilder::new(store) }
    }

    /// Appends a field.
    pub fn push(&mut self, ty: ElaborateTypeId) -> &mut Self {
        self.tuple.push(ty);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.tuple.name(name);
        self
    }

    /// Builds a ElaborateType::Tuple.
    pub fn build(&self) -> ElaborateType
        where
            S: MultiStore<Identifier> + MultiStore<ElaborateTypeId>,
    {
        ElaborateType::Tuple(self.tuple.build())
    }
}

impl<S> ElaborateTypeUnresolvedBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: ItemIdentifier) -> Self {
        Self { name, path: PathBuilder::new(store), }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.path.push(item);
        self
    }

    /// Builds a ElaborateType::Unresolved.
    pub fn build(&self) -> ElaborateType
        where
            S: MultiStore<PathComponent>
    {
        ElaborateType::Unresolved(self.name, self.path.build())
    }
}


//
//  Implementation Details (Type)
//

#[derive(Clone, Debug)]
pub struct TypeIdFactory<S>(Rc<S>);

#[derive(Clone, Debug)]
pub struct TypeIdTupleBuilder<S> {
    builder: TypeTupleBuilder<S>,
}

impl<S> TypeIdFactory<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self { TypeIdFactory(store) }

    /// Shortcut: creates a Bool Type.
    pub fn bool_(&self) -> TypeId { TypeId::bool_() }

    /// Shortcut: creates a Int Type.
    pub fn int(&self) -> TypeId { TypeId::int() }

    /// Shortcut: creates a String Type.
    pub fn string(&self) -> TypeId { TypeId::string() }

    /// Shortcut: creates a Void Type.
    pub fn void(&self) -> TypeId { TypeId::void() }

    /// Creates a TypeIdEnumBuilder.
    pub fn enum_(&self, name: EnumId) -> TypeId
        where
            S: Store<Type, TypeId>
    {
        self.0.borrow_mut().push(Type::Enum(name), Range::default())
    }

    /// Creates a TypeIdInterfaceBuilder.
    pub fn interface(&self, name: InterfaceId) -> TypeId
        where
            S: Store<Type, TypeId>
    {
        self.0.borrow_mut().push(Type::Int(name), Range::default())
    }

    /// Creates a TypeIdRecordBuilder.
    pub fn record(&self, name: RecordId) -> TypeId
        where
            S: Store<Type, TypeId>
    {
        self.0.borrow_mut().push(Type::Rec(name), Range::default())
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
        self.0.borrow_mut().push(Type::Unresolved, Range::default())
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
            S: Store<Type, TypeId> + MultiStore<TypeId> + MultiStore<Identifier>,
    {
        let ty = self.builder.build();
        self.builder.tuple.store.borrow_mut().push(ty, Range::default())
    }
}


//
//  Implementation Details (Elaborate Type)
//

#[derive(Clone, Debug)]
pub struct ElaborateTypeIdFactory<S>(Rc<S>);

#[derive(Clone, Debug)]
pub struct ElaborateTypeIdEnumBuilder<S>{
    builder: ElaborateTypeEnumBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeIdInterfaceBuilder<S>{
    builder: ElaborateTypeInterfaceBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeIdRecordBuilder<S> {
    builder: ElaborateTypeRecordBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeIdTupleBuilder<S> {
    builder: ElaborateTypeTupleBuilder<S>,
}

#[derive(Clone, Debug)]
pub struct ElaborateTypeIdUnresolvedBuilder<S> {
    builder: ElaborateTypeUnresolvedBuilder<S>,
}

impl<S> ElaborateTypeIdFactory<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self { ElaborateTypeIdFactory(store) }

    /// Shortcut: creates a Bool ElaborateType.
    pub fn bool_(&self) -> ElaborateTypeId { ElaborateTypeId::bool_() }

    /// Shortcut: creates a Int ElaborateType.
    pub fn int(&self) -> ElaborateTypeId { ElaborateTypeId::int() }

    /// Shortcut: creates a String ElaborateType.
    pub fn string(&self) -> ElaborateTypeId { ElaborateTypeId::string() }

    /// Shortcut: creates a Void ElaborateType.
    pub fn void(&self) -> ElaborateTypeId { ElaborateTypeId::void() }

    /// Creates a ElaborateTypeIdEnumBuilder.
    pub fn enum_(&self, name: EnumId) -> ElaborateTypeIdEnumBuilder<S> {
        ElaborateTypeIdEnumBuilder::new(self.0.clone(), name)
    }

    /// Creates a ElaborateTypeIdInterfaceBuilder.
    pub fn interface(&self, name: InterfaceId) -> ElaborateTypeIdInterfaceBuilder<S> {
        ElaborateTypeIdInterfaceBuilder::new(self.0.clone(), name)
    }

    /// Creates a ElaborateTypeIdRecordBuilder.
    pub fn record(&self, name: RecordId) -> ElaborateTypeIdRecordBuilder<S> {
        ElaborateTypeIdRecordBuilder::new(self.0.clone(), name)
    }

    /// Creates a ElaborateTypeIdTupleBuilder.
    pub fn tuple(&self) -> ElaborateTypeIdTupleBuilder<S> {
        ElaborateTypeIdTupleBuilder::new(self.0.clone())
    }

    /// Shortcut: creates an unnamed Unresolved ElaborateType.
    pub fn unresolved(&self) -> ElaborateTypeId
        where
            S: Store<ElaborateType, ElaborateTypeId> + MultiStore<PathComponent>
    {
        self.unresolved_named(ItemIdentifier::unresolved()).build()
    }

    /// Creates a ElaborateTypeIdUnresolvedBuilder.
    pub fn unresolved_named(&self, name: ItemIdentifier)
        -> ElaborateTypeIdUnresolvedBuilder<S>
    {
        ElaborateTypeIdUnresolvedBuilder::new(self.0.clone(), name)
    }
}

impl<S> ElaborateTypeIdEnumBuilder<S> {
    /// Creates a new instance.
    pub fn new(store: Rc<S>, name: EnumId) -> Self {
        ElaborateTypeIdEnumBuilder { builder: ElaborateTypeEnumBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a ElaborateType::Enum.
    pub fn build(&self) -> ElaborateTypeId
        where
            S: Store<ElaborateType, ElaborateTypeId> + MultiStore<PathComponent>,
    {
        let ty = self.builder.build();
        self.builder.path.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> ElaborateTypeIdInterfaceBuilder<S> {
    /// Creates a new instance.
    pub fn new(store: Rc<S>, name: InterfaceId) -> Self {
        ElaborateTypeIdInterfaceBuilder { builder: ElaborateTypeInterfaceBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a ElaborateType::Int.
    pub fn build(&self) -> ElaborateTypeId
        where
            S: Store<ElaborateType, ElaborateTypeId> + MultiStore<PathComponent>,
    {
        let ty = self.builder.build();
        self.builder.path.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> ElaborateTypeIdRecordBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: RecordId) -> Self {
        ElaborateTypeIdRecordBuilder { builder: ElaborateTypeRecordBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a ElaborateType::Rec.
    pub fn build(&self) -> ElaborateTypeId
        where
            S: Store<ElaborateType, ElaborateTypeId> + MultiStore<PathComponent>,
    {
        let ty = self.builder.build();
        self.builder.path.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> ElaborateTypeIdTupleBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>) -> Self {
        ElaborateTypeIdTupleBuilder { builder: ElaborateTypeTupleBuilder::new(store) }
    }

    /// Appends a field.
    pub fn push(&mut self, typ: ElaborateTypeId) -> &mut Self {
        self.builder.push(typ);
        self
    }

    /// Names the last field.
    pub fn name(&mut self, name: ValueIdentifier) -> &mut Self {
        self.builder.name(name);
        self
    }

    /// Builds a ElaborateType::Tuple.
    pub fn build(&self) -> ElaborateTypeId
        where
            S: Store<ElaborateType, ElaborateTypeId> + MultiStore<ElaborateTypeId> + MultiStore<Identifier>,
    {
        let ty = self.builder.build();
        self.builder.tuple.store.borrow_mut().push(ty, Range::default())
    }
}

impl<S> ElaborateTypeIdUnresolvedBuilder<S> {
    /// Creates an instance.
    pub fn new(store: Rc<S>, name: ItemIdentifier) -> Self {
        ElaborateTypeIdUnresolvedBuilder { builder: ElaborateTypeUnresolvedBuilder::new(store, name) }
    }

    /// Appends a component to the path.
    pub fn push_component(&mut self, item: PathComponent) -> &mut Self {
        self.builder.push_component(item);
        self
    }

    /// Builds a ElaborateType::Unresolved.
    pub fn build(&self) -> ElaborateTypeId
        where
            S: Store<ElaborateType, ElaborateTypeId> + MultiStore<PathComponent>
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
    receiver: Option<ExpressionId>,
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

    /// Sets a type.
    pub fn type_(&mut self, ty: Type) -> &mut Self {
        self.typ = ty;
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
            .push_statements(self.statements.iter().copied());
        let expr = Expression::Block(stmts, self.expr.clone());

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    /// Creates a Block Value.
    pub fn build_with_type(&self) -> ExpressionId {
        let typ = self.compute_type();
        let range = self.compute_range();

        let stmts = self.tree.borrow_mut()
            .push_statements(self.statements.iter().copied());
        let expr = Expression::Block(stmts, self.expr.clone());

        self.tree.borrow_mut().push_expression(typ, expr, range)
    }

    fn compute_type(&self) -> Type {
        if self.typ != Type::unresolved() {
            self.typ
        } else if let Some(expr) = self.expr {
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
            receiver: None,
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

    /// Sets the receiver.
    pub fn receiver(&mut self, receiver: ExpressionId) -> &mut Self {
        self.receiver = Some(receiver);
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

    /// Shortcut: sets a user-defined method.
    pub fn method(&mut self, receiver: ExpressionId, method: FunctionId) -> &mut Self {
        self.receiver = Some(receiver);
        self.callable = Callable::Method(method);
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

    /// Pushes an unresolved user-defined method.
    pub fn push_method(&mut self, method: FunctionId) -> &mut Self {
        self.unresolved.push(Callable::Method(method));
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
                .push_callables(self.unresolved.iter().copied());
            Callable::Unresolved(unresolved)
        };

        let typ = self.typ;
        let range = self.compute_range(&callable);

        let args = self.arguments.build();
        let expr = Expression::Call(callable, self.receiver, args);

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
                Function(_) | Method(_) | Unresolved(_) => (0, 0),
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
            Type::Int(name, ..) => Implicit::ToInt(name, self.expr),
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
            .push_statements(self.statements.iter().copied());
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
    elaborate_type: ElaborateType,
    tuple: TupleBuilder<Tree, Id>,
    transformer: Box<dyn Fn(Tuple<Id>) -> E>,
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
    names: Vec<Identifier>,
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
        transformer: Box<dyn Fn(Tuple<Id>) -> E>,
    )
        -> Self
    {
        ConstructorBuilder {
            type_,
            elaborate_type: ElaborateType::default(),
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

    /// Specifies the elaborate type, if any.
    pub fn elaborate(&mut self, ty: ElaborateType) -> &mut Self {
        self.elaborate_type = ty;
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
            Id: Copy,
    {
        use std::convert::TryInto;

        let ty = self.type_;
        let el = if self.elaborate_type == ElaborateType::default() {
            ty.try_into().expect("Elaborate")
        } else {
            self.elaborate_type
        };
        let range = self.range;

        let element: E = (self.transformer)(self.tuple.build());

        let mut tree = self.tuple.store.borrow_mut();

        let result = TypedStore::push(&mut *tree, ty, element, range);
        TypedStore::set_elaborate_type(&mut *tree, result, el);
        result
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
        let len = self.fields.len();
        if len > 0 && self.names.len() == self.fields.len() - 1 {
            self.names.push(name.id());
        }
        self
    }

    fn build(&self) -> Tuple<T>
        where
            S: MultiStore<T> + MultiStore<Identifier>,
    {
        let names = self.store.borrow_mut().push_slice(&self.names);
        self.build_named(names)
    }

    fn build_named(&self, names: Id<[Identifier]>) -> Tuple<T>
        where
            S: MultiStore<T> + MultiStore<Identifier>,
    {
        {
            let store = self.store.borrow();
            let ns = store.get_slice(names);

            //  FIXME(matthieu-m): use is_sorted_by_key.
            debug_assert!(ns.windows(2).all(|w| w[0] <= w[1]),
                "{:?} ({:?}) is not sorted", names, ns);
        }

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
