//! Tree
//!
//! A stand-alone flat representation of the tree which composes an item,
//! either the body of a function, or a stand-alone expression.
//!
//! A stand-alone representation has the benefit that the Tree may be
//! understood in isolation.
//!
//! A flat representation has 3 key benefits:
//! -   Direct addressing: it is possible to keep the ID of any node.
//! -   Efficient allocation: few allocations, and compact representation.
//! -   Efficient in-place mutation.
//!
//! Furthermore, those benefits are achieved whilst retaining control over
//! mutability, allowing to "freeze" the graph at some point, unlike inner
//! mutability which persists long after it ceased to be necessary.
//!
//! Note:   this layout is inspired by the realization that ECS are a great fit
//!         for Rust.

use crate::basic::com::{Range, Store, MultiStore};
use crate::basic::sea::{MultiTable, Table};

use crate::model::hir::*;

//
//  Public Types
//

/// Tree.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Tree {
    //  # Invariants
    //
    //  An element of the tree should only refer to elements that already
    //  existed (in tree) when this element was inserted. Furthermore, a given
    //  element should only be referred to once: this is a tree, not a DAG!
    //
    //  This is not enforced, and not checked. There may even be cycles.

    //  Root.

    /// Function Signature the Tree represents the body of, if any, as well as
    /// the patterns formed by its arguments.
    ///
    /// The body itself is found in expr_root.
    function: Option<(FunctionSignature, Id<[PatternId]>)>,
    /// Expression the Tree represents.
    root: Option<ExpressionId>,

    //  Expressions.

    /// Range associated to a given expression.
    expr_range: Table<ExpressionId, Range>,
    /// TypeId associated to a given expression.
    expr_type: Table<ExpressionId, TypeId>,
    /// ElaborateTypeId associated to a given expression, if any.
    expr_elaborate_type: Table<ExpressionId, ElaborateTypeId>,
    /// Expression.
    expression: Table<ExpressionId, Expression>,

    //  Patterns.

    /// Range associated to a given pattern.
    pat_range: Table<PatternId, Range>,
    /// TypeId associated to a given pattern.
    pat_type: Table<PatternId, TypeId>,
    /// ElaborateTypeId associated to a given pattern, if any.
    pat_elaborate_type: Table<PatternId, ElaborateTypeId>,
    /// Pattern.
    pattern: Table<PatternId, Pattern>,


    //  Satellites.

    /// Elaborate Types.
    elaborate_types: Table<ElaborateTypeId, ElaborateType>,
    /// Types.
    tys: Table<TypeId, Type>,

    /// Unresolved callables.
    callables: KeyedMulti<Callable>,
    /// Elaborate Types of enums and tuples.
    elaborate_type_ids: KeyedMulti<ElaborateTypeId>,
    /// Expressions of calls, constructors and tuples.
    expression_ids: KeyedMulti<ExpressionId>,
    /// Names of constructors, records and tuples.
    names: KeyedMulti<Identifier>,
    /// Path components.
    paths: KeyedMulti<PathComponent>,
    /// Patterns of constructors and tuples.
    pattern_ids: KeyedMulti<PatternId>,
    /// Statements of blocks and loops.
    stmts: KeyedMulti<Statement>,
    /// Types of enums and tuples.
    type_ids: KeyedMulti<TypeId>,
}


//
//  Public Methods
//

impl Tree {
    //  Generic.

    /// Creates a new instance.
    pub fn new() -> Self { Tree::default() }


    //  Root.

    /// Returns the function signature, if any.
    pub fn get_function(&self) -> Option<FunctionSignature> {
        self.function.map(|(f, _)| f)
    }

    /// Returns the function arguments, if any.
    pub fn get_function_arguments(&self) -> Option<Id<[PatternId]>> {
        self.function.map(|(_, p)| p)
    }

    /// Sets the function signature.
    pub fn set_function(&mut self, fun: FunctionSignature, registry: &dyn Registry) {
        let arguments = if fun.arguments.is_empty() {
            &[]
        } else {
            registry.get_arguments(fun.arguments)
        };
        let types = if fun.argument_types.is_empty() {
            &[]
        } else {
            registry.get_type_ids(fun.argument_types)
        };
        debug_assert!(arguments.len() == types.len());

        let mut fields = Vec::with_capacity(types.len());
        for (&argument, &ty) in arguments.iter().zip(types) {
            let ty = if let Some(b) = ty.builtin() {
                Type::Builtin(b)
            } else {
                registry.get_type(ty)
            };

            let pattern = Pattern::Var(argument);
            let pattern = self.push_pattern(ty, pattern, argument.1);

            fields.push(pattern);
        }

        let patterns = self.push_pattern_ids(fields);

        self.function = Some((fun, patterns));
    }

    /// Returns the expression, if any.
    pub fn get_root(&self) -> Option<ExpressionId> { self.root }

    /// Sets the root.
    pub fn set_root(&mut self, root: ExpressionId) { self.root = Some(root); }


    //  Gvns.

    /// Returns the type associated to a GVN.
    pub fn get_gvn_type_id(&self, gvn: Gvn) -> TypeId {
        if let Some(e) = gvn.as_expression() {
            self.get_expression_type_id(e)
        } else if let Some(p) = gvn.as_pattern() {
            self.get_pattern_type_id(p)
        } else {
            unreachable!("Indeterminate {:?}", gvn)
        }
    }

    /// Returns the type associated to a GVN.
    pub fn get_gvn_type(&self, gvn: Gvn) -> Type {
        let id = self.get_gvn_type_id(gvn);
        self.get_type(id)
    }

    /// Sets the type associated to a GVN.
    pub fn set_gvn_type(&mut self, gvn: Gvn, ty: Type) -> TypeId {
        if let Some(e) = gvn.as_expression() {
            self.set_expression_type(e, ty)
        } else if let Some(p) = gvn.as_pattern() {
            self.set_pattern_type(p, ty)
        } else {
            unreachable!("Indeterminate {:?}", gvn)
        }
    }

    /// Returns the elaborate type associated to a GVN, if any.
    pub fn get_gvn_elaborate_type_id(&self, gvn: Gvn) -> Option<ElaborateTypeId> {
        if let Some(e) = gvn.as_expression() {
            self.get_expression_elaborate_type_id(e)
        } else if let Some(p) = gvn.as_pattern() {
            self.get_pattern_elaborate_type_id(p)
        } else {
            unreachable!("Indeterminate {:?}", gvn)
        }
    }

    /// Returns the elaborate type associated to a GVN, if any.
    pub fn get_gvn_elaborate_type(&self, gvn: Gvn) -> Option<ElaborateType> {
        self.get_gvn_elaborate_type_id(gvn).map(|id| self.get_elaborate_type(id))
    }

    /// Sets the elaborate type associated to a GVN.
    pub fn set_gvn_elaborate_type(&mut self, gvn: Gvn, ty: ElaborateType)
        -> ElaborateTypeId
    {
        if let Some(e) = gvn.as_expression() {
            self.set_expression_elaborate_type(e, ty)
        } else if let Some(p) = gvn.as_pattern() {
            self.set_pattern_elaborate_type(p, ty)
        } else {
            unreachable!("Indeterminate {:?}", gvn)
        }
    }


    //  Expressions.

    /// Returns the number of expressions.
    pub fn len_expressions(&self) -> usize { self.expression.len() }

    /// Returns the expression IDs.
    pub fn get_expressions(&self) -> impl Iterator<Item = ExpressionId> {
        (0..self.expression.len()).into_iter()
            .map(|i| ExpressionId::new(i as u32))
    }

    /// Returns the range associated to an expression.
    pub fn get_expression_range(&self, id: ExpressionId) -> Range {
        *self.expr_range.at(&id)
    }

    /// Returns the type ID associated to an expression.
    pub fn get_expression_type_id(&self, id: ExpressionId) -> TypeId {
        *self.expr_type.at(&id)
    }

    /// Returns the type associated to an expression.
    pub fn get_expression_type(&self, id: ExpressionId) -> Type {
        let ty = self.get_expression_type_id(id);
        self.get_type(ty)
    }

    /// Returns the elaborate type ID associated to an expression, if any.
    pub fn get_expression_elaborate_type_id(&self, id: ExpressionId)
        -> Option<ElaborateTypeId>
    {
        let ty = *self.expr_elaborate_type.at(&id);
        if ty != ElaborateTypeId::default() {
            Some(ty)
        } else {
            None
        }
    }

    /// Returns the elaborate type associated to an expression, if any.
    pub fn get_expression_elaborate_type(&self, id: ExpressionId)
        -> Option<ElaborateType>
    {
        self.get_expression_elaborate_type_id(id)
            .map(|id| self.get_elaborate_type(id))
    }

    /// Returns the expression.
    pub fn get_expression(&self, id: ExpressionId) -> Expression {
        *self.expression.at(&id)
    }

    /// Inserts a new expression.
    ///
    /// Returns the ExpressionId created for it.
    pub fn push_expression(&mut self, typ: Type, expr: Expression, range: Range)
        -> ExpressionId
    {
        debug_assert!(self.expression.len() == self.expr_range.len());
        debug_assert!(self.expression.len() == self.expr_type.len());
        debug_assert!(self.expression.len() == self.expr_elaborate_type.len());

        let ty = self.push_type(typ);

        let id = self.expr_range.extend(range);
        self.expr_type.push(&id, ty);
        self.expr_elaborate_type.push(&id, Default::default());
        self.expression.push(&id, expr);

        id
    }

    /// Sets the expression.
    pub fn set_expression(&mut self, id: ExpressionId, e: Expression) {
        *self.expression.at_mut(&id) = e;
    }

    /// Sets the type associated to an expression.
    pub fn set_expression_type(&mut self, id: ExpressionId, typ: Type)
        -> TypeId
    {
        let ty = *self.expr_type.at_mut(&id);
        let ty = self.set_type(ty, typ);
        *self.expr_type.at_mut(&id) = ty;
        ty
    }

    /// Sets the elaborate type associated to an expression.
    pub fn set_expression_elaborate_type(
        &mut self,
        id: ExpressionId,
        typ: ElaborateType,
    )
        -> ElaborateTypeId
    {
        let ty = *self.expr_elaborate_type.at_mut(&id);
        let ty = self.set_elaborate_type(ty, typ);
        *self.expr_elaborate_type.at_mut(&id) = ty;
        ty
    }


    //  Patterns.

    /// Returns the number of patterns.
    pub fn len_patterns(&self) -> usize { self.pattern.len() }

    /// Returns the pattern IDs.
    pub fn get_patterns(&self) -> impl Iterator<Item = PatternId> {
        (0..self.pattern.len()).into_iter()
            .map(|i| PatternId::new(i as u32))
    }

    /// Returns the range associated to a pattern.
    pub fn get_pattern_range(&self, id: PatternId) -> Range {
        *self.pat_range.at(&id)
    }

    /// Returns the type ID associated to an pattern.
    pub fn get_pattern_type_id(&self, id: PatternId) -> TypeId {
        *self.pat_type.at(&id)
    }

    /// Returns the type associated to a pattern.
    pub fn get_pattern_type(&self, id: PatternId) -> Type {
        let ty = self.get_pattern_type_id(id);
        self.get_type(ty)
    }

    /// Returns the elaborate type ID associated to a pattern, if any.
    pub fn get_pattern_elaborate_type_id(&self, id: PatternId)
        -> Option<ElaborateTypeId>
    {
        let ty = *self.pat_elaborate_type.at(&id);
        if ty != ElaborateTypeId::default() {
            Some(ty)
        } else {
            None
        }
    }

    /// Returns the elaborate type associated to a pattern, if any.
    pub fn get_pattern_elaborate_type(&self, id: PatternId)
        -> Option<ElaborateType>
    {
        self.get_pattern_elaborate_type_id(id)
            .map(|id| self.get_elaborate_type(id))
    }

    /// Returns the pattern associated to the id.
    pub fn get_pattern(&self, id: PatternId) -> Pattern {
        *self.pattern.at(&id)
    }

    /// Inserts a new pattern.
    ///
    /// Returns the PatternId created for it.
    pub fn push_pattern(&mut self, typ: Type, pattern: Pattern, range: Range)
        -> PatternId
    {
        debug_assert!(self.pattern.len() == self.pat_range.len());
        debug_assert!(self.pattern.len() == self.pat_type.len());
        debug_assert!(self.pattern.len() == self.pat_elaborate_type.len());

        let ty = self.push_type(typ);

        let id = self.pat_range.extend(range);
        self.pat_type.push(&id, ty);
        self.pat_elaborate_type.push(&id, Default::default());
        self.pattern.push(&id, pattern);

        id
    }

    /// Sets the pattern associated to the id.
    pub fn set_pattern(&mut self, id: PatternId, p: Pattern) {
        *self.pattern.at_mut(&id) = p;
    }

    /// Sets the type associated to a pattern.
    pub fn set_pattern_type(&mut self, id: PatternId, typ: Type) -> TypeId {
        let ty = *self.pat_type.at_mut(&id);
        let ty = self.set_type(ty, typ);
        *self.pat_type.at_mut(&id) = ty;
        ty
    }

    /// Sets the elaborate type associated to a pattern.
    pub fn set_pattern_elaborate_type(
        &mut self,
        id: PatternId,
        typ: ElaborateType,
    )
        -> ElaborateTypeId
    {
        let ty = *self.pat_elaborate_type.at_mut(&id);
        let ty = self.set_elaborate_type(ty, typ);
        *self.pat_elaborate_type.at_mut(&id) = ty;
        ty
    }


    //  Satellites.

    /// Returns the number of elaborate types.
    pub fn len_elaborate_types(&self) -> usize { self.elaborate_types.len() }

    /// Returns the elaborate type associated to the id.
    pub fn get_elaborate_type(&self, id: ElaborateTypeId) -> ElaborateType {
        if let Some(b) = id.builtin() {
            ElaborateType::Builtin(b)
        } else {
            *self.elaborate_types.at(&id)
        }
    }

    /// Sets the elaborate type associated to the id.
    ///
    /// #   Panics
    ///
    /// Panics if attempting to associate a non built-in ElaborateType to a
    /// built-in ElaborateTypeId.
    pub fn set_elaborate_type(&mut self, id: ElaborateTypeId, ty: ElaborateType)
        -> ElaborateTypeId
    {
        if let Some(_) = id.builtin() {
            if let ElaborateType::Builtin(b) = ty {
                return ElaborateTypeId::from(b);
            }

            panic!("Cannot update a built-in to a non built-in");
        }

        let id = if id == Default::default() {
            self.push_elaborate_type(ty)
        } else {
            *self.elaborate_types.at_mut(&id) = ty;
            id
        };

        if let ElaborateType::Builtin(b) = ty {
            ElaborateTypeId::from(b)
        } else {
            id
        }
    }

    /// Inserts a new elaborate type.
    ///
    /// Returns the id created for it.
    pub fn push_elaborate_type(&mut self, typ: ElaborateType)
        -> ElaborateTypeId
    {
        Self::push_elaborate_type_impl(&mut self.elaborate_types, typ)
    }


    /// Returns the number of types.
    pub fn len_types(&self) -> usize { self.tys.len() }

    /// Returns the type associated to the id.
    pub fn get_type(&self, id: TypeId) -> Type {
        if let Some(b) = id.builtin() {
            Type::Builtin(b)
        } else {
            *self.tys.at(&id)
        }
    }

    /// Sets the type associated to the id.
    ///
    /// #   Panics
    ///
    /// Panics if attempting to associate a non built-in Type to a
    /// built-in TypeId.
    pub fn set_type(&mut self, id: TypeId, ty: Type) -> TypeId {
        if let Some(_) = id.builtin() {
            if let Type::Builtin(b) = ty {
                return TypeId::from(b);
            }

            panic!("Cannot update a built-in to a non built-in");
        }

        *self.tys.at_mut(&id) = ty;

        if let Type::Builtin(b) = ty {
            TypeId::from(b)
        } else {
            id
        }
    }

    /// Inserts a new type.
    ///
    /// Returns the id created for it.
    pub fn push_type(&mut self, typ: Type) -> TypeId {
        Self::push_type_impl(&mut self.tys, typ)
    }


    /// Returns the callables associated to the id.
    pub fn get_callables(&self, id: Id<[Callable]>) -> &[Callable] {
        if id.is_empty() { &[] } else { self.callables.get(&id) }
    }

    /// Inserts a new array of callables.
    ///
    /// Returns the id created for it.
    pub fn push_callables<I>(&mut self, callables: I) -> Id<[Callable]>
        where
            I: IntoIterator<Item = Callable>,
    {
        self.callables.create(callables).unwrap_or(Id::empty())
    }


    /// Returns the number of type ids.
    pub fn len_elaborate_type_ids(&self) -> usize { self.elaborate_type_ids.len() }

    /// Returns the type ids associated to the id.
    pub fn get_elaborate_type_ids(&self, id: Id<[ElaborateTypeId]>)
        -> &[ElaborateTypeId]
    {
        if id.is_empty() { &[] } else { self.elaborate_type_ids.get(&id) }
    }

    /// Inserts a new array of type ids.
    ///
    /// Returns the id created for it.
    pub fn push_elaborate_type_ids<I>(&mut self, types: I)
        -> Id<[ElaborateTypeId]>
        where
            I: IntoIterator<Item = ElaborateTypeId>,
    {
        self.elaborate_type_ids.create(types).unwrap_or(Id::empty())
    }

    /// Inserts a new array of types.
    ///
    /// Returns the id created for it.
    pub fn push_elaborate_types<I>(&mut self, types: I) -> Id<[ElaborateTypeId]>
        where
            I: IntoIterator<Item = ElaborateType>,
    {
        let tys = &mut self.elaborate_types;
        self.elaborate_type_ids
            .create(
                types.into_iter()
                    .map(|t| Self::push_elaborate_type_impl(tys, t))
            )
            .unwrap_or(Id::empty())
    }


    /// Returns the expressions associated to the id.
    pub fn get_expression_ids(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] {
        if id.is_empty() { &[] } else { self.expression_ids.get(&id) }
    }

    /// Inserts a new array of expressions.
    ///
    /// Returns the id created for it.
    pub fn push_expression_ids<I>(&mut self, expressions: I) -> Id<[ExpressionId]>
        where
            I: IntoIterator<Item = ExpressionId>,
    {
        self.expression_ids.create(expressions).unwrap_or(Id::empty())
    }


    /// Returns the number of names.
    pub fn len_names(&self) -> usize { self.names.len() }

    /// Returns the names associated to the id.
    pub fn get_names(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id.is_empty() { &[] } else { self.names.get(&id) }
    }

    /// Inserts a new array of names.
    ///
    /// Returns the id created for it.
    pub fn push_names<I>(&mut self, names: I) -> Id<[Identifier]>
        where
            I: IntoIterator<Item = Identifier>,
    {
        self.names.create(names).unwrap_or(Id::empty())
    }


    /// Returns the number of paths.
    pub fn len_paths(&self) -> usize { self.paths.len() }

    /// Returns the path associated to the id.
    pub fn get_path(&self, id: Id<[PathComponent]>) -> &[PathComponent] {
        if id.is_empty() { &[] } else { self.paths.get(&id) }
    }

    /// Inserts a new array of path components.
    ///
    /// Returns the id created for it.
    pub fn push_path<I>(&mut self, path: I) -> Id<[PathComponent]>
        where
            I: IntoIterator<Item = PathComponent>,
    {
        self.paths.create(path).unwrap_or(Id::empty())
    }


    /// Returns the patterns associated to the id.
    pub fn get_pattern_ids(&self, id: Id<[PatternId]>) -> &[PatternId] {
        if id.is_empty() { &[] } else { self.pattern_ids.get(&id) }
    }

    /// Inserts a new array of patterns.
    ///
    /// Returns the id created for it.
    pub fn push_pattern_ids<I>(&mut self, patterns: I) -> Id<[PatternId]>
        where
            I: IntoIterator<Item = PatternId>,
    {
        self.pattern_ids.create(patterns).unwrap_or(Id::empty())
    }


    /// Returns the statements associated to the id.
    pub fn get_statements(&self, id: Id<[Statement]>) -> &[Statement] {
        if id.is_empty() { &[] } else { self.stmts.get(&id) }
    }

    /// Inserts a new array of statements.
    ///
    /// Returns the id created for it.
    pub fn push_statements<I>(&mut self, stmts: I) -> Id<[Statement]>
        where
            I: IntoIterator<Item = Statement>,
    {
        self.stmts.create(stmts).unwrap_or(Id::empty())
    }


    /// Returns the number of type ids.
    pub fn len_type_ids(&self) -> usize { self.type_ids.len() }

    /// Returns the type ids associated to the id.
    pub fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id.is_empty() { &[] } else { self.type_ids.get(&id) }
    }

    /// Inserts a new array of type ids.
    ///
    /// Returns the id created for it.
    pub fn push_type_ids<I>(&mut self, types: I) -> Id<[TypeId]>
        where
            I: IntoIterator<Item = TypeId>,
    {
        self.type_ids.create(types).unwrap_or(Id::empty())
    }

    /// Inserts a new array of types.
    ///
    /// Returns the id created for it.
    pub fn push_types<I>(&mut self, types: I) -> Id<[TypeId]>
        where
            I: IntoIterator<Item = Type>,
    {
        let tys = &mut self.tys;
        self.type_ids
            .create(
                types.into_iter()
                    .map(|t| Self::push_type_impl(tys, t))
            )
            .unwrap_or(Id::empty())
    }
}

impl Tree {
    fn push_elaborate_type_impl(
        table: &mut Table<ElaborateTypeId, ElaborateType>,
        typ: ElaborateType,
    )
        -> ElaborateTypeId
    {
        let id = table.extend(typ);

        if let ElaborateType::Builtin(b) = typ {
            ElaborateTypeId::from(b)
        } else {
            id
        }
    }

    fn push_type_impl(table: &mut Table<TypeId, Type>, typ: Type) -> TypeId {
        let id = table.extend(typ);

        if let Type::Builtin(b) = typ {
            TypeId::from(b)
        } else {
            id
        }
    }
}


//
//  Implementations of TypedStore for Tree
//

/// Abstraction over Expression and Pattern
pub trait TypedStore<I> {
    /// Type corresponding to the Id.
    type Element;

    /// Number of items.
    fn len(&self) -> usize;

    /// Returns the range associated to the element.
    fn get_range(&self, id: I) -> Range;

    /// Returns the Type Id associated to the element.
    fn get_type(&self, id: I) -> TypeId;

    /// Returns the element.
    fn get(&self, id: I) -> Self::Element;

    /// Pushes a new element.
    fn push(&mut self, typ: Type, element: Self::Element, range: Range) -> I;

    /// Sets the elaborate type of an element.
    fn set_elaborate_type(&mut self, id: I, typ: ElaborateType) -> ElaborateTypeId;
}

impl TypedStore<ExpressionId> for Tree {
    type Element = Expression;

    fn len(&self) -> usize { self.len_expressions() }

    fn get_range(&self, id: ExpressionId) -> Range { self.get_expression_range(id) }

    fn get_type(&self, id: ExpressionId) -> TypeId { self.get_expression_type_id(id) }

    fn get(&self, id: ExpressionId) -> Expression { self.get_expression(id) }

    fn push(&mut self, typ: Type, element: Expression, range: Range) -> ExpressionId {
        self.push_expression(typ, element, range)
    }

    fn set_elaborate_type(&mut self, id: ExpressionId, typ: ElaborateType)
        -> ElaborateTypeId
    {
        self.set_expression_elaborate_type(id, typ)
    }
}

impl TypedStore<PatternId> for Tree {
    type Element = Pattern;

    fn len(&self) -> usize { self.len_patterns() }

    fn get_range(&self, id: PatternId) -> Range { self.get_pattern_range(id) }

    fn get_type(&self, id: PatternId) -> TypeId { self.get_pattern_type_id(id) }

    fn get(&self, id: PatternId) -> Pattern { self.get_pattern(id) }

    fn push(&mut self, typ: Type, element: Pattern, range: Range) -> PatternId {
        self.push_pattern(typ, element, range)
    }

    fn set_elaborate_type(&mut self, id: PatternId, typ: ElaborateType)
        -> ElaborateTypeId
    {
        self.set_pattern_elaborate_type(id, typ)
    }
}


//
//  Implementations of Store for Tree
//

impl Store<ElaborateType, ElaborateTypeId> for Tree {
    fn len(&self) -> usize { self.len_elaborate_types() }

    fn get(&self, id: ElaborateTypeId) -> ElaborateType {
        self.get_elaborate_type(id)
    }

    fn get_range(&self, _: ElaborateTypeId) -> Range {
        unimplemented!("<Tree as Store<ElaborateType>>::get_range")
    }

    fn push(&mut self, item: ElaborateType, _: Range) -> ElaborateTypeId {
        self.push_elaborate_type(item)
    }
}

impl Store<Type, TypeId> for Tree {
    fn len(&self) -> usize { self.len_types() }

    fn get(&self, id: TypeId) -> Type { self.get_type(id) }

    fn get_range(&self, _: TypeId) -> Range {
        unimplemented!("<Tree as Store<Type>>::get_range")
    }

    fn push(&mut self, item: Type, _: Range) -> TypeId {
        self.push_type(item)
    }
}


//
//  Implementations of MultiStore for Tree
//

impl MultiStore<Callable> for Tree {
    fn get_slice(&self, id: Id<[Callable]>) -> &[Callable] {
        self.get_callables(id)
    }

    fn push_slice(&mut self, items: &[Callable]) -> Id<[Callable]> {
        self.push_callables(items.iter().copied())
    }
}

impl MultiStore<ElaborateTypeId> for Tree {
    fn get_slice(&self, id: Id<[ElaborateTypeId]>) -> &[ElaborateTypeId] {
        self.get_elaborate_type_ids(id)
    }

    fn push_slice(&mut self, items: &[ElaborateTypeId]) -> Id<[ElaborateTypeId]> {
        self.push_elaborate_type_ids(items.iter().copied())
    }
}

impl MultiStore<ExpressionId> for Tree {
    fn get_slice(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] {
        self.get_expression_ids(id)
    }

    fn push_slice(&mut self, items: &[ExpressionId]) -> Id<[ExpressionId]> {
        self.push_expression_ids(items.iter().copied())
    }
}

impl MultiStore<Identifier> for Tree {
    fn get_slice(&self, id: Id<[Identifier]>) -> &[Identifier] {
        self.get_names(id)
    }

    fn push_slice(&mut self, items: &[Identifier]) -> Id<[Identifier]> {
        self.push_names(items.iter().copied())
    }
}

impl MultiStore<PathComponent> for Tree {
    fn get_slice(&self, id: Id<[PathComponent]>) -> &[PathComponent] {
        self.get_path(id)
    }

    fn push_slice(&mut self, items: &[PathComponent]) -> Id<[PathComponent]> {
        self.push_path(items.iter().copied())
    }
}

impl MultiStore<PatternId> for Tree {
    fn get_slice(&self, id: Id<[PatternId]>) -> &[PatternId] {
        self.get_pattern_ids(id)
    }

    fn push_slice(&mut self, items: &[PatternId]) -> Id<[PatternId]> {
        self.push_pattern_ids(items.iter().copied())
    }
}

impl MultiStore<Statement> for Tree {
    fn get_slice(&self, id: Id<[Statement]>) -> &[Statement] {
        self.get_statements(id)
    }

    fn push_slice(&mut self, items: &[Statement]) -> Id<[Statement]> {
        self.push_statements(items.iter().copied())
    }
}

impl MultiStore<TypeId> for Tree {
    fn get_slice(&self, id: Id<[TypeId]>) -> &[TypeId] {
        self.get_type_ids(id)
    }

    fn push_slice(&mut self, items: &[TypeId]) -> Id<[TypeId]> {
        self.push_type_ids(items.iter().copied())
    }
}


//
//  Private Types
//

type KeyedMulti<T> = MultiTable<Id<[T]>, T>;


//
//  Private Trait Implementations
//

#[cfg(test)]
pub mod samples {
    use super::*;

    /// A complete Tree corresponding to the following:
    ///
    /// "true" OR "false", depending on the parameter.
    pub fn bool_expression(b: bool) -> Tree {
        let mut tree = Tree::default();

        let range = range(0, if b { 4 } else { 5 });
        let bool_ = tree.push_expression(Type::bool_(), Expression::bool_(b), range);

        tree.set_root(bool_);

        tree
    }

    /// A complete Tree corresponding to the following:
    ///
    /// "1 + 2"
    pub fn add_expression() -> Tree {
        let mut tree = Tree::default();

        let one = tree.push_expression(Type::int(), Expression::int(1), range(0, 1));
        let two = tree.push_expression(Type::int(), Expression::int(2), range(4, 1));

        let args = tree.push_expression_ids([one, two].iter().copied());
        let args = Tuple::unnamed(args);

        let op = Callable::Builtin(BuiltinFunction::Add);
        let add = tree.push_expression(Type::int(), Expression::Call(op, None, args), range(0, 5));

        tree.set_root(add);

        tree
    }

    fn range(pos: usize, len: usize) -> Range {
        Range::new(pos, len)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn add_expression() {
        let tree = samples::add_expression();

        let (one, two) = (ExpressionId::new(0), ExpressionId::new(1));

        assert_eq!(
            tree.get_expression(one),
            Expression::int(1)
        );
        assert_eq!(
            tree.get_expression(two),
            Expression::int(2)
        );
        assert_eq!(
            tree.get_expression(ExpressionId::new(2)),
            Expression::Call(
                Callable::Builtin(BuiltinFunction::Add),
                None,
                Tuple::unnamed(Id::new(0)),
            )
        );
        assert_eq!(
            tree.get_expression_ids(Id::new(0)),
            &[one, two]
        );
    }
}
