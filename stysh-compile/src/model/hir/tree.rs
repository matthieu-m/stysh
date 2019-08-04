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

use basic::com::{Range, Store, MultiStore};
use basic::sea::{MultiTable, Table};

use model::hir::*;

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
    function: Option<(FunctionSignature, Tuple<PatternId>)>,
    /// Expression the Tree represents.
    root: Option<ExpressionId>,

    //  Expressions.

    /// Range associated to a given expression.
    expr_range: Table<ExpressionId, Range>,
    /// TypeId associated to a given expression.
    expr_type: Table<ExpressionId, TypeId>,
    /// Expression.
    expression: Table<ExpressionId, Expression>,

    //  Patterns.

    /// Range associated to a given pattern.
    pat_range: Table<PatternId, Range>,
    /// TypeId associated to a given pattern.
    pat_type: Table<PatternId, TypeId>,
    /// Pattern.
    pattern: Table<PatternId, Pattern>,


    //  Satellites.

    /// Types.
    tys: Table<TypeId, Type>,

    /// Unresolved callables.
    callables: KeyedMulti<Callable>,
    /// Expressions of calls, constructors and tuples.
    expressions: KeyedMulti<ExpressionId>,
    /// Names of constructors, records and tuples.
    names: KeyedMulti<ValueIdentifier>,
    /// Path components.
    paths: KeyedMulti<PathComponent>,
    /// Patterns of constructors and tuples.
    patterns: KeyedMulti<PatternId>,
    /// Statements of blocks and loops.
    stmts: KeyedMulti<Statement>,
    /// Types of enums and tuples.
    types: KeyedMulti<TypeId>,
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
    pub fn get_function_arguments(&self) -> Option<Tuple<PatternId>> {
        self.function.map(|(_, p)| p)
    }

    /// Sets the function signature.
    pub fn set_function(&mut self, fun: FunctionSignature, registry: &Registry) {
        let names = if fun.arguments.names.is_empty() {
            &[]
        } else {
            registry.get_names(fun.arguments.names)
        };
        let types = if fun.arguments.fields.is_empty() {
            &[]
        } else {
            registry.get_type_ids(fun.arguments.fields)
        };
        debug_assert!(names.len() == types.len());

        let mut fields = Vec::with_capacity(types.len());
        for (&name, &ty) in names.iter().zip(types) {
            let ty = if let Some(b) = ty.builtin() {
                Type::Builtin(b)
            } else {
                registry.get_type(ty)
            };

            let pattern = Pattern::Var(name);
            let pattern = self.push_pattern(ty, pattern, name.1);

            fields.push(pattern);
        }

        let fields = self.push_patterns(fields);
        let names = self.push_names(names.iter().cloned());
        let arguments = Tuple { fields, names, };

        self.function = Some((fun, arguments));
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
    pub fn set_gvn_type(&mut self, gvn: Gvn, ty: Type) {
        if let Some(e) = gvn.as_expression() {
            self.set_expression_type(e, ty)
        } else if let Some(p) = gvn.as_pattern() {
            self.set_pattern_type(p, ty)
        } else {
            unreachable!("Indeterminate {:?}", gvn)
        }
    }


    //  Expressions.

    /// Returns the number of expressions.
    pub fn len_expressions(&self) -> usize { self.expression.len() }

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

        let ty = self.push_type(typ);

        let id = self.expr_range.extend(range);
        self.expr_type.push(&id, ty);
        self.expression.push(&id, expr);

        id
    }

    /// Sets the expression.
    pub fn set_expression(&mut self, id: ExpressionId, e: Expression) {
        *self.expression.at_mut(&id) = e;
    }

    /// Sets the type associated to an expression.
    pub fn set_expression_type(&mut self, id: ExpressionId, typ: Type) {
        let ty = *self.expr_type.at_mut(&id);
        let ty = self.set_type(ty, typ);
        *self.expr_type.at_mut(&id) = ty;
    }


    //  Patterns.

    /// Returns the number of patterns.
    pub fn len_patterns(&self) -> usize { self.pattern.len() }

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

        let ty = self.push_type(typ);

        let id = self.pat_range.extend(range);
        self.pat_type.push(&id, ty);
        self.pattern.push(&id, pattern);

        id
    }

    /// Sets the pattern associated to the id.
    pub fn set_pattern(&mut self, id: PatternId, p: Pattern) {
        *self.pattern.at_mut(&id) = p;
    }

    /// Sets the type associated to a pattern.
    pub fn set_pattern_type(&mut self, id: PatternId, typ: Type) {
        let ty = *self.pat_type.at_mut(&id);
        let ty = self.set_type(ty, typ);
        *self.pat_type.at_mut(&id) = ty;
    }


    //  Satellites.

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


    /// Returns the expressions associated to the id.
    pub fn get_expressions(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] {
        if id.is_empty() { &[] } else { self.expressions.get(&id) }
    }

    /// Inserts a new array of expressions.
    ///
    /// Returns the id created for it.
    pub fn push_expressions<I>(&mut self, expressions: I) -> Id<[ExpressionId]>
        where
            I: IntoIterator<Item = ExpressionId>,
    {
        self.expressions.create(expressions).unwrap_or(Id::empty())
    }


    /// Returns the number of names.
    pub fn len_names(&self) -> usize { self.names.len() }

    /// Returns the names associated to the id.
    pub fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        if id.is_empty() { &[] } else { self.names.get(&id) }
    }

    /// Inserts a new array of names.
    ///
    /// Returns the id created for it.
    pub fn push_names<I>(&mut self, names: I) -> Id<[ValueIdentifier]>
        where
            I: IntoIterator<Item = ValueIdentifier>,
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
    pub fn get_patterns(&self, id: Id<[PatternId]>) -> &[PatternId] {
        if id.is_empty() { &[] } else { self.patterns.get(&id) }
    }

    /// Inserts a new array of patterns.
    ///
    /// Returns the id created for it.
    pub fn push_patterns<I>(&mut self, patterns: I) -> Id<[PatternId]>
        where
            I: IntoIterator<Item = PatternId>,
    {
        self.patterns.create(patterns).unwrap_or(Id::empty())
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
    pub fn len_type_ids(&self) -> usize { self.types.len() }

    /// Returns the type ids associated to the id.
    pub fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id.is_empty() { &[] } else { self.types.get(&id) }
    }

    /// Inserts a new array of type ids.
    ///
    /// Returns the id created for it.
    pub fn push_type_ids<I>(&mut self, types: I) -> Id<[TypeId]>
        where
            I: IntoIterator<Item = TypeId>,
    {
        self.types.create(types).unwrap_or(Id::empty())
    }

    /// Inserts a new array of types.
    ///
    /// Returns the id created for it.
    pub fn push_types<I>(&mut self, types: I) -> Id<[TypeId]>
        where
            I: IntoIterator<Item = Type>,
    {
        let tys = &mut self.tys;
        self.types
            .create(
                types.into_iter()
                    .map(|t| Self::push_type_impl(tys, t))
            )
            .unwrap_or(Id::empty())
    }
}

impl Tree {
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
}


//
//  Implementations of Store for Tree
//

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
        self.push_callables(items.iter().cloned())
    }
}

impl MultiStore<ExpressionId> for Tree {
    fn get_slice(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] {
        self.get_expressions(id)
    }

    fn push_slice(&mut self, items: &[ExpressionId]) -> Id<[ExpressionId]> {
        self.push_expressions(items.iter().cloned())
    }
}

impl MultiStore<ValueIdentifier> for Tree {
    fn get_slice(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        self.get_names(id)
    }

    fn push_slice(&mut self, items: &[ValueIdentifier]) -> Id<[ValueIdentifier]> {
        self.push_names(items.iter().cloned())
    }
}

impl MultiStore<PathComponent> for Tree {
    fn get_slice(&self, id: Id<[PathComponent]>) -> &[PathComponent] {
        self.get_path(id)
    }

    fn push_slice(&mut self, items: &[PathComponent]) -> Id<[PathComponent]> {
        self.push_path(items.iter().cloned())
    }
}

impl MultiStore<PatternId> for Tree {
    fn get_slice(&self, id: Id<[PatternId]>) -> &[PatternId] {
        self.get_patterns(id)
    }

    fn push_slice(&mut self, items: &[PatternId]) -> Id<[PatternId]> {
        self.push_patterns(items.iter().cloned())
    }
}

impl MultiStore<Statement> for Tree {
    fn get_slice(&self, id: Id<[Statement]>) -> &[Statement] {
        self.get_statements(id)
    }

    fn push_slice(&mut self, items: &[Statement]) -> Id<[Statement]> {
        self.push_statements(items.iter().cloned())
    }
}

impl MultiStore<TypeId> for Tree {
    fn get_slice(&self, id: Id<[TypeId]>) -> &[TypeId] {
        self.get_type_ids(id)
    }

    fn push_slice(&mut self, items: &[TypeId]) -> Id<[TypeId]> {
        self.push_type_ids(items.iter().cloned())
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

        let args = tree.push_expressions([one, two].iter().cloned());
        let args = Tuple::unnamed(args);

        let op = Callable::Builtin(BuiltinFunction::Add);
        let add = tree.push_expression(Type::int(), Expression::Call(op, args), range(0, 5));

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
                Tuple::unnamed(Id::new(0)),
            )
        );
        assert_eq!(
            tree.get_expressions(Id::new(0)),
            &[one, two]
        );
    }
}