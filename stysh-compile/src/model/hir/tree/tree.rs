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

use std::{cmp, fmt, iter};
use std::collections::HashMap;

use basic::com::Range;
use basic::sea::{MultiTable, Table, TableIndex};

use model::hir::{self, *};

//
//  Public Types
//

/// Tree.
#[derive(Clone, Debug, Default)]
pub struct Tree {
    //  # Invariants
    //
    //  An element of the tree should only refer to elements that already
    //  existed (in tree) when this element was inserted. Furthermore, a given
    //  element should only be referred to once: this is a tree, not a DAG!
    //
    //  This is not enforced, and not checked. There may even be cycles.

    //  Root.

    root: Option<Root>,

    //  Types.

    /// Enum name to list of records.
    enums: HashMap<Range, Id<[TypeId]>>,
    /// Record name to list of fields.
    records: HashMap<Range, Tuple<TypeId>>,

    //  Expressions.

    /// Range associated to a given expression.
    expr_range: Table<ExpressionId, Range>,
    /// TypeId associated to a given expression.
    expr_type: Table<ExpressionId, TypeId>,
    /// Expression.
    expression: Table<ExpressionId, Expr>,

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
    paths: KeyedMulti<ItemIdentifier>,
    /// Patterns of constructors and tuples.
    patterns: KeyedMulti<PatternId>,
    /// Statements of blocks and loops.
    stmts: KeyedMulti<Stmt>,
    /// Types of enums and tuples.
    types: KeyedMulti<TypeId>,
}

/// Root.
///
/// The root of the tree.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Root {
    /// Expression.
    Expression(ExpressionId),
    /// Function.
    ///
    /// A function definition is composed of:
    /// -   Its name.
    /// -   Its arguments, as a tuple of patterns.
    /// -   Its result type.
    /// -   Its body, as an expression.
    Function(ItemIdentifier, Tuple<PatternId>, TypeId, ExpressionId),
    /// Pattern.
    Pattern(PatternId),
}

/// ExpressionHandle.
///
/// Combines all elements of an Expression into a single handle.
#[derive(Clone, Copy, Debug)]
pub struct ExpressionHandle<'a> {
    /// Expression ID.
    pub id: ExpressionId,
    /// Expression.
    pub expr: &'a Expr,
    /// Type of the expression.
    pub typ: &'a Type,
    /// Range spanned by the expression.
    pub range: Range,
}

/// ExpressionHandleMut.
///
/// Combines all elements of an Expression into a single handle.
#[derive(Debug)]
pub struct ExpressionHandleMut<'a> {
    /// Expression ID.
    pub id: ExpressionId,
    /// Expression.
    pub expr: &'a mut Expr,
    /// Type of the expression.
    pub typ: &'a mut Type,
    /// Range spanned by the expression.
    pub range: &'a mut Range,
}

/// PatternHandle.
///
/// Combines all elements of an Pattern into a single handle.
#[derive(Clone, Copy, Debug)]
pub struct PatternHandle<'a> {
    /// Pattern ID.
    pub id: PatternId,
    /// Pattern.
    pub pattern: &'a Pattern,
    /// Type of the pattern.
    pub typ: &'a Type,
    /// Range spanned by the pattern.
    pub range: Range,
}

/// PatternHandleMut.
///
/// Combines all elements of an Pattern into a single handle.
#[derive(Debug)]
pub struct PatternHandleMut<'a> {
    /// Pattern ID.
    pub id: PatternId,
    /// Pattern.
    pub pattern: &'a mut Pattern,
    /// Type of the pattern.
    pub typ: &'a mut Type,
    /// Range spanned by the pattern.
    pub range: &'a mut Range,
}

//
//  Public Methods
//

impl Tree {
    //  Generic.

    /// Creates a new instance.
    pub fn new() -> Self { Tree::default() }


    //  Root.

    /// Returns the root.
    pub fn get_root(&self) -> Option<Root> { self.root.clone() }

    /// Returns the root.
    pub fn get_root_mut(&mut self) -> Option<&mut Root> {
        self.root.as_mut()
    }

    /// Sets the root.
    pub fn set_root(&mut self, root: Root) { self.root = Some(root); }

    /// Sets the root as Expression.
    pub fn set_root_expression(&mut self, expr: ExpressionId) {
        self.root = Some(Root::Expression(expr));
    }

    /// Sets the root as Function.
    pub fn set_root_function(
        &mut self,
        name: ItemIdentifier,
        arguments: Tuple<PatternId>,
        result: TypeId,
        body: ExpressionId,
    )
    {
        self.root = Some(Root::Function(name, arguments, result, body));
    }

    /// Sets the root as Pattern.
    pub fn set_root_pattern(&mut self, pattern: PatternId) {
        self.root = Some(Root::Pattern(pattern));
    }


    //  Types.

    /// Returns the enum's lightweight definition, or None.
    ///
    /// The result is None if and only if the enum is unknown.
    ///
    /// The slice of variants referred to may be empty even if the enum is
    /// known, such as for `:enum Never {}`.
    pub fn get_enum(&self, name: ItemIdentifier) -> Option<Type> {
        self.enums.get(&name.1)
            .map(|records| Type::Enum(name, PathId::empty(), *records))
    }

    /// Inserts the definition of an enum.
    ///
    /// If the enum already exists, the definition is NOT updated.
    pub fn insert_enum(&mut self, def: &Enum) -> Type {
        let name = def.prototype.name;
        let path = PathId::empty();

        if let Some(records) = self.enums.get(&name.1) {
            return Type::Enum(name, path, *records);
        }

        let mut records = vec!();
        for v in &def.variants {
            let ty = self.insert_record(&v);
            let ty = self.push_type(ty);
            records.push(ty);
        }
        let records = self.push_type_ids(&records);
        self.enums.insert(name.1, records);

        Type::Enum(name, path, records)
    }

    /// Returns the record's lightweight definition, or None.
    ///
    /// The result is None if and only if the record is unknown.
    pub fn get_record(&self, name: ItemIdentifier) -> Option<Type> {
        self.records.get(&name.1)
            .map(|tuple| Type::Rec(name, PathId::empty(), *tuple))
    }

    /// Inserts the definition of a record.
    ///
    /// If the record already exists, the definition is NOT updated.
    pub fn insert_record(&mut self, def: &Record) -> Type {
        let name = def.prototype.name;
        let path = PathId::empty();

        if let Some(fields) = self.records.get(&name.1) {
            return Type::Rec(name, path, *fields);
        }

        let fields = self.insert_types_tuple(&def.definition);
        self.records.insert(name.1, fields);

        Type::Rec(name, path, fields)
    }

    /// Returns an Iterator over the fields of each Record.
    pub fn iter_records<'a>(&'a self)
        -> impl iter::Iterator<Item = &'a Tuple<TypeId>>
    {
        self.records.values()
    }

    //  No `iter_records_mut`: links between Record and Field are immutable.


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
    pub fn get_gvn_type(&self, gvn: Gvn) -> &Type {
        let id = self.get_gvn_type_id(gvn);
        self.get_type(id)
    }

    /// Returns the type associated to a GVN.
    pub fn get_gvn_type_mut(&mut self, gvn: Gvn) -> &mut Type {
        let id = self.get_gvn_type_id(gvn);
        self.get_type_mut(id)
    }


    //  Expressions.

    /// Returns the number of expressions.
    pub fn len_expressions(&self) -> usize { self.expression.len() }

    /// Returns the range associated to an expression.
    pub fn get_expression_range(&self, id: ExpressionId) -> Range {
        *self.expr_range.at(&id)
    }

    /// Returns the range associated to an expression.
    pub fn get_expression_range_mut(&mut self, id: ExpressionId) -> &mut Range {
        self.expr_range.at_mut(&id)
    }

    /// Returns the type ID associated to an expression.
    pub fn get_expression_type_id(&self, id: ExpressionId) -> TypeId {
        *self.expr_type.at(&id)
    }

    //  No `get_expression_type_id_mut`: links between Expressions and Types are immutable.

    /// Returns the type associated to an expression.
    pub fn get_expression_type(&self, id: ExpressionId) -> &Type {
        let ty = self.get_expression_type_id(id);
        self.tys.at(&ty)
    }

    /// Returns the type associated to an expression.
    pub fn get_expression_type_mut(&mut self, id: ExpressionId) -> &mut Type {
        let ty = self.get_expression_type_id(id);
        self.tys.at_mut(&ty)
    }

    /// Returns the expression.
    pub fn get_expression(&self, id: ExpressionId) -> &Expr {
        self.expression.at(&id)
    }

    /// Returns the expression.
    pub fn get_expression_mut(&mut self, id: ExpressionId) -> &mut Expr {
        self.expression.at_mut(&id)
    }

    /// Returns the expression handle.
    pub fn get_expression_handle<'a>(&'a self, id: ExpressionId)
        -> ExpressionHandle<'a>
    {
        ExpressionHandle {
            id,
            expr: self.get_expression(id),
            typ: self.get_expression_type(id),
            range: self.get_expression_range(id),
        }
    }

    /// Returns the expression handle.
    pub fn get_expression_handle_mut<'a>(&'a mut self, id: ExpressionId)
        -> ExpressionHandleMut<'a>
    {
        let ty = self.get_expression_type_id(id);

        ExpressionHandleMut {
            id,
            expr: self.expression.at_mut(&id),
            typ: self.tys.at_mut(&ty),
            range: self.expr_range.at_mut(&id),
        }
    }

    /// Inserts a new expression.
    ///
    /// Returns the ExpressionId created for it.
    pub fn push_expression(&mut self, typ: Type, expr: Expr, range: Range)
        -> ExpressionId
    {
        let id = ExpressionId::new(self.expression.len() as u32);

        let ty = self.push_type(typ);

        self.expr_range.push(&id, range);
        self.expr_type.push(&id, ty);
        self.expression.push(&id, expr);

        id
    }

    /// Returns an Iterator over ExpressionHandle.
    pub fn iter_expression_handles<'a>(&'a self)
        -> impl iter::Iterator<Item = ExpressionHandle<'a>>
    {
        ExpressionIter { tree: self, index: 0 }
    }

    /// Returns an Iterator over ExpressionHandleMut.
    pub fn iter_expression_handles_mut<'a>(&'a mut self)
        -> impl iter::Iterator<Item = ExpressionHandleMut<'a>>
    {
        ExpressionIterMut { tree: self, index: 0 }
    }


    //  Patterns.

    /// Returns the range associated to a pattern.
    pub fn get_pattern_range(&self, id: PatternId) -> Range {
        *self.pat_range.at(&id)
    }

    /// Returns the range associated to a pattern.
    pub fn get_pattern_range_mut(&mut self, id: PatternId) -> &mut Range {
        self.pat_range.at_mut(&id)
    }

    /// Returns the type ID associated to an pattern.
    pub fn get_pattern_type_id(&self, id: PatternId) -> TypeId {
        *self.pat_type.at(&id)
    }

    //  No `get_pattern_type_id_mut`: links between Patterns and Types are immutable.

    /// Returns the type associated to a pattern.
    pub fn get_pattern_type(&self, id: PatternId) -> &Type {
        let ty = self.get_pattern_type_id(id);
        self.tys.at(&ty)
    }

    /// Returns the type associated to a pattern.
    pub fn get_pattern_type_mut(&mut self, id: PatternId) -> &mut Type {
        let ty = self.get_pattern_type_id(id);
        self.tys.at_mut(&ty)
    }

    /// Returns the pattern associated to the id.
    pub fn get_pattern(&self, id: PatternId) -> &Pattern {
        self.pattern.at(&id)
    }

    /// Returns the pattern associated to the id.
    pub fn get_pattern_mut(&mut self, id: PatternId) -> &mut Pattern {
        self.pattern.at_mut(&id)
    }

    /// Returns the pattern handle.
    pub fn get_pattern_handle<'a>(&'a self, id: PatternId)
        -> PatternHandle<'a>
    {
        PatternHandle {
            id,
            pattern: self.get_pattern(id),
            typ: self.get_pattern_type(id),
            range: self.get_pattern_range(id),
        }
    }

    /// Returns the pattern handle.
    pub fn get_pattern_handle_mut<'a>(&'a mut self, id: PatternId)
        -> PatternHandleMut<'a>
    {
        let ty = self.get_pattern_type_id(id);
        PatternHandleMut {
            id,
            pattern: self.pattern.at_mut(&id),
            typ: self.tys.at_mut(&ty),
            range: self.pat_range.at_mut(&id),
        }
    }

    /// Inserts a new pattern.
    ///
    /// Returns the PatternId created for it.
    pub fn push_pattern(&mut self, typ: Type, pattern: Pattern, range: Range)
        -> PatternId
    {
        let id = PatternId::new(self.pattern.len() as u32);

        let ty = self.push_type(typ);

        self.pat_range.push(&id, range);
        self.pat_type.push(&id, ty);
        self.pattern.push(&id, pattern);

        id
    }

    /// Returns an Iterator over PatternHandle.
    pub fn iter_pattern_handles<'a>(&'a self)
        -> impl iter::Iterator<Item = PatternHandle<'a>>
    {
        PatternIter { tree: self, index: 0 }
    }

    /// Returns an Iterator over PatternHandleMut.
    pub fn iter_pattern_handles_mut<'a>(&'a mut self)
        -> impl iter::Iterator<Item = PatternHandleMut<'a>>
    {
        PatternIterMut { tree: self, index: 0 }
    }


    //  Satellites.

    /// Returns the type associated to the id.
    pub fn get_type(&self, id: TypeId) -> &Type {
        self.tys.at(&id)
    }

    /// Returns the type associated to the id.
    pub fn get_type_mut(&mut self, id: TypeId) -> &mut Type {
        self.tys.at_mut(&id)
    }

    /// Sets the type associated to the id.
    pub fn set_type(&mut self, id: TypeId, ty: Type) {
        *self.tys.at_mut(&id) = ty;
    }

    /// Inserts a new type.
    ///
    /// Returns the id created for it.
    pub fn push_type(&mut self, typ: Type) -> TypeId {
        let ty = TypeId::new(self.tys.len() as u32);
        self.tys.push(&ty, typ);
        ty
    }

    /// Inserts a new type.
    ///
    /// Returns the id created for it.
    pub fn push_type_definition(&mut self, typ: &TypeDefinition) -> TypeId {
        use self::TypeDefinition::*;

        //  FIXME(matthieum): handle paths.
        match typ {
            Builtin(builtin) => self.push_type(Type::Builtin(*builtin)),
            Enum(e, _) => {
                let e = self.insert_enum(e);
                self.push_type(e)
            },
            Rec(rec, _) => {
                let rec = self.insert_record(rec);
                self.push_type(rec)
            },
            Tuple(tup) => {
                let fields = tup.fields.apply(|fs| {
                    let mut fields = vec!();
                    for f in fs {
                        fields.push(self.push_type_definition(f));
                    }
                    self.push_type_ids(&fields)
                });
                let names = tup.names.apply(|names| self.push_names(names));
                self.push_type(Type::Tuple(hir::Tuple { fields, names, }))
            },
            Unresolved(name, _) =>
                self.push_type(Type::Unresolved(*name, Default::default())),
            UnresolvedEnum(proto, _) =>
                self.push_type(Type::Unresolved(proto.name, Default::default())),
            UnresolvedRec(proto, _) =>
                self.push_type(Type::Unresolved(proto.name, Default::default())),
        }
    }

    /// Returns an Iterator over references to Types.
    pub fn iter_types<'a>(&'a self)
        -> impl iter::Iterator<Item = &'a Type>
    {
        self.tys.iter()
    }

    /// Returns an Iterator over mutable references to Types.
    pub fn iter_types_mut<'a>(&'a mut self)
        -> impl iter::Iterator<Item = &'a mut Type>
    {
        self.tys.iter_mut()
    }

    /// Returns the callables associated to the id.
    pub fn get_callables(&self, id: Id<[Callable]>) -> &[Callable] {
        self.callables.get(&id)
    }

    /// Returns the callables associated to the id.
    pub fn get_callables_mut(&mut self, id: Id<[Callable]>) -> &mut [Callable] {
        self.callables.get_mut(&id)
    }

    /// Inserts a new array of callables.
    ///
    /// Returns the id created for it.
    pub fn push_callables(&mut self, callables: &[Callable]) -> Id<[Callable]> {
        Self::push_slice(&mut self.callables, callables)
    }

    /// Returns an Iterator over Callables.
    pub fn iter_callables<'a>(&'a self)
        -> impl iter::Iterator<Item = &'a Callable>
    {
        self.callables.iter().flatten()
    }

    /// Returns an Iterator over mutable Callables.
    pub fn iter_callables_mut<'a>(&'a mut self)
        -> impl iter::Iterator<Item = &'a mut Callable>
    {
        self.callables.iter_mut().flatten()
    }

    /// Returns the expressions associated to the id.
    pub fn get_expressions(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] {
        self.expressions.get(&id)
    }

    /// Returns the expressions associated to the id.
    pub fn get_expressions_mut(&mut self, id: Id<[ExpressionId]>) -> &mut [ExpressionId] {
        self.expressions.get_mut(&id)
    }

    /// Inserts a new array of expressions.
    ///
    /// Returns the id created for it.
    pub fn push_expressions(&mut self, expressions: &[ExpressionId]) -> Id<[ExpressionId]> {
        Self::push_slice(&mut self.expressions, expressions)
    }

    /// Returns the names associated to the id.
    pub fn get_names(&self, id: Id<[ValueIdentifier]>) -> &[ValueIdentifier] {
        self.names.get(&id)
    }

    /// Returns the names associated to the id.
    pub fn get_names_mut(&mut self, id: Id<[ValueIdentifier]>)
        -> &mut [ValueIdentifier]
    {
        self.names.get_mut(&id)
    }

    /// Inserts a new array of names.
    ///
    /// Returns the id created for it.
    pub fn push_names(&mut self, names: &[ValueIdentifier])
        -> Id<[ValueIdentifier]>
    {
        Self::push_slice(&mut self.names, names)
    }

    /// Returns an Iterator over sequences of ValueIdentifier.
    pub fn iter_names<'a>(&'a self)
        -> impl iter::Iterator<Item = &'a ValueIdentifier>
    {
        self.names.iter().flatten()
    }

    /// Returns an Iterator over mutable sequences of ValueIdentifier.
    pub fn iter_names_mut<'a>(&'a mut self)
        -> impl iter::Iterator<Item = &'a mut ValueIdentifier>
    {
        self.names.iter_mut().flatten()
    }

    /// Returns the path associated to the id.
    pub fn get_path(&self, id: Id<[ItemIdentifier]>) -> &[ItemIdentifier] {
        self.paths.get(&id)
    }

    /// Returns the path associated to the id.
    pub fn get_path_mut(&mut self, id: Id<[ItemIdentifier]>) -> &mut [ItemIdentifier] {
        self.paths.get_mut(&id)
    }

    /// Inserts a new array of path components.
    ///
    /// Returns the id created for it.
    pub fn push_path(&mut self, path: &[ItemIdentifier]) -> Id<[ItemIdentifier]> {
        Self::push_slice(&mut self.paths, path)
    }

    /// Returns an Iterator over path components.
    pub fn iter_path<'a>(&'a self)
        -> impl iter::Iterator<Item = &'a ItemIdentifier>
    {
        self.paths.iter().flatten()
    }

    /// Returns an Iterator over mutable path components.
    pub fn iter_path_mut<'a>(&'a mut self)
        -> impl iter::Iterator<Item = &'a mut ItemIdentifier>
    {
        self.paths.iter_mut().flatten()
    }

    /// Returns the patterns associated to the id.
    pub fn get_patterns(&self, id: Id<[PatternId]>) -> &[PatternId] {
        self.patterns.get(&id)
    }

    /// Returns the get_patterns associated to the id.
    pub fn get_patterns_mut(&mut self, id: Id<[PatternId]>) -> &mut [PatternId] {
        self.patterns.get_mut(&id)
    }

    /// Inserts a new array of patterns.
    ///
    /// Returns the id created for it.
    pub fn push_patterns(&mut self, patterns: &[PatternId]) -> Id<[PatternId]> {
        Self::push_slice(&mut self.patterns, patterns)
    }

    /// Returns the statements associated to the id.
    pub fn get_statements(&self, id: Id<[Stmt]>) -> &[Stmt] {
        self.stmts.get(&id)
    }

    /// Returns the statements associated to the id.
    pub fn get_statements_mut(&mut self, id: Id<[Stmt]>) -> &mut [Stmt] {
        self.stmts.get_mut(&id)
    }

    /// Inserts a new array of statements.
    ///
    /// Returns the id created for it.
    pub fn push_statements(&mut self, stmts: &[Stmt]) -> Id<[Stmt]> {
        Self::push_slice(&mut self.stmts, stmts)
    }

    /// Returns the type ids associated to the id.
    pub fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        self.types.get(&id)
    }

    //  No `get_type_ids_mut`: links between types are immutable.

    /// Inserts a new array of type ids.
    ///
    /// Returns the id created for it.
    pub fn push_type_ids(&mut self, types: &[TypeId]) -> Id<[TypeId]> {
        Self::push_slice(&mut self.types, types)
    }

    /// Inserts a new array of types.
    ///
    /// Returns the id created for it.
    pub fn push_types(&mut self, types: &[Type]) -> Id<[TypeId]> {
        let types: Vec<_> = types.iter().map(|t| self.push_type(*t)).collect();
        Self::push_slice(&mut self.types, &types)
    }

    /// Returns an Iterator over type ids.
    pub fn iter_type_ids<'a>(&'a self)
        -> impl iter::Iterator<Item = &'a TypeId>
    {
        self.types.iter().flatten()
    }

    //  No `iter_type_ids_mut`: links between types are immutable.
}

//
//  Private Types
//

type KeyedMulti<T> = MultiTable<Id<[T]>, T>;

struct ExpressionIter<'a> {
    tree: &'a Tree,
    index: usize,
}

struct ExpressionIterMut<'a> {
    tree: &'a mut Tree,
    index: usize,
}

struct PatternIter<'a> {
    tree: &'a Tree,
    index: usize,
}

struct PatternIterMut<'a> {
    tree: &'a mut Tree,
    index: usize,
}

//
//  Private methods
//

impl Tree {
    /// Inserts a TypeDefinition.
    fn insert_type(&mut self, typ: &TypeDefinition) -> Type {
        use self::TypeDefinition::*;

        match typ {
            Builtin(t) => Type::Builtin(*t),
            Enum(e, _) => self.insert_enum(e),
            Rec(r, _) => self.insert_record(r),
            Tuple(t) => Type::Tuple(self.insert_types_tuple(t)),
            Unresolved(..) | UnresolvedEnum(..) | UnresolvedRec(..)
                => panic!("Cannot insert Unresolved types!"),
        }
    }

    /// Inserts a DynTyple<TypeDefinition>.
    fn insert_types_tuple(&mut self, tuple: &DynTuple<TypeDefinition>)
        -> Tuple<TypeId>
    {
        let mut fields = vec!();
        for f in &tuple.fields {
            let ty = self.insert_type(&f);
            let ty = self.push_type(ty);
            fields.push(ty);
        }
        let fields = self.push_type_ids(&fields);

        let names = tuple.names.apply(|names| self.push_names(names));

        Tuple { fields, names }
    }

    /// Generic Push.
    fn push_slice<T: Clone + ?Sized>(
        table: &mut KeyedMulti<T>,
        value: &[T]
    )
        -> Id<[T]>
    {
        if value.is_empty() {
            return Id::empty();
        }

        let id = Id::new(table.len() as u32);
        table.push(&id, value);
        id
    }
}

//
//  Public Trait Implementations
//

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        TreePrinter::tree(self).write(f)
    }
}

impl<'a> iter::IntoIterator for &'a Tree {
    type Item = DepthTreeItem<'a>;
    type IntoIter = DepthTreeIter<'a>;

    fn into_iter(self) -> DepthTreeIter<'a> { DepthTreeIter::from_tree(self) }
}

impl cmp::PartialEq for Tree {
    fn eq(&self, other: &Tree) -> bool {
        TreeComparator::new(self, other).trees_eq()
    }
}

impl cmp::Eq for Tree {}

//
//  Private Trait Implementations
//

impl<'a> iter::Iterator for ExpressionIter<'a> {
    type Item = ExpressionHandle<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.tree.expression.len() {
            let index = ExpressionId::from_index(self.index);
            debug_assert!(index.index() == self.index);

            self.index += 1;
            Some(self.tree.get_expression_handle(index))
        } else {
            None
        }
    }
}

impl<'a> iter::Iterator for ExpressionIterMut<'a> {
    type Item = ExpressionHandleMut<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.tree.expression.len() {
            let index = ExpressionId::from_index(self.index);
            debug_assert!(index.index() == self.index);

            //  Safety:
            //  -   non-overlapping indices.
            //  -   constrained lifetime.
            let tree: &'a mut Tree =
                unsafe { &mut *(self.tree as *mut _) };

            self.index += 1;
            Some(tree.get_expression_handle_mut(index))
        } else {
            None
        }
    }
}

impl<'a> iter::Iterator for PatternIter<'a> {
    type Item = PatternHandle<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.tree.pattern.len() {
            let index = PatternId::from_index(self.index);
            debug_assert!(index.index() == self.index);

            self.index += 1;
            Some(self.tree.get_pattern_handle(index))
        } else {
            None
        }
    }
}

impl<'a> iter::Iterator for PatternIterMut<'a> {
    type Item = PatternHandleMut<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.tree.pattern.len() {
            let index = PatternId::from_index(self.index);
            debug_assert!(index.index() == self.index);

            //  Safety:
            //  -   non-overlapping indices.
            //  -   constrained lifetime.
            let tree: &'a mut Tree =
                unsafe { &mut *(self.tree as *mut _) };

            self.index += 1;
            Some(tree.get_pattern_handle_mut(index))
        } else {
            None
        }
    }
}

#[cfg(test)]
pub mod samples {
    use super::*;

    /// A complete Tree corresponding to the following:
    ///
    /// "true" OR "false", depending on the parameter.
    pub fn bool_expression(b: bool) -> Tree {
        let mut tree = Tree::default();

        let range = range(0, if b { 4 } else { 5 });
        let bool_ = tree.push_expression(Type::bool_(), Expr::bool_(b), range);

        tree.set_root(Root::Expression(bool_));

        tree
    }

    /// A complete Tree corresponding to the following:
    ///
    /// "1 + 2"
    pub fn add_expression() -> Tree {
        let mut tree = Tree::default();

        let one = tree.push_expression(Type::int(), Expr::int(1), range(0, 1));
        let two = tree.push_expression(Type::int(), Expr::int(2), range(4, 1));

        let args = tree.push_expressions(&[one, two]);
        let args = Tuple::unnamed(args);

        let op = Callable::Builtin(BuiltinFunction::Add);
        let add = tree.push_expression(Type::int(), Expr::Call(op, args), range(0, 5));

        tree.set_root(Root::Expression(add));

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
            &Expr::int(1)
        );
        assert_eq!(
            tree.get_expression(two),
            &Expr::int(2)
        );
        assert_eq!(
            tree.get_expression(ExpressionId::new(2)),
            &Expr::Call(
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
