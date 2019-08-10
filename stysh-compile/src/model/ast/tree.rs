//! Tree
//!
//! A stand-alone flat representation of the tree which composes an expression.
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

use std::fmt;

use crate::basic::com::{Range, Span, Store, MultiStore};
use crate::basic::sea::{MultiTable, Table};

use crate::model::ast::*;

//
//  Public Types
//

/// Tree.
///
/// A Tree normally represents an expression, such as the body of a function or
/// the initializer of a constant.
///
/// For ease of testing, however, it can also represent a pattern or statement,
/// see the root.
#[derive(Clone, Default, PartialEq, Eq)]
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


    //  Singles.

    /// Expression.
    expression: KeyedSingle<Expression>,
    /// Range of an expression.
    expression_range: KeyedRange<Expression>,

    /// Pattern.
    pattern: KeyedSingle<Pattern>,
    /// Range of a pattern.
    pattern_range: KeyedRange<Pattern>,

    /// Statement.
    statement: KeyedSingle<Statement>,

    /// Type.
    type_: KeyedSingle<Type>,
    /// Range of a type.
    type_range: KeyedRange<Type>,


    //  Multis.

    /// Arguments.
    arguments: KeyedMulti<Argument>,
    /// Expression IDs.
    expression_ids: KeyedMulti<ExpressionId>,
    /// Identifiers.
    identifiers: KeyedMulti<Identifier>,
    /// Pattern IDs.
    pattern_ids: KeyedMulti<PatternId>,
    /// Positions.
    positions: KeyedMulti<u32>,
    /// Statement IDs.
    statement_ids: KeyedMulti<StatementId>,
    /// String Fragments.
    string_fragments: KeyedMulti<StringFragment>,
    /// Type IDs.
    type_ids: KeyedMulti<TypeId>,
}

/// Root.
///
/// The root of the tree.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Root {
    /// Expression.
    Expression(ExpressionId),
    /// Function.
    Function(Function, ExpressionId),
    /// Pattern.
    Pattern(PatternId),
    /// Statement.
    Statement(StatementId),
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

    /// Returns the expression root.
    pub fn get_root_expression(&self) -> Option<ExpressionId> {
        if let Some(Root::Expression(id)) = self.root {
            Some(id)
        } else {
            None
        }
    }

    /// Returns the function root.
    pub fn get_root_function(&self) -> Option<(Function, ExpressionId)> {
        if let Some(Root::Function(fun, body)) = self.root {
            Some((fun, body))
        } else {
            None
        }
    }

    /// Sets the root.
    pub fn set_root(&mut self, root: Root) { self.root = Some(root); }


    //  Expressions.

    /// Returns the number of expressions.
    pub fn len_expressions(&self) -> usize { self.expression.len() }

    /// Returns the expression associated to the ID.
    pub fn get_expression(&self, id: ExpressionId) -> Expression {
        *self.expression.at(&id)
    }

    /// Returns the range of the expression associated to the ID.
    pub fn get_expression_range(&self, id: ExpressionId) -> Range {
        *self.expression_range.at(&id)
    }

    /// Pushes a new expression.
    ///
    /// Returns the ID created for it.
    pub fn push_expression(&mut self, expr: Expression, range: Range) -> ExpressionId {
        debug_assert!(self.expression.len() == self.expression_range.len());

        let id = Id::new(self.expression.len() as u32);
        self.expression.push(&id, expr);
        self.expression_range.push(&id, range);

        id
    }


    //  Patterns.

    /// Returns the number of patterns.
    pub fn len_patterns(&self) -> usize { self.pattern.len() }

    /// Returns the pattern associated to the ID.
    pub fn get_pattern(&self, id: PatternId) -> Pattern {
        *self.pattern.at(&id)
    }

    /// Returns the range of the pattern associated to the ID.
    pub fn get_pattern_range(&self, id: PatternId) -> Range {
        *self.pattern_range.at(&id)
    }

    /// Pushes a new pattern.
    ///
    /// Returns the ID created for it.
    pub fn push_pattern(&mut self, pat: Pattern, range: Range) -> PatternId {
        debug_assert!(self.pattern.len() == self.pattern_range.len());

        let id = Id::new(self.pattern.len() as u32);
        self.pattern.push(&id, pat);
        self.pattern_range.push(&id, range);

        id
    }


    //  Statements.

    /// Returns the number of statements.
    pub fn len_statements(&self) -> usize { self.statement.len() }

    /// Returns the statement associated to the ID.
    pub fn get_statement(&self, id: StatementId) -> Statement {
        *self.statement.at(&id)
    }

    /// Pushes a new statement.
    ///
    /// Returns the ID created for it.
    pub fn push_statement(&mut self, statement: Statement) -> StatementId {
        let id = Id::new(self.statement.len() as u32);
        self.statement.push(&id, statement);

        id
    }


    //  Types.

    /// Returns the number of types.
    pub fn len_types(&self) -> usize { self.type_.len() }

    /// Returns the type associated to the ID.
    pub fn get_type(&self, id: TypeId) -> Type {
        *self.type_.at(&id)
    }

    /// Returns the range of the type associated to the ID.
    pub fn get_type_range(&self, id: TypeId) -> Range {
        *self.type_range.at(&id)
    }

    /// Pushes a new type.
    ///
    /// Returns the ID created for it.
    pub fn push_type(&mut self, ty: Type, range: Range) -> TypeId {
        debug_assert!(self.type_.len() == self.type_range.len());

        let id = Id::new(self.type_.len() as u32);
        self.type_.push(&id, ty);
        self.type_range.push(&id, range);

        id
    }


    //  Arguments

    /// Returns the arguments associated to the ID.
    pub fn get_arguments(&self, id: Id<[Argument]>) -> &[Argument] {
        if id.is_empty() { &[] } else { self.arguments.get(&id) }
    }

    /// Pushes a new slice of arguments.
    ///
    /// Returns the ID created for it.
    pub fn push_arguments(&mut self, arguments: &[Argument]) -> Id<[Argument]> {
        Self::push_slice(&mut self.arguments, arguments)
    }


    //  Expression IDs

    /// Returns the expression IDs associated to the ID.
    pub fn get_expression_ids(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] {
        if id.is_empty() { &[] } else { self.expression_ids.get(&id) }
    }

    /// Pushes a new slice of expression IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_expression_ids(&mut self, expression_ids: &[ExpressionId]) -> Id<[ExpressionId]> {
        Self::push_slice(&mut self.expression_ids, expression_ids)
    }


    //  Identifiers

    /// Returns the identifiers associated to the ID.
    pub fn get_identifiers(&self, id: Id<[Identifier]>) -> &[Identifier] {
        if id.is_empty() { &[] } else { self.identifiers.get(&id) }
    }

    /// Pushes a new slice of expression IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_identifiers(&mut self, identifiers: &[Identifier]) -> Id<[Identifier]> {
        Self::push_slice(&mut self.identifiers, identifiers)
    }


    //  Pattern IDs

    /// Returns the pattern IDs associated to the ID.
    pub fn get_pattern_ids(&self, id: Id<[PatternId]>) -> &[PatternId] {
        if id.is_empty() { &[] } else { self.pattern_ids.get(&id) }
    }

    /// Pushes a new slice of pattern IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_pattern_ids(&mut self, pattern_ids: &[PatternId]) -> Id<[PatternId]> {
        Self::push_slice(&mut self.pattern_ids, pattern_ids)
    }


    //  Positions

    /// Returns the positions associated to the ID.
    pub fn get_positions(&self, id: Id<[u32]>) -> &[u32] {
        if id.is_empty() { &[] } else { self.positions.get(&id) }
    }

    /// Pushes a new slice of positions.
    ///
    /// Returns the ID created for it.
    pub fn push_positions(&mut self, positions: &[u32]) -> Id<[u32]> {
        Self::push_slice(&mut self.positions, positions)
    }


    //  Statement IDs

    /// Returns the statement IDs associated to the ID.
    pub fn get_statement_ids(&self, id: Id<[StatementId]>) -> &[StatementId] {
        if id.is_empty() { &[] } else { self.statement_ids.get(&id) }
    }

    /// Pushes a new slice of statement IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_statement_ids(&mut self, statement_ids: &[StatementId]) -> Id<[StatementId]> {
        Self::push_slice(&mut self.statement_ids, statement_ids)
    }


    //  String Fragments

    /// Returns the string fragments associated to the ID.
    pub fn get_string_fragments(&self, id: Id<[StringFragment]>) -> &[StringFragment] {
        if id.is_empty() { &[] } else { self.string_fragments.get(&id) }
    }

    /// Pushes a new slice of string fragments.
    ///
    /// Returns the ID created for it.
    pub fn push_string_fragments(&mut self, string_fragments: &[StringFragment]) -> Id<[StringFragment]> {
        Self::push_slice(&mut self.string_fragments, string_fragments)
    }


    //  Type IDs

    /// Returns the type IDs associated to the ID.
    pub fn get_type_ids(&self, id: Id<[TypeId]>) -> &[TypeId] {
        if id.is_empty() { &[] } else { self.type_ids.get(&id) }
    }

    /// Pushes a new slice of type IDs.
    ///
    /// Returns the ID created for it.
    pub fn push_type_ids(&mut self, type_ids: &[TypeId]) -> Id<[TypeId]> {
        Self::push_slice(&mut self.type_ids, type_ids)
    }
}


//
//  Implementation of Store
//

impl Store<Expression> for Tree {
    fn len(&self) -> usize { self.len_expressions() }
    fn get(&self, id: ExpressionId) -> Expression { self.get_expression(id) }
    fn get_range(&self, id: ExpressionId) -> Range { self.get_expression_range(id) }
    fn push(&mut self, e: Expression, r: Range) -> ExpressionId { self.push_expression(e, r) }
}

impl Store<Pattern> for Tree {
    fn len(&self) -> usize { self.len_patterns() }
    fn get(&self, id: PatternId) -> Pattern { self.get_pattern(id) }
    fn get_range(&self, id: PatternId) -> Range { self.get_pattern_range(id) }
    fn push(&mut self, p: Pattern, r: Range) -> PatternId { self.push_pattern(p, r) }
}

impl Store<Statement> for Tree {
    fn len(&self) -> usize { self.len_statements() }
    fn get(&self, id: StatementId) -> Statement { self.get_statement(id) }
    fn get_range(&self, id: StatementId) -> Range { self.get_statement(id).span() }
    fn push(&mut self, s: Statement, _: Range) -> StatementId { self.push_statement(s) }
}

impl Store<Type> for Tree {
    fn len(&self) -> usize { self.len_types() }
    fn get(&self, id: TypeId) -> Type { self.get_type(id) }
    fn get_range(&self, id: TypeId) -> Range { self.get_type_range(id) }
    fn push(&mut self, ty: Type, r: Range) -> TypeId { self.push_type(ty, r) }
}


//
//  Implementation of MultiStore
//

impl MultiStore<Argument> for Tree {
    fn get_slice(&self, id: Id<[Argument]>) -> &[Argument] { self.get_arguments(id) }
    fn push_slice(&mut self, items: &[Argument]) -> Id<[Argument]> { self.push_arguments(items) }
}

impl MultiStore<ExpressionId> for Tree {
    fn get_slice(&self, id: Id<[ExpressionId]>) -> &[ExpressionId] { self.get_expression_ids(id) }
    fn push_slice(&mut self, items: &[ExpressionId]) -> Id<[ExpressionId]> { self.push_expression_ids(items) }
}

impl MultiStore<Identifier> for Tree {
    fn get_slice(&self, id: Id<[Identifier]>) -> &[Identifier] { self.get_identifiers(id) }
    fn push_slice(&mut self, items: &[Identifier]) -> Id<[Identifier]> { self.push_identifiers(items) }
}

impl MultiStore<PatternId> for Tree {
    fn get_slice(&self, id: Id<[PatternId]>) -> &[PatternId] { self.get_pattern_ids(id) }
    fn push_slice(&mut self, items: &[PatternId]) -> Id<[PatternId]> { self.push_pattern_ids(items) }
}

impl MultiStore<u32> for Tree {
    fn get_slice(&self, id: Id<[u32]>) -> &[u32] { self.get_positions(id) }
    fn push_slice(&mut self, items: &[u32]) -> Id<[u32]> { self.push_positions(items) }
}

impl MultiStore<StatementId> for Tree {
    fn get_slice(&self, id: Id<[StatementId]>) -> &[StatementId] { self.get_statement_ids(id) }
    fn push_slice(&mut self, items: &[StatementId]) -> Id<[StatementId]> { self.push_statement_ids(items) }
}

impl MultiStore<StringFragment> for Tree {
    fn get_slice(&self, id: Id<[StringFragment]>) -> &[StringFragment] { self.get_string_fragments(id) }
    fn push_slice(&mut self, items: &[StringFragment]) -> Id<[StringFragment]> { self.push_string_fragments(items) }
}

impl MultiStore<TypeId> for Tree {
    fn get_slice(&self, id: Id<[TypeId]>) -> &[TypeId] { self.get_type_ids(id) }
    fn push_slice(&mut self, items: &[TypeId]) -> Id<[TypeId]> { self.push_type_ids(items) }
}


//
//  Implementations of std traits
//

impl fmt::Debug for Tree {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Tree {{ ")?;
        if let Some(root) = self.root {
            write!(f, "root: {:?}, ", root)?;
        }

        if !self.expression.is_empty() {
            write!(f, "expression: {:?}, ", self.expression)?;
            write!(f, "expression_range: {:?}, ", self.expression_range)?;
        }
        if !self.pattern.is_empty() {
            write!(f, "pattern: {:?}, ", self.pattern)?;
            write!(f, "pattern_range: {:?}, ", self.pattern_range)?;
        }
        if !self.statement.is_empty() {
            write!(f, "statement: {:?}, ", self.statement)?;
        }
        if !self.type_.is_empty() {
            write!(f, "type_: {:?}, ", self.type_)?;
            write!(f, "type_range: {:?}, ", self.type_range)?;
        }

        if !self.arguments.is_empty() {
            write!(f, "arguments: {:?}, ", self.arguments)?;
        }
        if !self.expression_ids.is_empty() {
            write!(f, "expression_ids: {:?}, ", self.expression_ids)?;
        }
        if !self.identifiers.is_empty() {
            write!(f, "identifiers: {:?}, ", self.identifiers)?;
        }
        if !self.pattern_ids.is_empty() {
            write!(f, "pattern_ids: {:?}, ", self.pattern_ids)?;
        }
        if !self.positions.is_empty() {
            write!(f, "positions: {:?}, ", self.positions)?;
        }
        if !self.statement_ids.is_empty() {
            write!(f, "statement_ids: {:?}, ", self.statement_ids)?;
        }
        if !self.string_fragments.is_empty() {
            write!(f, "string_fragments: {:?}, ", self.string_fragments)?;
        }
        if !self.type_ids.is_empty() {
            write!(f, "type_ids: {:?}, ", self.type_ids)?;
        }
        write!(f, "}}")
    }
}


//
//  Private Types
//

type KeyedSingle<T> = Table<Id<T>, T>;
type KeyedRange<T> = Table<Id<T>, Range>;
type KeyedMulti<T> = MultiTable<Id<[T]>, T>;


//
//  Private methods
//

impl Tree {
    /// Generic Push.
    fn push_slice<T: Copy>(
        table: &mut KeyedMulti<T>,
        value: &[T]
    )
        -> Id<[T]>
    {
        table.create(value.iter().cloned()).unwrap_or(Id::empty())
    }
}
