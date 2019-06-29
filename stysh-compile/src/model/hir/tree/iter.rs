//! Iteration over a `Tree`.

use std::iter;

use model::hir::{self, *};

//
//  Public Types.
//

/// DepthTreeIter.
///
/// In depth traversal of a Tree.
#[derive(Clone, Debug)]
pub struct DepthTreeIter<'a> {
    tree: &'a Tree,
    stack: Vec<DepthStep<'a>>,
}

/// DepthTreeItem.
///
/// The Item yielded by a `DepthTreeIter`.
#[derive(Clone, Copy, Debug)]
pub enum DepthTreeItem<'a> {
    /// A Function.
    Function(ItemIdentifier),
    /// A Callable.
    Callable(&'a Callable),
    /// An Expression, with the name of the field in its parent node.
    Expression(ExpressionHandle, &'static str),
    /// An Expression Field -- part of a tuple.
    ExpressionField(ExpressionHandle, ValueIdentifier),
    /// A Pattern, with the name of the field in its parent node.
    Pattern(PatternHandle, &'static str),
    /// A Pattern Field -- part of a tuple.
    PatternField(PatternHandle, ValueIdentifier),
    /// A Statement.
    Statement(&'a Stmt),
    /// A Type, with the name of the field in its parent node.
    Type(Type, &'static str),
    /// A Type Field -- part of a tuple.
    TypeField(Type, ValueIdentifier),
    /// Starting a sequence, with number of elements.
    SeqStart(usize),
    /// Stopping a sequence.
    SeqStop,
}

//
//  Public Methods.
//

impl<'a> DepthTreeIter<'a> {
    /// Creates an instance.
    pub fn from_tree(tree: &'a Tree) -> Self {
        tree.get_root().map(|root| {
            use self::Root::*;

            match root {
                Expression(id) => DepthTreeIter::from_expression(tree, id),
                Pattern(id) => DepthTreeIter::from_pattern(tree, id),
                Function(name, args, res, id)
                    => DepthTreeIter::from_function(tree, name, args, res, id),
            }
        }).unwrap_or(DepthTreeIter { tree, stack: vec!() })
    }

    /// Creates an instance.
    pub fn from_expression(tree: &'a Tree, expr: ExpressionId) -> Self {
        DepthTreeIter { tree, stack: vec!(DepthStep::Expr(expr, "")) }
    }

    /// Creates an instance.
    pub fn from_pattern(tree: &'a Tree, pat: PatternId) -> Self {
        DepthTreeIter { tree, stack: vec!(DepthStep::Pat(pat, "")) }
    }

    /// Creates an instance.
    pub fn from_function(
        tree: &'a Tree,
        name: ItemIdentifier,
        arguments: Tuple<PatternId>,
        result: TypeId,
        body: ExpressionId
    )
        -> Self
    {
        let fields = tree.get_patterns(arguments.fields);
        let names = tree.get_names(arguments.names);
        let result = tree.get_type(result);

        let stack = vec!(
            DepthStep::SeqStop,
            DepthStep::Expr(body, "body"),
            DepthStep::SeqStart(1),

            DepthStep::SeqStop,
            DepthStep::Type(result, "result"),
            DepthStep::SeqStart(1),

            //  Automatic SeqStop at end of tuple.
            DepthStep::PatTuple(fields, names),
            DepthStep::SeqStart(fields.len()),

            DepthStep::Function(name),
        );

        DepthTreeIter { tree, stack }
    }
}

impl<'a> iter::Iterator for DepthTreeIter<'a> {
    type Item = DepthTreeItem<'a>;

    fn next(&mut self) -> Option<DepthTreeItem<'a>> {
        if let Some(step) = self.stack.pop() {
            Some(self.next_step(step))
        } else {
            None
        }
    }
}

//
//  Private Types.
//

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum DepthStep<'a> {
    Function(ItemIdentifier),
    Expr(ExpressionId, &'static str),
    Pat(PatternId, &'static str),
    Type(Type, &'static str),
    Callables(&'a [Callable]),
    Statements(&'a [Stmt]),
    ExprTuple(&'a [ExpressionId], &'a [ValueIdentifier]),
    PatTuple(&'a [PatternId], &'a [ValueIdentifier]),
    TypeTuple(&'a [TypeId], &'a [ValueIdentifier]),
    SeqStart(usize),
    SeqStop,
}

//
//  Private Methods
//

impl<'a> DepthTreeIter<'a> {
    /// Next Step.
    fn next_step(&mut self, step: DepthStep<'a>) -> DepthTreeItem<'a> {
        use self::DepthStep::*;

        match step {
            Function(name) => DepthTreeItem::Function(name),
            Expr(id, name) => self.next_expression(id, name),
            Pat(id, name) => self.next_pattern(id, name),
            Type(typ, name) => self.next_type(typ, name),
            Callables(c) => self.next_callable(c),
            Statements(s) => self.next_statement(s),
            ExprTuple(f, n) => self.next_expression_field(f, n),
            PatTuple(p, n) => self.next_pattern_field(p, n),
            TypeTuple(t, n) => self.next_type_field(t, n),
            SeqStart(len) => DepthTreeItem::SeqStart(len),
            SeqStop => DepthTreeItem::SeqStop,
        }
    }

    fn next_expression(&mut self, expr: ExpressionId, name: &'static str) -> DepthTreeItem<'a> {
        let handle = self.tree.get_expression_handle(expr);

        self.push_expression_children(&handle.expr);

        DepthTreeItem::Expression(handle, name)
    }

    fn next_pattern(&mut self, pat: PatternId, name: &'static str) -> DepthTreeItem<'a> {
        let handle = self.tree.get_pattern_handle(pat);

        self.push_pattern_children(&handle.pattern);

        DepthTreeItem::Pattern(handle, name)
    }

    fn next_type(&mut self, typ: Type, name: &'static str) -> DepthTreeItem<'a> {
        self.push_type_children(&typ);

        DepthTreeItem::Type(typ, name)
    }

    fn next_callable(&mut self, callables: &'a [Callable]) -> DepthTreeItem<'a> {
        if let Some((head, tail)) = callables.split_first() {
            self.stack.push(DepthStep::Callables(tail));

            self.push_callable_children(head);

            DepthTreeItem::Callable(head)
        } else {
            DepthTreeItem::SeqStop
        }
    }

    fn next_statement(&mut self, statements: &'a [Stmt]) -> DepthTreeItem<'a> {
        if let Some((head, tail)) = statements.split_first() {
            self.stack.push(DepthStep::Statements(tail));

            self.push_statement_children(head);

            DepthTreeItem::Statement(head)
        } else {
            DepthTreeItem::SeqStop
        }
    }

    fn next_expression_field(
        &mut self,
        exprs: &'a [ExpressionId],
        names: &'a [ValueIdentifier],
    )
        -> DepthTreeItem<'a>
    {
        assert!(names.is_empty() || names.len() == exprs.len());

        if let Some((head, tail)) = exprs.split_first() {
            let (name, names) = names.split_first()
                .map(|(n, ns)| (*n, ns))
                .unwrap_or((Default::default(), &[]));
            let handle = self.tree.get_expression_handle(*head);

            self.stack.push(DepthStep::ExprTuple(tail, names));

            self.push_expression_children(&handle.expr);

            DepthTreeItem::ExpressionField(handle, name)
        } else {
            DepthTreeItem::SeqStop
        }
    }

    fn next_pattern_field(
        &mut self,
        pats: &'a [PatternId],
        names: &'a [ValueIdentifier],
    )
        -> DepthTreeItem<'a>
    {
        assert!(names.is_empty() || names.len() == pats.len());

        if let Some((head, tail)) = pats.split_first() {
            let (name, names) = names.split_first()
                .map(|(n, ns)| (*n, ns))
                .unwrap_or((Default::default(), &[]));
            let handle = self.tree.get_pattern_handle(*head);

            self.stack.push(DepthStep::PatTuple(tail, names));

            self.push_pattern_children(&handle.pattern);

            DepthTreeItem::PatternField(handle, name)
        } else {
            DepthTreeItem::SeqStop
        }
    }

    fn next_type_field(
        &mut self,
        types: &'a [TypeId],
        names: &'a [ValueIdentifier],
    )
        -> DepthTreeItem<'a>
    {
        assert!(names.is_empty() || names.len() == types.len());

        if let Some((head, tail)) = types.split_first() {
            let (name, names) = names.split_first()
                .map(|(n, ns)| (*n, ns))
                .unwrap_or((Default::default(), &[]));
            self.stack.push(DepthStep::TypeTuple(tail, names));

            let ty = self.tree.get_type(*head);
            self.push_type_children(&ty);

            DepthTreeItem::TypeField(ty, name)
        } else {
            DepthTreeItem::SeqStop
        }
    }

    fn push_callable_children(&mut self, c: &Callable) {
        use self::Callable::*;

        match c {
            Function(_, args, res) => {
                let fields = self.tree.get_type_ids(args.fields);
                let names = self.tree.get_names(args.names);
                let res = self.tree.get_type(*res);

                self.stack.extend_from_slice(&[
                    DepthStep::SeqStop,
                    DepthStep::Type(res, "result"),
                    DepthStep::SeqStart(1),

                    //  Automatic SeqStop at end of tuple.
                    DepthStep::TypeTuple(fields, names),
                    DepthStep::SeqStart(fields.len()),
                ]);
            },
            Unresolved(id) => {
                self.stack.push(DepthStep::SeqStop);

                let unresolved = self.tree.get_callables(*id);
                self.stack.push(DepthStep::Callables(unresolved));
                self.stack.push(DepthStep::SeqStart(unresolved.len()));
            },
            Builtin(_) | Unknown(_) => (),
        }
    }

    fn push_expression_children(&mut self, e: &Expr) {
        use self::Expr::*;

        match e {
            Block(stmts, e) => {
                if let Some(e) = e {
                    self.stack.push(DepthStep::SeqStop);
                    self.stack.push(DepthStep::Expr(*e, "last"));
                    self.stack.push(DepthStep::SeqStart(1));
                }
                let stmts = self.tree.get_statements(*stmts);
                //  Automatic SeqStop at end of statements.
                self.stack.push(DepthStep::Statements(stmts));
                self.stack.push(DepthStep::SeqStart(stmts.len()));
            },
            Call(_, tup) => self.push_expression_tuple(*tup),
            Constructor(tup) => self.push_expression_tuple(*tup),
            FieldAccess(expr, _) => {
                self.stack.push(DepthStep::SeqStop);
                self.stack.push(DepthStep::Expr(*expr, "of"));
                self.stack.push(DepthStep::SeqStart(1));
            },
            If(cond, true_, false_) => {
                self.stack.push(DepthStep::SeqStop);
                self.stack.push(DepthStep::Expr(*false_, "false"));
                self.stack.push(DepthStep::Expr(*true_, "true"));
                self.stack.push(DepthStep::Expr(*cond, "condition"));
                self.stack.push(DepthStep::SeqStart(3));
            },
            Implicit(hir::Implicit::ToEnum(_, expr)) => {
                self.stack.push(DepthStep::SeqStop);
                self.stack.push(DepthStep::Expr(*expr, "from"));
                self.stack.push(DepthStep::SeqStart(1));
            },
            Loop(stmts) => {
                let stmts = self.tree.get_statements(*stmts);
                //  Automatic SeqStop at end of statements.
                self.stack.push(DepthStep::Statements(stmts));
                self.stack.push(DepthStep::SeqStart(stmts.len()));
            },
            Tuple(tup) => self.push_expression_tuple(*tup),
            BuiltinVal(_) | Ref(..) | UnresolvedRef(..) => (),
        }
    }

    fn push_expression_tuple(&mut self, t: Tuple<ExpressionId>) {
        let exprs = self.tree.get_expressions(t.fields);
        let names = self.tree.get_names(t.names);

        //  Automatic SeqStop at end of tuple.
        self.stack.push(DepthStep::ExprTuple(exprs, names));
        self.stack.push(DepthStep::SeqStart(exprs.len()));
    }

    fn push_pattern_children(&mut self, p: &Pattern) {
        use self::Pattern::*;

        match p {
            Constructor(tup) | Tuple(tup) => self.push_pattern_tuple(*tup),
            Ignored(_) | Var(_) => (),
        }
    }

    fn push_pattern_tuple(&mut self, t: Tuple<PatternId>) {
        let patterns = self.tree.get_patterns(t.fields);
        let names = self.tree.get_names(t.names);

        //  Automatic SeqStop at end of tuple.
        self.stack.push(DepthStep::PatTuple(patterns, names));
        self.stack.push(DepthStep::SeqStart(patterns.len()));
    }

    fn push_statement_children(&mut self, s: &Stmt) {
        self.stack.push(DepthStep::SeqStop);

        match s {
            Stmt::Return(r) => {
                self.stack.push(DepthStep::Expr(r.value, "value"));
                self.stack.push(DepthStep::SeqStart(1));
            },
            Stmt::Set(r) => {
                self.stack.push(DepthStep::Expr(r.right, "right"));
                self.stack.push(DepthStep::Expr(r.left, "left"));
                self.stack.push(DepthStep::SeqStart(2));
            },
            Stmt::Var(b) => {
                self.stack.push(DepthStep::Expr(b.right, "right"));
                self.stack.push(DepthStep::Pat(b.left, "left"));
                self.stack.push(DepthStep::SeqStart(2));
            }
        }
    }

    fn push_type_children(&mut self, t: &Type) {
        use self::Type::*;

        match t {
            Enum(_, _, recs) => self.push_type_tuple(hir::Tuple::unnamed(*recs)),
            Rec(_, _, tup) | Tuple(tup) => self.push_type_tuple(*tup),
            Builtin(_) | Unresolved(..) => (),
        }
    }

    fn push_type_tuple(&mut self, t: Tuple<TypeId>) {
        let fields = self.tree.get_type_ids(t.fields);
        let names = self.tree.get_names(t.names);

        //  Automatic SeqStop at end of tuple.
        self.stack.push(DepthStep::TypeTuple(fields, names));
        self.stack.push(DepthStep::SeqStart(fields.len()));
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;

    fn add_expression() {
        let tree = samples::add_expression();

        let one = tree.get_expression_handle(ExpressionId::new(0));
        let two = tree.get_expression_handle(ExpressionId::new(1));
        let call = tree.get_expression_handle(ExpressionId::new(2));

        //  TODO
    }
}
*/
