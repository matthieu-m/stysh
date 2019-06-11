//! Comparison of two trees.
//!
//! Walks two trees in parallel, ensuring that they are equivalent.
//!
//! The exact equivalence level can be tuned by disabling comparison of:
//! -   GVN.
//! -   ranges (except identifiers).
//! -   strings (interned IDs).
//!
//! The comparison is short-circuiting and does not recurse.
//!
//! Furthermore, a maximum number of steps is built-in, to avoid infinite loops.

use basic::{com, mem};

use model::hir::*;

use super::*;

//
//  Public Types.
//

/// TreeComparator.
#[derive(Clone, Debug)]
pub struct TreeComparator<'a> {
    left_tree: &'a Tree,
    right_tree: &'a Tree,
    inner: Inner,
}

//
//  Public Methods.
//

impl<'a> TreeComparator<'a> {
    /// Creates an instance.
    pub fn new(left_tree: &'a Tree, right_tree: &'a Tree) -> Self {
        let inner = Inner::new();
        TreeComparator { left_tree, right_tree, inner }
    }

    /// Returns the maximum number of steps configured.
    ///
    /// When there are 0 remaining steps, a panic will occur.
    pub fn get_steps(&self) -> usize { self.inner.steps }

    /// Returns true if Gvn are compared.
    pub fn compares_gvn(&self) -> bool { self.inner.with_gvn }

    /// Returns true if ranges are compared.
    pub fn compares_range(&self) -> bool { self.inner.with_range }

    /// Returns true if strings (interned ID) are compared.
    pub fn compares_strings(&self) -> bool { self.inner.with_strings }

    /// Sets the maximum number of steps.
    ///
    /// When there are 0 remaining steps, a panic will occur.
    pub fn set_steps(&mut self, steps: usize) -> &mut Self {
        self.inner.steps = steps;
        self
    }

    /// Sets whether to compare Gvn or not.
    pub fn set_gvn(&mut self, with_gvn: bool) -> &mut Self {
        self.inner.with_gvn = with_gvn;
        self
    }

    /// Sets whether to compare ranges or not.
    pub fn set_range(&mut self, with_range: bool) -> &mut Self {
        self.inner.with_range = with_range;
        self
    }

    /// Sets whether to compare strings (interned ID) or not.
    pub fn set_strings(&mut self, with_strings: bool) -> &mut Self {
        self.inner.with_strings = with_strings;
        self
    }

    /// Compares the trees.
    ///
    /// # Panics
    ///
    /// Panics if the maximum number of steps is reached, to prevent infinite
    /// loops.
    pub fn trees_eq(&self) -> bool {
        let left = DepthTreeIter::from_tree(self.left_tree);
        let right = DepthTreeIter::from_tree(self.right_tree);

        self.inner.depth_iter_eq(left, right)
    }

    /// Compares two Expressions.
    ///
    /// # Panics
    ///
    /// Panics if the maximum number of steps is reached, to prevent infinite
    /// loops.
    pub fn expressions_eq(&self, left: ExpressionId, right: ExpressionId)
        -> bool
    {
        let left = DepthTreeIter::from_expression(self.left_tree, left);
        let right = DepthTreeIter::from_expression(self.right_tree, right);

        self.inner.depth_iter_eq(left, right)
    }

    /// Compares two Patterns.
    ///
    /// # Panics
    ///
    /// Panics if the maximum number of steps is reached, to prevent infinite
    /// loops.
    pub fn patterns_eq(&self, left: PatternId, right: PatternId) -> bool {
        let left = DepthTreeIter::from_pattern(self.left_tree, left);
        let right = DepthTreeIter::from_pattern(self.right_tree, right);

        self.inner.depth_iter_eq(left, right)
    }

    /// Compares two Statements.
    ///
    /// # Panics
    ///
    /// Panics if the maximum number of steps is reached, to prevent infinite
    /// loops.
    pub fn statements_eq(&self, left: &Stmt, right: &Stmt) -> bool {
        use self::Stmt::*;

        match (left, right) {
            (Return(left), Return(right))
                => self.statements_returns_eq(left, right),
            (Set(left), Set(right))
                => self.statements_rebindings_eq(left, right),
            (Var(left), Var(right))
                => self.statements_bindings_eq(left, right),
            (_, _)
                => false,
        }
    }
}

//
//  Private Types
//

#[derive(Clone, Debug)]
struct Inner {
    steps: usize,
    with_gvn: bool,
    with_range: bool,
    with_strings: bool,
}

//
//  Private Methods
//

impl<'a> TreeComparator<'a> {
    fn statements_bindings_eq(&self, left: &Binding, right: &Binding)
        -> bool
    {
        self.patterns_eq(left.left, right.left) &&
            self.expressions_eq(left.right, right.right)
    }

    fn statements_rebindings_eq(&self, left: &ReBinding, right: &ReBinding)
        -> bool
    {
        self.expressions_eq(left.left, right.left) &&
            self.expressions_eq(left.right, right.right)
    }

    fn statements_returns_eq(&self, left: &Return, right: &Return)
        -> bool
    {
        self.expressions_eq(left.value, right.value)
    }
}

impl Inner {
    fn new() -> Inner {
        Inner { steps: 10_000, with_gvn: true, with_range: true, with_strings: true }
    }

    fn depth_iter_eq(&self, mut left: DepthTreeIter<'_>, mut right: DepthTreeIter<'_>)
        -> bool
    {
        for _ in 0..self.steps {
            let left = left.next();
            let right = right.next();

            match (left, right) {
                (Some(left), Some(right)) => {
                    if !self.depth_eq(left, right) {
                        return false;
                    }
                    //  continue
                },
                (None, None)
                    => return true,
                (_, _)
                    => return false,
            };
        }

        panic!(
            "Ran out of steps ({} configured): left is {:?} and right is {:?}",
            self.steps,
            left,
            right
        );
    }

    fn depth_eq(&self, left: DepthTreeItem<'_>, right: DepthTreeItem<'_>) -> bool {
        use self::DepthTreeItem::*;

        match (left, right) {
            (Function(left), Function(right))
                => self.item_identifiers_eq(left, right),
            (Callable(left), Callable(right))
                => self.callables_eq(left, right),
            (Expression(left, left_name), Expression(right, right_name))
                => left_name == right_name &&
                    self.expressions_eq(left, right),
            (ExpressionField(left, left_id), ExpressionField(right, right_id))
                => self.value_identifiers_eq(left_id, right_id) &&
                    self.expressions_eq(left, right),
            (Pattern(left, left_name), Pattern(right, right_name))
                => left_name == right_name &&
                    self.patterns_eq(left, right),
            (PatternField(left, left_id), PatternField(right, right_id))
                => self.value_identifiers_eq(left_id, right_id) &&
                    self.patterns_eq(left, right),
            (Statement(left), Statement(right))
                => self.statements_eq(left, right),
            (Type(left, left_name), Type(right, right_name))
                => left_name == right_name &&
                    self.types_eq(&left, &right),
            (TypeField(left, left_id), TypeField(right, right_id))
                => self.value_identifiers_eq(left_id, right_id) &&
                    self.types_eq(&left, &right),
            (SeqStart(left), SeqStart(right))
                => left == right,
            (SeqStop, SeqStop) => true,
            (..) => false,
        }
    }

    fn callables_eq(&self, left: &Callable, right: &Callable) -> bool {
        use self::Callable::*;

        match (left, right) {
            (Builtin(left), Builtin(right))
                => left == right,
            (Function(left_id, _, _), Function(right_id, _, _))
                => self.item_identifiers_eq(*left_id, *right_id),
            (Unknown(left), Unknown(right))
                => self.value_identifiers_eq(*left, *right),
            (Unresolved(..), Unresolved(..))
                => true,
            (..)
                => false,
        }
    }

    fn expressions_eq(
        &self,
        left: ExpressionHandle<'_>,
        right: ExpressionHandle<'_>,
    )
        -> bool
    {
        self.expression_ids_eq(left.id, right.id) &&
            self.ranges_eq(left.range, right.range) &&
            left.typ == right.typ &&
            self.expressions_expr_eq(&left.expr, &right.expr)
    }

    fn expressions_expr_eq(&self, left: &Expr, right: &Expr) -> bool {
        use self::Expr::*;

        match (left, right) {
            //  Actual content.
            (BuiltinVal(left), BuiltinVal(right))
                => left == right,
            (FieldAccess(_, left), FieldAccess(_, right))
                => left == right,
            (Implicit(left), Implicit(right))
                => self.expressions_implicit_eq(left, right),
            (Ref(left_id, left_gvn), Ref(right_id, right_gvn))
                => self.value_identifiers_eq(*left_id, *right_id) &&
                    self.gvns_eq(*left_gvn, *right_gvn),
            (UnresolvedRef(left), UnresolvedRef(right))
                => left == right,
            //  Structural only.
            (Block(..), Block(..)) |
            (Call(..), Call(..)) |
            (Constructor(..), Constructor(..)) |
            (If(..), If(..)) |
            (Loop(..), Loop(..)) |
            (Tuple(..), Tuple(..))
                => true,
            //  Not matching.
            (_, _) => false,
        }
    }

    fn expressions_implicit_eq(&self, left: &Implicit, right: &Implicit) -> bool {
        use self::Implicit::*;

        match (left, right) {
            (ToEnum(left, _), ToEnum(right, _)) => left == right,
        }
    }

    fn patterns_eq(&self, left: PatternHandle<'_>, right: PatternHandle<'_>)
        -> bool
    {
        self.pattern_ids_eq(left.id, right.id) &&
            self.ranges_eq(left.range, right.range) &&
            left.typ == right.typ &&
            self.patterns_pat_eq(&left.pattern, &right.pattern)
    }

    fn patterns_pat_eq(&self, left: &Pattern, right: &Pattern)
        -> bool
    {
        use self::Pattern::*;

        match (left, right) {
            (Ignored(left), Ignored(right))
                => self.ranges_eq(*left, *right),
            (Var(left), Var(right))
                => self.value_identifiers_eq(*left, *right),
            (Constructor(..), Constructor(..)) |
            (Tuple(..), Tuple(..))
                => true,
            (_, _) => false,
        }
    }

    fn statements_eq(&self, left: &Stmt, right: &Stmt) -> bool {
        use self::Stmt::*;

        match (left, right) {
            (Return(left), Return(right))
                => self.ranges_eq(left.range, right.range),
            (Set(left), Set(right))
                => self.ranges_eq(left.range, right.range),
            (Var(left), Var(right))
                => self.ranges_eq(left.range, right.range),
            (_, _)
                => false,
        }
    }

    fn types_eq(&self, left: &Type, right: &Type) -> bool {
        use self::Type::*;

        match (left, right) {
            (Builtin(left), Builtin(right))
                => left == right,
            (Tuple(_), Tuple(_))
                => true,
            (Enum(left_id, _, _), Enum(right_id, _, _)) |
            (Rec(left_id, _, _), Rec(right_id, _, _)) |
            (Unresolved(left_id, _), Unresolved(right_id, _))
                => self.item_identifiers_eq(*left_id, *right_id),
            (_, _)
                => false,
        }
    }

    fn item_identifiers_eq(&self, left: ItemIdentifier, right: ItemIdentifier) -> bool {
        self.strings_eq(left.0, right.0) && left.1 == right.1
    }

    fn value_identifiers_eq(&self, left: ValueIdentifier, right: ValueIdentifier) -> bool {
        self.strings_eq(left.0, right.0) && left.1 == right.1
    }

    fn gvns_eq(&self, left: Gvn, right: Gvn) -> bool {
        !self.with_gvn || left == right
    }

    fn expression_ids_eq(&self, left: ExpressionId, right: ExpressionId) -> bool {
        !self.with_gvn || left == right
    }

    fn pattern_ids_eq(&self, left: PatternId, right: PatternId) -> bool {
        !self.with_gvn || left == right
    }

    fn ranges_eq(&self, left: com::Range, right: com::Range) -> bool {
        !self.with_range || left == right
    }

    fn strings_eq(&self, left: mem::InternId, right: mem::InternId) -> bool {
        !self.with_strings || left == right
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_bool() {
        let true_ = samples::bool_expression(true);
        let false_ = samples::bool_expression(false);

        let id = ExpressionId::new(0);

        assert!(TreeComparator::new(&true_, &true_).expressions_eq(id, id));
        assert!(TreeComparator::new(&false_, &false_).expressions_eq(id, id));

        assert!(!TreeComparator::new(&true_, &false_).expressions_eq(id, id));
        assert!(!TreeComparator::new(&false_, &true_).expressions_eq(id, id));
    }
}
