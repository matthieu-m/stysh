//! A Debug printer for a particular expression or pattern.
//!
//! Represents the tree in a tree format, with indentation, to facilitate
//! debugging.
//!
//! Furthermore, a maximum number of steps is built-in, to avoid infinite loops.

use std::fmt;

use model::hir::*;

use super::*;

//
//  Public Types.
//

/// TreePrinter.
#[derive(Clone, Debug)]
pub struct TreePrinter<'a> {
    iter: DepthTreeIter<'a>,
    steps: usize,
}

//
//  Public Methods.
//

impl<'a> TreePrinter<'a> {
    /// Creates a new instance.
    pub fn tree(tree: &'a Tree) -> TreePrinter<'a> {
        TreePrinter { iter: DepthTreeIter::from_tree(tree), steps: 10_000 }
    }

    /// Creates a new instance.
    pub fn expression(tree: &'a Tree, expr: ExpressionId)
        -> TreePrinter<'a>
    {
        TreePrinter { iter: DepthTreeIter::from_expression(tree, expr), steps: 10_000 }
    }

    /// Creates a new instance.
    pub fn pattern(tree: &'a Tree, pat: PatternId)
        -> TreePrinter<'a>
    {
        TreePrinter { iter: DepthTreeIter::from_pattern(tree, pat), steps: 10_000 }
    }

    /// Returns the maximum number of steps configured.
    ///
    /// When there are 0 remaining steps, an error is returned.
    pub fn get_steps(&self) -> usize { self.steps }

    /// Sets the maximum number of steps.
    ///
    /// When there are 0 remaining steps, an error is returned.
    pub fn set_steps(&mut self, steps: usize) -> &mut Self {
        self.steps = steps;
        self
    }

    /// Outputs the tree to a stream.
    ///
    /// Returns an error if the underlying writer does, or if there are 0
    /// remaining steps.
    pub fn write(&self, writer: &mut fmt::Write) -> Result<(), fmt::Error> {
        let mut inner = Inner {
            iter: self.iter.clone(),
            writer: writer,
            steps: self.steps,
            indentation: 0
        };
        inner.write()
    }
}

//
//  Private Types
//

struct Inner<'a> {
    iter: DepthTreeIter<'a>,
    writer: &'a mut fmt::Write,
    steps: usize,
    indentation: usize,
}

//
//  Private Methods
//

impl<'a> Inner<'a> {
    fn write(&mut self) -> Result<(), fmt::Error> {
        for _ in 0..self.steps {
            if let Some(item) = self.iter.next() {
                self.write_item(item)?;
                continue;
            }

            return Ok(());
        }

        panic!(
            "Ran out of steps ({} configured): iterator is {:?}",
            self.steps,
            self.iter
        );
    }

    fn write_item(&mut self, item: DepthTreeItem<'_>) -> Result<(), fmt::Error> {
        use self::DepthTreeItem::*;

        match item {
            Function(name) => self.write_function(name),
            Callable(callable) => self.write_callable(callable),
            Expression(expr, name) => self.write_expression(expr, name),
            ExpressionField(expr, field) => self.write_expression_field(expr, field),
            Pattern(pat, name) => self.write_pattern(pat, name),
            PatternField(pat, field) => self.write_pattern_field(pat, field),
            Statement(stmt) => self.write_statement(stmt),
            Type(typ, name) => self.write_type(typ, name),
            TypeField(typ, field) => self.write_type_field(typ, field),
            SeqStart(_) => { self.indentation += 4; Ok(()) },
            SeqStop => { self.indentation -= 4; Ok(()) },
        }
    }

    fn write_function(&mut self, name: ItemIdentifier) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        write!(self.writer, "Function {:?}", name.0)
    }

    fn write_callable(&mut self, callable: &Callable) -> Result<(), fmt::Error> {
        use self::Callable::*;

        self.write_indentation()?;

        match callable {
            Builtin(fun) => write!(self.writer, "{}\n", fun),
            Function(name, _, _) => write!(self.writer, "Function {:?}\n", name.0),
            Unknown(id) => write!(self.writer, "Unknown {:?}\n", id.0),
            Unresolved(_) => write!(self.writer, "Unresolved\n"),
        }
    }

    fn write_expression(&mut self, h: ExpressionHandle, name: &str) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        if !name.is_empty() {
            write!(self.writer, "{}: ", name)?;
        }

        self.write_expression_impl(h)
    }

    fn write_expression_field(&mut self, h: ExpressionHandle, field: ValueIdentifier) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        if field.0 != Default::default() {
            write!(self.writer, "{:?}: ", field.0)?;
        }

        self.write_expression_impl(h)
    }

    fn write_expression_impl(&mut self, h: ExpressionHandle) -> Result<(), fmt::Error> {
        use self::Expr::*;

        match h.expr {
            Block(..) => write!(self.writer, "Block")?,
            BuiltinVal(b) => write!(self.writer, "{:?}", b)?,
            Call(..) => write!(self.writer, "Call")?,
            Constructor(..) => write!(self.writer, "Constructor")?,
            FieldAccess(_, field) => write!(self.writer, "{:?}", field)?,
            If(..) => write!(self.writer, "If")?,
            Implicit(..) => write!(self.writer, "Implicit")?,
            Loop(..) => write!(self.writer, "Loop")?,
            Ref(id, gvn) => write!(self.writer, "Ref {:?} to {:?}", id.0, gvn)?,
            Tuple(..) => write!(self.writer, "Tuple")?,
            UnresolvedRef(id) => write!(self.writer, "UnresolvedRef {:?}", id.0)?,
        }

        write!(self.writer, " -> {:?} {:?} {:?}\n", h.typ, h.range, Gvn::from(h.id))
    }

    fn write_pattern(&mut self, h: PatternHandle, name: &str) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        if !name.is_empty() {
            write!(self.writer, "{}: ", name)?;
        }

        self.write_pattern_impl(h)
    }

    fn write_pattern_field(&mut self, h: PatternHandle, field: ValueIdentifier) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        if field.0 != Default::default() {
            write!(self.writer, "{:?}: ", field.0)?;
        }

        self.write_pattern_impl(h)
    }

    fn write_pattern_impl(&mut self, h: PatternHandle) -> Result<(), fmt::Error> {
        use self::Pattern::*;

        match h.pattern {
            Constructor(..) => write!(self.writer, "Constructor")?,
            Ignored(..) => write!(self.writer, "Ignored")?,
            Tuple(..) => write!(self.writer, "Tuple")?,
            Var(id) => write!(self.writer, "{:?}", id.0)?,
        }

        write!(self.writer, " -> {:?} {:?} {:?}\n", h.typ, h.range, Gvn::from(h.id))
    }

    fn write_statement(&mut self, stmt: &Stmt) -> Result<(), fmt::Error> {
        use self::Stmt::*;

        self.write_indentation()?;

        match stmt {
            Return(r) => write!(self.writer, "Return {:?}\n", r.range),
            Set(r) => write!(self.writer, "Set {:?}\n", r.range),
            Var(b) => write!(self.writer, "Var {:?}\n", b.range),
        }
    }

    fn write_type(&mut self, typ: Type, name: &str) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        if !name.is_empty() {
            write!(self.writer, "{}: ", name)?;
        }

        self.write_type_impl(typ)
    }

    fn write_type_field(&mut self, typ: Type, field: ValueIdentifier) -> Result<(), fmt::Error> {
        self.write_indentation()?;

        if field.0 != Default::default() {
            write!(self.writer, "{:?}: ", field.0)?;
        }

        self.write_type_impl(typ)
    }

    fn write_type_impl(&mut self, typ: Type) -> Result<(), fmt::Error> {
        use self::Type::*;

        match typ {
            Builtin(b) => write!(self.writer, "{}", b),
            Enum(name, _, _) => write!(self.writer, "Enum {:?}", name.0),
            Rec(name, _, _) => write!(self.writer, "Rec {:?}", name.0),
            Tuple(_) => write!(self.writer, "Tuple"),
            Unresolved(name, _) => write!(self.writer, "Unresolved {:?}", name.0),
        }
    }

    fn write_indentation(&mut self) -> Result<(), fmt::Error> {
        write!(self.writer, "{:.*}", self.indentation, "  ")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_bool() {
        let tree = samples::bool_expression(true);

        assert_eq!(
            write_expression(&tree, 0),
            cat(&[
                "Bool(true) -> Builtin(Bool) 4@0 Gvn(ExpressionId(0))",
            ])
        )
    }

    fn write_expression(tree: &Tree, expr: u32) -> Result<String, fmt::Error> {
        let printer = TreePrinter::expression(&tree, ExpressionId::new(expr));

        write_printer(&printer)
    }

    fn write_printer(printer: &TreePrinter<'_>) -> Result<String, fmt::Error> {
        let mut s = String::new();
        printer.write(&mut s)?;
        Ok(s)
    }

    fn cat(lines: &[&str]) -> Result<String, fmt::Error> {
        let mut result = String::from("");
        for i in lines.iter() {
            result.push_str(i);
            result.push_str("\n");
        }
        Ok(result)
    }
}
