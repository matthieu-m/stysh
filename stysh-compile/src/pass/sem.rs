//! Semantic passes, aka name resolution, type checking, ... 
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use basic::{com, mem};

use model::{syn, sem};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'g, 'local> {
    code_fragment: com::CodeFragment,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> GraphBuilder<'g, 'local> {
    /// Creates a new instance of the graph builder.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(
        source: com::CodeFragment,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> GraphBuilder<'g, 'local>
    {
        GraphBuilder {
            code_fragment: source,
            global_arena: global,
            local_arena: local
        }
    }

    /// Translates a syntactic tree into a semantic graph.
    pub fn translate(&mut self, item: &syn::Node) -> sem::Value<'g> {
        match *item {
            syn::Node::Expr(e) => self.translate_expr(&e),
        }
    }
}

//
//  Implementation Details
//
impl<'g, 'local> GraphBuilder<'g, 'local> {
    fn translate_expr(&mut self, expr: &syn::Expression)
        -> sem::Value<'g>
    {
        use model::syn::Expression;

        match *expr {
            Expression::BinOp(op, left, right) =>
                self.translate_binary_operator(op, left, right),
            Expression::Lit(lit, range) => self.translate_literal(lit, range),
        }
    }

    fn translate_binary_operator(
        &mut self,
        op: syn::BinaryOperator,
        left: &syn::Expression,
        right: &syn::Expression
    )
        -> sem::Value<'g>
    {
        let range = left.range().extend(right.range());

        let left = self.translate_expr(left);
        let right = self.translate_expr(right);

        let op = match op {
            syn::BinaryOperator::Plus => sem::BuiltinFunction::Add,
        };

        let mut buffer = mem::Array::with_capacity(2, self.local_arena);
        buffer.push(left);
        buffer.push(right);

        let arguments = self.global_arena.insert_slice(buffer.into_slice());

        sem::Value::BuiltinCall(op, arguments, range)
    }

    fn translate_literal(&mut self, lit: syn::Literal, range: com::Range)
        -> sem::Value<'g>
    {
        match lit {
            syn::Literal::Integral => self.translate_literal_integral(range),
        }
    }

    fn translate_literal_integral(&mut self, range: com::Range)
        -> sem::Value<'g>
    {
        let mut value = 0;
        for byte in self.source(range) {
            match *byte {
                b'0'...b'9' => value += (byte - b'0') as i64,
                b'_' => (),
                _ => unimplemented!(),
            }
        }

        sem::Value::BuiltinVal(sem::BuiltinValue::Int(value), range)
    }

    fn source(&self, range: com::Range) -> &[u8] {
        &self.code_fragment[range]
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::syn;
    use model::sem::*;
    use super::GraphBuilder;

    #[test]
    fn first_translate() {
        let global_arena = mem::Arena::new();
        let mut local_arena = mem::Arena::new();

        let fragment = com::CodeFragment::new(b"1 + 2".to_vec());

        let (left_range, right_range) = (range(0, 1), range(4, 1));

        let left_hand = lit_integral(left_range);
        let right_hand = lit_integral(right_range);

        let node =
            syn::Node::Expr(
                syn::Expression::BinOp(
                    syn::BinaryOperator::Plus,
                    &left_hand,
                    &right_hand,
                )
            );

        let value =
            GraphBuilder::new(fragment, &global_arena, &local_arena)
            .translate(&node);
        local_arena.recycle();

        assert_eq!(
            value,
            Value::BuiltinCall(
                BuiltinFunction::Add,
                &[
                    Value::BuiltinVal(BuiltinValue::Int(1), left_range),
                    Value::BuiltinVal(BuiltinValue::Int(2), right_range),
                ],
                com::Range::new(0, 5)
            )
        );
    }

    fn lit_integral(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Lit(syn::Literal::Integral, range)
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
