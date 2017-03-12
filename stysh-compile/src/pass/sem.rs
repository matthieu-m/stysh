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
            _ => unimplemented!(),
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
            _ => unimplemented!(),
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

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range,
            expr: sem::Expr::BuiltinCall(op, arguments),
        }
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

        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range,
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(value)),
        }
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

    #[test]
    fn first_translate() {
        let global_arena = mem::Arena::new();

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

        assert_eq!(
            semit(&global_arena, fragment, &node),
            Value {
                type_: Type::Builtin(BuiltinType::Int),
                range: range(0, 5),
                expr: Expr::BuiltinCall(
                    BuiltinFunction::Add,
                    &[ int(1, left_range), int(2, right_range) ],
                )
            }
        );
    }

    fn semit<'g>(
        global_arena: &'g mem::Arena,
        fragment: com::CodeFragment,
        node: &syn::Node
    )
        -> Value<'g>
    {
        use super::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(fragment, global_arena, &local_arena)
                .translate(node);
        local_arena.recycle();

        result
    }

    fn lit_integral(range: com::Range) -> syn::Expression<'static> {
        syn::Expression::Lit(syn::Literal::Integral, range)
    }

    fn int(value: i64, range: com::Range) -> Value<'static> {
        Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: range,
            expr: Expr::BuiltinVal(BuiltinValue::Int(value)),
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
