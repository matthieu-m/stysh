//! Control Flow passes, aka building the Control Flow Graph.
//!
//! This module is in charge of transforming the Abstract Semantic Graph (often
//! confusingly dubbed AST) into a CFG in a variant of the SSA form.

use std::cell::RefCell;

use basic::{com, mem};
use model::{sem, sir};

use super::proto::*;

/// Stysh CFG builder.
///
/// Builds the Control-Flow Graph.
pub struct GraphBuilder<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> GraphBuilder<'g, 'local>
    where 'g: 'local
{
    /// Creates a new instance of a GraphBuilder.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(global: &'g mem::Arena, local: &'local mem::Arena)
        -> GraphBuilder<'g, 'local>
    {
        GraphBuilder { global_arena: global, local_arena: local }
    }

    /// Translates a semantic expression into its control-flow graph.
    pub fn from_value(&self, expr: &sem::Value<'g>)
        -> sir::ControlFlowGraph<'g>
    {
        let mut imp = GraphBuilderImpl::new(
            self.global_arena,
            self.local_arena
        );

        imp.from_value(
            ProtoBlock::new(expr.range.into(), self.local_arena),
            expr
        );

        sir::ControlFlowGraph { blocks: imp.into_blocks() }
    }

    /// Translates a semantic function into its control-flow graph.
    pub fn from_function(&self, fun: &sem::Function<'g>)
        -> sir::ControlFlowGraph<'g>
    {
        let mut arguments = mem::Array::with_capacity(
            fun.prototype.arguments.len(),
            self.local_arena
        );

        for &a in fun.prototype.arguments {
            if let sem::Binding::Argument(value, type_, _) = a {
                arguments.push((value.0.into(), type_));
                continue;
            }
            panic!("All arguments should be of type Binding::Argument");
        }

        let mut first = ProtoBlock::new(
            fun.body.range.into(),
            self.local_arena
        );
        first.arguments = arguments;

        let mut imp = GraphBuilderImpl::new(
            self.global_arena,
            self.local_arena
        );

        imp.from_value(first, &fun.body);

        sir::ControlFlowGraph { blocks: imp.into_blocks() }
    }
}

//
//  Implementation Details
//
struct GraphBuilderImpl<'g, 'local>
    where 'g: 'local
{
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
    blocks: mem::Array<'local, RefCell<ProtoBlock<'g, 'local>>>,
}

impl<'g, 'local> GraphBuilderImpl<'g, 'local>
    where 'g: 'local
{
    //
    //  High-level methods
    //
    fn new(
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena,
    )
        -> GraphBuilderImpl<'g, 'local>
    {
        GraphBuilderImpl {
            global_arena: global_arena,
            local_arena: local_arena,
            blocks: mem::Array::with_capacity(1, local_arena),
        }
    }

    fn from_value(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        value: &sem::Value<'g>
    )
    {
        current = self.convert_value(current, value);

        let return_value =
            sir::ValueId::new_instruction(current.instructions.len() - 1);
        current.exit = ProtoTerminator::Return(return_value);

        self.blocks.push(RefCell::new(current));
    }

    fn into_blocks(&self) -> &'g [sir::BasicBlock<'g>] {
        let map = self.resolve_arguments();

        let mut result =
            mem::Array::with_capacity(self.blocks.len(), self.global_arena);

        for b in &self.blocks {
            result.push(b.borrow().into_block(&map, self.global_arena));
        }

        result.into_slice()
    }

    //
    //  Low-level methods
    //
    fn convert_value(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        value: &sem::Value<'g>
    )
        -> ProtoBlock<'g, 'local>
    {
        let range = value.range;

        match value.expr {
            sem::Expr::ArgumentRef(id)
                => self.convert_identifier(current, id),
            sem::Expr::Block(stmts, v)
                => self.convert_block(current, stmts, v, range),
            sem::Expr::BuiltinCall(fun, args)
                => self.convert_call(current, fun, args, range),
            sem::Expr::BuiltinVal(val)
                => self.convert_literal(current, val, range),
            sem::Expr::Tuple(tuple)
                => self.convert_tuple(current, value.type_, tuple, range),
            sem::Expr::VariableRef(id)
                => self.convert_identifier(current, id),
            _ => unimplemented!(),
        }
    }

    fn convert_block(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        stmts: &[sem::Stmt<'g>],
        value: &sem::Value<'g>,
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        for &s in stmts {
            match s {
                sem::Stmt::Var(sem::Binding::Variable(var, value, _))
                    =>
                {
                    current = self.convert_value(current, &value);
                    let id = sir::ValueId::new_instruction(
                        current.instructions.len() - 1
                    );
                    current.bindings.push((var.0.into(), id));
                },
                _ => unimplemented!(),
            }
        }

        self.convert_value(
            current,
            &sem::Value {
                type_: value.type_, 
                range: range,
                expr: value.expr
            }
        )
    }

    fn convert_call(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        fun: sem::BuiltinFunction,
        args: &[sem::Value<'g>],
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, args);

        current.push_instr(
            sir::Instruction::CallFunction(fun, arguments, range)
        );

        current
    }

    fn convert_identifier(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        value: sem::ValueIdentifier,
    )
        -> ProtoBlock<'g, 'local>
    {
        current.bind(value.into());
        current
    }

    fn convert_literal(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        val: sem::BuiltinValue<'g>,
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        current.push_instr(sir::Instruction::Load(val, range));
        current
    }

    fn convert_tuple(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        type_: sem::Type<'g>,
        tuple: sem::Tuple<'g, sem::Value<'g>>,
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, tuple.fields);

        current.push_instr(sir::Instruction::New(type_, arguments, range));

        current
    }

    fn convert_array_of_values(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        values: &[sem::Value<'g>],
    )
        -> (ProtoBlock<'g, 'local>, &'g [sir::ValueId])
    {
        for v in values {
            current = self.convert_value(current, v);
        }

        let mut arguments =
            mem::Array::with_capacity(values.len(), self.global_arena);
        for a in values {
            arguments.push(current.bind(Self::binding_of(a)));
        }

        (current, arguments.into_slice())
    }

    fn binding_of(value: &sem::Value) -> BindingId {
        match value.expr {
            sem::Expr::ArgumentRef(a) => a.into(),
            sem::Expr::VariableRef(v) => v.into(),
            _ => value.range.into()
        }
    }

    fn resolve_arguments(&self)
        -> mem::ArrayMap<'local, BlockId, sir::BlockId>
    {
        //  TODO(matthieum): Ensure that each predecessor correctly fill in the
        //  arguments of its successors.
        let map = {
            let mut map = mem::ArrayMap::with_capacity(
                self.blocks.len(),
                self.local_arena
            );
            for (index, block) in self.blocks.iter().enumerate() {
                map.insert(block.borrow().id, sir::BlockId::new(index));
            }
            map
        };
        map
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::sem;
    use model::sir::*;


    #[test]
    fn value_simple() {
        let global_arena = mem::Arena::new();

        let (left, right) = (lit_integral(1, 0, 1), lit_integral(2, 4, 1));
        let arguments = &[left, right];
        let expr_range = range(0, 5);

        let val = sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: expr_range,
            expr: sem::Expr::BuiltinCall(
                sem::BuiltinFunction::Add,
                arguments,
            )
        };

        assert_eq!(
            valueit(&global_arena, &val).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := add($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn tuple_simple() {
        let global_arena = mem::Arena::new();

        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        assert_eq!(
            valueit(
                &global_arena,
                &sem::Value {
                    type_: sem::Type::Tuple(sem::Tuple { fields: &[int, int] }),
                    range: range(0, 5),
                    expr: sem::Expr::Tuple(sem::Tuple {
                        fields: &[lit_integral(1, 0, 1), lit_integral(2, 4, 1)]
                    }),
                }
            ).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := new (Int, Int) ($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn block_simple() {
        let global_arena = mem::Arena::new();
        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        //  { :var a := 1; :var b := 2; a + b }
        let (a, b) = (value(7, 1), value(20, 1));

        assert_eq!(
            valueit(
                &global_arena,
                &sem::Value {
                    type_: sem::Type::Builtin(sem::BuiltinType::Int),
                    range: range(0, 35),
                    expr: sem::Expr::Block(
                        &[
                            sem::Stmt::Var(sem::Binding::Variable(
                                a,
                                lit_integral(1, 12, 1),
                                range(2, 12)
                            )),
                            sem::Stmt::Var(sem::Binding::Variable(
                                b,
                                lit_integral(2, 25, 1),
                                range(15, 12)
                            )),
                        ],
                        &sem::Value {
                            type_: int,
                            range: range(28, 5),
                            expr: sem::Expr::BuiltinCall(
                                sem::BuiltinFunction::Add,
                                &[
                                    resolved_variable(a, int),
                                    resolved_variable(b, int),
                                ]
                            ),
                        },
                    )
                }
            ).to_string(),
            cat(&[
                "0:",
                "    $0 := load 1 ; 1@12",
                "    $1 := load 2 ; 1@25",
                "    $2 := add($0, $1) ; 35@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn fun_simple() {
        let global_arena = mem::Arena::new();
        let int = sem::Type::Builtin(sem::BuiltinType::Int);

        let (first, second) = (value(9, 1), value(17, 1));

        assert_eq!(
            funit(
                &global_arena,
                &sem::Function {
                    prototype: &sem::FunctionProto {
                        arguments: &[
                            argument(first, int),
                            argument(second, int),
                        ],
                        result: int,
                    },
                    body: sem::Value {
                        type_: int,
                        range: range(34, 5),
                        expr: sem::Expr::BuiltinCall(
                            sem::BuiltinFunction::Add,
                            &[
                                resolved_argument(first, int),
                                resolved_argument(second, int),
                            ]
                        ),
                    }
                }
            ).to_string(),
            cat(&[
                "0:",
                "    $0 := add(@0, @1) ; 5@34",
                "    return $0",
                ""
            ])
        );
    }

    fn valueit<'g>(global_arena: &'g mem::Arena, expr: &sem::Value<'g>)
        -> ControlFlowGraph<'g>
    {
        use pass::ssa::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(global_arena, &local_arena)
                .from_value(expr);
        local_arena.recycle();

        result
    }

    fn funit<'g>(global_arena: &'g mem::Arena, fun: &sem::Function<'g>)
        -> ControlFlowGraph<'g>
    {
        use pass::ssa::GraphBuilder;

        let mut local_arena = mem::Arena::new();

        let result =
            GraphBuilder::new(global_arena, &local_arena)
                .from_function(fun);
        local_arena.recycle();

        result
    }

    fn argument<'a>(value: sem::ValueIdentifier, type_: sem::Type<'a>)
        -> sem::Binding<'a>
    {
        sem::Binding::Argument(value, type_, range(0, 0))
    }

    fn lit_integral(value: i64, offset: usize, length: usize)
        -> sem::Value<'static>
    {
        sem::Value {
            type_: sem::Type::Builtin(sem::BuiltinType::Int),
            range: range(offset, length),
            expr: sem::Expr::BuiltinVal(sem::BuiltinValue::Int(value)),
        }
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn resolved_argument<'a>(value: sem::ValueIdentifier, type_: sem::Type<'a>)
        -> sem::Value<'a>
    {
        sem::Value {
            type_: type_,
            range: range(0, 0),
            expr: sem::Expr::ArgumentRef(value),
        }
    }

    fn resolved_variable<'a>(value: sem::ValueIdentifier, type_: sem::Type<'a>)
        -> sem::Value<'a>
    {
        sem::Value {
            type_: type_,
            range: range(0, 0),
            expr: sem::Expr::VariableRef(value),
        }
    }

    fn value(start: usize, length: usize) -> sem::ValueIdentifier {
        sem::ValueIdentifier(range(start, length))
    }

    fn cat(lines: &[&str]) -> String {
        let mut result = String::from("");
        for i in lines.iter() {
            result.push_str(i);
            result.push_str("\n");
        }
        result
    }
}
