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

        let return_value = current.last_value();
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
        let r = value.range;

        match value.expr {
            sem::Expr::ArgumentRef(id)
                => self.convert_identifier(current, id),
            sem::Expr::Block(stmts, v)
                => self.convert_block(current, stmts, v),
            sem::Expr::BuiltinVal(val)
                => self.convert_literal(current, val, r),
            sem::Expr::Call(callable, args)
                => self.convert_call(current, callable, args, r),
            sem::Expr::Constructor(rec, args)
                => self.convert_constructor(current, rec, args, r),
            sem::Expr::FieldAccess(v, i)
                => self.convert_field_access(current, value.type_, v, i, r),
            sem::Expr::If(cond, true_, false_)
                => self.convert_if(current, cond, true_, false_, r),
            sem::Expr::Tuple(tuple)
                => self.convert_tuple(current, value.type_, tuple, r),
            sem::Expr::VariableRef(id)
                => self.convert_identifier(current, id),
            _ => panic!("unimplemented - convert_value - {:?}", value.expr),
        }
    }

    fn convert_block(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        stmts: &[sem::Stmt<'g>],
        value: &sem::Value<'g>,
    )
        -> ProtoBlock<'g, 'local>
    {
        for &s in stmts {
            match s {
                sem::Stmt::Set(re) => {
                    current = self.convert_rebind(current, re);
                },
                sem::Stmt::Var(sem::Binding::Variable(var, value, _)) => {
                    current = self.convert_value(current, &value);
                    let id = current.last_value();
                    let var = if let sem::Pattern::Var(var) = var {
                        var
                    } else {
                        unimplemented!("Pattern {:?}", var)
                    };
                    current.push_binding(var.0.into(), id, value.type_);
                },
                sem::Stmt::Var(sem::Binding::Argument(..)) => unimplemented!(),
            }
        }

        self.convert_value(current, value)
    }

    fn convert_call(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        callable: sem::Callable<'g>,
        args: &[sem::Value<'g>],
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        //  :and and :or have short-circuiting semantics.
        if let sem::Callable::Builtin(b) = callable {
            use model::sem::BuiltinFunction::{And, Or};

            match b {
                And | Or => return self.convert_call_shortcircuit(
                    current,
                    b,
                    args,
                    range
                ),
                _ => (),
            };
        }

        let (mut current, arguments) =
            self.convert_array_of_values(current, args);

        current.push_instr(sir::Instruction::Call(callable, arguments, range));

        current
    }

    fn convert_call_shortcircuit(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        fun: sem::BuiltinFunction,
        args: &[sem::Value<'g>],
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        //  Short circuiting expressions are close to sugar for if/else
        //  expressions.
        use model::sem::BuiltinFunction::{And, Or};

        debug_assert!(args.len() == 2, "Too many arguments: {:?}", args);

        match fun {
            And => self.convert_if_impl(
                current,
                &args[0],
                Some(&args[1]),
                None,
                range
            ),
            Or => self.convert_if_impl(
                current,
                &args[0],
                None,
                Some(&args[1]),
                range
            ),
            _ => unreachable!("{:?} at {:?}", fun, range),
        }
    }

    fn convert_constructor(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        rec: sem::RecordProto,
        args: &[sem::Value<'g>],
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, args);

        current.push_instr(
            sir::Instruction::New(sem::Type::Rec(rec), arguments, range)
        );

        current
    }

    fn convert_field_access(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        type_: sem::Type<'g>,
        value: &'g sem::Value<'g>,
        field: u16,
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        let mut current = self.convert_value(current, value);
        let value_id = current.last_value();

        current.push_instr(
            sir::Instruction::Field(type_, value_id, field, range)
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
        current.last_value = Some(current.bind(value.into()).0);
        current
    }

    fn convert_if(
        &mut self,
        current: ProtoBlock<'g, 'local>,
        condition: &sem::Value<'g>,
        true_: &sem::Value<'g>,
        false_: &sem::Value<'g>,
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        self.convert_if_impl(
            current,
            condition,
            Some(true_),
            Some(false_),
            range
        )
    }

    fn convert_if_impl(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        condition: &sem::Value<'g>,
        true_: Option<&sem::Value<'g>>,
        false_: Option<&sem::Value<'g>>,
        range: com::Range,
    )
        -> ProtoBlock<'g, 'local>
    {
        fn create_branch<'g, 'local>(
            imp: &mut GraphBuilderImpl<'g, 'local>,
            pred_id: BlockId,
            if_id: BlockId,
            branch_id: BlockId,
            value: &sem::Value<'g>,
        )
        {
            let mut block = ProtoBlock::new(branch_id, imp.local_arena);
            block.predecessors.push(pred_id);

            block = imp.convert_value(block, value);
            let last_value = block.last_value();
            block.bindings.push((BindingId(if_id.0), last_value, value.type_));

            block.exit = ProtoTerminator::Jump(
                ProtoJump::new(if_id, imp.local_arena)
            );

            imp.blocks.push(RefCell::new(block));
        }

        let if_id: BlockId = range.into();
        let true_id: Option<BlockId> = true_.map(|t| t.range.into());
        let false_id: Option<BlockId> = false_.map(|f| f.range.into());

        current = self.convert_value(current, condition);
        let current_id = current.id;

        current.exit = ProtoTerminator::Branch(
            current.last_value(),
            mem::Array::from_slice(
                &[
                    ProtoJump::new(true_id.unwrap_or(if_id), self.local_arena),
                    ProtoJump::new(false_id.unwrap_or(if_id), self.local_arena),
                ],
                self.local_arena
            ),
        );

        self.blocks.push(RefCell::new(current));

        if let Some(true_) = true_ {
            create_branch(self, current_id, if_id, true_id.unwrap(), true_);
        }
        if let Some(false_) = false_ {
            create_branch(self, current_id, if_id, false_id.unwrap(), false_);
        }

        let either_branch =
            true_.unwrap_or_else(|| false_.expect("One branch shall be valid"));

        let mut result = ProtoBlock::new(if_id, self.local_arena);
        result.arguments.push((BindingId(if_id.0), either_branch.type_));
        result.predecessors.extend(&[
            true_id.unwrap_or(current_id),
            false_id.unwrap_or(current_id),
        ]);
        result
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

    fn convert_rebind(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        re: sem::ReBinding<'g>,
    )
        -> ProtoBlock<'g, 'local>
    {
        //  Rebinding is binding an existing name to a new value.
        //
        //  This means that despite the familiar syntax "a.0 := 1;" does NOT
        //  actually modifies "a". Instead, it creates a new value of "a",
        //  which is identical to the former "a" except for the one updated
        //  field.
        //
        //  That is in:
        //      :var a := (1, 2);
        //      :set a.0 := 3;
        //
        //  the rebinding is actually translated as:
        //      :set a := (3, a.1);
        //
        //  And similarly in:
        //      :set a.1.2 := 3;
        //
        //  the rebinding is actually translated as:
        //      :set a := (a.0, (a.1.0, a.1.1, 3, a.1.3, ...), a.2, ...);
        //
        //  where I expect the motivation for the syntactic sugar to be clear.
        //
        //  This only really matters if the value is aliased, but alias
        //  analysis is the job of the optimizer; here only semantics matter.
        //
        //  The following bit of code captures this reversal.

        fn extract_fields<'a, 'g>(v: &'a sem::Value<'g>)
            -> &'a [sem::Type<'g>]
        {
            //  TODO(matthieum): support Rec, which requires a Registry.
            if let sem::Type::Tuple(t) = v.type_ {
                return &t.fields;
            }

            unimplemented!("Can only access tuple fields!");
        }

        fn recurse<'g, 'local>(
            me: &mut GraphBuilderImpl<'g, 'local>,
            mut current: ProtoBlock<'g, 'local>,
            left: &sem::Value<'g>
        )
            -> ProtoBlock<'g, 'local>
        {
            if let sem::Expr::VariableRef(n) = left.expr {
                let id = current.last_value();
                current.push_rebinding(n.0.into(), id, left.type_);
                return current;
            }

            let (value, index) =
                if let sem::Expr::FieldAccess(v, i) = left.expr {
                    (v, i)
                } else {
                    unimplemented!("{:?}", left);
                };

            let id = current.last_value();

            //  Load tuple
            current = me.convert_value(current, value);
            let tuple_id = current.last_value();

            //  Load other fields
            let fields = extract_fields(&value);
            let mut args =
                mem::Array::with_capacity(fields.len(), me.global_arena);

            for i in 0..(fields.len() as u16) {
                if i == index {
                    args.push(id);
                } else {
                    current.push_instr(sir::Instruction::Field(
                        fields[i as usize],
                        tuple_id,
                        i,
                        value.range
                    ));
                    args.push(current.last_value());
                }
            }

            //  Construct a new tuple, identical to the former except for the
            //  one new field.
            current.push_instr(sir::Instruction::New(
                value.type_,
                args.into_slice(),
                value.range,
            ));

            recurse(me, current, &value)
        }

        current = self.convert_value(current, &re.right);

        recurse(self, current, &re.left)
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
            arguments.push(current.bind(Self::binding_of(a)).0);
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
        //  Map each proto block ID to its definitive block ID.
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

        let mut bindings = mem::Array::new(self.local_arena);

        //  Ensure each predecessor forwards the correct arguments to their
        //  successor.
        for block in self.blocks.iter().rev() {
            let block = block.borrow();

            bindings.clear();
            for &(argument, _) in block.arguments.as_slice() {
                bindings.push(argument);
            }

            for id in block.predecessors.as_slice() {
                let id = map.get(id).expect("Known block!");
                let mut pred = self.blocks[id.index()].borrow_mut();
                pred.bind_successor(block.id, &*bindings);
            }
        }

        //  Propagate the types of the arguments from predecessor to successor.
        for block in self.blocks.iter() {
            let mut block = block.borrow_mut();

            if let Some(&pred) = block.predecessors.get(0) {
                let pred = map.get(&pred).expect("Known block!");
                let pred = self.blocks[pred.index()].borrow();

                let jump = pred.exit.get_jump(block.id);
                block.set_arguments_types(jump);
            }
        }

        map
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::sem::*;
    use model::sir::*;


    #[test]
    fn value_simple() {
        let global_arena = mem::Arena::new();

        let (left, right) = (lit_integral(1, 0, 1), lit_integral(2, 4, 1));
        let arguments = &[left, right];
        let expr_range = range(0, 5);

        let val = Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: expr_range,
            expr: Expr::Call(
                Callable::Builtin(BuiltinFunction::Add),
                arguments,
            )
        };

        assert_eq!(
            valueit(&global_arena, &val).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := __add__($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn tuple_simple() {
        let global_arena = mem::Arena::new();

        let int = Type::Builtin(BuiltinType::Int);

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Tuple(Tuple { fields: &[int, int] }),
                    range: range(0, 5),
                    expr: Expr::Tuple(Tuple {
                        fields: &[lit_integral(1, 0, 1), lit_integral(2, 4, 1)]
                    }),
                }
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@0",
                "    $1 := load 2 ; 1@4",
                "    $2 := new (Int, Int) ($0, $1) ; 5@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn enum_simple() {
        let global_arena = mem::Arena::new();

        let basic_rec_prototype = RecordProto {
            name: ItemIdentifier(range(15, 4)),
            range: range(15, 4),
            enum_: ItemIdentifier(range(6, 6)),
        };

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Rec(basic_rec_prototype),
                    range: range(30, 12),
                    expr: Expr::Constructor(basic_rec_prototype, &[]),
                }
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := new <4@15> () ; 12@30",
                "    return $0",
                ""
            ])
        );
    }

    #[test]
    fn record_arguments() {
        //  ":rec Args(Int);    Args(42)"
        let global_arena = mem::Arena::new();

        let basic_rec_prototype = RecordProto {
            name: ItemIdentifier(range(5, 4)),
            range: range(0, 15),
            enum_: ItemIdentifier::unresolved(),
        };

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Rec(basic_rec_prototype),
                    range: range(19, 8),
                    expr: Expr::Constructor(
                        basic_rec_prototype,
                        &[
                            lit_integral(42, 24, 2)
                        ]
                    ),
                }
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 2a ; 2@24",
                "    $1 := new <4@5> ($0) ; 8@19",
                "    return $1",
                ""
            ])
        );
    }

    #[test]
    fn record_field() {
        //  ":rec Args(Int, Int);   Args(4, 42).1"
        let global_arena = mem::Arena::new();

        let basic_rec_prototype = RecordProto {
            name: ItemIdentifier(range(5, 4)),
            range: range(0, 20),
            enum_: ItemIdentifier::unresolved(),
        };

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Builtin(BuiltinType::Int),
                    range: range(23, 13),
                    expr: Expr::FieldAccess(
                        &Value {
                            type_: Type::Rec(basic_rec_prototype),
                            range: range(23, 11),
                            expr: Expr::Constructor(
                                basic_rec_prototype,
                                &[
                                    lit_integral(4, 28, 2),
                                    lit_integral(42, 31, 3),
                                ]
                            ),
                        },
                        1
                    ),
                },
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 4 ; 2@28",
                "    $1 := load 2a ; 3@31",
                "    $2 := new <4@5> ($0, $1) ; 11@23",
                "    $3 := field 1 of $2 ; 13@23",
                "    return $3",
                ""
            ])
        );
    }

    #[test]
    fn block_simple() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        //  { :var a := 1; :var b := 2; a + b }
        let (a, b) = (value(7, 1), value(20, 1));

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Builtin(BuiltinType::Int),
                    range: range(0, 35),
                    expr: Expr::Block(
                        &[
                            Stmt::Var(Binding::Variable(
                                Pattern::Var(a),
                                lit_integral(1, 12, 1),
                                range(2, 12)
                            )),
                            Stmt::Var(Binding::Variable(
                                Pattern::Var(b),
                                lit_integral(2, 25, 1),
                                range(15, 12)
                            )),
                        ],
                        &Value {
                            type_: int,
                            range: range(28, 5),
                            expr: Expr::Call(
                                Callable::Builtin(
                                    BuiltinFunction::Add
                                ),
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
                "0 ():",
                "    $0 := load 1 ; 1@12",
                "    $1 := load 2 ; 1@25",
                "    $2 := __add__($0, $1) ; 5@28",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn block_rebinding() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        //  { :var a := 1; :set a := 2; a }
        let a = value(7, 1);

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Builtin(BuiltinType::Int),
                    range: range(0, 35),
                    expr: Expr::Block(
                        &[
                            Stmt::Var(Binding::Variable(
                                Pattern::Var(a),
                                lit_integral(1, 12, 1),
                                range(2, 12)
                            )),
                            Stmt::Set(ReBinding {
                                left: resolved_variable(a, int),
                                right: lit_integral(2, 25, 1),
                                range: range(15, 12)
                            }),
                        ],
                        &resolved_variable(a, int),
                    )
                }
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@12",
                "    $1 := load 2 ; 1@25",
                "    return $1",
                ""
            ])
        );
    }

    #[test]
    fn block_rebinding_nested_field() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);
        let t_a_1_slice = &[int, int];
        let t_a_1 = Type::Tuple(Tuple { fields: t_a_1_slice });
        let t_a_slice = &[int, t_a_1];
        let t_a = Type::Tuple(Tuple { fields: t_a_slice });

        //  { :var a := (1, (2, 3)); :set a.1.0 := 4; a }
        let a = value(7, 1);

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: Type::Builtin(BuiltinType::Int),
                    range: range(0, 45),
                    expr: Expr::Block(
                        &[
                            Stmt::Var(Binding::Variable(
                                Pattern::Var(a),
                                Value {
                                    type_: t_a,
                                    range: range(12, 11),
                                    expr: Expr::Tuple(Tuple { fields: &[
                                        lit_integral(1, 13, 1),
                                        Value {
                                            type_: t_a_1,
                                            range: range(16, 6),
                                            expr: Expr::Tuple(Tuple { fields: &[
                                                lit_integral(2, 17, 1),
                                                lit_integral(3, 19, 1),
                                            ]}),
                                        }
                                    ]}),
                                },
                                range(2, 22)
                            )),
                            Stmt::Set(ReBinding {
                                left: Value {
                                    type_: int,
                                    range: range(30, 5),
                                    expr: Expr::FieldAccess(
                                        &Value {
                                            type_: t_a_1,
                                            range: range(30, 3),
                                            expr: Expr::FieldAccess(
                                                &resolved_variable(a, t_a),
                                                1
                                            ),
                                        },
                                        0
                                    ),
                                },
                                right: lit_integral(4, 39, 1),
                                range: range(25, 16)
                            }),
                        ],
                        &resolved_variable(a, t_a),
                    )
                }
            ).to_string(),
            cat(&[
                "0 ():",
                //  :var a := (1, (2, 3));
                "    $0 := load 1 ; 1@13",
                "    $1 := load 2 ; 1@17",
                "    $2 := load 3 ; 1@19",
                "    $3 := new (Int, Int) ($1, $2) ; 6@16",
                "    $4 := new (Int, (Int, Int)) ($0, $3) ; 11@12",
                //  :set a.1.0 := 4;
                "    $5 := load 4 ; 1@39",
                "    $6 := field 1 of $4 ; 3@30",
                "    $7 := field 1 of $6 ; 3@30",
                "    $8 := new (Int, Int) ($5, $7) ; 3@30",
                "    $9 := field 0 of $4 ; 0@0",
                "    $10 := new (Int, (Int, Int)) ($9, $8) ; 0@0",
                //  a
                "    return $10",
                ""
            ])
        );
    }

    #[test]
    fn fun_simple() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        let (first, second) = (value(9, 1), value(17, 1));

        assert_eq!(
            funit(
                &global_arena,
                &Function {
                    prototype: &FunctionProto {
                        name: ItemIdentifier(range(0, 0)),
                        range: range(0, 0),
                        arguments: &[
                            argument(first, int),
                            argument(second, int),
                        ],
                        result: int,
                    },
                    body: Value {
                        type_: int,
                        range: range(34, 5),
                        expr: Expr::Call(
                            Callable::Builtin(BuiltinFunction::Add),
                            &[
                                resolved_argument(first, int),
                                resolved_argument(second, int),
                            ]
                        ),
                    }
                }
            ).to_string(),
            cat(&[
                "0 (Int, Int):",
                "    $0 := __add__(@0, @1) ; 5@34",
                "    return $0",
                ""
            ])
        );
    }

    #[test]
    fn if_simple() {
        let global_arena = mem::Arena::new();
        let int = Type::Builtin(BuiltinType::Int);

        //  "if true { 1 } else { 2 }"

        assert_eq!(
            valueit(
                &global_arena,
                &Value {
                    type_: int,
                    range: range(0, 24),
                    expr: Expr::If(
                        &bool_literal(true, 3, 4),
                        &Value {
                            type_: int,
                            range: range(8, 5),
                            expr: Expr::Block(
                                &[],
                                &lit_integral(1, 10, 1),
                            ),
                        },
                        &Value {
                            type_: int,
                            range: range(19, 5),
                            expr: Expr::Block(
                                &[],
                                &lit_integral(2, 21, 1),
                            ),
                        }
                    )
                }
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load true ; 4@3",
                "    branch $0 in [0 => <1> (), 1 => <2> ()]",
                "",
                "1 ():",
                "    $0 := load 1 ; 1@10",
                "    jump <0> ($0)",
                "",
                "2 ():",
                "    $0 := load 2 ; 1@21",
                "    jump <0> ($0)",
                "",
                "3 (Int):",
                "    return @0",
                ""
            ])
        );
    }

    #[test]
    fn if_shortcircuit() {
        use self::BuiltinFunction::*;

        //  ":fun in(a: Int, b: Int, c: Int) -> Bool { ... }"
        fn maker<'g>(arena: &'g mem::Arena, content: Value)
            -> &'g Function<'g>
        {
            let bool_ = Type::Builtin(BuiltinType::Bool);
            let int = Type::Builtin(BuiltinType::Int);
            let (a, b, c) = (value(8, 1), value(16, 1), value(24, 1));

            let proto = arena.intern(&FunctionProto {
                name: ItemIdentifier(range(5, 2)),
                range: range(0, 39),
                arguments: &[
                    argument(a, int),
                    argument(b, int),
                    argument(c, int),
                ],
                result: bool_,
            });

            let fun = arena.intern(&Function {
                prototype: &proto,
                body: block(
                    bool_,
                    range(53, 105),
                    &[],
                    &content
                )
            });

            arena.insert(fun)
        }

        let global_arena = mem::Arena::new();
        let bool_ = Type::Builtin(BuiltinType::Bool);
        let int = Type::Builtin(BuiltinType::Int);

        let (a, b, c) = (value(8, 1), value(16, 1), value(24, 1));

        let left = global_arena.intern(&call(
            bool_,
            range(42, 6),
            Callable::Builtin(LessThanOrEqual),
            &[
                resolved_argument(a, int),
                resolved_argument(b, int),
            ]
        ));

        let right = global_arena.intern(&call(
            bool_,
            range(54, 5),
            Callable::Builtin(LessThan),
            &[
                resolved_argument(b, int),
                resolved_argument(c, int),
            ]
        ));

        //  "a <= b :and b < c"

        assert_eq!(
            funit(
                &global_arena,
                maker(
                    &global_arena,
                    call(
                        bool_,
                        range(42, 17),
                        Callable::Builtin(And),
                        &[ left, right ]
                    )
                )
            ).to_string(),
            cat(&[
                "0 (Int, Int, Int):",
                "    $0 := __lte__(@0, @1) ; 6@42",
                "    branch $0 in [0 => <1> (@1, @2), 1 => <2> ($0)]",
                "",
                "1 (Int, Int):",
                "    $0 := __lt__(@0, @1) ; 5@54",
                "    jump <2> ($0)",
                "",
                "2 (Bool):",
                "    return @0",
                ""
            ])
        );

        //  "a <= b :or  b < c"

        assert_eq!(
            funit(
                &global_arena,
                maker(
                    &global_arena,
                    call(
                        bool_,
                        range(42, 17),
                        Callable::Builtin(Or),
                        &[ left, right ]
                    )
                )
            ).to_string(),
            cat(&[
                "0 (Int, Int, Int):",
                "    $0 := __lte__(@0, @1) ; 6@42",
                "    branch $0 in [0 => <2> ($0), 1 => <1> (@1, @2)]",
                "",
                "1 (Int, Int):",
                "    $0 := __lt__(@0, @1) ; 5@54",
                "    jump <2> ($0)",
                "",
                "2 (Bool):",
                "    return @0",
                ""
            ])
        );
    }

    #[test]
    fn if_with_arguments() {
        use self::BuiltinFunction::*;

        let global_arena = mem::Arena::new();
        let bool_ = Type::Builtin(BuiltinType::Bool);
        let int = Type::Builtin(BuiltinType::Int);

        //  ":fun fib(current: Int, next: Int, count: Int) -> Int {"     55
        //  "    :if count == 0 {"                                       76
        //  "        current"                                            92
        //  "    } :else {"                                             106
        //  "       fib(next, current + next, count - 1)"               150
        //  "    }"                                                     157
        //  "}"                                                         159

        let (current, next, count) = (value(9, 7), value(23, 4), value(34, 5));

        let proto = global_arena.intern(&FunctionProto {
            name: ItemIdentifier(range(5, 3)),
            range: range(0, 52),
            arguments: &[
                argument(current, int),
                argument(next, int),
                argument(count, int),
            ],
            result: int,
        });

        let fun = global_arena.intern(&Function {
            prototype: &proto,
            body: block(
                int,
                range(53, 105),
                &[],
                &if_(
                    int,
                    59,
                    &call(
                        bool_,
                        range(63, 10),
                        Callable::Builtin(Equal),
                        &[
                            resolved_argument(count, int),
                            lit_integral(0, 72, 1),
                        ]
                    ),
                    &block(
                        int,
                        range(74, 32),
                        &[],
                        &resolved_argument(current, int),
                    ),
                    &block(
                        int,
                        range(106, 50),
                        &[],
                        &call(
                            int,
                            range(113, 36),
                            Callable::Function(proto),
                            &[
                                resolved_argument(next, int),
                                call(
                                    int,
                                    range(123, 14),
                                    Callable::Builtin(Add),
                                    &[
                                        resolved_argument(current, int),
                                        resolved_argument(next, int),
                                    ]
                                ),
                                call(
                                    int,
                                    range(139, 9),
                                    Callable::Builtin(Substract),
                                    &[
                                        resolved_argument(count, int),
                                        lit_integral(1, 147, 1)
                                    ]
                                )
                            ]
                        ),
                    )
                )
            )
        });

        assert_eq!(
            funit(&global_arena, &fun).to_string(),
            cat(&[
                "0 (Int, Int, Int):",
                "    $0 := load 0 ; 1@72",
                "    $1 := __eq__(@2, $0) ; 10@63",
                "    branch $1 in [0 => <1> (@0), 1 => <2> (@1, @0, @2)]",
                "",
                "1 (Int):",
                "    jump <3> (@0)",
                "",
                "2 (Int, Int, Int):",
                "    $0 := __add__(@1, @0) ; 14@123",
                "    $1 := load 1 ; 1@147",
                "    $2 := __sub__(@2, $1) ; 9@139",
                "    $3 := <3@5>(@0, $0, $2) ; 36@113",
                "    jump <3> ($3)",
                "",
                "3 (Int):",
                "    return @0",
                "",
            ])
        )
    }

    fn valueit<'g>(global_arena: &'g mem::Arena, expr: &Value<'g>)
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

    fn funit<'g>(global_arena: &'g mem::Arena, fun: &Function<'g>)
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

    fn argument<'a>(value: ValueIdentifier, type_: Type<'a>)
        -> Binding<'a>
    {
        Binding::Argument(value, type_, range(0, 0))
    }

    fn bool_literal(value: bool, offset: usize, length: usize)
        -> Value<'static>
    {
        Value {
            type_: Type::Builtin(BuiltinType::Bool),
            range: range(offset, length),
            expr: Expr::BuiltinVal(BuiltinValue::Bool(value)),
        }
    }

    fn block<'a>(
        type_: Type<'a>,
        range: com::Range,
        stmts: &'a [Stmt<'a>],
        value: &'a Value<'a>
    )
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: range,
            expr: Expr::Block(stmts, value)
        }
    }

    fn call<'a>(
        type_: Type<'a>,
        range: com::Range,
        callable: Callable<'a>,
        arguments: &'a [Value<'a>]
    )
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: range,
            expr: Expr::Call(
                callable,
                arguments,
            )
        }
    }

    fn if_<'a>(
        type_: Type<'a>,
        offset: usize,
        condition: &'a Value<'a>,
        true_branch: &'a Value<'a>,
        false_branch: &'a Value<'a>
    )
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: com::Range::new(offset, 0).extend(false_branch.range),
            expr: Expr::If(
                condition,
                true_branch,
                false_branch
            )
        }
    }

    fn lit_integral(value: i64, offset: usize, length: usize)
        -> Value<'static>
    {
        Value {
            type_: Type::Builtin(BuiltinType::Int),
            range: range(offset, length),
            expr: Expr::BuiltinVal(BuiltinValue::Int(value)),
        }
    }

    fn range(offset: usize, length: usize) -> com::Range {
        com::Range::new(offset, length)
    }

    fn resolved_argument<'a>(value: ValueIdentifier, type_: Type<'a>)
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: range(0, 0),
            expr: Expr::ArgumentRef(value),
        }
    }

    fn resolved_variable<'a>(value: ValueIdentifier, type_: Type<'a>)
        -> Value<'a>
    {
        Value {
            type_: type_,
            range: range(0, 0),
            expr: Expr::VariableRef(value),
        }
    }

    fn value(start: usize, length: usize) -> ValueIdentifier {
        ValueIdentifier(range(start, length))
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
