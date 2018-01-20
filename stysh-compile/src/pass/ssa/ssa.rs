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
                sem::Stmt::Var(sem::Binding::Variable(pat, value, _)) => {
                    current = self.convert_value(current, &value);
                    let id = current.last_value();
                    current =
                        self.convert_pattern(current, id, pat, value.type_);
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

    fn convert_pattern(
        &mut self,
        mut current: ProtoBlock<'g, 'local>,
        matched: sir::ValueId,
        pattern: sem::Pattern<'g>,
        type_: sem::Type<'g>,
    )
        -> ProtoBlock<'g, 'local>
    {
        fn extract_tuple_types<'b>(t: sem::Type<'b>) -> &'b [sem::Type<'b>] {
            if let sem::Type::Tuple(ref t) = t {
                return &t.fields;
            }

            unimplemented!("Expected tuple, got {:?}", t);
        }

        match pattern {
            sem::Pattern::Tuple(pat, _) => {
                let types = extract_tuple_types(type_);
                assert_eq!(pat.fields.len(), types.len());

                for (index, (p, t)) in
                    pat.fields.iter().zip(types.iter()).enumerate() {
                    let i = index as u16;
                    current.push_instr(
                        sir::Instruction::Field(*t, matched, i, p.range())
                    );
                    let id = current.last_value();
                    current = self.convert_pattern(current, id, *p, *t);
                }
            },
            sem::Pattern::Var(var) => {
                current.push_binding(var.0.into(), matched, type_);
            },
        };

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
    use basic::mem;
    use model::sem_builder::Factory as SemFactory;
    use model::sem::*;
    use model::sir::*;

    #[test]
    fn value_simple() {
        let global_arena = mem::Arena::new();

        let v = SemFactory::new(&global_arena).value();
        let val = v.call().push(v.int(1, 0)).push(v.int(2, 4)).build();

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

        let v = SemFactory::new(&global_arena).value();

        assert_eq!(
            valueit(
                &global_arena,
                &v.tuple().push(v.int(1, 1)).push(v.int(2, 5)).build()
            ).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@1",
                "    $1 := load 2 ; 1@5",
                "    $2 := new (Int, Int) ($0, $1) ; 7@0",
                "    return $2",
                ""
            ])
        );
    }

    #[test]
    fn enum_simple() {
        let global_arena = mem::Arena::new();
        let f = SemFactory::new(&global_arena);
        let (i, p, v) = (f.item(), f.proto(), f.value());

        let r = p.rec(i.id(15, 4), 15).enum_(i.id(6, 6)).build();

        assert_eq!(
            valueit(
                &global_arena,
                &v.constructor(r).build().with_range(30, 12)
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
        let f = SemFactory::new(&global_arena);
        let (i, p, v) = (f.item(), f.proto(), f.value());

        let r = p.rec(i.id(5, 4), 0).build();

        assert_eq!(
            valueit(
                &global_arena,
                &v.constructor(r).push(v.int(42, 24)).build().with_range(19, 8)
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
        let f = SemFactory::new(&global_arena);
        let (i, p, v) = (f.item(), f.proto(), f.value());

        let r = p.rec(i.id(5, 4), 0).build();
        let c =
            v.constructor(r)
                .push(v.int(4, 28))
                .push(v.int(42, 31))
                .build()
                .with_range(23, 11);

        assert_eq!(
            valueit(&global_arena, &v.field_access(1, c).build()).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 4 ; 1@28",
                "    $1 := load 2a ; 2@31",
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
        let f = SemFactory::new(&global_arena);
        let (s, v) = (f.stmt(), f.value());

        //  { :var a := 1; :var b := 2; a + b }
        let (a, b) = (v.id(7, 1), v.id(20, 1));
        let add =
            v.call().push(v.int_ref(a, 28)).push(v.int_ref(b, 32)).build();
        let block =
            v.block(add)
                .push(s.var_id(a, v.int(1, 12)))
                .push(s.var_id(b, v.int(2, 25)))
                .build();

        assert_eq!(
            valueit(&global_arena, &block).to_string(),
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
        let f = SemFactory::new(&global_arena);
        let (s, v) = (f.stmt(), f.value());

        //  { :var a := 1; :set a := 2; a }
        let a = v.id(7, 1);
        let block =
            v.block(v.int_ref(a, 28))
                .push(s.var_id(a, v.int(1, 12)))
                .push(s.set(v.int_ref(a, 20), v.int(2, 25)))
                .build();

        assert_eq!(
            valueit(&global_arena, &block).to_string(),
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
        let f = SemFactory::new(&global_arena);
        let (s, v) = (f.stmt(), f.value());

        //  { :var a := (1, (2, 3)); :set a.1.0 := 4; a }
        let a = v.id(7, 1);

        let a_1_v = v.tuple().push(v.int(2, 17)).push(v.int(3, 20)).build();
        let a_v = v.tuple().push(v.int(1, 13)).push(a_1_v).build();

        let block =
            v.block(v.var_ref(a_v.type_, a, 42))
                .push(s.var_id(a, a_v))
                .push(s.set(
                    v.field_access(
                        0,
                        v.field_access(1, v.var_ref(a_v.type_, a, 30)).build(),
                    ).build(),
                    v.int(4, 39),
                ))
                .build();

        assert_eq!(
            valueit(&global_arena, &block).to_string(),
            cat(&[
                "0 ():",
                //  :var a := (1, (2, 3));
                "    $0 := load 1 ; 1@13",
                "    $1 := load 2 ; 1@17",
                "    $2 := load 3 ; 1@20",
                "    $3 := new (Int, Int) ($1, $2) ; 6@16",
                "    $4 := new (Int, (Int, Int)) ($0, $3) ; 11@12",
                //  :set a.1.0 := 4;
                "    $5 := load 4 ; 1@39",
                "    $6 := field 1 of $4 ; 3@30",
                "    $7 := field 1 of $6 ; 3@30",
                "    $8 := new (Int, Int) ($5, $7) ; 3@30",
                "    $9 := field 0 of $4 ; 1@30",
                "    $10 := new (Int, (Int, Int)) ($9, $8) ; 1@30",
                //  a
                "    return $10",
                ""
            ])
        );
    }

    #[test]
    fn block_tuple_binding() {
        let global_arena = mem::Arena::new();
        let f = SemFactory::new(&global_arena);
        let (p, s, v) = (f.pat(), f.stmt(), f.value());

        //  { :var (a, b) := (1, 2); a }
        let (a, b) = (v.id(8, 1), v.id(11, 1));
        let binding = p.tuple().push(p.var(a)).push(p.var(b)).build();
        let value = v.tuple().push(v.int(1, 18)).push(v.int(2, 21)).build();
        let block =
            v.block(v.var_ref(value.type_, a, 25))
                .push(s.var(binding, value))
                .build();

        assert_eq!(
            valueit(&global_arena, &block).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@18",
                "    $1 := load 2 ; 1@21",
                "    $2 := new (Int, Int) ($0, $1) ; 6@17",
                "    $3 := field 0 of $2 ; 1@8",
                "    $4 := field 1 of $2 ; 1@11",
                "    return $3",
                ""
            ])
        );
    }

    #[test]
    fn fun_simple() {
        let global_arena = mem::Arena::new();
        let f = SemFactory::new(&global_arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        let (a, b) = (v.id(9, 1), v.id(17, 1));

        let f =
            p.fun(i.id(5, 3), t.int())
                .push(a, t.int())
                .push(b, t.int())
                .range(0, 31)
                .build();

        let body =
            v.call()
                .push(v.arg_ref(t.int(), a, 34))
                .push(v.arg_ref(t.int(), b, 38))
                .build();

        let function = i.fun(f, v.block(body).build());

        assert_eq!(
            funit(&global_arena, &function).to_string(),
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
        let v = SemFactory::new(&global_arena).value();

        //  ":if true { 1 } :else { 2 }"
        let if_ = v.if_(v.bool_(true, 4), v.int(1, 11), v.int(2, 23)).build();

        assert_eq!(
            valueit(&global_arena, &if_).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load true ; 4@4",
                "    branch $0 in [0 => <1> (), 1 => <2> ()]",
                "",
                "1 ():",
                "    $0 := load 1 ; 1@11",
                "    jump <0> ($0)",
                "",
                "2 ():",
                "    $0 := load 2 ; 1@23",
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

        let global_arena = mem::Arena::new();
        let f = SemFactory::new(&global_arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        let (a, b, c) = (v.id(8, 1), v.id(16, 1), v.id(24, 1));

        let f =
            p.fun(i.id(5, 2), t.bool_())
                .push(a, t.int())
                .push(b, t.int())
                .push(c, t.int())
                .range(0, 61)
                .build();

        let left = 
            v.call()
                .builtin(LessThanOrEqual)
                .push(v.arg_ref(t.int(), a, 42))
                .push(v.arg_ref(t.int(), b, 47))
                .build();

        let right =
            v.call()
                .builtin(LessThan)
                .push(v.arg_ref(t.int(), b, 54))
                .push(v.arg_ref(t.int(), c, 58))
                .build();

        let maker = |b| {
            let call = v.call().builtin(b).push(left).push(right).build();
            i.fun(f, v.block(call).build())
        };

        //  ":fun in(a: Int, b: Int, c: Int) -> Bool { a <= b :and b < c }"
        assert_eq!(
            funit(&global_arena, &maker(And)).to_string(),
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

        //  ":fun in(a: Int, b: Int, c: Int) -> Bool { a <= b :or  b < c }"
        assert_eq!(
            funit(&global_arena, &maker(Or)).to_string(),
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
        let f = SemFactory::new(&global_arena);
        let (i, p, t, v) = (f.item(), f.proto(), f.type_(), f.value());

        //  ":fun fib(current: Int, next: Int, count: Int) -> Int {"     55
        //  "    :if count == 0 {"                                       76
        //  "        current"                                            92
        //  "    } :else {"                                             106
        //  "       fib(next, current + next, count - 1)"               150
        //  "    }"                                                     157
        //  "}"                                                         159

        let (current, next, count) = (v.id(9, 7), v.id(23, 4), v.id(34, 5));

        let proto =
            p.fun(i.id(5, 3), t.int())
                .push(current, t.int())
                .push(next, t.int())
                .push(count, t.int())
                .range(0, 52)
                .build();

        let if_ =
            v.if_(
                v.call()
                    .builtin(Equal)
                    .push(v.arg_ref(t.int(), count, 63))
                    .push(v.int(0, 72))
                    .build(),
                v.block(v.arg_ref(t.int(), current, 84))
                    .build()
                    .with_range(74, 32),
                v.block(
                    v.call()
                        .function(proto)
                        .push(v.arg_ref(t.int(), next, 117))
                        .push(
                            v.call()
                                .push(v.arg_ref(t.int(), current, 123))
                                .push(v.arg_ref(t.int(), next, 133))
                                .build()
                        )
                        .push(
                            v.call()
                                .builtin(Substract)
                                .push(v.arg_ref(t.int(), count, 139))
                                .push(v.int(1, 147))
                                .build()
                        )
                        .build()
                ).build()
                    .with_range(106, 50)
            ).build();

        let fun = i.fun(proto, v.block(if_).build().with_range(53, 105));

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

    fn cat(lines: &[&str]) -> String {
        let mut result = String::from("");
        for i in lines.iter() {
            result.push_str(i);
            result.push_str("\n");
        }
        result
    }
}
