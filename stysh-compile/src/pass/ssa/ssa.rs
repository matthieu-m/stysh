//! Control Flow passes, aka building the Control Flow Graph.
//!
//! This module is in charge of transforming the Abstract Semantic Graph (often
//! confusingly dubbed AST) into a CFG in a variant of the SSA form.

use std::collections::HashMap;

use basic::com::{self, Span};
use basic::mem::{DynArray, Ptr};

use model::{hir, sir};

use super::proto::*;

/// Stysh CFG builder.
///
/// Builds the Control-Flow Graph.
pub struct GraphBuilder;

impl GraphBuilder {
    /// Creates a new instance of a GraphBuilder.
    pub fn new() -> GraphBuilder { GraphBuilder }

    /// Translates a semantic expression into its control-flow graph.
    pub fn from_value(&self, expr: &hir::Value)
        -> sir::ControlFlowGraph
    {
        let mut imp = GraphBuilderImpl::new();

        imp.from_value(
            ProtoBlock::new(hir::Gvn(1).into()),
            expr
        );

        sir::ControlFlowGraph { blocks: imp.into_blocks() }
    }

    /// Translates a semantic function into its control-flow graph.
    pub fn from_function(&self, fun: &hir::Function)
        -> sir::ControlFlowGraph
    {
        let arguments = DynArray::with_capacity(fun.prototype.arguments.len());

        for a in &fun.prototype.arguments {
            arguments.push((a.gvn.into(), a.type_));
        }

        let mut first = ProtoBlock::new(fun.body.gvn.into());
        first.arguments = arguments;

        let mut imp = GraphBuilderImpl::new();

        imp.from_value(first, &fun.body);

        sir::ControlFlowGraph { blocks: imp.into_blocks() }
    }
}

//
//  Implementation Details
//
struct GraphBuilderImpl {
    blocks: DynArray<ProtoBlock>,
}

impl GraphBuilderImpl {
    //
    //  High-level methods
    //
    fn new() -> GraphBuilderImpl {
        GraphBuilderImpl {
            blocks: DynArray::with_capacity(1),
        }
    }

    fn from_value(
        &mut self,
        current: ProtoBlock,
        value: &hir::Value,
    )
    {
        if let Some(mut current) = self.convert_value(current, value) {
            let return_value = current.last_value();
            current.exit = ProtoTerminator::Return(return_value);

            self.blocks.push(current);
        }
    }

    fn into_blocks(&self) -> DynArray<sir::BasicBlock> {
        let map = self.resolve_arguments();

        let result = DynArray::with_capacity(self.blocks.len());

        for b in &self.blocks {
            result.push(b.into_block(&map));
        }

        result
    }

    //
    //  Low-level methods
    //
    fn convert_value(
        &mut self,
        current: ProtoBlock,
        value: &hir::Value,
    )
        -> Option<ProtoBlock>
    {
        let t = value.type_.clone();
        let r = value.range;
        let gvn = value.gvn;

        match &value.expr {
            hir::Expr::Block(stmts, v)
                => self.convert_block(current, stmts, &v.as_ref().map(|p| p.get())),
            hir::Expr::BuiltinVal(val)
                => Some(self.convert_literal(current, val, gvn, r)),
            hir::Expr::Call(callable, args)
                => Some(self.convert_call(current, callable, args, gvn, r)),
            hir::Expr::Constructor(c)
                => Some(self.convert_constructor(current, c, gvn, r)),
            hir::Expr::FieldAccess(v, f)
                => Some(self.convert_field_access(current, t, &v.get(), f, r)),
            hir::Expr::If(cond, true_, false_)
                => Some(self.convert_if(current, cond, true_, false_, t, gvn)),
            hir::Expr::Loop(stmts)
                => self.convert_loop(current, stmts, gvn),
            hir::Expr::Ref(_, gvn)
                => Some(self.convert_identifier(current, t, *gvn)),
            hir::Expr::Tuple(tuple)
                => Some(self.convert_tuple(current, t, tuple, gvn, r)),
            _ => panic!("unimplemented - convert_value - {:?}", value.expr),
        }
    }

    fn convert_value_opt(
        &mut self,
        current: ProtoBlock,
        value: &Option<hir::Value>
    )
        -> Option<ProtoBlock>
    {
        if let Some(value) = value {
            self.convert_value(current, value)
        } else {
            Some(current)
        }
    }

    fn convert_binding(
        &mut self,
        mut current: ProtoBlock,
        b: &hir::Binding,
    )
        -> ProtoBlock
    {
        current = self.convert_value(current, &b.right).expect("!Void");
        let id = current.last_value();

        self.convert_pattern(current, id, &b.left, b.right.type_.clone())
    }

    fn convert_block(
        &mut self,
        current: ProtoBlock,
        stmts: &DynArray<hir::Stmt>,
        value: &Option<hir::Value>,
    )
        -> Option<ProtoBlock>
    {
        self.convert_statements(current, stmts)
            .and_then(|current| self.convert_value_opt(current, value))
    }

    fn convert_call(
        &mut self,
        current: ProtoBlock,
        callable: &hir::Callable,
        args: &DynArray<hir::Value>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        //  :and and :or have short-circuiting semantics.
        if let hir::Callable::Builtin(b) = callable {
            use model::hir::BuiltinFunction::{And, Or};

            match b {
                And | Or => return self.convert_call_shortcircuit(
                    current,
                    *b,
                    args,
                    gvn,
                    range
                ),
                _ => (),
            };
        }

        let callable = if let hir::Callable::Builtin(b) = callable {
            sir::Callable::Builtin(*b)
        } else if let hir::Callable::Function(prototype) = callable {
            sir::Callable::Function(prototype.clone())
        } else {
            unreachable!("Incomplete HIR: {:?}", callable);
        };

        let (mut current, arguments) =
            self.convert_array_of_values(current, args);

        current.push_instr(
            gvn.into(),
            sir::Instruction::Call(callable, arguments, range)
        );

        current
    }

    fn convert_call_shortcircuit(
        &mut self,
        current: ProtoBlock,
        fun: hir::BuiltinFunction,
        args: &DynArray<hir::Value>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        //  Short circuiting expressions are close to sugar for if/else
        //  expressions.
        use model::hir::BuiltinFunction::{And, Or};

        debug_assert!(args.len() == 2, "Too many arguments: {:?}", args);

        let r = args.at(0).range;

        match fun {
            And => self.convert_if_impl(
                current,
                &args.at(0),
                &args.at(1),
                &hir::Value::bool_(false).with_range(r.offset(), r.length()),
                hir::Type::bool_(),
                gvn
            ),
            Or => self.convert_if_impl(
                current,
                &args.at(0),
                &hir::Value::bool_(true).with_range(r.offset(), r.length()),
                &args.at(1),
                hir::Type::bool_(),
                gvn
            ),
            _ => unreachable!("{:?} at {:?}", fun, range),
        }
    }

    fn convert_constructor(
        &mut self,
        current: ProtoBlock,
        cons: &hir::Constructor<hir::Value>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, &cons.arguments.fields);

        current.push_instr(
            gvn.into(),
            sir::Instruction::New(cons.type_.clone(), arguments, range)
        );

        current
    }

    fn convert_field_access(
        &mut self,
        current: ProtoBlock,
        type_: hir::Type,
        value: &hir::Value,
        field: &hir::Field,
        range: com::Range,
    )
        -> ProtoBlock
    {
        let mut current = self.convert_value(current, value).expect("!Void");
        let value_id = current.last_value();

        current.push_instr(
            value.gvn.into(),
            sir::Instruction::Field(type_, value_id, field.index(), range)
        );

        current
    }

    fn convert_identifier(
        &mut self,
        mut current: ProtoBlock,
        type_: hir::Type,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        current.last_value = Some(current.bind(gvn.into(), type_));
        current
    }

    fn convert_if(
        &mut self,
        current: ProtoBlock,
        condition: &Ptr<hir::Value>,
        true_: &Ptr<hir::Value>,
        false_: &Ptr<hir::Value>,
        type_: hir::Type,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        self.convert_if_impl(
            current,
            &condition.get(),
            &true_.get(),
            &false_.get(),
            type_,
            gvn
        )
    }

    fn convert_if_impl(
        &mut self,
        mut current: ProtoBlock,
        condition: &hir::Value,
        true_: &hir::Value,
        false_: &hir::Value,
        type_: hir::Type,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        fn create_branch(
            imp: &mut GraphBuilderImpl,
            pred_id: BlockId,
            if_id: BlockId,
            branch_id: BlockId,
            value: &hir::Value,
        )
            -> Option<BlockId>
        {
            let mut block = ProtoBlock::new(branch_id);
            block.predecessors.push(pred_id);

            block = imp.convert_value(block, value)?;

            let id = BindingId(if_id.0);
            let last_value = block.last_value();
            block.bindings.push((id, last_value, value.type_.clone()));

            block.exit = ProtoTerminator::Jump(
                ProtoJump::new(if_id)
            );

            imp.blocks.push(block);

            Some(branch_id)
        }

        let if_id: BlockId = gvn.into();
        let true_id: BlockId = true_.gvn.into();
        let false_id: BlockId = false_.gvn.into();

        current = self.convert_value(current, condition).expect("!Void");
        let current_id = current.id;

        current.exit = ProtoTerminator::Branch(
            current.last_value(),
            DynArray::new(vec!(
                ProtoJump::new(true_id),
                ProtoJump::new(false_id),
            )),
        );

        self.blocks.push(current);

        let result = ProtoBlock::new(if_id);
        result.arguments.push((BindingId(if_id.0), type_));

        if let Some(id) = create_branch(self, current_id, if_id, true_id, true_) {
            result.predecessors.push(id);
        }
        if let Some(id) = create_branch(self, current_id, if_id, false_id, false_) {
            result.predecessors.push(id);
        }

        result
    }

    fn convert_literal(
        &mut self,
        mut current: ProtoBlock,
        val: &hir::BuiltinValue,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        current.push_instr(gvn.into(), sir::Instruction::Load(val.clone(), range));
        current
    }

    fn convert_loop(
        &mut self,
        mut current: ProtoBlock,
        stmts: &DynArray<hir::Stmt>,
        gvn: hir::Gvn,
    )
        -> Option<ProtoBlock>
    {
        let current_id = current.id;
        let head_id: BlockId = gvn.into();

        current.exit = ProtoTerminator::Jump(self.jump(head_id));
        self.blocks.push(current);

        let head = ProtoBlock::new(head_id);
        head.predecessors.push(current_id);
        let head_index = self.blocks.len();

        if let Some(mut tail) = self.convert_statements(head, stmts) {
            if self.blocks.len() == head_index {
                tail.predecessors.push(tail.id);
            } else {
                self.blocks.update(
                    head_index,
                    |b| { b.predecessors.push(tail.id); b }
                );
            }

            tail.exit = ProtoTerminator::Jump(self.jump(head_id));
            self.blocks.push(tail);
        }

        None
    }

    fn convert_pattern(
        &mut self,
        mut current: ProtoBlock,
        matched: sir::ValueId,
        pattern: &hir::Pattern,
        type_: hir::Type,
    )
        -> ProtoBlock
    {
        use self::hir::Pattern::*;

        let (patterns, types) = match pattern {
            Ignored(..) => { return current; },
            Var(.., gvn) => {
                current.push_binding(gvn.into(), matched, type_);
                return current;
            },
            Constructor(pattern, ..) => (
                pattern.arguments.fields.clone(),
                type_.fields().fields.clone(),
            ),
            Tuple(pattern, ..) => (
                pattern.fields.clone(),
                type_.fields().fields.clone(),
            ),
        };

        assert_eq!(patterns.len(), types.len());

        for (index, (p, t)) in patterns.iter().zip(types.iter()).enumerate() {
            let i = index as u16;
            let id = current.push_immediate(
                sir::Instruction::Field(t.clone(), matched, i, p.span()),
            );
            current = self.convert_pattern(current, id, &p, t);
        }

        current
    }

    fn convert_rebind(
        &mut self,
        mut current: ProtoBlock,
        re: &hir::ReBinding,
    )
        -> ProtoBlock
    {
        current = self.convert_value(current, &re.right).expect("!Void");

        self.convert_rebind_recurse(current, &re.left)
    }

    fn convert_rebind_recurse(
        &mut self,
        mut current: ProtoBlock,
        left: &hir::Value,
    )
        -> ProtoBlock
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
        match &left.expr {
            hir::Expr::FieldAccess(v, f)
                => self.convert_rebind_recurse_field(current, &v.get(), *f, left.gvn),
            hir::Expr::Ref(_, gvn) => {
                let id = current.last_value();
                current.push_rebinding(gvn.into(), id, left.type_.clone());
                return current;
            },
            hir::Expr::Tuple(t)
                => self.convert_rebind_recurse_tuple(current, &t),
            _ => unimplemented!("{:?}", left),
        }
    }

    fn convert_rebind_recurse_field(
        &mut self,
        mut current: ProtoBlock,
        value: &hir::Value,
        field: hir::Field,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        let id = current.last_value();
        let index = field.index();

        //  Load tuple
        current = self.convert_value(current, value).expect("!Void");
        let tuple_id = current.last_value();

        //  Load other fields
        let fields = &value.type_.fields().fields;
        assert!(fields.len() > index as usize, "{} > {}", fields.len(), index);

        let args = DynArray::with_capacity(fields.len());

        for i in 0..(fields.len() as u16) {
            if i == index {
                args.push(id);
            } else {
                let id = current.push_immediate(sir::Instruction::Field(
                    fields.at(i as usize),
                    tuple_id,
                    i,
                    value.range
                ));
                args.push(id);
            }
        }

        //  Construct a new tuple, identical to the former except for the
        //  one new field.
        current.push_instr(
            gvn.into(),
            sir::Instruction::New(
                value.type_.clone(),
                args,
                value.range,
            ),
        );

        self.convert_rebind_recurse(current, &value)
    }

    fn convert_rebind_recurse_tuple(
        &mut self,
        mut current: ProtoBlock,
        tuple: &hir::Tuple<hir::Value>,
    )
        -> ProtoBlock
    {
        let id = current.last_value();

        for (index, field) in tuple.fields.iter().enumerate() {
            current.push_instr(
                field.gvn.into(),
                sir::Instruction::Field(
                    field.type_.clone(),
                    id,
                    index as u16,
                    field.range,
                ),
            );

            current = self.convert_rebind_recurse(current, &field);
        }

        current
    }

    fn convert_tuple(
        &mut self,
        current: ProtoBlock,
        type_: hir::Type,
        tuple: &hir::Tuple<hir::Value>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, &tuple.fields);

        current.push_instr(
            gvn.into(),
            sir::Instruction::New(type_, arguments, range)
        );

        current
    }

    fn convert_array_of_values(
        &mut self,
        mut current: ProtoBlock,
        values: &DynArray<hir::Value>,
    )
        -> (ProtoBlock, DynArray<sir::ValueId>)
    {
        for v in values {
            current = self.convert_value(current, &v).expect("!Void");
        }

        let arguments = DynArray::with_capacity(values.len());
        for a in values {
            arguments.push(current.bind(Self::binding_of(&a), a.type_));
        }

        (current, arguments)
    }

    fn convert_statements(
        &mut self,
        mut current: ProtoBlock,
        stmts: &DynArray<hir::Stmt>,
    )
        -> Option<ProtoBlock>
    {
        for s in stmts {
            match s {
                hir::Stmt::Return(r) => {
                    current =
                        self.convert_value(current, &r.value).expect("!Void");
                    current.exit =
                        ProtoTerminator::Return(current.last_value());
                    self.blocks.push(current);
                    return None;
                },
                hir::Stmt::Set(re) => {
                    current = self.convert_rebind(current, &re);
                },
                hir::Stmt::Var(b) => {
                    current = self.convert_binding(current, &b);
                },
            }
        }

        Some(current)
    }

    fn binding_of(value: &hir::Value) -> BindingId {
        match value.expr {
            hir::Expr::Ref(_, gvn) => gvn.into(),
            _ => value.gvn.into()
        }
    }

    fn jump(&self, to: BlockId) -> ProtoJump { ProtoJump::new(to) }

    fn resolve_arguments(&self) -> HashMap<BlockId, sir::BlockId> {
        //  Map each proto block ID to its definitive block ID.
        let map = {
            let mut map = HashMap::with_capacity(self.blocks.len());
            for (index, block) in self.blocks.iter().enumerate() {
                map.insert(block.id, sir::BlockId::new(index));
            }
            map
        };

        //  Ensure each predecessor forwards the correct arguments to their
        //  successor. Beware of potential loops.
        let mut bound = 1;
        while bound > 0 {
            bound = 0;

            for block_index in (0..self.blocks.len()).rev() {
                let mut block = self.blocks.at(block_index);

                let mut self_successor = false;
                for id in &block.predecessors {
                    if id == block.id {
                        self_successor = true;
                        continue;
                    }

                    let id = map.get(&id).expect("Known block!");
                    self.blocks.update(id.index(), |mut pred| {
                        bound += pred.bind_successor(block.id, &block.arguments);
                        pred
                    });
                }

                if self_successor {
                    bound += block.bind_self_successor();
                    self.blocks.replace(block_index, block);
                }
            }
        }

        //  Check that each jump correctly forwards the right number of
        //  arguments.
        for block in &self.blocks {
            for pred in &block.predecessors {
                let pred = map.get(&pred).expect("Known block!");
                let pred = self.blocks.at(pred.index());
                let jump = pred.exit.get_jump(block.id);

                debug_assert!(
                    block.arguments.len() == jump.arguments.len(),
                    "Mismatched jump from {:?} ({:?}) to {:?} ({:?})",
                    pred.id,
                    jump.arguments,
                    block.id,
                    block.arguments
                );
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
    use model::hir::builder::*;
    use model::hir::gn::*;
    use model::hir::*;
    use model::sir::*;

    #[test]
    fn value_simple() {
        let env = Env::new();

        let v = env.hir().5;
        let val = v.call().push(v.int(1, 0)).push(v.int(2, 4)).build();

        assert_eq!(
            env.valueit(val).to_string(),
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
        let env = Env::new();

        let v = env.hir().5;

        assert_eq!(
            env.valueit(
                v.tuple().push(v.int(1, 1)).push(v.int(2, 5)).build()
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
        let env = Env::new();
        let (i, _, p, _, _, v) = env.hir();

        let r = p.rec(i.id(15, 4), 15).enum_(i.id(6, 6)).build();

        assert_eq!(
            env.valueit(v.constructor(r, 30, 12).build_value()).to_string(),
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
        let env = Env::new();
        let (i, _, p, _, _, v) = env.hir();

        let r = p.rec(i.id(5, 4), 0).build();
        let rec = Type::UnresolvedRec(r, Default::default(), Default::default());

        assert_eq!(
            env.valueit(
                v.constructor(rec, 19, 8).push(v.int(42, 24)).build_value()
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
        let env = Env::new();
        let (i, _, p, _, _, v) = env.hir();

        let r = p.rec(i.id(5, 4), 0).build();
        let c =
            v.constructor(r, 23, 11)
                .push(v.int(4, 28))
                .push(v.int(42, 31))
                .build_value();

        assert_eq!(
            env.valueit(v.field_access(c).index(1).build()).to_string(),
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
        let env = Env::new();
        let (_, _, _, s, _, v) = env.hir();

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
            env.valueit(block).to_string(),
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
        let env = Env::new();
        let (_, _, _, s, _, v) = env.hir();

        //  { :var a := 1; :set a := 2; a }
        let a = v.id(7, 1);
        let block =
            v.block(v.int_ref(a, 28))
                .push(s.var_id(a, v.int(1, 12)))
                .push(s.set(v.int_ref(a, 20), v.int(2, 25)))
                .build();

        assert_eq!(
            env.valueit(block).to_string(),
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
        let env = Env::new();
        let (_, _, _, s, _, v) = env.hir();

        //  { :var a := (1, (2, 3)); :set a.1.0 := 4; a }
        let a = v.id(7, 1);

        let a_1_v = v.tuple().push(v.int(2, 17)).push(v.int(3, 20)).build();
        let a_v = v.tuple().push(v.int(1, 13)).push(a_1_v).build();

        let block =
            v.block(v.name_ref(a, 42).with_type(a_v.type_.clone()))
                .push(s.var_id(a, a_v.clone()))
                .push(s.set(
                    v.field_access(
                        v.field_access(v.name_ref(a, 30).with_type(a_v.type_))
                            .index(1)
                            .build(),
                    ).build(),
                    v.int(4, 39),
                ))
                .build();

        assert_eq!(
            env.valueit(block).to_string(),
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
    fn block_rebinding_tuple() {
        let env = Env::new();
        let (_, p, _, s, _, v) = env.hir();

        //  "{ :var (a, b) := (1, 2); :set (a, b) := (b, a); a }"
        let (a, b) = (v.id(8, 1), v.id(11, 1));
        let block =
            v.block(v.int_ref(a, 48))
                .push(s.var(
                    p.tuple().push(p.var(a)).push(p.var(b)).build(),
                    v.tuple().push(v.int(1, 18)).push(v.int(2, 21)).build(),
                ))
                .push(s.set(
                    v.tuple()
                        .push(v.int_ref(a, 31))
                        .push(v.int_ref(b, 34))
                        .build(),
                    v.tuple()
                        .push(v.int_ref(b, 41))
                        .push(v.int_ref(a, 44))
                        .build(),
                ))
                .build();

        assert_eq!(
            env.valueit(block).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@18",
                "    $1 := load 2 ; 1@21",
                "    $2 := new (Int, Int) ($0, $1) ; 6@17",
                "    $3 := field 0 of $2 ; 1@8",
                "    $4 := field 1 of $2 ; 1@11",
                "    $5 := new (Int, Int) ($4, $3) ; 6@40",
                "    $6 := field 0 of $5 ; 1@31",
                "    $7 := field 1 of $5 ; 1@34",
                "    return $6",
                ""
            ])
        );
    }

    #[test]
    fn block_constructor_binding() {
        let env = Env::new();
        let (i, p, po, s, t, v) = env.hir();

        //  :rec X(Int, Int);   { :var X(a, b) := X(1, 2); a }
        let r = po.rec(i.id(5, 1), 0).build();
        let r = i.rec(r).push(t.int()).push(t.int()).build();

        let rec = Type::Rec(r, Default::default(), Default::default());

        let (a, b) = (v.id(29, 1), v.id(32, 1));
        let binding =
            p.constructor(rec.clone(), 27, 7)
                .push(p.var(a))
                .push(p.var(b))
                .build_pattern();
        let value: Value =
            v.constructor(rec, 38, 7)
                .push(v.int(1, 40))
                .push(v.int(2, 43))
                .build_value();
        let block =
            v.block(v.name_ref(a, 47).with_type(value.type_.clone()))
                .push(s.var(binding, value))
                .build();

        assert_eq!(
            env.valueit(block).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@40",
                "    $1 := load 2 ; 1@43",
                "    $2 := new <1@5> ($0, $1) ; 7@38",
                "    $3 := field 0 of $2 ; 1@29",
                "    $4 := field 1 of $2 ; 1@32",
                "    return $3",
                ""
            ])
        );
    }

    #[test]
    fn block_return_simple() {
        let env = Env::new();
        let (_, _, _, s, _, v) = env.hir();

        //  { :return 1; }
        let block = v.block_div().push(s.ret(v.int(1, 10))).build();

        assert_eq!(
            env.valueit(block).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 1 ; 1@10",
                "    return $0",
                ""
            ])
        );
    }

    #[test]
    fn block_tuple_binding() {
        let env = Env::new();
        let (_, p, _, s, _, v) = env.hir();

        //  { :var (a, b) := (1, 2); a }
        let (a, b) = (v.id(8, 1), v.id(11, 1));
        let binding = p.tuple().push(p.var(a)).push(p.var(b)).build();
        let value = v.tuple().push(v.int(1, 18)).push(v.int(2, 21)).build();
        let block =
            v.block(v.name_ref(a, 25).with_type(value.type_.clone()))
                .push(s.var(binding, value))
                .build();

        assert_eq!(
            env.valueit(block).to_string(),
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
        let env = Env::new();
        let (i, _, p, _, t, v) = env.hir();

        let (a, b) = (v.id(9, 1), v.id(17, 1));

        let f =
            p.fun(i.id(5, 3), t.int())
                .push(a, t.int())
                .push(b, t.int())
                .range(0, 31)
                .build();

        let body =
            v.call()
                .push(v.name_ref(a, 34).with_type(t.int()))
                .push(v.name_ref(b, 38).with_type(t.int()))
                .build();

        let function = i.fun(f, v.block(body).build());

        assert_eq!(
            env.funit(function).to_string(),
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
        let env = Env::new();
        let v = env.hir().5;

        //  ":if true { 1 } :else { 2 }"
        let if_ = v.if_(v.bool_(true, 4), v.int(1, 11), v.int(2, 23)).build();

        assert_eq!(
            env.valueit(if_).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load true ; 4@4",
                "    branch $0 in [0 => <1> (), 1 => <2> ()]",
                "",
                "1 ():",
                "    $0 := load 1 ; 1@11",
                "    jump <3> ($0)",
                "",
                "2 ():",
                "    $0 := load 2 ; 1@23",
                "    jump <3> ($0)",
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

        let env = Env::new();
        let (i, _, p, _, t, v) = env.hir();

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
                .push(v.name_ref(a, 42).with_type(t.int()))
                .push(v.name_ref(b, 47).with_type(t.int()))
                .build();

        let right =
            v.call()
                .builtin(LessThan)
                .push(v.name_ref(b, 54).with_type(t.int()))
                .push(v.name_ref(c, 58).with_type(t.int()))
                .build();

        let maker = |b| {
            let call = v.call().builtin(b).push(left).push(right).build();
            i.fun(f, v.block(call).build())
        };

        //  ":fun in(a: Int, b: Int, c: Int) -> Bool { a <= b :and b < c }"
        assert_eq!(
            env.funit((maker.clone())(And)).to_string(),
            cat(&[
                "0 (Int, Int, Int):",
                "    $0 := __lte__(@0, @1) ; 6@42",
                "    branch $0 in [0 => <1> (@1, @2), 1 => <2> ()]",
                "",
                "1 (Int, Int):",
                "    $0 := __lt__(@0, @1) ; 5@54",
                "    jump <3> ($0)",
                "",
                "2 ():",
                "    $0 := load false ; 6@42",
                "    jump <3> ($0)",
                "",
                "3 (Bool):",
                "    return @0",
                ""
            ])
        );

        //  ":fun in(a: Int, b: Int, c: Int) -> Bool { a <= b :or  b < c }"
        assert_eq!(
            env.funit(maker(Or)).to_string(),
            cat(&[
                "0 (Int, Int, Int):",
                "    $0 := __lte__(@0, @1) ; 6@42",
                "    branch $0 in [0 => <1> (), 1 => <2> (@1, @2)]",
                "",
                "1 ():",
                "    $0 := load true ; 6@42",
                "    jump <3> ($0)",
                "",
                "2 (Int, Int):",
                "    $0 := __lt__(@0, @1) ; 5@54",
                "    jump <3> ($0)",
                "",
                "3 (Bool):",
                "    return @0",
                ""
            ])
        );
    }

    #[test]
    fn if_return() {
        let env = Env::new();
        let (_, _, _, s, _, v) = env.hir();

        //  ":if true { :return 1; } :else { 2 }"
        let if_ = v.if_(
            v.bool_(true, 4),
            v.block_div().push(s.ret(v.int(1, 19))).build(),
            v.block(v.int(2, 32)).build(),
        ).build();

        assert_eq!(
            env.valueit(if_).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load true ; 4@4",
                "    branch $0 in [0 => <1> (), 1 => <2> ()]",
                "",
                "1 ():",
                "    $0 := load 1 ; 1@19",
                "    return $0",
                "",
                "2 ():",
                "    $0 := load 2 ; 1@32",
                "    jump <3> ($0)",
                "",
                "3 (Int):",
                "    return @0",
                ""
            ])
        );
    }

    #[test]
    fn if_with_arguments() {
        use self::BuiltinFunction::*;

        let env = Env::new();
        let (i, _, p, _, t, v) = env.hir();

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
                    .push(v.name_ref(count, 63).with_type(t.int()))
                    .push(v.int(0, 72))
                    .build(),
                v.block(v.name_ref(current, 84).with_type(t.int()))
                    .build()
                    .with_type(t.int())
                    .with_range(74, 32),
                v.block(
                    v.call()
                        .function(proto.clone())
                        .push(v.name_ref(next, 117).with_type(t.int()))
                        .push(
                            v.call()
                                .push(v.name_ref(current, 123).with_type(t.int()))
                                .push(v.name_ref(next, 133).with_type(t.int()))
                                .build()
                        )
                        .push(
                            v.call()
                                .builtin(Substract)
                                .push(v.name_ref(count, 139).with_type(t.int()))
                                .push(v.int(1, 147))
                                .build()
                        )
                        .build()
                )
                    .build()
                    .with_type(t.int())
                    .with_range(106, 50)
            ).build();

        let fun = i.fun(proto, v.block(if_).build().with_range(53, 105));

        assert_eq!(
            env.funit(fun).to_string(),
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

    #[test]
    fn loop_increment() {
        let env = Env::new();
        let (_, _, _, s, _, v) = env.hir();

        //  "{ :var i := 0; :loop { :set i := i + 1; } }"
        let i = v.id(7, 1);
        let var = s.var_id(i, v.int(0, 12));
        let add = v.call().push(v.int_ref(i, 33)).push(v.int(1, 37)).build();
        let loop_ = v.loop_().push(s.set(v.int_ref(i, 28), add)).build();
        let block = v.block(loop_).push(var).build();

        assert_eq!(
            env.valueit(block).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 0 ; 1@12",
                "    jump <1> ($0)",
                "",
                "1 (Int):",
                "    $0 := load 1 ; 1@37",
                "    $1 := __add__(@0, $0) ; 5@33",
                "    jump <1> ($1)",
                "",
            ])
        );
    }

    #[test]
    fn loop_argument_forwarding() {
        let env = Env::new();
        let (_, _, _, s, t, v) = env.hir();

        //  "{"                                               2
        //  "    :var n := 8;"                               19
        //  "    :var current := 0;"                         41
        //  "    :loop {"                                    53
        //  "        :set n := :if n == 0 {"                 84
        //  "            :return current;"                  113
        //  "        } :else {"                             131
        //  "            :set current := current + 1;"      172
        //  "            n - 1"                             190
        //  "        };"                                    201
        //  "    }"                                         206
        //  "}"                                             208
        let (n, current) = (v.id(11, 1), v.id(28, 7));

        let block = v.block(
            v.loop_()
                .push(s.set(
                    v.int_ref(n, 66),
                    v.if_(
                        v.call()
                            .builtin(BuiltinFunction::Equal)
                            .push(v.int_ref(n, 75))
                            .push(v.int(0, 80))
                            .build(),
                        v.block_div()
                            .push(s.ret(v.int_ref(current, 104)))
                            .build(),
                        v.block(
                            v.call()
                                .builtin(BuiltinFunction::Substract)
                                .push(v.int_ref(n, 184))
                                .push(v.int(1, 188))
                                .build()
                        )
                            .push(s.set(
                                v.int_ref(current, 148),
                                v.call()
                                    .push(v.int_ref(current, 159))
                                    .push(v.int(1, 169))
                                    .build(),
                            ))
                            .build()
                            .with_type(t.int())
                    ).build()
                ))
                .build()
        )
            .push(s.var_id(n, v.int(8, 16)))
            .push(s.var_id(current, v.int(0, 39)))
            .build();

        assert_eq!(
            env.valueit(block).to_string(),
            cat(&[
                "0 ():",
                "    $0 := load 8 ; 1@16",
                "    $1 := load 0 ; 1@39",
                "    jump <1> ($0, $1)",
                "",
                "1 (Int, Int):",
                "    $0 := load 0 ; 1@80",
                "    $1 := __eq__(@0, $0) ; 6@75",
                "    branch $1 in [0 => <2> (@1), 1 => <3> (@1, @0)]",
                "",
                "2 (Int):",
                "    return @0",
                "",
                "3 (Int, Int):",
                "    $0 := load 1 ; 1@169",
                "    $1 := __add__(@0, $0) ; 11@159",
                "    $2 := load 1 ; 1@188",
                "    $3 := __sub__(@1, $2) ; 5@184",
                "    jump <4> ($3, $1)",
                "",
                "4 (Int, Int):",
                "    jump <1> (@0, @1)",
                "",
            ])
        );
    }

    struct Env;

    impl Env {
        fn new() -> Env { Env }

        fn hir(&self) -> (
            ItemFactory,
            PatternFactory,
            PrototypeFactory,
            StmtFactory,
            TypeFactory,
            ValueFactory,
        )
        {
            let f = Factory::new();
            (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.value())
        }

        fn funit(&self, fun: Function) -> ControlFlowGraph {
            let fun = GlobalNumberer::new().number_function(fun);
            println!("funit - {:#?}", fun);
            println!("");

            self.builder().from_function(&fun)
        }

        fn valueit(&self, expr: Value) -> ControlFlowGraph {
            let expr = GlobalNumberer::new().number_value(expr);
            println!("valueit - {:#?}", expr);
            println!("");

            self.builder().from_value(&expr)
        }

        fn builder(&self) -> super::GraphBuilder { super::GraphBuilder::new() }
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
