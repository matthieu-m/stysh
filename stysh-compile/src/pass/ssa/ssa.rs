//! Control Flow passes, aka building the Control Flow Graph.
//!
//! This module is in charge of transforming the Abstract Semantic Graph (often
//! confusingly dubbed AST) into a CFG in a variant of the SSA form.

use std::{cell, collections::HashMap, fmt};

use basic::com;
use basic::mem::DynArray;
use basic::sea::{Table, TableIndex};

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
    pub fn from_expression(&self, tree: &hir::Tree, expr: hir::ExpressionId)
        -> sir::ControlFlowGraph
    {
        let mut imp = GraphBuilderImpl::new(tree);

        imp.from_expression(
            ProtoBlock::new(hir::Gvn(0).into()),
            expr
        );

        sir::ControlFlowGraph { blocks: imp.into_blocks() }
    }

    /// Translates a semantic function into its control-flow graph.
    pub fn from_function(&self, fun: &hir::Function)
        -> sir::ControlFlowGraph
    {
        let (args, expr) = if let Some(hir::Root::Function(_, args, _, expr)) =
            fun.body.get_root()
        {
            (args, expr)
        } else {
            panic!("Function body is not an expression!");
        };

        let patterns = fun.body.get_patterns(args.fields);
        let arguments = DynArray::with_capacity(fun.prototype.arguments.len());

        for (&p, a) in patterns.iter().zip(fun.prototype.arguments.iter()) {
            arguments.push((p.into(), a.type_));
        }

        let mut first = ProtoBlock::new(expr.into());
        first.arguments = arguments;

        let mut imp = GraphBuilderImpl::new(&fun.body);

        imp.from_expression(first, expr);

        sir::ControlFlowGraph { blocks: imp.into_blocks() }
    }
}

//
//  Implementation Details
//
struct LocalExpressionId(com::CoreId);

struct LocalExpression {
    expr: hir::Expr,
    type_: hir::Type,
    range: com::Range,
}

struct GraphBuilderImpl<'a> {
    tree: &'a hir::Tree,
    blocks: DynArray<ProtoBlock>,
    expression_offset: u32,
    expression: cell::RefCell<Table<LocalExpressionId, LocalExpression>>,
}

impl<'a> GraphBuilderImpl<'a> {
    //
    //  High-level methods
    //
    fn new(tree: &'a hir::Tree) -> GraphBuilderImpl {
        let expression_offset = tree.len_expressions() as u32;

        GraphBuilderImpl {
            tree,
            blocks: DynArray::with_capacity(1),
            expression_offset,
            expression: Default::default(),
        }
    }

    fn from_expression(
        &mut self,
        current: ProtoBlock,
        expr: hir::ExpressionId,
    )
    {
        if let Some(mut current) = self.convert_expression(current, expr) {
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
    fn convert_expression(
        &mut self,
        current: ProtoBlock,
        id: hir::ExpressionId,
    )
        -> Option<ProtoBlock>
    {
        let t = self.convert_type(self.get_expression_type(id));
        let r = self.get_expression_range(id);
        let gvn = id.into();

        match self.get_expression(id) {
            hir::Expr::Block(stmts, e)
                => self.convert_block(current, stmts, e),
            hir::Expr::BuiltinVal(val)
                => Some(self.convert_literal(current, val, gvn, r)),
            hir::Expr::Call(callable, args)
                => Some(self.convert_call(current, callable, args, gvn, r)),
            hir::Expr::Constructor(c)
                => Some(self.convert_constructor(current, c, gvn, r)),
            hir::Expr::FieldAccess(e, f)
                => Some(self.convert_field_access(current, t, e, f, r)),
            hir::Expr::If(cond, true_, false_)
                => Some(self.convert_if(current, cond, true_, false_, t, gvn)),
            hir::Expr::Loop(stmts)
                => self.convert_loop(current, stmts, gvn),
            hir::Expr::Ref(_, gvn)
                => Some(self.convert_identifier(current, t, gvn)),
            hir::Expr::Tuple(tuple)
                => Some(self.convert_tuple(current, t, tuple, gvn, r)),
            expr
                => panic!("unimplemented - convert_expression - {:?}", expr),
        }
    }

    fn convert_expression_opt(
        &mut self,
        current: ProtoBlock,
        expr: Option<hir::ExpressionId>
    )
        -> Option<ProtoBlock>
    {
        if let Some(expr) = expr {
            self.convert_expression(current, expr)
        } else {
            Some(current)
        }
    }

    fn convert_binding(
        &mut self,
        mut current: ProtoBlock,
        b: hir::Binding,
    )
        -> ProtoBlock
    {
        let ty = self.convert_type_of(b.right.into());

        current = self.convert_expression(current, b.right).expect("!Void");
        let id = current.last_value();

        self.convert_pattern(current, id, b.left, ty)
    }

    fn convert_block(
        &mut self,
        current: ProtoBlock,
        stmts: hir::Id<[hir::Stmt]>,
        value: Option<hir::ExpressionId>,
    )
        -> Option<ProtoBlock>
    {
        self.convert_statements(current, stmts)
            .and_then(|current| self.convert_expression_opt(current, value))
    }

    fn convert_call(
        &mut self,
        current: ProtoBlock,
        callable: hir::Callable,
        args: hir::Tuple<hir::ExpressionId>,
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
                    b,
                    args,
                    gvn,
                    range
                ),
                _ => (),
            };
        }

        let callable = if let hir::Callable::Builtin(b) = callable {
            sir::Callable::Builtin(b)
        } else if let hir::Callable::Function(name, args, res) = callable {
            let prototype = self.convert_function_prototype(name, args, res);
            sir::Callable::Function(prototype)
        } else {
            unreachable!("Incomplete HIR: {:?}", callable);
        };

        let (mut current, arguments) =
            self.convert_array_of_values(current, args.fields);

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
        args: hir::Tuple<hir::ExpressionId>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        //  Short circuiting expressions are close to sugar for if/else
        //  expressions.
        use model::hir::BuiltinFunction::{And, Or};

        let arguments = self.tree.get_expressions(args.fields);
        debug_assert!(arguments.len() == 2, "Too many arguments: {:?}", arguments);

        let r = self.get_expression_range(arguments[0]);
        let t = hir::Type::bool_();

        match fun {
            And => {
                let other =
                    self.push_local_expression(hir::Expr::bool_(false), t, r);
                self.convert_if_impl(
                    current,
                    arguments[0],
                    arguments[1],
                    other,
                    hir::TypeDefinition::bool_(),
                    gvn
                )
            },
            Or => {
                let other =
                    self.push_local_expression(hir::Expr::bool_(true), t, r);
                self.convert_if_impl(
                    current,
                    arguments[0],
                    other,
                    arguments[1],
                    hir::TypeDefinition::bool_(),
                    gvn
                )
            },
            _ => unreachable!("{:?} at {:?}", fun, range),
        }
    }

    fn convert_constructor(
        &mut self,
        current: ProtoBlock,
        cons: hir::Tuple<hir::ExpressionId>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, cons.fields);

        let ty = self.convert_type_of(gvn);

        current.push_instr(
            gvn.into(),
            sir::Instruction::New(ty, arguments, range)
        );

        current
    }

    fn convert_field_access(
        &mut self,
        current: ProtoBlock,
        type_: hir::TypeDefinition,
        expr: hir::ExpressionId,
        field: hir::Field,
        range: com::Range,
    )
        -> ProtoBlock
    {
        let mut current = self.convert_expression(current, expr).expect("!Void");
        let value_id = current.last_value();

        current.push_instr(
            expr.into(),
            sir::Instruction::Field(type_, value_id, field.index(), range)
        );

        current
    }

    fn convert_identifier(
        &mut self,
        mut current: ProtoBlock,
        type_: hir::TypeDefinition,
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
        condition: hir::ExpressionId,
        true_: hir::ExpressionId,
        false_: hir::ExpressionId,
        type_: hir::TypeDefinition,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        self.convert_if_impl(
            current,
            condition,
            true_,
            false_,
            type_,
            gvn
        )
    }

    fn convert_if_impl(
        &mut self,
        mut current: ProtoBlock,
        condition: hir::ExpressionId,
        true_: hir::ExpressionId,
        false_: hir::ExpressionId,
        type_: hir::TypeDefinition,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        fn create_branch(
            imp: &mut GraphBuilderImpl,
            pred_id: BlockId,
            if_id: BlockId,
            branch_id: BlockId,
            expr: hir::ExpressionId,
            type_: hir::TypeDefinition,
        )
            -> Option<BlockId>
        {
            let mut block = ProtoBlock::new(branch_id);
            block.predecessors.push(pred_id);

            block = imp.convert_expression(block, expr)?;

            let id = BindingId(if_id.0);
            let last_value = block.last_value();
            block.bindings.push((id, last_value, type_));

            block.exit = ProtoTerminator::Jump(
                ProtoJump::new(if_id)
            );

            imp.blocks.push(block);

            Some(branch_id)
        }

        let if_id: BlockId = gvn.into();
        let true_id: BlockId = true_.into();
        let false_id: BlockId = false_.into();

        current = self.convert_expression(current, condition).expect("!Void");
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
        result.arguments.push((BindingId(if_id.0), type_.clone()));

        if let Some(id) = create_branch(self, current_id, if_id, true_id, true_, type_.clone()) {
            result.predecessors.push(id);
        }
        if let Some(id) = create_branch(self, current_id, if_id, false_id, false_, type_) {
            result.predecessors.push(id);
        }

        result
    }

    fn convert_literal(
        &mut self,
        mut current: ProtoBlock,
        val: hir::BuiltinValue,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        current.push_instr(gvn.into(), sir::Instruction::Load(val, range));
        current
    }

    fn convert_loop(
        &mut self,
        mut current: ProtoBlock,
        stmts: hir::Id<[hir::Stmt]>,
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
        id: hir::PatternId,
        type_: hir::TypeDefinition,
    )
        -> ProtoBlock
    {
        use self::hir::Pattern::*;

        let pattern = self.tree.get_pattern(id);

        let (patterns, types) = match pattern {
            Ignored(..) => { return current; },
            Var(_) => {
                current.push_binding(id.into(), matched, type_);
                return current;
            },
            Constructor(tup) | Tuple(tup) => (
                self.tree.get_patterns(tup.fields),
                type_.fields().fields,
            ),
        };

        assert_eq!(patterns.len(), types.len());

        for (index, (&p, t)) in patterns.iter().zip(types.iter()).enumerate() {
            let i = index as u16;
            let range = self.tree.get_pattern_range(p);
            let id = current.push_immediate(
                sir::Instruction::Field(t.clone(), matched, i, range),
            );
            current = self.convert_pattern(current, id, p, t);
        }

        current
    }

    fn convert_rebind(
        &mut self,
        mut current: ProtoBlock,
        re: hir::ReBinding,
    )
        -> ProtoBlock
    {
        current = self.convert_expression(current, re.right).expect("!Void");

        self.convert_rebind_recurse(current, re.left)
    }

    fn convert_rebind_recurse(
        &mut self,
        mut current: ProtoBlock,
        left: hir::ExpressionId,
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
        let expr = self.get_expression(left);

        match expr {
            hir::Expr::FieldAccess(e, f)
                => self.convert_rebind_recurse_field(current, e, f, left.into()),
            hir::Expr::Ref(_, gvn) => {
                let id = current.last_value();
                let ty = self.convert_type(self.get_expression_type(left));
                current.push_rebinding(gvn.into(), id, ty);
                current
            },
            hir::Expr::Tuple(t)
                => self.convert_rebind_recurse_tuple(current, t),
            _ => unimplemented!("{:?}", left),
        }
    }

    fn convert_rebind_recurse_field(
        &mut self,
        mut current: ProtoBlock,
        expr: hir::ExpressionId,
        field: hir::Field,
        gvn: hir::Gvn,
    )
        -> ProtoBlock
    {
        let id = current.last_value();
        let index = field.index();
        let ty = self.convert_type_of(expr.into());
        let range = self.get_expression_range(expr);

        //  Load tuple
        current = self.convert_expression(current, expr).expect("!Void");
        let tuple_id = current.last_value();

        //  Load other fields
        let fields = &ty.fields().fields;
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
                    range
                ));
                args.push(id);
            }
        }

        //  Construct a new tuple, identical to the former except for the
        //  one new field.
        current.push_instr(
            gvn.into(),
            sir::Instruction::New(
                ty,
                args,
                range,
            ),
        );

        self.convert_rebind_recurse(current, expr)
    }

    fn convert_rebind_recurse_tuple(
        &mut self,
        mut current: ProtoBlock,
        tuple: hir::Tuple<hir::ExpressionId>,
    )
        -> ProtoBlock
    {
        let id = current.last_value();
        let fields = self.tree.get_expressions(tuple.fields);

        for (index, &field) in fields.iter().enumerate() {
            let gvn = field.into();
            let ty = self.convert_type_of(gvn);
            let range = self.get_expression_range(field);
            current.push_instr(
                gvn.into(),
                sir::Instruction::Field(
                    ty,
                    id,
                    index as u16,
                    range,
                ),
            );

            current = self.convert_rebind_recurse(current, field);
        }

        current
    }

    fn convert_tuple(
        &mut self,
        current: ProtoBlock,
        type_: hir::TypeDefinition,
        tuple: hir::Tuple<hir::ExpressionId>,
        gvn: hir::Gvn,
        range: com::Range,
    )
        -> ProtoBlock
    {
        let (mut current, arguments) =
            self.convert_array_of_values(current, tuple.fields);

        current.push_instr(
            gvn.into(),
            sir::Instruction::New(type_, arguments, range)
        );

        current
    }

    fn convert_array_of_values(
        &mut self,
        mut current: ProtoBlock,
        expressions: hir::Id<[hir::ExpressionId]>,
    )
        -> (ProtoBlock, DynArray<sir::ValueId>)
    {
        let expressions = self.tree.get_expressions(expressions);
        for &e in expressions {
            current = self.convert_expression(current, e).expect("!Void");
        }

        let arguments = DynArray::with_capacity(expressions.len());
        for &e in expressions {
            let ty = self.convert_type_of(e.into());
            arguments.push(current.bind(self.binding_of(e), ty));
        }

        (current, arguments)
    }

    fn convert_statements(
        &mut self,
        mut current: ProtoBlock,
        stmts: hir::Id<[hir::Stmt]>,
    )
        -> Option<ProtoBlock>
    {
        let stmts = self.tree.get_statements(stmts);

        for &s in stmts {
            match s {
                hir::Stmt::Return(r) => {
                    current =
                        self.convert_expression(current, r.value).expect("!Void");
                    current.exit =
                        ProtoTerminator::Return(current.last_value());
                    self.blocks.push(current);
                    return None;
                },
                hir::Stmt::Set(re) => {
                    current = self.convert_rebind(current, re);
                },
                hir::Stmt::Var(b) => {
                    current = self.convert_binding(current, b);
                },
            }
        }

        Some(current)
    }

    //
    //  FIXME: Make all those conversions back to Items unnecessary.
    //
    fn convert_function_prototype(
        &self,
        name: hir::ItemIdentifier,
        arguments: hir::Tuple<hir::TypeId>,
        result: hir::TypeId
    )
        -> hir::FunctionProto
    {
        let arguments = {
            let fields = self.tree.get_type_ids(arguments.fields);
            let names = self.tree.get_names(arguments.names);

            let arguments = fields.iter().zip(names.iter())
                .map(|(&ty, &name)| hir::Argument {
                    name,
                    type_: self.convert_type_id(ty),
                    range: name.1
                })
                .collect();

            DynArray::new(arguments)
        };

        let result = self.convert_type_id(result);

        hir::FunctionProto {
            name,
            range: name.1,
            arguments,
            result,
        }
    }

    fn convert_type_id(&self, ty: hir::TypeId) -> hir::TypeDefinition {
        let ty = self.tree.get_type(ty);
        self.convert_type(ty)
    }

    fn convert_type(&self, ty: hir::Type) -> hir::TypeDefinition {
        use self::hir::Type as T;
        use self::hir::TypeDefinition as TD;

        match ty {
            T::Builtin(b) => TD::Builtin(b),
            T::Rec(name, _, fields) => self.convert_type_record(name, fields),
            T::Tuple(tup) => self.convert_type_tuple(tup),
            _ => unimplemented!("convert_type - {:?}", ty),
        }
    }

    fn convert_type_record(
        &self,
        name: hir::ItemIdentifier,
        fields: hir::Tuple<hir::TypeId>
    )
        -> hir::TypeDefinition
    {
        let prototype = hir::RecordProto {
            name,
            range: Default::default(),
            enum_: Default::default(),
        };

        let definition = self.convert_type_tuple_impl(fields);

        hir::TypeDefinition::Rec(
            hir::Record { prototype, definition, },
            Default::default(),
        )
    }

    fn convert_type_tuple(&self, tup: hir::Tuple<hir::TypeId>)
        -> hir::TypeDefinition
    {
        hir::TypeDefinition::Tuple(self.convert_type_tuple_impl(tup))
    }

    fn convert_type_tuple_impl(&self, tup: hir::Tuple<hir::TypeId>)
        -> hir::DynTuple<hir::TypeDefinition>
    {
        let fields = self.tree.get_type_ids(tup.fields);
        let names = self.tree.get_names(tup.names);

        let fields = fields.iter()
            .map(|t| self.convert_type_id(*t))
            .collect();
        let fields = DynArray::new(fields);

        let names = names.iter().cloned().collect();
        let names = DynArray::new(names);

        hir::DynTuple{ fields, names, }
    }

    fn convert_type_of(&self, gvn: hir::Gvn) -> hir::TypeDefinition {
        let ty = if let Some(e) = gvn.as_expression() {
            self.get_expression_type(e)
        } else if let Some(p) = gvn.as_pattern() {
            self.tree.get_pattern_type(p)
        } else {
            unimplemented!("Unknown GVN {:?}", gvn);
        };
        self.convert_type(ty)
    }

    fn binding_of(&self, id: hir::ExpressionId) -> BindingId {
        let expr = self.get_expression(id);
        match expr {
            hir::Expr::Ref(_, gvn) => gvn.into(),
            _ => id.into()
        }
    }

    fn get_expression(&self, id: hir::ExpressionId) -> hir::Expr {
        if let Some(local) = LocalExpressionId::new(self.expression_offset, id) {
            self.expression.borrow().at(&local).expr
        } else {
            self.tree.get_expression(id)
        }
    }

    fn get_expression_type(&self, id: hir::ExpressionId) -> hir::Type {
        if let Some(local) = LocalExpressionId::new(self.expression_offset, id) {
            self.expression.borrow().at(&local).type_
        } else {
            self.tree.get_expression_type(id)
        }
    }

    fn get_expression_range(&self, id: hir::ExpressionId) -> com::Range {
        if let Some(local) = LocalExpressionId::new(self.expression_offset, id) {
            self.expression.borrow().at(&local).range
        } else {
            self.tree.get_expression_range(id)
        }
    }

    fn push_local_expression(
        &self,
        expr: hir::Expr,
        type_: hir::Type,
        range: com::Range,
    )
        -> hir::ExpressionId
    {
        let local_id = LocalExpressionId(
            com::CoreId::new(self.expression.borrow().len() as u32)
        );

        let expr = LocalExpression { expr, type_, range };
        self.expression.borrow_mut().push(&local_id, expr);

        local_id.as_expression_id(self.expression_offset)
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

impl LocalExpressionId {
    fn new(offset: u32, expr: hir::ExpressionId) -> Option<LocalExpressionId> {
        if expr.index() >= offset as usize {
            Some(LocalExpressionId(com::CoreId::new(expr.index() as u32 - offset)))
        } else {
            None
        }
    }

    fn as_expression_id(&self, offset: u32) -> hir::ExpressionId {
        hir::ExpressionId::new(self.index() as u32 + offset)
    }
}

impl fmt::Debug for LocalExpressionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "LocalExpressionId({})", self.index())
    }
}

impl TableIndex for LocalExpressionId {
    fn from_index(index: usize) -> Self {
        LocalExpressionId(com::CoreId::new(index as u32))
    }

    fn index(&self) -> usize { self.0.raw() as usize }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::rc;

    use basic::{com, mem};

    use model::hir::builder::*;
    use model::hir::*;
    use model::sir;

    #[test]
    fn value_simple() {
        let env = Env::new(b"1 + 2");

        let (_, _, _, _, t, v) = env.hir();
        let val = v.call()
            .builtin(BuiltinFunction::Add, t.int())
            .push(v.int(1, 0))
            .push(v.int(2, 4))
            .build();

        assert_eq!(
            env.exprit(val).to_string(),
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
        let env = Env::new(b"(1,  2)");

        let v = env.hir().5;

        assert_eq!(
            env.exprit(
                v.tuple()
                    .push(v.int(1, 1))
                    .push(v.int(2, 5))
                    .range(0, 7)
                    .build()
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
    fn record_arguments() {
        let env = Env::new(b":rec Args(Int);    Args(42)");
        let (i, _, p, _, _, v) = env.hir();

        let rec = {
            let rec = i.rec(p.rec(env.item_id(5, 4), 0).build())
                .push(TypeDefinition::int())
                .build();
            env.insert_record(rec)
        };

        assert_eq!(
            env.exprit(
                v.constructor(rec).push(v.int(42, 24)).range(19, 8).build()
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
        let env = Env::new(b":rec Args(Int, Int);   Args(4, 42).1");
        let (i, _, p, _, _, v) = env.hir();

        let rec = {
            let rec = i.rec(p.rec(env.item_id(5, 4), 0).build())
                .push(TypeDefinition::int())
                .push(TypeDefinition::int())
                .build();
            env.insert_record(rec)
        };

        let c = v.constructor(rec)
            .push(v.int(4, 28))
            .push(v.int(42, 31))
            .range(23, 11)
            .build();

        assert_eq!(
            env.exprit(v.field_access(c).index(1).build()).to_string(),
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
        let env = Env::new(b"{ :var a := 1; :var b := 2; a + b }");
        let (_, _, _, s, t, v) = env.hir();

        let (a, b) = (env.var_id(7, 1), env.var_id(20, 1));
        let add = v.call()
            .builtin(BuiltinFunction::Add, t.int())
            .push(v.int_ref(a, 28).pattern(0).build())
            .push(v.int_ref(b, 32).pattern(1).build())
            .build();
        let block =
            v.block(add)
                .push(s.var_id(a, v.int(1, 12)))
                .push(s.var_id(b, v.int(2, 25)))
                .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b"{ :var a := 1; :set a := 2; a }");
        let (_, _, _, s, _, v) = env.hir();

        let a = env.var_id(7, 1);
        let block =
            v.block(v.int_ref(a, 28).pattern(0).build())
                .push(s.var_id(a, v.int(1, 12)))
                .push(s.set(v.int_ref(a, 20).pattern(0).build(), v.int(2, 25)))
                .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b"{ :var a := (1, (2, 3)); :set a.1.0 := 4; a }");
        let (_, _, _, s, _, v) = env.hir();

        let a = env.var_id(7, 1);

        let a_1_v = v.tuple().push(v.int(2, 17)).push(v.int(3, 20)).range(16, 6).build();
        let a_v = v.tuple().push(v.int(1, 13)).push(a_1_v).range(12, 11).build();

        let a_ref = |pos| v.name_ref(a, pos).type_(env.type_of(a_v)).pattern(0).build();

        let block =
            v.block(a_ref(42))
                .push(s.var_id(a, a_v))
                .push(s.set(
                    v.field_access(
                        v.field_access(a_ref(30))
                            .index(1)
                            .build(),
                    ).build(),
                    v.int(4, 39),
                ))
                .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b"{ :var (a, b) := (1, 2); :set (a, b) := (b, a); a }");
        let (_, p, _, s, _, v) = env.hir();

        let (a, b) = (env.var_id(8, 1), env.var_id(11, 1));

        let a_ref = |pos| v.int_ref(a, pos).pattern(0).build();
        let b_ref = |pos| v.int_ref(b, pos).pattern(1).build();

        let block =
            v.block(a_ref(48))
                .push(s.var(
                    p.tuple().push(p.var(a)).push(p.var(b)).range(7, 6).build(),
                    v.tuple().push(v.int(1, 18)).push(v.int(2, 21)).range(17, 6).build(),
                ))
                .push(s.set(
                    v.tuple()
                        .push(a_ref(31))
                        .push(b_ref(34))
                        .range(30, 6)
                        .build(),
                    v.tuple()
                        .push(b_ref(41))
                        .push(a_ref(44))
                        .range(40, 6)
                        .build(),
                ))
                .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b":rec X(Int, Int);   { :var X(a, b) := X(1, 2); a }");
        let (i, p, po, s, _, v) = env.hir();

        let rec = {
            let rec = i.rec(po.rec(env.item_id(5, 1), 0).build())
                .push(TypeDefinition::int())
                .push(TypeDefinition::int())
                .build();
            env.insert_record(rec)
        };

        let (a, b) = (env.var_id(29, 1), env.var_id(32, 1));
        let binding =
            p.constructor(rec)
                .push(p.var(a))
                .push(p.var(b))
                .range(27, 7)
                .build();
        let value =
            v.constructor(rec)
                .push(v.int(1, 40))
                .push(v.int(2, 43))
                .range(38, 7)
                .build();
        let block =
            v.block(v.int_ref(a, 47).pattern(0).build())
                .push(s.var(binding, value))
                .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b"{ :return 1; }");
        let (_, _, _, s, _, v) = env.hir();

        let block = v.block_expression_less()
            .push(s.ret(v.int(1, 10)))
            .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b"{ :var (a, b) := (1, 2); a }");
        let (_, p, _, s, _, v) = env.hir();

        let (a, b) = (env.var_id(8, 1), env.var_id(11, 1));

        let binding = p.tuple().push(p.var(a)).push(p.var(b)).range(7, 6).build();
        let value = v.tuple().push(v.int(1, 18)).push(v.int(2, 21)).range(17, 6).build();

        let block =
            v.block(v.int_ref(a, 25).pattern(0).build())
                .push(s.var(binding, value))
                .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let env = Env::new(b":fun add(a: Int, b: Int) -> Int { a + b }");
        let (_, _, p, _, t, v) = env.hir();

        let (a, b) = (env.var_id(9, 1), env.var_id(17, 1));
        let int = || TypeDefinition::int();

        let prototype =
            p.fun(env.item_id(5, 3), int())
                .push(a, int())
                .push(b, int())
                .range(0, 31)
                .build();
        env.insert_function(&prototype);

        let body =
            v.block(
                v.call()
                    .builtin(BuiltinFunction::Add, t.int())
                    .push(v.int_ref(a, 34).pattern(0).build())
                    .push(v.int_ref(b, 38).pattern(1).build())
                    .build()
            ).build_with_type();

        assert_eq!(
            env.funit(prototype, body).to_string(),
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
        let env = Env::new(b":if true { 1 } :else { 2 }");
        let v = env.hir().5;

        let if_ = v.if_(v.bool_(true, 4), v.int(1, 11), v.int(2, 23)).build();

        assert_eq!(
            env.exprit(if_).to_string(),
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
        fn short_circuit(fun: BuiltinFunction, fragment: &[u8]) -> String {
            let env = Env::new(fragment);
            let (_, _, p, _, t, v) = env.hir();

            let (a, b, c) = (env.var_id(8, 1), env.var_id(16, 1), env.var_id(24, 1));
            let int = || TypeDefinition::int();

            let f =
                p.fun(env.item_id(5, 2), TypeDefinition::bool_())
                    .push(a, int())
                    .push(b, int())
                    .push(c, int())
                    .range(0, 61)
                    .build();
            env.insert_function(&f);

            let left = 
                v.call()
                    .builtin(BuiltinFunction::LessThanOrEqual, t.bool_())
                    .push(v.int_ref(a, 42).pattern(0).build())
                    .push(v.int_ref(b, 47).pattern(1).build())
                    .build();

            let right =
                v.call()
                    .builtin(BuiltinFunction::LessThan, t.bool_())
                    .push(v.int_ref(b, 54).pattern(1).build())
                    .push(v.int_ref(c, 58).pattern(2).build())
                    .build();

            let block =
                v.block(
                    v.call().builtin(fun, t.bool_()).push(left).push(right).build()
                ).build_with_type();

            env.funit(f, block).to_string()
        }

        assert_eq!(
            short_circuit(
                BuiltinFunction::And,
                b":fun in(a: Int, b: Int, c: Int) -> Bool { a <= b :and b < c }"
            ),
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

        assert_eq!(
            short_circuit(
                BuiltinFunction::Or,
                b":fun in(a: Int, b: Int, c: Int) -> Bool { a <= b :or  b < c }"
            ),
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
        let env = Env::new(b":if true { :return 1; } :else { 2 }");
        let (_, _, _, s, _, v) = env.hir();

        let if_ = v.if_(
            v.bool_(true, 4),
            v.block_expression_less().push(s.ret(v.int(1, 19))).build_with_type(),
            v.block(v.int(2, 32)).build_with_type(),
        ).build();

        assert_eq!(
            env.exprit(if_).to_string(),
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

        let fragment = cat(&[
            ":fun fib(current: Int, next: Int, count: Int) -> Int {",   //  55
            "    :if count == 0 {",                                     //  76
            "        current",                                          //  92
            "    } :else {",                                            // 106
            "       fib(next, current + next, count - 1)",              // 150
            "    }",                                                    // 157
            "}",                                                        // 159
        ]);
        let env = Env::new(fragment.as_bytes());
        let (_, _, p, _, t, v) = env.hir();

        let int = || TypeDefinition::int();
        let (current, next, count) =
            (env.var_id(9, 7), env.var_id(23, 4), env.var_id(34, 5));
        let current_ref = |pos| v.int_ref(current, pos).pattern(0).build();
        let next_ref = |pos| v.int_ref(next, pos).pattern(1).build();
        let count_ref = |pos| v.int_ref(count, pos).pattern(2).build();

        let fib = env.item_id(5, 3);
        let proto =
            p.fun(fib, int())
                .push(current, int())
                .push(next, int())
                .push(count, int())
                .range(0, 52)
                .build();
        let callable = env.insert_function(&proto);

        let if_ =
            v.if_(
                v.call()
                    .builtin(Equal, t.bool_())
                    .push(count_ref(63))
                    .push(v.int(0, 72))
                    .build(),
                v.block(current_ref(84))
                    .range(74, 32)
                    .build_with_type(),
                v.block(
                    v.call()
                        .callable(callable)
                        .type_(t.int())
                        .push(next_ref(117))
                        .push(
                            v.call()
                                .builtin(Add, t.int())
                                .push(current_ref(123))
                                .push(next_ref(133))
                                .build()
                        )
                        .push(
                            v.call()
                                .builtin(Substract, t.int())
                                .push(count_ref(139))
                                .push(v.int(1, 147))
                                .build()
                        )
                        .build()
                )
                    .range(106, 50)
                    .build_with_type()
            ).build();

        let block = v.block(if_).range(53, 105).build_with_type();

        assert_eq!(
            env.funit(proto, block).to_string(),
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
        let env = Env::new(b"{ :var i := 0; :loop { :set i := i + 1; } }");
        let (_, _, _, s, t, v) = env.hir();

        let i = env.var_id(7, 1);
        let var = s.var_id(i, v.int(0, 12));
        let add = v.call()
            .builtin(BuiltinFunction::Add, t.int())
            .push(v.int_ref(i, 33).pattern(0).build())
            .push(v.int(1, 37))
            .build();
        let loop_ = v.loop_()
            .push(s.set(v.int_ref(i, 28).pattern(0).build(), add))
            .build();
        let block = v.block(loop_).push(var).build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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
        let fragment = cat(&[
            "{",                                            //   2
            "    :var n := 8;",                             //  19
            "    :var current := 0;",                       //  41
            "    :loop {",                                  //  53
            "        :set n := :if n == 0 {",               //  84
            "            :return current;",                 // 113
            "        } :else {",                            // 131
            "            :set current := current + 1;",     // 172
            "            n - 1",                            // 190
            "        };",                                   // 201
            "    }",                                        // 206
            "}",                                            // 208
        ]);
        let env = Env::new(fragment.as_bytes());
        let (_, _, _, s, t, v) = env.hir();

        let (n, current) = (env.var_id(11, 1), env.var_id(28, 7));

        let block = v.block(
            v.loop_()
                .push(s.set(
                    v.int_ref(n, 66).pattern(0).build(),
                    v.if_(
                        v.call()
                            .builtin(BuiltinFunction::Equal, t.bool_())
                            .push(v.int_ref(n, 75).pattern(0).build())
                            .push(v.int(0, 80))
                            .build(),
                        v.block_expression_less()
                            .push(s.ret(v.int_ref(current, 104).pattern(1).build()))
                            .build_with_type(),
                        v.block(
                            v.call()
                                .builtin(BuiltinFunction::Substract, t.int())
                                .push(v.int_ref(n, 184).pattern(0).build())
                                .push(v.int(1, 188))
                                .build()
                        )
                            .push(s.set(
                                v.int_ref(current, 148).pattern(1).build(),
                                v.call()
                                    .builtin(BuiltinFunction::Add, t.int())
                                    .push(v.int_ref(current, 159).pattern(1).build())
                                    .push(v.int(1, 169))
                                    .build(),
                            ))
                            .build_with_type()
                    ).build()
                ))
                .build()
        )
            .push(s.var_id(n, v.int(8, 16)))
            .push(s.var_id(current, v.int(0, 39)))
            .build_with_type();

        assert_eq!(
            env.exprit(block).to_string(),
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

    struct Env {
        tree: RcTree,
        resolver: interning::Resolver
    }

    impl Env {
        fn new(fragment: &[u8]) -> Env {
            let interner = rc::Rc::new(mem::Interner::new());
            let resolver = interning::Resolver::new(fragment, interner);
            Env {
                resolver,
                tree: Default::default(),
            }
        }

        fn hir(&self) -> (
            ItemFactory,
            PatternFactory,
            PrototypeFactory,
            StmtFactory,
            TypeFactory,
            ValueFactory,
        )
        {
            let f = Factory::new(self.tree.clone());
            (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.value())
        }

        fn item_id(&self, pos: usize, len: usize) -> ItemIdentifier {
            let range = range(pos, len);
            ItemIdentifier(self.resolver.from_range(range), range)
        }

        fn var_id(&self, pos: usize, len: usize) -> ValueIdentifier {
            let range = range(pos, len);
            ValueIdentifier(self.resolver.from_range(range), range)
        }

        fn insert_function(&self, p: &FunctionProto) -> Callable {
            let (arguments, types) = {
                let mut arguments = vec!();
                let mut names = vec!();
                let mut types = vec!();

                for a in &p.arguments {
                    let ty = self.tree.borrow_mut().push_type_definition(&a.type_);
                    let ty = self.tree.borrow().get_type(ty);

                    let pattern = Pattern::Var(a.name);
                    let pattern = self.tree.borrow_mut().push_pattern(ty, pattern, a.range);

                    arguments.push(pattern);
                    types.push(self.tree.borrow().get_pattern_type_id(pattern));
                    names.push(a.name);
                }

                self.tree.borrow_mut().push_patterns(&arguments);
                self.tree.borrow_mut().push_names(&names);

                let patterns = self.tree.borrow_mut().push_patterns(&arguments);
                let types = self.tree.borrow_mut().push_type_ids(&types);
                let names = self.tree.borrow_mut().push_names(&names);

                (
                    Tuple { fields: patterns, names, },
                    Tuple { fields: types, names, },
                )
            };

            let result = self.tree.borrow_mut().push_type_definition(&p.result);

            self.tree.borrow_mut()
                .set_root_function(p.name, arguments, result, Default::default());

            Callable::Function(p.name, types, result)
        }

        fn insert_record(&self, r: Record) -> Type {
            self.tree.borrow_mut().insert_record(&r)
        }

        fn type_of(&self, e: ExpressionId) -> Type {
            self.tree.borrow().get_expression_type(e)
        }

        fn funit(&self, prototype: FunctionProto, body: ExpressionId)
            -> sir::ControlFlowGraph
        {
            let mut tree = self.tree.borrow().clone();
            if let Some(Root::Function(name, args, res, _)) = tree.get_root() {
                tree.set_root_function(name, args, res, body);
            } else {
                panic!("funit - Root not a function.");
            }

            let fun = Function { prototype, body: tree, };

            println!("funit - {:#?}", fun);
            println!("");

            self.builder().from_function(&fun)
        }

        fn exprit(&self, expr: ExpressionId) -> sir::ControlFlowGraph {
            let mut tree = self.tree.borrow().clone();
            tree.set_root_expression(expr);

            println!("exprit - {:#?}", tree);
            println!("");

            self.builder().from_expression(&tree, expr)
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

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
