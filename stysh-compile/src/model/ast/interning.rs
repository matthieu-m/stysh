//! Interning.
//!
//! Test facilities to remove or add the InternId.

use std::rc;

use basic::com;
use basic::mem::{self, CloneInto};

use model::ast::*;

/// Resolver
pub struct Resolver<'g> {
    source: &'g [u8],
    interner: rc::Rc<mem::Interner>,
    global_arena: &'g mem::Arena,
}

/// Scrubber
pub struct Scrubber<'g> {
    global_arena: &'g mem::Arena,
}

//
//  Public interface of Resolver
//
impl<'g> Resolver<'g> {
    /// Creates an instance.
    pub fn new(
        source: &'g [u8],
        interner: rc::Rc<mem::Interner>,
        global_arena: &'g mem::Arena
    )
        -> Self
    {
        Resolver { source, interner, global_arena }
    }

    /// Returns reference to Interner.
    pub fn interner(&self) -> &mem::Interner { &*self.interner }

    /// Resolves InternId, recursively.
    pub fn resolve(&self, ast: Node) -> Node<'g> {
        use self::Node::*;

        match ast {
            Expr(expr) => Expr(self.resolve_expr(expr)),
            Item(item) => Item(self.resolve_item(item)),
            Stmt(stmt) => Stmt(self.resolve_stmt(stmt)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_expr(&self, expr: Expression) -> Expression<'g> {
        use self::Expression::*;

        match expr {
            BinOp(op, pos, left, right) => BinOp(
                op,
                pos,
                self.insert(self.resolve_expr(*left)),
                self.insert(self.resolve_expr(*right)),
            ),
            Block(n) => Block(self.insert(self.resolve_block(*n))),
            Constructor(c) => Constructor(self.resolve_constructor(c)),
            FieldAccess(f) => FieldAccess(self.resolve_field_access(f)),
            FunctionCall(f) => FunctionCall(self.resolve_function_call(f)),
            If(i) => If(self.insert(self.resolve_if(*i))),
            Lit(l) => Lit(self.resolve_literal(l)),
            Loop(l) => Loop(self.insert(self.resolve_loop(*l))),
            PreOp(op, pos, e)
                => PreOp(op, pos, self.insert(self.resolve_expr(*e))),
            Tuple(t) => Tuple(self.resolve_tuple(t)),
            Var(v) => Var(self.resolve_variable_identifier(v)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_item(&self, item: Item) -> Item<'g> {
        use self::Item::*;

        match item {
            Enum(e) => Enum(self.resolve_enum(e)),
            Fun(f) => Fun(self.resolve_function(f)),
            Rec(r) => Rec(self.resolve_record(r)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_pattern(&self, p: Pattern) -> Pattern<'g> {
        use self::Pattern::*;

        match p {
            Constructor(c) => Constructor(self.resolve_constructor_pattern(c)),
            Ignored(r) => Ignored(r),
            Tuple(t) => Tuple(self.resolve_tuple_pattern(t)),
            Var(v) => Var(self.resolve_variable_identifier(v)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_stmt(&self, stmt: Statement) -> Statement<'g> {
        use self::Statement::*;

        match stmt {
            Set(v) => Set(self.resolve_re_binding(v)),
            Return(r) => Return(self.resolve_return(r)),
            Var(v) => Var(self.resolve_binding(v)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_type(&self, t: Type) -> Type<'g> {
        use self::Type::*;

        match t {
            Missing(r) => Missing(r),
            Nested(t, p) => Nested(
                self.resolve_type_identifier(t),
                self.resolve_path(p),
            ),
            Simple(t) => Simple(self.resolve_type_identifier(t)),
            Tuple(t) => Tuple(self.resolve_tuple_type(t)),
        }
    }

}

//
//  Public interface of Scrubber
//
impl<'g> Scrubber<'g> {
    /// Creates an instance.
    pub fn new(global_arena: &'g mem::Arena) -> Self {
        Scrubber { global_arena }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub(&self, ast: Node) -> Node<'g> {
        use self::Node::*;

        match ast {
            Expr(expr) => Expr(self.scrub_expr(expr)),
            Item(item) => Item(self.scrub_item(item)),
            Stmt(stmt) => Stmt(self.scrub_stmt(stmt)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_enum(&self, e: Enum) -> Enum<'g> {
        let mut variants = self.array(e.variants.len());
        for v in e.variants {
            variants.push(self.scrub_inner_record(*v));
        }

        Enum {
            name: e.name.with_id(Default::default()),
            variants: variants.into_slice(),
            keyword: e.keyword,
            open: e.open,
            close: e.close,
            commas: CloneInto::clone_into(e.commas, self.global_arena),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_expr(&self, expr: Expression) -> Expression<'g> {
        use self::Expression::*;

        match expr {
            BinOp(op, pos, left, right) => BinOp(
                op,
                pos,
                self.insert(self.scrub_expr(*left)),
                self.insert(self.scrub_expr(*right)),
            ),
            Block(n) => Block(self.insert(self.scrub_block(*n))),
            Constructor(c) => Constructor(self.scrub_constructor(c)),
            FieldAccess(f) => FieldAccess(self.scrub_field_access(f)),
            FunctionCall(f) => FunctionCall(self.scrub_function_call(f)),
            If(i) => If(self.insert(self.scrub_if(*i))),
            Lit(l) => Lit(self.scrub_literal(l)),
            Loop(l) => Loop(self.insert(self.scrub_loop(*l))),
            PreOp(op, pos, e)
                => PreOp(op, pos, self.insert(self.scrub_expr(*e))),
            Tuple(t) => Tuple(self.scrub_tuple(t)),
            Var(v) => Var(v.with_id(Default::default())),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_function(&self, f: Function) -> Function<'g> {
        let mut arguments = self.array(f.arguments.len());
        for a in f.arguments {
            arguments.push(self.scrub_argument(*a));
        }

        Function {
            name: f.name.with_id(Default::default()),
            arguments: arguments.into_slice(),
            result: self.scrub_type(f.result),
            body: self.scrub_block(f.body),
            keyword: f.keyword,
            open: f.open,
            close: f.close,
            arrow: f.arrow,
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_item(&self, item: Item) -> Item<'g> {
        use self::Item::*;

        match item {
            Enum(e) => Enum(self.scrub_enum(e)),
            Fun(f) => Fun(self.scrub_function(f)),
            Rec(r) => Rec(self.scrub_record(r)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_pattern(&self, p: Pattern) -> Pattern<'g> {
        use self::Pattern::*;

        match p {
            Constructor(c) => Constructor(self.scrub_constructor_pattern(c)),
            Ignored(r) => Ignored(r),
            Tuple(t) => Tuple(self.scrub_tuple_pattern(t)),
            Var(v) => Var(v.with_id(Default::default())),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_record(&self, r: Record) -> Record<'g> {
        Record {
            inner: self.scrub_inner_record(r.inner),
            keyword: r.keyword,
            semi_colon: r.semi_colon,
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_stmt(&self, stmt: Statement) -> Statement<'g> {
        use self::Statement::*;

        match stmt {
            Set(v) => Set(self.scrub_re_binding(v)),
            Return(r) => Return(self.scrub_return(r)),
            Var(v) => Var(self.scrub_binding(v)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_type(&self, t: Type) -> Type<'g> {
        use self::Type::*;

        match t {
            Missing(r) => Missing(r),
            Nested(t, p)
                => Nested(t.with_id(Default::default()), self.scrub_path(p)),
            Simple(t) => Simple(t.with_id(Default::default())),
            Tuple(t) => Tuple(self.scrub_tuple_type(t)),
        }
    }
}

//
//  Implementation of Interner
//
impl<'g> Resolver<'g> {
    fn resolve_argument(&self, a: Argument) -> Argument<'g> {
        Argument {
            name: self.resolve_variable_identifier(a.name),
            type_: self.resolve_type(a.type_),
            colon: a.colon,
            comma: a.comma,
        }
    }

    fn resolve_binding(&self, b: VariableBinding) -> VariableBinding<'g> {
        VariableBinding {
            pattern: self.resolve_pattern(b.pattern),
            type_: b.type_.map(|t| self.resolve_type(t)),
            expr: self.resolve_expr(b.expr),
            var: b.var,
            colon: b.colon,
            bind: b.bind,
            semi: b.semi,
        }
    }

    fn resolve_block(&self, block: Block) -> Block<'g> {
        let mut stmts = self.array(block.statements.len());
        for s in block.statements {
            stmts.push(self.resolve_stmt(*s));
        }

        let expr = block.expression.map(|e| self.insert(self.resolve_expr(*e)));

        Block {
            statements: stmts.into_slice(),
            expression: expr,
            open: block.open,
            close: block.close,
        }
    }

    fn resolve_constructor<'a>(&self, c: Constructor<'a, Expression<'a>>)
        -> Constructor<'g, Expression<'g>>
    {
        self.resolve_constructor_impl(c, |e| self.resolve_expr(e))
    }

    fn resolve_constructor_pattern<'a>(&self, c: Constructor<'a, Pattern<'a>>)
        -> Constructor<'g, Pattern<'g>>
    {
        self.resolve_constructor_impl(c, |p| self.resolve_pattern(p))
    }

    fn resolve_enum(&self, e: Enum) -> Enum<'g> {
        let mut variants = self.array(e.variants.len());
        for v in e.variants {
            variants.push(self.resolve_inner_record(*v));
        }

        Enum {
            name: self.resolve_type_identifier(e.name),
            variants: variants.into_slice(),
            keyword: e.keyword,
            open: e.open,
            close: e.close,
            commas: CloneInto::clone_into(e.commas, self.global_arena),
        }
    }

    fn resolve_field_access(&self, f: FieldAccess) -> FieldAccess<'g> {
        FieldAccess {
            accessed: self.insert(self.resolve_expr(*f.accessed)),
            field: self.resolve_field_identifier(f.field),
        }
    }

    fn resolve_field_identifier(&self, f: FieldIdentifier) -> FieldIdentifier {
        f.with_id(self.from_range(f.span().skip_left(1)))
    }

    fn resolve_function(&self, f: Function) -> Function<'g> {
        let mut arguments = self.array(f.arguments.len());
        for a in f.arguments {
            arguments.push(self.resolve_argument(*a));
        }

        Function {
            name: self.resolve_variable_identifier(f.name),
            arguments: arguments.into_slice(),
            result: self.resolve_type(f.result),
            body: self.resolve_block(f.body),
            keyword: f.keyword,
            open: f.open,
            close: f.close,
            arrow: f.arrow,
        }
    }

    fn resolve_function_call(&self, f: FunctionCall) -> FunctionCall<'g> {
        FunctionCall {
            function: self.insert(self.resolve_expr(*f.function)),
            arguments: self.resolve_tuple(f.arguments),
        }
    }

    fn resolve_if(&self, i: IfElse) -> IfElse<'g> {
        IfElse {
            condition: self.resolve_expr(i.condition),
            true_expr: self.resolve_block(i.true_expr),
            false_expr: self.resolve_block(i.false_expr),
            if_: i.if_,
            else_: i.else_,
        }
    }

    fn resolve_inner_record(&self, r: InnerRecord) -> InnerRecord<'g> {
        use self::InnerRecord::*;

        match r {
            Missing(r) => Missing(r),
            Tuple(name, t) => Tuple(
                self.resolve_type_identifier(name),
                self.resolve_tuple_type(t),
            ),
            Unexpected(r) => Unexpected(r),
            Unit(t) => Unit(self.resolve_type_identifier(t)),
        }
    }

    fn resolve_literal(&self, l: Literal) -> Literal<'g> {
        use self::Literal::*;

        let resolved = match l {
            Bytes(f, _, r) => Bytes(f, self.from_fragments(f), r),
            String(f, _, r) => String(f, self.from_fragments(f), r),
            other => other,
        };

        self.global_arena.intern(&resolved)
    }

    fn resolve_loop(&self, l: Loop) -> Loop<'g> {
        let mut stmts = self.array(l.statements.len());
        for s in l.statements {
            stmts.push(self.resolve_stmt(*s));
        }

        Loop {
            statements: stmts.into_slice(),
            loop_: l.loop_,
            open: l.open,
            close: l.close,
        }
    }

    fn resolve_path(&self, p: Path) -> Path<'g> {
        let mut components = self.array(p.components.len());
        for c in p.components {
            components.push(self.resolve_type_identifier(*c));
        }

        Path {
            components: components.into_slice(),
            colons: CloneInto::clone_into(p.colons, self.global_arena),
        }
    }

    fn resolve_re_binding(&self, v: VariableReBinding)
        -> VariableReBinding<'g>
    {
        VariableReBinding {
            left: self.resolve_expr(v.left),
            expr: self.resolve_expr(v.expr),
            set: v.set,
            bind: v.bind,
            semi: v.semi,
        }
    }

    fn resolve_record(&self, r: Record) -> Record<'g> {
        Record {
            inner: self.resolve_inner_record(r.inner),
            keyword: r.keyword,
            semi_colon: r.semi_colon,
        }
    }

    fn resolve_return(&self, r: Return) -> Return<'g> {
        Return {
            expr: r.expr.map(|e| self.resolve_expr(e)),
            ret: r.ret,
            semi: r.semi,
        }
    }

    fn resolve_tuple<'a>(&self, t: Tuple<'a, Expression<'a>>)
        -> Tuple<'g, Expression<'g>>
    {
        self.resolve_tuple_impl(t, |e| self.resolve_expr(e))
    }

    fn resolve_tuple_pattern<'a>(&self, t: Tuple<'a, Pattern<'a>>)
        -> Tuple<'g, Pattern<'g>>
    {
        self.resolve_tuple_impl(t, |p| self.resolve_pattern(p))
    }

    fn resolve_tuple_type<'a>(&self, t: Tuple<'a, Type<'a>>)
        -> Tuple<'g, Type<'g>>
    {
        self.resolve_tuple_impl(t, |t| self.resolve_type(t))
    }

    fn resolve_type_identifier(&self, t: TypeIdentifier) -> TypeIdentifier {
        t.with_id(self.from_range(t.span()))
    }

    fn resolve_variable_identifier(&self, v: VariableIdentifier)
        -> VariableIdentifier
    {
        v.with_id(self.from_range(v.span()))
    }

    fn array<T: 'g>(&self, cap: usize) -> mem::Array<'g, T> {
        mem::Array::with_capacity(cap, self.global_arena)
    }

    fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    fn resolve_constructor_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        c: Constructor<'a, T>,
        scrubber: F
    )
        -> Constructor<'g, U>
    {
        Constructor {
            type_: self.resolve_type(c.type_),
            arguments: self.resolve_tuple_impl(c.arguments, scrubber),
        }
    }

    fn resolve_tuple_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        t: Tuple<'a, T>,
        scrubber: F,
    )
        -> Tuple<'g, U>
    {
        let mut fields = self.array(t.fields.len());
        for f in t.fields {
            fields.push(scrubber(*f));
        }

        let mut names = self.array(t.names.len());
        for n in t.names {
            names.push((self.from_range(n.1.skip_left(1)), n.1));
        }

        Tuple {
            fields: fields.into_slice(),
            commas: CloneInto::clone_into(t.commas, self.global_arena),
            names: names.into_slice(),
            separators: CloneInto::clone_into(t.separators, self.global_arena),
            open: t.open,
            close: t.close,
        }
    }

    fn from_fragments(&self, fragments: &[StringFragment]) -> mem::InternId {
        use self::StringFragment::*;

        let mut buffer = vec!();

        for f in fragments {
            match f {
                Text(tok) => buffer.extend_from_slice(&self.source[tok.span()]),
                SpecialCharacter(tok) => match &self.source[tok.span()] {
                    b"N" => buffer.push(b'\n'),
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        }

        self.interner.insert(&*buffer)
    }

    fn from_range(&self, range: com::Range) -> mem::InternId {
        let raw = &self.source[range];
        self.interner.insert(raw)
    }
}

//
//  Implementation of Scrubber
//
impl<'g> Scrubber<'g> {
    fn scrub_argument(&self, a: Argument) -> Argument<'g> {
        Argument {
            name: a.name.with_id(Default::default()),
            type_: self.scrub_type(a.type_),
            colon: a.colon,
            comma: a.comma,
        }
    }

    fn scrub_binding(&self, b: VariableBinding) -> VariableBinding<'g> {
        VariableBinding {
            pattern: self.scrub_pattern(b.pattern),
            type_: b.type_.map(|t| self.scrub_type(t)),
            expr: self.scrub_expr(b.expr),
            var: b.var,
            colon: b.colon,
            bind: b.bind,
            semi: b.semi,
        }
    }

    fn scrub_block(&self, block: Block) -> Block<'g> {
        let mut stmts = self.array(block.statements.len());
        for s in block.statements {
            stmts.push(self.scrub_stmt(*s));
        }

        let expr = block.expression.map(|e| self.insert(self.scrub_expr(*e)));

        Block {
            statements: stmts.into_slice(),
            expression: expr,
            open: block.open,
            close: block.close,
        }
    }

    fn scrub_constructor<'a>(&self, c: Constructor<'a, Expression<'a>>)
        -> Constructor<'g, Expression<'g>>
    {
        self.scrub_constructor_impl(c, |e| self.scrub_expr(e))
    }

    fn scrub_constructor_pattern<'a>(&self, c: Constructor<'a, Pattern<'a>>)
        -> Constructor<'g, Pattern<'g>>
    {
        self.scrub_constructor_impl(c, |p| self.scrub_pattern(p))
    }

    fn scrub_field_access(&self, f: FieldAccess) -> FieldAccess<'g> {
        FieldAccess {
            accessed: self.insert(self.scrub_expr(*f.accessed)),
            field: f.field.with_id(Default::default()),
        }
    }

    fn scrub_function_call(&self, f: FunctionCall) -> FunctionCall<'g> {
        FunctionCall {
            function: self.insert(self.scrub_expr(*f.function)),
            arguments: self.scrub_tuple(f.arguments),
        }
    }

    fn scrub_if(&self, i: IfElse) -> IfElse<'g> {
        IfElse {
            condition: self.scrub_expr(i.condition),
            true_expr: self.scrub_block(i.true_expr),
            false_expr: self.scrub_block(i.false_expr),
            if_: i.if_,
            else_: i.else_,
        }
    }

    fn scrub_inner_record(&self, r: InnerRecord) -> InnerRecord<'g> {
        use self::InnerRecord::*;

        match r {
            Missing(r) => Missing(r),
            Tuple(name, t) => Tuple(
                name.with_id(Default::default()),
                self.scrub_tuple_type(t),
            ),
            Unexpected(r) => Unexpected(r),
            Unit(t) => Unit(t.with_id(Default::default())),
        }
    }

    fn scrub_literal(&self, l: Literal) -> Literal<'g> {
        use self::Literal::*;

        let scrubbed = match l {
            Bytes(f, _, r) => Bytes(f, Default::default(), r),
            String(f, _, r) => String(f, Default::default(), r),
            other => other,
        };

        self.global_arena.intern(&scrubbed)
    }

    fn scrub_loop(&self, l: Loop) -> Loop<'g> {
        let mut stmts = self.array(l.statements.len());
        for s in l.statements {
            stmts.push(self.scrub_stmt(*s));
        }

        Loop {
            statements: stmts.into_slice(),
            loop_: l.loop_,
            open: l.open,
            close: l.close,
        }
    }

    fn scrub_path(&self, p: Path) -> Path<'g> {
        let mut components = self.array(p.components.len());
        for c in p.components {
            components.push(c.with_id(Default::default()));
        }

        Path {
            components: components.into_slice(),
            colons: CloneInto::clone_into(p.colons, self.global_arena),
        }
    }

    fn scrub_re_binding(&self, v: VariableReBinding) -> VariableReBinding<'g> {
        VariableReBinding {
            left: self.scrub_expr(v.left),
            expr: self.scrub_expr(v.expr),
            set: v.set,
            bind: v.bind,
            semi: v.semi,
        }
    }

    fn scrub_return(&self, r: Return) -> Return<'g> {
        Return {
            expr: r.expr.map(|e| self.scrub_expr(e)),
            ret: r.ret,
            semi: r.semi,
        }
    }

    fn scrub_tuple<'a>(&self, t: Tuple<'a, Expression<'a>>)
        -> Tuple<'g, Expression<'g>>
    {
        self.scrub_tuple_impl(t, |e| self.scrub_expr(e))
    }

    fn scrub_tuple_pattern<'a>(&self, t: Tuple<'a, Pattern<'a>>)
        -> Tuple<'g, Pattern<'g>>
    {
        self.scrub_tuple_impl(t, |p| self.scrub_pattern(p))
    }

    fn scrub_tuple_type<'a>(&self, t: Tuple<'a, Type<'a>>)
        -> Tuple<'g, Type<'g>>
    {
        self.scrub_tuple_impl(t, |t| self.scrub_type(t))
    }

    fn array<T: 'g>(&self, cap: usize) -> mem::Array<'g, T> {
        mem::Array::with_capacity(cap, self.global_arena)
    }

    fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    fn scrub_constructor_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        c: Constructor<'a, T>,
        scrubber: F
    )
        -> Constructor<'g, U>
    {
        Constructor {
            type_: self.scrub_type(c.type_),
            arguments: self.scrub_tuple_impl(c.arguments, scrubber),
        }
    }

    fn scrub_tuple_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        t: Tuple<'a, T>,
        scrubber: F,
    )
        -> Tuple<'g, U>
    {
        let mut fields = self.array(t.fields.len());
        for f in t.fields {
            fields.push(scrubber(*f));
        }

        let mut names = self.array(t.names.len());
        for n in t.names {
            names.push((Default::default(), n.1));
        }

        Tuple {
            fields: fields.into_slice(),
            commas: CloneInto::clone_into(t.commas, self.global_arena),
            names: names.into_slice(),
            separators: CloneInto::clone_into(t.separators, self.global_arena),
            open: t.open,
            close: t.close,
        }
    }
}
