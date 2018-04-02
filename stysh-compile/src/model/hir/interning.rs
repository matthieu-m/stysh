//! Interning.
//!
//! Test facilities to remove the InternId.

use std::rc;

use basic::com::{self, Span};
use basic::mem::{self, CloneInto};

use model::hir::*;

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
    pub fn resolve_enum(&self, e: Enum) -> Enum<'g> {
        Enum {
            prototype: self.insert(self.resolve_enum_prototype(*e.prototype)),
            variants: self.resolve_array(e.variants, |r| self.resolve_record(r)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_enum_prototype(&self, e: EnumProto) -> EnumProto {
        EnumProto {
            name: self.resolve_item_id(e.name),
            range: e.range,
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_function(&self, f: Function) -> Function<'g> {
        Function {
            prototype: self.insert(self.resolve_function_prototype(*f.prototype)),
            //  TODO:   Actually resolve body.
            body: CloneInto::clone_into(&f.body, self.global_arena),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_function_prototype(&self, f: FunctionProto) -> FunctionProto<'g> {
        FunctionProto {
            name: self.resolve_item_id(f.name),
            range: f.range,
            arguments: self.resolve_array(f.arguments, |a| self.resolve_argument(a)),
            result: self.resolve_type(f.result),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_item(&self, i: Item) -> Item<'g> {
        use self::Item::*;

        match i {
            Enum(e) => Enum(self.resolve_enum(e)),
            Fun(f) => Fun(self.resolve_function(f)),
            Rec(r) => Rec(self.resolve_record(r)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_pattern(&self, p: Pattern) -> Pattern<'g> {
        use self::Pattern::*;

        match p {
            Constructor(c, g)
                => Constructor(self.resolve_constructor_pattern(c), g),
            Ignored(r) => Ignored(r),
            Tuple(t, r, g) => Tuple(self.resolve_tuple_pattern(t), r, g),
            Var(v, g) => Var(self.resolve_value_id(v), g),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_prototype(&self, p: Prototype) -> Prototype<'g> {
        use self::Prototype::*;

        match p {
            Enum(e) => Enum(self.resolve_enum_prototype(e)),
            Fun(f) => Fun(self.resolve_function_prototype(f)),
            Rec(r) => Rec(self.resolve_record_prototype(r)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_record(&self, r: Record) -> Record<'g> {
        Record {
            prototype: self.insert(self.resolve_record_prototype(*r.prototype)),
            definition: self.resolve_tuple_type(r.definition),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_record_prototype(&self, r: RecordProto) -> RecordProto {
        RecordProto {
            name: self.resolve_item_id(r.name),
            range: r.range,
            enum_: self.resolve_item_id(r.enum_),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_statement(&self, stmt: Stmt) -> Stmt<'g> {
        use self::Stmt::*;

        match stmt {
            Return(r) => Return(self.resolve_return(r)),
            Set(re) => Set(self.resolve_re_binding(re)),
            Var(b) => Var(self.resolve_binding(b)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_type(&self, t: Type) -> Type<'g> {
        use self::Type::*;

        match t {
            Builtin(b) => Builtin(b),
            Enum(e, p, g)
                => Enum(self.insert(self.resolve_enum(*e)), self.resolve_path(p), g),
            Rec(r, p, g)
                => Rec(self.insert(self.resolve_record(*r)), self.resolve_path(p), g),
            Tuple(t, g) => Tuple(self.resolve_tuple_type(t), g),
            Unresolved(i, p, g)
                => Unresolved(self.resolve_item_id(i), self.resolve_path(p), g),
            UnresolvedEnum(e, p, g)
                => UnresolvedEnum(self.resolve_enum_prototype(e), self.resolve_path(p), g),
            UnresolvedRec(r, p, g)
                => UnresolvedRec(self.resolve_record_prototype(r), self.resolve_path(p), g),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_value(&self, v: Value) -> Value<'g> {
        Value {
            type_: self.resolve_type(v.type_),
            range: v.range,
            expr: self.resolve_expression(v.expr),
            gvn: v.gvn,
        }
    }

    /// Obtains the InternId of the specified range.
    pub fn from_range(&self, range: com::Range) -> mem::InternId {
        if range == Default::default() {
            Default::default()
        } else {
            let raw = &self.source[range];
            self.interner.insert(raw)
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
    pub fn scrub_enum(&self, e: Enum) -> Enum<'g> {
        Enum {
            prototype: self.insert(self.scrub_enum_prototype(*e.prototype)),
            variants: self.scrub_array(e.variants, |r| self.scrub_record(r)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_function(&self, f: Function) -> Function<'g> {
        Function {
            prototype: self.insert(self.scrub_function_prototype(*f.prototype)),
            body: self.scrub_value(f.body),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_item(&self, i: Item) -> Item<'g> {
        use self::Item::*;

        match i {
            Enum(e) => Enum(self.scrub_enum(e)),
            Fun(f) => Fun(self.scrub_function(f)),
            Rec(r) => Rec(self.scrub_record(r)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_pattern(&self, p: Pattern) -> Pattern<'g> {
        use self::Pattern::*;

        match p {
            Constructor(c, g)
                => Constructor(self.scrub_constructor_pattern(c), g),
            Ignored(r) => Ignored(r),
            Tuple(t, r, g) => Tuple(self.scrub_tuple_pattern(t), r, g),
            Var(v, g) => Var(self.scrub_value_id(v), g),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_prototype(&self, p: Prototype) -> Prototype<'g> {
        use self::Prototype::*;

        match p {
            Enum(e) => Enum(self.scrub_enum_prototype(e)),
            Fun(f) => Fun(self.scrub_function_prototype(f)),
            Rec(r) => Rec(self.scrub_record_prototype(r)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_record(&self, r: Record) -> Record<'g> {
        Record {
            prototype: self.insert(self.scrub_record_prototype(*r.prototype)),
            definition: self.scrub_tuple_type(r.definition),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_statement(&self, stmt: Stmt) -> Stmt<'g> {
        use self::Stmt::*;

        match stmt {
            Return(r) => Return(self.scrub_return(r)),
            Set(re) => Set(self.scrub_re_binding(re)),
            Var(b) => Var(self.scrub_binding(b)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_type(&self, t: Type) -> Type<'g> {
        use self::Type::*;

        match t {
            Builtin(t) => Builtin(t),
            Enum(e, p, g)
                => Enum(self.insert(self.scrub_enum(*e)), self.scrub_path(p), g),
            Rec(r, p, g)
                => Rec(self.insert(self.scrub_record(*r)), self.scrub_path(p), g),
            Tuple(t, g) => Tuple(self.scrub_tuple_type(t), g),
            Unresolved(id, p, g)
                => Unresolved(self.scrub_item_id(id), self.scrub_path(p), g),
            UnresolvedEnum(e, p, g)
                => UnresolvedEnum(self.scrub_enum_prototype(e), self.scrub_path(p), g),
            UnresolvedRec(r, p, g)
                => UnresolvedRec(self.scrub_record_prototype(r), self.scrub_path(p), g),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_value(&self, v: Value) -> Value<'g> {
        Value {
            type_: self.scrub_type(v.type_),
            range: v.range,
            expr: self.scrub_expression(v.expr),
            gvn: v.gvn,
        }
    }
}

//
//  Implementation details of Resolver
//

impl<'g> Resolver<'g> {
    fn resolve_argument(&self, a: Argument) -> Argument<'g> {
        let name = self.resolve_value_id(a.name);
        let type_ = self.resolve_type(a.type_);
        let (range, gvn) = (a.range, a.gvn);

        Argument { name, type_, range, gvn }
    }

    fn resolve_binding(&self, b: Binding) -> Binding<'g> {
        let left = self.resolve_pattern(b.left);
        let right = self.resolve_value(b.right);
        let range = b.range;

        Binding { left, right, range }
    }

    fn resolve_block(&self, stmts: &[Stmt], v: Option<&Value>) -> Expr<'g> {
        Expr::Block(
            self.resolve_statements(stmts),
            v.map(|v| self.insert(self.resolve_value(*v))),
        )
    }

    fn resolve_call(&self, c: Callable, args: &[Value]) -> Expr<'g> {
        Expr::Call(
            self.resolve_callable(c),
            self.resolve_array(args, |v| self.resolve_value(v)),
        )
    }

    fn resolve_callable(&self, c: Callable) -> Callable<'g> {
        use self::Callable::*;

        match c {
            Builtin(f) => Builtin(f),
            Function(f) => Function(self.resolve_function_prototype(f)),
            Unknown(v) => Unknown(self.resolve_value_id(v)),
            Unresolved(c) => Unresolved(
                self.resolve_array(c, |c| self.resolve_callable(c))
            ),
        }
    }

    fn resolve_constructor_pattern(&self, c: Constructor<Pattern>)
        -> Constructor<'g, Pattern<'g>>
    {
        self.resolve_constructor_impl(c, |p| self.resolve_pattern(p))
    }

    fn resolve_constructor_value(&self, c: Constructor<Value>)
        -> Constructor<'g, Value<'g>>
    {
        self.resolve_constructor_impl(c, |v| self.resolve_value(v))
    }

    fn resolve_expression(&self, expr: Expr) -> Expr<'g> {
        use self::Expr::*;

        match expr {
            Block(stmts, v) => self.resolve_block(stmts, v),
            BuiltinVal(v) => BuiltinVal(CloneInto::clone_into(&v, self.global_arena)),
            Call(c, a) => self.resolve_call(c, a),
            Constructor(c) => Constructor(self.resolve_constructor_value(c)),
            FieldAccess(v, f) => FieldAccess(
                self.insert(self.resolve_value(*v)),
                self.resolve_field(f),
            ),
            If(condition, if_, else_) => If(
                self.insert(self.resolve_value(*condition)),
                self.insert(self.resolve_value(*if_)),
                self.insert(self.resolve_value(*else_)),
            ),
            Implicit(i) => Implicit(self.resolve_implicit(i)),
            Loop(s) => Loop(self.resolve_statements(s)),
            Ref(v, g) => Ref(self.resolve_value_id(v), g),
            Tuple(t) => Tuple(self.resolve_tuple_value(t)),
            UnresolvedRef(id) => UnresolvedRef(self.resolve_value_id(id)),
        }
    }

    fn resolve_field(&self, f: Field) -> Field {
        match f {
            Field::Index(..) => f,
            Field::Unresolved(n) => Field::Unresolved(self.resolve_value_id(n)),
        }
    }

    fn resolve_implicit(&self, i: Implicit) -> Implicit<'g> {
        use self::Implicit::*;

        match i {
            ToEnum(e, v) => ToEnum(
                self.resolve_enum_prototype(e),
                self.insert(self.resolve_value(*v)),
            ),
        }
    }

    fn resolve_item_id(&self, i: ItemIdentifier) -> ItemIdentifier {
        i.with_id(self.from_range(i.span()))
    }

    fn resolve_path(&self, p: Path) -> Path<'g> {
        Path {
            components: self.resolve_array(p.components, |c| self.resolve_type(c)),
        }
    }

    fn resolve_re_binding(&self, r: ReBinding) -> ReBinding<'g> {
        ReBinding {
            left: self.resolve_value(r.left),
            right: self.resolve_value(r.right),
            range: r.range,
        }
    }

    fn resolve_return(&self, r: Return) -> Return<'g> {
        Return {
            value: self.resolve_value(r.value),
            range: r.range,
        }
    }

    fn resolve_statements(&self, stmts: &[Stmt]) -> &'g [Stmt<'g>] {
        self.resolve_array(stmts, |s| self.resolve_statement(s))
    }

    fn resolve_tuple_pattern(&self, t: Tuple<Pattern>) -> Tuple<'g, Pattern<'g>> {
        self.resolve_tuple_impl(t, |p| self.resolve_pattern(p))
    }

    fn resolve_tuple_type(&self, t: Tuple<Type>) -> Tuple<'g, Type<'g>> {
        self.resolve_tuple_impl(t, |t| self.resolve_type(t))
    }

    fn resolve_tuple_value(&self, t: Tuple<Value>) -> Tuple<'g, Value<'g>> {
        self.resolve_tuple_impl(t, |v| self.resolve_value(v))
    }

    fn resolve_value_id(&self, v: ValueIdentifier) -> ValueIdentifier {
        v.with_id(self.from_range(v.span()))
    }

    fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    fn resolve_array<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        input: &[T],
        resolver: F,
    )
        -> &'g [U]
    {
        let mut result =
            mem::Array::with_capacity(input.len(), self.global_arena);
        for i in input {
            result.push(resolver(*i));
        }
        result.into_slice()
    }

    fn resolve_constructor_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        c: Constructor<'a, T>,
        resolver: F,
    )
        -> Constructor<'g, U>
    {
        Constructor {
            type_: self.resolve_type(c.type_),
            arguments: self.resolve_tuple_impl(c.arguments, resolver),
            range: c.range,
        }
    }

    fn resolve_tuple_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        c: Tuple<'a, T>,
        resolver: F,
    )
        -> Tuple<'g, U>
    {
        Tuple {
            fields: self.resolve_array(c.fields, resolver),
            names: self.resolve_array(c.names, |v| self.resolve_value_id(v)),
        }
    }

}

//
//  Implementation details of Scrubber
//

impl<'g> Scrubber<'g> {
    fn scrub_argument(&self, a: Argument) -> Argument<'g> {
        let name = self.scrub_value_id(a.name);
        let type_ = self.scrub_type(a.type_);
        let (range, gvn) = (a.range, a.gvn);

        Argument { name, type_, range, gvn }
    }

    fn scrub_binding(&self, b: Binding) -> Binding<'g> {
        let left = self.scrub_pattern(b.left);
        let right = self.scrub_value(b.right);
        let range = b.range;

        Binding { left, right, range }
    }

    fn scrub_block(&self, stmts: &[Stmt], v: Option<&Value>) -> Expr<'g> {
        Expr::Block(
            self.scrub_statements(stmts),
            v.map(|v| self.insert(self.scrub_value(*v))),
        )
    }

    fn scrub_call(&self, c: Callable, args: &[Value]) -> Expr<'g> {
        Expr::Call(
            self.scrub_callable(c),
            self.scrub_array(args, |v| self.scrub_value(v)),
        )
    }

    fn scrub_callable(&self, c: Callable) -> Callable<'g> {
        use self::Callable::*;

        match c {
            Builtin(f) => Builtin(f),
            Function(f) => Function(self.scrub_function_prototype(f)),
            Unknown(v) => Unknown(self.scrub_value_id(v)),
            Unresolved(c) => Unresolved(
                self.scrub_array(c, |c| self.scrub_callable(c))
            ),
        }
    }

    fn scrub_constructor_pattern(&self, c: Constructor<Pattern>)
        -> Constructor<'g, Pattern<'g>>
    {
        self.scrub_constructor_impl(c, |p| self.scrub_pattern(p))
    }

    fn scrub_constructor_value(&self, c: Constructor<Value>)
        -> Constructor<'g, Value<'g>>
    {
        self.scrub_constructor_impl(c, |v| self.scrub_value(v))
    }

    fn scrub_enum_prototype(&self, e: EnumProto) -> EnumProto {
        EnumProto {
            name: self.scrub_item_id(e.name),
            range: e.range,
        }
    }

    fn scrub_expression(&self, expr: Expr) -> Expr<'g> {
        use self::Expr::*;

        match expr {
            Block(stmts, v) => self.scrub_block(stmts, v),
            BuiltinVal(v) => BuiltinVal(CloneInto::clone_into(&v, self.global_arena)),
            Call(c, a) => self.scrub_call(c, a),
            Constructor(c) => Constructor(self.scrub_constructor_value(c)),
            FieldAccess(v, f) => FieldAccess(
                self.insert(self.scrub_value(*v)),
                self.scrub_field(f),
            ),
            If(condition, if_, else_) => If(
                self.insert(self.scrub_value(*condition)),
                self.insert(self.scrub_value(*if_)),
                self.insert(self.scrub_value(*else_)),
            ),
            Implicit(i) => Implicit(self.scrub_implicit(i)),
            Loop(s) => Loop(self.scrub_statements(s)),
            Ref(v, g) => Ref(self.scrub_value_id(v), g),
            Tuple(t) => Tuple(self.scrub_tuple_value(t)),
            UnresolvedRef(id) => UnresolvedRef(self.scrub_value_id(id)),
        }
    }

    fn scrub_field(&self, f: Field) -> Field {
        match f {
            Field::Index(..) => f,
            Field::Unresolved(n) => Field::Unresolved(self.scrub_value_id(n)),
        }
    }

    fn scrub_function_prototype(&self, f: FunctionProto) -> FunctionProto<'g> {
        FunctionProto {
            name: self.scrub_item_id(f.name),
            range: f.range,
            arguments: self.scrub_array(f.arguments, |a| self.scrub_argument(a)),
            result: self.scrub_type(f.result),
        }
    }

    fn scrub_implicit(&self, i: Implicit) -> Implicit<'g> {
        use self::Implicit::*;

        match i {
            ToEnum(e, v) => ToEnum(
                self.scrub_enum_prototype(e),
                self.insert(self.scrub_value(*v)),
            ),
        }
    }

    fn scrub_item_id(&self, id: ItemIdentifier) -> ItemIdentifier {
        id.with_id(Default::default())
    }

    fn scrub_path(&self, p: Path) -> Path<'g> {
        Path {
            components: self.scrub_array(p.components, |c| self.scrub_type(c)),
        }
    }

    fn scrub_re_binding(&self, r: ReBinding) -> ReBinding<'g> {
        ReBinding {
            left: self.scrub_value(r.left),
            right: self.scrub_value(r.right),
            range: r.range,
        }
    }

    fn scrub_record_prototype(&self, r: RecordProto) -> RecordProto {
        RecordProto {
            name: self.scrub_item_id(r.name),
            range: r.range,
            enum_: self.scrub_item_id(r.enum_),
        }
    }

    fn scrub_return(&self, r: Return) -> Return<'g> {
        Return {
            value: self.scrub_value(r.value),
            range: r.range,
        }
    }

    fn scrub_statements(&self, stmts: &[Stmt]) -> &'g [Stmt<'g>] {
        self.scrub_array(stmts, |s| self.scrub_statement(s))
    }

    fn scrub_tuple_pattern(&self, t: Tuple<Pattern>) -> Tuple<'g, Pattern<'g>> {
        self.scrub_tuple_impl(t, |p| self.scrub_pattern(p))
    }

    fn scrub_tuple_type(&self, t: Tuple<Type>) -> Tuple<'g, Type<'g>> {
        self.scrub_tuple_impl(t, |t| self.scrub_type(t))
    }

    fn scrub_tuple_value(&self, t: Tuple<Value>) -> Tuple<'g, Value<'g>> {
        self.scrub_tuple_impl(t, |v| self.scrub_value(v))
    }

    fn scrub_value_id(&self, id: ValueIdentifier) -> ValueIdentifier {
        id.with_id(Default::default())
    }

    fn array<T: 'g>(&self, cap: usize) -> mem::Array<'g, T> {
        mem::Array::with_capacity(cap, self.global_arena)
    }

    fn insert<T: 'g>(&self, t: T) -> &'g T { self.global_arena.insert(t) }

    fn scrub_array<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        input: &[T],
        scrubber: F,
    )
        -> &'g [U]
    {
        let mut result = self.array(input.len());
        for i in input {
            result.push(scrubber(*i));
        }
        result.into_slice()
    }

    fn scrub_constructor_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        c: Constructor<'a, T>,
        scrubber: F,
    )
        -> Constructor<'g, U>
    {
        Constructor {
            type_: self.scrub_type(c.type_),
            arguments: self.scrub_tuple_impl(c.arguments, scrubber),
            range: c.range,
        }
    }

    fn scrub_tuple_impl<'a, T: Copy + 'a, U: 'g, F: Fn(T) -> U>(
        &self,
        c: Tuple<'a, T>,
        scrubber: F,
    )
        -> Tuple<'g, U>
    {
        Tuple {
            fields: self.scrub_array(c.fields, scrubber),
            names: self.scrub_array(c.names, |v| self.scrub_value_id(v)),
        }
    }

}
