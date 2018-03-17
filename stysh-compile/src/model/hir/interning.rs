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
            name: self.resolve_item_identifier(e.name),
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
            name: self.resolve_item_identifier(f.name),
            range: f.range,
            arguments: self.resolve_array(f.arguments, |b| self.resolve_binding(b)),
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
            name: self.resolve_item_identifier(r.name),
            range: r.range,
            enum_: self.resolve_item_identifier(r.enum_),
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
            prototype: self.insert(self.scrub_enum_proto(*e.prototype)),
            variants: self.scrub_array(e.variants, |r| self.scrub_record(r)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_function(&self, f: Function) -> Function<'g> {
        Function {
            prototype: self.insert(self.scrub_function_proto(*f.prototype)),
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
            Constructor(c) => Constructor(self.scrub_constructor_pattern(c)),
            Ignored(r) => Ignored(r),
            Tuple(t, r) => Tuple(self.scrub_tuple_pattern(t), r),
            Var(v, g) => Var(self.scrub_value_id(v), g),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_prototype(&self, p: Prototype) -> Prototype<'g> {
        use self::Prototype::*;

        match p {
            Enum(e) => Enum(self.scrub_enum_proto(e)),
            Fun(f) => Fun(self.scrub_function_proto(f)),
            Rec(r) => Rec(self.scrub_record_proto(r)),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_record(&self, r: Record) -> Record<'g> {
        Record {
            prototype: self.insert(self.scrub_record_proto(*r.prototype)),
            definition: self.scrub_tuple_type(r.definition),
        }
    }

    /// Scrubs InternId, recursively.
    pub fn scrub_type(&self, t: Type) -> Type<'g> {
        use self::Type::*;

        match t {
            Builtin(t) => Builtin(t),
            Enum(e) => Enum(self.scrub_enum_proto(e)),
            Rec(r) => Rec(self.scrub_record_proto(r)),
            Tuple(t) => Tuple(self.scrub_tuple_type(t)),
            Unresolved(id) => Unresolved(self.scrub_item_id(id)),
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
    fn resolve_binding(&self, b: Binding) -> Binding<'g> {
        use self::Binding::*;

        match b {
            Argument(v, g, t, r) => Argument(
                self.resolve_value_identifier(v),
                g,
                self.resolve_type(t),
                r,
            ),
            //  TODO:   Actually resolve variable.
            Variable(p, v, r) => Variable(
                CloneInto::clone_into(&p, self.global_arena),
                CloneInto::clone_into(&v, self.global_arena),
                r
            ),
        }
    }

    fn resolve_item_identifier(&self, i: ItemIdentifier) -> ItemIdentifier {
        i.with_id(self.from_range(i.span()))
    }

    fn resolve_tuple_type(&self, t: Tuple<Type>) -> Tuple<'g, Type<'g>> {
        Tuple {
            fields: self.resolve_array(t.fields, |t| self.resolve_type(t))
        }
    }

    fn resolve_type(&self, t: Type) -> Type<'g> {
        use self::Type::*;

        match t {
            Builtin(b) => Builtin(b),
            Enum(e) => Enum(self.resolve_enum_prototype(e)),
            Rec(r) => Rec(self.resolve_record_prototype(r)),
            Tuple(t) => Tuple(self.resolve_tuple_type(t)),
            Unresolved(i) => Unresolved(self.resolve_item_identifier(i)),
        }
    }

    fn resolve_value_identifier(&self, v: ValueIdentifier) -> ValueIdentifier {
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

    fn from_range(&self, range: com::Range) -> mem::InternId {
        let raw = &self.source[range];
        self.interner.insert(raw)
    }
}

//
//  Implementation details of Scrubber
//

impl<'g> Scrubber<'g> {
    fn scrub_binding(&self, b: Binding) -> Binding<'g> {
        use self::Binding::*;

        match b {
            Argument(id, g, t, r) => Argument(
                self.scrub_value_id(id),
                g,
                self.scrub_type(t),
                r,
            ),
            Variable(p, v, r) => Variable(
                self.scrub_pattern(p),
                self.scrub_value(v),
                r
            ),
        }
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
            Function(f) => Function(self.scrub_function_proto(f)),
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

    fn scrub_enum_proto(&self, e: EnumProto) -> EnumProto {
        EnumProto {
            name: self.scrub_item_id(e.name),
            range: e.range,
        }
    }

    fn scrub_expression(&self, expr: Expr) -> Expr<'g> {
        use self::Expr::*;

        match expr {
            ArgumentRef(v, g) => ArgumentRef(self.scrub_value_id(v), g),
            Block(stmts, v) => self.scrub_block(stmts, v),
            BuiltinVal(v) => BuiltinVal(CloneInto::clone_into(&v, self.global_arena)),
            Call(c, a) => self.scrub_call(c, a),
            Constructor(c) => Constructor(self.scrub_constructor_value(c)),
            FieldAccess(v, i)
                => FieldAccess(self.insert(self.scrub_value(*v)), i),
            If(condition, if_, else_) => If(
                self.insert(self.scrub_value(*condition)),
                self.insert(self.scrub_value(*if_)),
                self.insert(self.scrub_value(*else_)),
            ),
            Implicit(i) => Implicit(self.scrub_implicit(i)),
            Loop(s) => Loop(self.scrub_statements(s)),
            Tuple(t) => Tuple(self.scrub_tuple_value(t)),
            UnresolvedField(v, id) => UnresolvedField(
                self.insert(self.scrub_value(*v)),
                self.scrub_value_id(id),
            ),
            UnresolvedRef(id) => UnresolvedRef(self.scrub_value_id(id)),
            VariableRef(id, g) => VariableRef(self.scrub_value_id(id), g),
        }
    }

    fn scrub_function_proto(&self, f: FunctionProto) -> FunctionProto<'g> {
        FunctionProto {
            name: self.scrub_item_id(f.name),
            range: f.range,
            arguments: self.scrub_array(f.arguments, |b| self.scrub_binding(b)),
            result: self.scrub_type(f.result),
        }
    }

    fn scrub_implicit(&self, i: Implicit) -> Implicit<'g> {
        use self::Implicit::*;

        match i {
            ToEnum(e, v) => ToEnum(
                self.scrub_enum_proto(e),
                self.insert(self.scrub_value(*v)),
            ),
        }
    }

    fn scrub_item_id(&self, id: ItemIdentifier) -> ItemIdentifier {
        id.with_id(Default::default())
    }

    fn scrub_re_binding(&self, r: ReBinding) -> ReBinding<'g> {
        ReBinding {
            left: self.scrub_value(r.left),
            right: self.scrub_value(r.right),
            range: r.range,
        }
    }

    fn scrub_record_proto(&self, r: RecordProto) -> RecordProto {
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

    fn scrub_statement(&self, stmt: Stmt) -> Stmt<'g> {
        use self::Stmt::*;

        match stmt {
            Return(r) => Return(self.scrub_return(r)),
            Set(re) => Set(self.scrub_re_binding(re)),
            Var(b) => Var(self.scrub_binding(b)),
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
            type_: self.scrub_record_proto(c.type_),
            arguments: self.scrub_array(c.arguments, scrubber),
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
        Tuple { fields: self.scrub_array(c.fields, scrubber) }
    }

}