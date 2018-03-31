//! Semantic pass: global value numbering.
//!
//! High-level item in charge of numbering.
//!
//! Note:   the only guarantee of this algorithm is that a unique ID is
//!         assigned to each and every value. The IDs should not be expected
//!         to appear in any order (depth-first, breadth-first, ...).

use basic::mem::{self, CloneInto};

use model::hir::*;

/// Global Value Numberer
///
/// Assigns a unique ID to each value within the given context. Since the
/// argument is immutable, returns a modified copy.
pub struct GlobalValueNumberer<'g, 'local> {
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'g, 'local> GlobalValueNumberer<'g, 'local> {
    /// Creates an instance.
    pub fn new(global_arena: &'g mem::Arena, local_arena: &'local mem::Arena)
        -> Self
    {
        GlobalValueNumberer { global_arena, local_arena }
    }

    /// Number each and every value contained within.
    pub fn number_function(&self, function: &Function) -> Function<'g> {
        let mut counter = 0;
        let mut imp = Impl::new(
            || { counter += 1; Gvn(counter) },
            self.global_arena,
            self.local_arena
        );

        Function {
            prototype: imp.function_proto_ref(function.prototype),
            body: imp.value(&function.body),
        }
    }

    /// Number each and every value contained within.
    pub fn number_value(&self, value: &Value) -> Value<'g> {
        let mut counter = 0;
        let mut imp = Impl::new(
            || { counter += 1; Gvn(counter) },
            self.global_arena,
            self.local_arena
        );

        imp.value(value)
    }

    /// Unnumber each and every value contained within.
    pub fn unnumber_function(&self, function: &Function) -> Function<'g> {
        let mut imp = Impl::new(
            Gvn::default,
            self.global_arena,
            self.local_arena
        );

        Function {
            prototype: imp.function_proto_ref(function.prototype),
            body: imp.value(&function.body),
        }
    }

    /// Unnumber each and every value contained within.
    pub fn unnumber_value(&self, value: &Value) -> Value<'g> {
        let mut imp = Impl::new(
            Gvn::default,
            self.global_arena,
            self.local_arena
        );

        imp.value(value)
    }
}

//
//  Implementation Details
//
struct Impl<'g, 'local, G: FnMut() -> Gvn> {
    generator: G,
    arena: &'g mem::Arena,
    bindings: mem::ArrayMap<'local, ValueIdentifier, Gvn>,
}

impl<'g, 'local, G: FnMut() -> Gvn> Impl<'g, 'local, G> {
    fn new(
        generator: G,
        global_arena: &'g mem::Arena,
        local_arena: &'local mem::Arena
    )
        -> Self
    {
        Impl {
            generator: generator,
            arena: global_arena,
            bindings: mem::ArrayMap::new(local_arena),
        }
    }

    //
    //  Pattern Matching
    //
    fn argument(&mut self, argument: &Argument) -> Argument<'g> {
        let (name, range) = (argument.name, argument.range);
        let type_ = self.intern(&argument.type_);
        let gvn = self.register_identifier(name);

        Argument { name, type_, range, gvn }
    }

    fn binding(&mut self, binding: &Binding) -> Binding<'g> {
        let right = self.value(&binding.right);
        let left = self.pattern(&binding.left);
        let range = binding.range;

        Binding { left, right, range }
    }

    fn callable(&mut self, callable: &Callable) -> Callable<'g> {
        use self::Callable::*;

        match *callable {
            Builtin(b) => Builtin(b),
            Function(p) => Function(self.intern(&p)),
            Unknown(id) => Unknown(id),
            Unresolved(cs)
                => Unresolved(mem::CloneInto::clone_into(cs, self.arena)),
        }
    }

    fn constructor_pattern(&mut self, constructor: &Constructor<Pattern>)
        -> Constructor<'g, Pattern<'g>>
    {
        Constructor {
            type_: self.intern(&constructor.type_),
            arguments: self.tuple_pattern(&constructor.arguments),
            range: constructor.range,
        }
    }

    fn constructor_value(&mut self, constructor: &Constructor<Value>)
        -> Constructor<'g, Value<'g>>
    {
        Constructor {
            type_: self.intern(&constructor.type_),
            arguments: self.tuple_value(&constructor.arguments),
            range: constructor.range,
        }
    }

    fn expr(&mut self, expr: &Expr) -> Expr<'g> {
        use self::Expr::*;

        match *expr {
            Block(ss, v) => Block(self.stmts(ss), v.map(|v| self.value_ref(v))),
            BuiltinVal(v) => BuiltinVal(self.intern(&v)),
            Call(c, vs) => Call(self.callable(&c), self.values(vs)),
            Constructor(c) => Constructor(self.constructor_value(&c)),
            FieldAccess(v, f) => FieldAccess(self.value_ref(v), f),
            If(c, t, f)
                => If(self.value_ref(c), self.value_ref(t), self.value_ref(f)),
            Implicit(i) => Implicit(self.implicit(&i)),
            Loop(ss) => Loop(self.stmts(ss)),
            Ref(id, _) => Ref(id, self.lookup_identifier(id)),
            Tuple(t) => Tuple(self.tuple_value(&t)),
            UnresolvedRef(id) => UnresolvedRef(id),
        }
    }

    fn function_proto_ref(&mut self, proto: &FunctionProto)
        -> &'g FunctionProto<'g>
    {
        let mut array = self.array(proto.arguments.len());

        for a in proto.arguments {
            array.push(self.argument(a));
        }

        self.intern_ref(&FunctionProto {
            name: proto.name,
            range: proto.range,
            arguments: array.into_slice(),
            result: self.intern(&proto.result),
        })
    }

    fn implicit(&mut self, implicit: &Implicit) -> Implicit<'g> {
        use self::Implicit::*;

        match *implicit {
            ToEnum(p, v) => ToEnum(p, self.value_ref(v)),
        }
    }

    fn pattern(&mut self, pattern: &Pattern) -> Pattern<'g> {
        use self::Pattern::*;

        match *pattern {
            Constructor(c, _)
                => Constructor(self.constructor_pattern(&c), self.next()),
            Ignored(r) => Ignored(r),
            Tuple(t, r, _) => Tuple(self.tuple_pattern(&t), r, self.next()),
            Var(id, _) => Var(id, self.register_identifier(id)),
        }
    }

    fn patterns(&mut self, patterns: &[Pattern]) -> &'g [Pattern<'g>] {
        let mut array = self.array(patterns.len());
        for p in patterns {
            array.push(self.pattern(p));
        }
        array.into_slice()
    }

    fn rebinding(&mut self, rebinding: &ReBinding) -> ReBinding<'g> {
        ReBinding {
            left: self.value(&rebinding.left),
            right: self.value(&rebinding.right),
            range: rebinding.range,
        }
    }

    fn return_(&mut self, ret: &Return) -> Return<'g> {
        Return {
            value: self.value(&ret.value),
            range: ret.range,
        }
    }

    fn stmt(&mut self, stmt: &Stmt) -> Stmt<'g> {
        use self::Stmt::*;

        match *stmt {
            Return(r) => Return(self.return_(&r)),
            Set(re) => Set(self.rebinding(&re)),
            Var(b) => Var(self.binding(&b)),
        }
    }

    fn stmts(&mut self, stmts: &[Stmt]) -> &'g [Stmt<'g>] {
        let mut array = self.array(stmts.len());
        for s in stmts {
            array.push(self.stmt(s));
        }
        array.into_slice()
    }

    fn tuple_pattern(&mut self, tuple: &Tuple<Pattern>)
        -> Tuple<'g, Pattern<'g>>
    {
        Tuple {
            fields: self.patterns(tuple.fields),
            names: CloneInto::clone_into(tuple.names, self.arena),
        }
    }

    fn tuple_value(&mut self, tuple: &Tuple<Value>) -> Tuple<'g, Value<'g>>
    {
        Tuple {
            fields: self.values(tuple.fields),
            names: CloneInto::clone_into(tuple.names, self.arena),
        }
    }

    fn value(&mut self, value: &Value) -> Value<'g> {
        let expr = self.expr(&value.expr);
        Value {
            type_: self.intern(&value.type_),
            range: value.range,
            expr: expr,
            gvn: self.next(),
        }
    }

    fn value_ref(&mut self, value: &Value) -> &'g Value<'g> {
        let v = self.value(value);
        self.arena.insert(v)
    }

    fn values(&mut self, values: &[Value]) -> &'g [Value<'g>] {
        let mut array = self.array(values.len());
        for v in values {
            array.push(self.value(v));
        }
        array.into_slice()
    }

    //
    //  Support
    //
    fn array<T: 'g>(&self, capacity: usize) -> mem::Array<'g, T> {
        mem::Array::with_capacity(capacity, self.arena)
    }

    fn intern<T: mem::CloneInto<'g> + Copy>(&self, t: &T)
        -> <T as mem::CloneInto<'g>>::Output
    {
        self.arena.intern(t)
    }

    fn intern_ref<T: mem::CloneInto<'g>>(&self, t: &T)
        -> &'g <T as mem::CloneInto<'g>>::Output
    {
        self.arena.intern_ref(t)
    }

    fn lookup_identifier(&self, id: ValueIdentifier) -> Gvn {
        self.bindings.get(&id).cloned().expect("Known identifier!")
    }

    fn next(&mut self) -> Gvn { (self.generator)() }

    fn register_identifier(&mut self, id: ValueIdentifier) -> Gvn {
        let gvn = self.next();
        self.bindings.insert(id, gvn);
        gvn
    }

}

#[cfg(test)]
mod tests {
    use basic::mem;
    use model::hir::*;
    use model::hir::builder::*;

    #[test]
    fn brush() {
        let global_arena = mem::Arena::new();
        let v = Factory::new(&global_arena).value();

        //  "1 + 2"
        let before = v.call().push(v.int(1, 0)).push(v.int(2, 4)).build();
        let after =
            v.call()
                .push(v.int(1, 0).with_gvn(1))
                .push(v.int(2, 4).with_gvn(2))
                .build()
                .with_gvn(3);

        assert_eq!(valueit(&global_arena, &before), after);
    }

    #[test]
    fn block_simple() {
        let global_arena = mem::Arena::new();
        let (_, p, _, s, _, v) = factories(&global_arena);

        //  "{ :var n := 2; n }"
        let n = v.id(7, 1);
        let before =
            v.block(v.int_ref(n, 15))
                .push(s.var(p.var(n), v.int(2, 12)))
                .build();
        let after =
            v.block(v.int_ref(n, 15).ref_gvn(2).with_gvn(3))
                .push(s.var(p.var(n).with_gvn(2), v.int(2, 12).with_gvn(1)))
                .build()
                .with_gvn(4);

        assert_eq!(valueit(&global_arena, &before), after);
    }

    fn factories<'g>(arena: &'g mem::Arena) -> (
        ItemFactory<'g>,
        PatternFactory<'g>,
        PrototypeFactory<'g>,
        StmtFactory<'g>,
        TypeFactory<'g>,
        ValueFactory<'g>,
    )
    {
        let f = Factory::new(arena);
        (f.item(), f.pat(), f.proto(), f.stmt(), f.type_(), f.value())
    }

    fn valueit<'g>(global_arena: &'g mem::Arena, value: &Value) -> Value<'g> {
        use super::GlobalValueNumberer;

        let mut local_arena = mem::Arena::new();

        let result =
            GlobalValueNumberer::new(global_arena, &local_arena)
                .number_value(value);
        local_arena.recycle();

        result
    }
}