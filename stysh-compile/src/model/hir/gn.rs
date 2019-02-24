//! Semantic pass: global numbering.
//!
//! High-level item in charge of numbering.
//!
//! Note:   the only guarantee of this algorithm is that a unique ID is
//!         assigned to each and every type and value. The IDs should not be
//!         expected to appear in any order (depth-first, breadth-first, ...).

use std::collections::HashMap;

use basic::com::{Range, Span};
use basic::mem::{DynArray, Ptr};

use model::hir::*;

/// Global Value Numberer
///
/// Assigns a unique ID to each value within the given context. Since the
/// argument is immutable, returns a modified copy.
pub struct GlobalNumberer;

impl GlobalNumberer {
    /// Creates an instance.
    pub fn new() -> Self { GlobalNumberer }

    /// Number each and every value contained within.
    pub fn number_function(&self, function: Function) -> Function {
        let mut i = BuiltinType::maximum_gin().0;
        let mut v = 0;
        let mut imp = self.imp(|| { i += 1; Gin(i) }, || { v += 1; Gvn(v) });

        Function {
            prototype: imp.function_prototype(function.prototype),
            body: imp.value(function.body),
        }
    }

    /// Number each and every pattern contained within.
    pub fn number_pattern(&self, pattern: Pattern) -> Pattern {
        let mut i = BuiltinType::maximum_gin().0;
        let mut v = 0;
        let mut imp = self.imp(|| { i += 1; Gin(i) }, || { v += 1; Gvn(v) });

        imp.pattern(pattern)
    }

    /// Number each and every type contained within.
    pub fn number_type(&self, ty: Type) -> Type {
        let mut i = BuiltinType::maximum_gin().0;
        let mut v = 0;
        let mut imp = self.imp(|| { i += 1; Gin(i) }, || { v += 1; Gvn(v) });

        imp.type_(ty)
    }

    /// Number each and every value contained within.
    pub fn number_value(&self, value: Value) -> Value {
        let mut i = BuiltinType::maximum_gin().0;
        let mut v = 0;
        let mut imp = self.imp(|| { i += 1; Gin(i) }, || { v += 1; Gvn(v) });

        imp.value(value)
    }

    /// Unnumber each and every value contained within.
    pub fn unnumber_function(&self, function: Function) -> Function {
        let mut imp = self.imp(Gin::default, Gvn::default);

        Function {
            prototype: imp.function_prototype(function.prototype),
            body: imp.value(function.body),
        }
    }

    /// Unnumber each and every pattern contained within.
    pub fn unnumber_pattern(&self, pattern: Pattern) -> Pattern {
        let mut imp = self.imp(Gin::default, Gvn::default);

        imp.pattern(pattern)
    }

    /// Unnumber each and every type contained within.
    pub fn unnumber_type(&self, ty: Type) -> Type {
        let mut imp = self.imp(Gin::default, Gvn::default);

        imp.type_(ty)
    }

    /// Unnumber each and every value contained within.
    pub fn unnumber_value(&self, value: Value) -> Value {
        let mut imp = self.imp(Gin::default, Gvn::default);

        imp.value(value)
    }
}

//
//  Implementation Details
//
struct Impl<I: FnMut() -> Gin, V: FnMut() -> Gvn> {
    gin: I,
    gvn: V,
    items: HashMap<Range, Gin>,
    values: HashMap<Range, Gvn>,
}

impl GlobalNumberer {
    fn imp<I, V>(&self, i: I, v: V) -> Impl<I, V>
        where
            I: FnMut() -> Gin,
            V: FnMut() -> Gvn,
    {
        Impl::new(i, v)
    }
}

impl<I: FnMut() -> Gin, V: FnMut() -> Gvn> Impl<I, V> {
    fn new(i: I, v: V) -> Self {
        Impl {
            gin: i,
            gvn: v,
            items: HashMap::new(),
            values: HashMap::new(),
        }
    }

    //
    //  Pattern Matching
    //
    fn argument(&mut self, argument: Argument) -> Argument {
        let (name, range) = (argument.name, argument.range);
        let type_ = self.type_(argument.type_);
        let gvn = self.register_value(name);

        Argument { name, type_, range, gvn }
    }

    fn binding(&mut self, binding: Binding) -> Binding {
        let right = self.value(binding.right);
        let left = self.pattern(binding.left);
        let range = binding.range;

        Binding { left, right, range }
    }

    fn callable(&mut self, callable: Callable) -> Callable {
        use self::Callable::*;

        match callable {
            Function(p) => Function(self.function_prototype(p)),
            c => c,
        }
    }

    fn constructor_pattern(&mut self, constructor: Constructor<Pattern>)
        -> Constructor<Pattern>
    {
        Constructor {
            type_: self.type_(constructor.type_),
            arguments: self.tuple_pattern(constructor.arguments),
            range: constructor.range,
        }
    }

    fn constructor_value(&mut self, constructor: Constructor<Value>)
        -> Constructor<Value>
    {
        Constructor {
            type_: self.type_(constructor.type_),
            arguments: self.tuple_value(constructor.arguments),
            range: constructor.range,
        }
    }

    fn expr(&mut self, expr: Expr) -> Expr {
        use self::Expr::*;

        match expr {
            Block(ss, v) => Block(self.stmts(ss), v.map(|v| self.value_ptr(v))),
            BuiltinVal(v) => BuiltinVal(v),
            Call(c, vs) => Call(self.callable(c), self.values(vs)),
            Constructor(c) => Constructor(self.constructor_value(c)),
            FieldAccess(v, f) => FieldAccess(self.value_ptr(v), f),
            If(c, t, f)
                => If(self.value_ptr(c), self.value_ptr(t), self.value_ptr(f)),
            Implicit(i) => Implicit(self.implicit(i)),
            Loop(ss) => Loop(self.stmts(ss)),
            Ref(id, _) => Ref(id, self.lookup_value(id)),
            Tuple(t) => Tuple(self.tuple_value(t)),
            UnresolvedRef(id) => UnresolvedRef(id),
        }
    }

    fn function_prototype(&mut self, proto: FunctionProto) -> FunctionProto {
        let array = DynArray::with_capacity(proto.arguments.len());

        for a in proto.arguments {
            array.push(self.argument(a));
        }

        FunctionProto {
            name: proto.name,
            range: proto.range,
            arguments: array,
            result: self.type_(proto.result),
        }
    }

    fn implicit(&mut self, implicit: Implicit) -> Implicit {
        use self::Implicit::*;

        match implicit {
            ToEnum(p, v) => ToEnum(p, self.value_ptr(v)),
        }
    }

    fn path(&mut self, path: Path) -> Path {
        Path { components: self.types(path.components) }
    }

    fn pattern(&mut self, pattern: Pattern) -> Pattern {
        use self::Pattern::*;

        match pattern {
            Constructor(c, _)
                => Constructor(self.constructor_pattern(c), self.next_value()),
            Ignored(r) => Ignored(r),
            Tuple(t, r, _) => Tuple(self.tuple_pattern(t), r, self.next_value()),
            Var(id, _) => Var(id, self.register_value(id)),
        }
    }

    fn patterns(&mut self, patterns: DynArray<Pattern>) -> DynArray<Pattern> {
        let array = DynArray::with_capacity(patterns.len());
        for p in patterns {
            array.push(self.pattern(p));
        }
        array
    }

    fn rebinding(&mut self, rebinding: ReBinding) -> ReBinding {
        ReBinding {
            left: self.value(rebinding.left),
            right: self.value(rebinding.right),
            range: rebinding.range,
        }
    }

    fn return_(&mut self, ret: Return) -> Return {
        Return {
            value: self.value(ret.value),
            range: ret.range,
        }
    }

    fn stmt(&mut self, stmt: Stmt) -> Stmt {
        use self::Stmt::*;

        match stmt {
            Return(r) => Return(self.return_(r)),
            Set(re) => Set(self.rebinding(re)),
            Var(b) => Var(self.binding(b)),
        }
    }

    fn stmts(&mut self, stmts: DynArray<Stmt>) -> DynArray<Stmt> {
        let array = DynArray::with_capacity(stmts.len());
        for s in stmts {
            array.push(self.stmt(s));
        }
        array
    }

    fn tuple_pattern(&mut self, tuple: Tuple<Pattern>) -> Tuple<Pattern> {
        Tuple {
            fields: self.patterns(tuple.fields),
            names: tuple.names.clone(),
        }
    }

    fn tuple_type(&mut self, tuple: Tuple<Type>) -> Tuple<Type> {
        Tuple {
            fields: self.types(tuple.fields),
            names: tuple.names.clone(),
        }
    }

    fn tuple_value(&mut self, tuple: Tuple<Value>) -> Tuple<Value>
    {
        Tuple {
            fields: self.values(tuple.fields),
            names: tuple.names.clone(),
        }
    }

    fn type_(&mut self, ty: Type) -> Type {
        use self::Type::*;

        match ty {
            Builtin(b) => Builtin(b),
            Enum(e, p, _) => Enum(
                e.clone(),
                self.path(p),
                self.register_item(e.prototype.name),
            ),
            Rec(r, p, _) => Rec(
                r.clone(),
                self.path(p),
                self.register_item(r.prototype.name),
            ),
            Tuple(t, _) => Tuple(self.tuple_type(t), self.next_item()),
            Unresolved(i, p, _)
                => Unresolved(i, self.path(p), self.next_item()),
            UnresolvedEnum(e, p, _)
                => UnresolvedEnum(e, self.path(p), self.next_item()),
            UnresolvedRec(r, p, _)
                => UnresolvedRec(r, self.path(p), self.next_item()),
        }
    }

    fn types(&mut self, types: DynArray<Type>) -> DynArray<Type> {
        let array = DynArray::with_capacity(types.len());
        for t in types.get_array() {
            array.push(self.type_(t));
        }
        array
    }

    fn value(&mut self, value: Value) -> Value {
        let expr = self.expr(value.expr);
        Value {
            type_: self.type_(value.type_),
            range: value.range,
            expr: expr,
            gvn: self.next_value(),
        }
    }

    fn value_ptr(&mut self, value: Ptr<Value>) -> Ptr<Value> {
        value.update(|v| self.value(v));
        value
    }

    fn values(&mut self, values: DynArray<Value>) -> DynArray<Value> {
        let array = DynArray::with_capacity(values.len());
        for v in values.get_array() {
            array.push(self.value(v));
        }
        array
    }

    //
    //  Support
    //
    fn lookup_value(&self, id: ValueIdentifier) -> Gvn {
        self.values.get(&id.span()).cloned().expect("Known identifier!")
    }

    fn next_item(&mut self) -> Gin { (self.gin)() }

    fn next_value(&mut self) -> Gvn { (self.gvn)() }

    fn register_item(&mut self, id: ItemIdentifier) -> Gin {
        if let Some(&gin) = self.items.get(&id.span()) {
            gin
        } else {
            let gin = self.next_item();
            self.items.insert(id.span(), gin);
            gin
        }
    }

    fn register_value(&mut self, id: ValueIdentifier) -> Gvn {
        if let Some(&gvn) = self.values.get(&id.span()) {
            gvn
        } else {
            let gvn = self.next_value();
            self.values.insert(id.span(), gvn);
            gvn
        }
    }

}

#[cfg(test)]
mod tests {
    use model::hir::*;
    use model::hir::builder::*;

    #[test]
    fn brush() {
        let v = Factory::new().value();

        //  "1 + 2"
        let before = v.call().push(v.int(1, 0)).push(v.int(2, 4)).build();
        let after =
            v.call()
                .push(v.int(1, 0).with_gvn(1))
                .push(v.int(2, 4).with_gvn(2))
                .build()
                .with_gin(5)
                .with_gvn(3);

        assert_eq!(valueit(before), after);
    }

    #[test]
    fn block_simple() {
        let (_, p, _, s, _, v) = factories();

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

        assert_eq!(valueit(before), after);
    }

    fn factories() -> (
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

    fn valueit(value: Value) -> Value {
        use super::GlobalNumberer;

        GlobalNumberer::new().number_value(value)
    }
}
