//! Interning.
//!
//! Test facilities to remove the InternId.

use std::rc;

use basic::com::{self, Span};
use basic::mem::{self, DynArray};

use model::hir::*;

/// Resolver
pub struct Resolver<'g> {
    source: &'g [u8],
    interner: rc::Rc<mem::Interner>,
}

//
//  Public interface of Resolver
//

impl<'g> Resolver<'g> {
    /// Creates an instance.
    pub fn new(
        source: &'g [u8],
        interner: rc::Rc<mem::Interner>,
    )
        -> Self
    {
        Resolver { source, interner }
    }

    /// Returns reference to Interner.
    pub fn interner(&self) -> &mem::Interner { &*self.interner }

    /// Resolves InternId, recursively.
    pub fn resolve_item_id(&self, i: ItemIdentifier) -> ItemIdentifier {
        i.with_id(self.from_range(i.span()))
    }

    /// Resolves InternId, recursively.
    pub fn resolve_value_id(&self, v: ValueIdentifier) -> ValueIdentifier {
        v.with_id(self.from_range(v.span()))
    }

    /// Resolves InternId, recursively.
    pub fn resolve_enum(&self, e: Enum) -> Enum {
        Enum {
            prototype: self.resolve_enum_prototype(e.prototype),
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
    pub fn resolve_function(&self, mut f: Function) -> Function {
        self.resolve_tree(&mut f.body);
        Function {
            prototype: self.resolve_function_prototype(f.prototype),
            body: f.body,
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_function_prototype(&self, f: FunctionProto) -> FunctionProto {
        FunctionProto {
            name: self.resolve_item_id(f.name),
            range: f.range,
            arguments: self.resolve_array(f.arguments, |a| self.resolve_argument(a)),
            result: self.resolve_type_definition(f.result),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_item(&self, i: Item) -> Item {
        use self::Item::*;

        match i {
            Enum(e) => Enum(self.resolve_enum(e)),
            Fun(f) => Fun(self.resolve_function(f)),
            Rec(r) => Rec(self.resolve_record(r)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_prototype(&self, p: Prototype) -> Prototype {
        use self::Prototype::*;

        match p {
            Enum(e) => Enum(self.resolve_enum_prototype(e)),
            Fun(f) => Fun(self.resolve_function_prototype(f)),
            Rec(r) => Rec(self.resolve_record_prototype(r)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_record(&self, r: Record) -> Record {
        Record {
            prototype: self.resolve_record_prototype(r.prototype),
            definition: self.resolve_dyn_tuple_type(r.definition),
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
    pub fn resolve_type_definition(&self, t: TypeDefinition) -> TypeDefinition {
        use self::TypeDefinition::*;

        match t {
            Builtin(b) => Builtin(b),
            Enum(e, p)
                => Enum(self.resolve_enum(e), self.resolve_path(p)),
            Rec(r, p)
                => Rec(self.resolve_record(r), self.resolve_path(p)),
            Tuple(t) => Tuple(self.resolve_dyn_tuple_type(t)),
            Unresolved(i, p)
                => Unresolved(self.resolve_item_id(i), self.resolve_path(p)),
            UnresolvedEnum(e, p)
                => UnresolvedEnum(self.resolve_enum_prototype(e), self.resolve_path(p)),
            UnresolvedRec(r, p)
                => UnresolvedRec(self.resolve_record_prototype(r), self.resolve_path(p)),
        }
    }

    /// Resolves InternId, recursively.
    pub fn resolve_type(&self, mut t: Type) -> Type {
        self.resolve_type_ref(&mut t);
        t
    }

    /// Resolves InternId, recursively.
    pub fn resolve_tree(&self, tree: &mut Tree) {
        self.resolve_root(tree);

        self.resolve_expression_handles(tree);
        self.resolve_pattern_handles(tree);

        self.resolve_callables(tree);
        self.resolve_names(tree);
        self.resolve_paths(tree);
        self.resolve_types(tree);
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
//  Implementation details of Resolver
//

impl<'g> Resolver<'g> {
    fn resolve_argument(&self, a: Argument) -> Argument {
        let name = self.resolve_value_id(a.name);
        let type_ = self.resolve_type_definition(a.type_);
        let range = a.range;

        Argument { name, type_, range }
    }

    fn resolve_callable(&self, callable: &mut Callable) {
        use self::Callable::*;

        match callable {
            Function(name, ..) => self.resolve_item_name(name),
            Unknown(name) => self.resolve_value_name(name),
            _ => (),
        }
    }

    fn resolve_callables(&self, tree: &mut Tree) {
        for callable in tree.iter_callables_mut() {
            self.resolve_callable(callable);
        }
    }

    fn resolve_expression(&self, expr: &mut Expr) {
        use self::Expr::*;

        match expr {
            Call(callable, ..) => self.resolve_callable(callable),
            FieldAccess(_, field) => self.resolve_field(field),
            Implicit(implicit) => self.resolve_implicit(implicit),
            Ref(name, _) | UnresolvedRef(name) => self.resolve_value_name(name),
            _ => (),
        }
    }

    fn resolve_expression_handles(&self, tree: &mut Tree) {
        for handle in tree.iter_expression_handles_mut() {
            self.resolve_type_ref(handle.typ);
            self.resolve_expression(handle.expr);
        }
    }

    fn resolve_field(&self, field: &mut Field) {
        use self::Field::*;

        match field {
            Unresolved(name) => self.resolve_value_name(name),
            _ => (),
        }
    }

    fn resolve_implicit(&self, implicit: &mut Implicit) {
        use self::Implicit::*;

        match implicit {
            ToEnum(name, _) => self.resolve_item_name(name),
        }
    }

    fn resolve_item_name(&self, i: &mut ItemIdentifier) {
        *i = self.resolve_item_id(i.clone());
    }

    fn resolve_names(&self, tree: &mut Tree) {
        for name in tree.iter_names_mut() {
            self.resolve_value_name(name);
        }
    }

    fn resolve_path(&self, p: Path) -> Path {
        Path {
            components: self.resolve_array(p.components, |c| self.resolve_type_definition(c)),
        }
    }

    fn resolve_paths(&self, tree: &mut Tree) {
        for component in tree.iter_path_mut() {
            self.resolve_item_name(component);
        }
    }

    fn resolve_pattern(&self, pattern: &mut Pattern) {
        use self::Pattern::*;

        match pattern {
            Var(name) => self.resolve_value_name(name),
            _ => (),
        }
    }

    fn resolve_pattern_handles(&self, tree: &mut Tree) {
        for handle in tree.iter_pattern_handles_mut() {
            self.resolve_type_ref(handle.typ);
            self.resolve_pattern(handle.pattern);
        }
    }

    fn resolve_root(&self, tree: &mut Tree) {
        match tree.get_root_mut() {
            Some(Root::Function(name, _, _, _)) =>
                self.resolve_item_name(name),
            _ => (),
        }
    }

    fn resolve_type_ref(&self, typ: &mut Type) {
        use self::Type::*;

        match typ {
            Enum(name, ..) | Rec(name, ..) | Unresolved(name, ..)
                => self.resolve_item_name(name),
            _ => (),
        }
    }

    fn resolve_types(&self, tree: &mut Tree) {
        for typ in tree.iter_types_mut() {
            self.resolve_type_ref(typ);
        }
    }

    fn resolve_dyn_tuple_type(&self, t: DynTuple<TypeDefinition>) -> DynTuple<TypeDefinition> {
        self.resolve_dyn_tuple_impl(t, |t| self.resolve_type_definition(t))
    }

    fn resolve_value_name(&self, v: &mut ValueIdentifier) {
        *v = self.resolve_value_id(v.clone());
    }

    fn resolve_array<T: Default, F: Fn(T) -> T>(
        &self,
        array: DynArray<T>,
        resolver: F,
    )
        -> DynArray<T>
    {
        array.update_all(|e| resolver(e));
        array
    }

    fn resolve_dyn_tuple_impl<T: Default, F: Fn(T) -> T>(
        &self,
        c: DynTuple<T>,
        resolver: F,
    )
        -> DynTuple<T>
    {
        DynTuple {
            fields: self.resolve_array(c.fields, resolver),
            names: self.resolve_array(c.names, |v| self.resolve_value_id(v)),
        }
    }
}
