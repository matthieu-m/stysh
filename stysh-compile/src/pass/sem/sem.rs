//! Semantic passes, aka name resolution, type checking, ...
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use std::cell;

use crate::basic::com::{Range, Span};

use crate::model::{ast, hir};
use crate::model::hir::Registry;

use super::Reg;
use super::{nef, tup, Context};
use super::sym::{self, scp};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'a> {
    scope: &'a dyn scp::Scope,
    repository: &'a hir::RepositorySnapshot,
    context: &'a Context,
    ast_module: &'a ast::Module,
    ast_tree: &'a ast::Tree,
    hir_module: &'a cell::RefCell<hir::Module>,
}

impl<'a> GraphBuilder<'a> {
    /// Creates a new instance of the graph builder.
    pub fn new(
        scope: &'a dyn scp::Scope,
        repository: &'a hir::RepositorySnapshot,
        context: &'a Context,
        ast_module: &'a ast::Module,
        ast_tree: &'a ast::Tree,
        hir_module: &'a cell::RefCell<hir::Module>,
    )
        -> GraphBuilder<'a>
    {
        GraphBuilder { scope, repository, context, ast_module, ast_tree, hir_module, }
    }

    /// Extracts and registers the name of an item.
    pub fn name(&mut self, item: ast::Item) -> hir::Item {
        use self::ast::Item::*;

        match item {
            Enum(e) => self.enum_name(e),
            Ext(e) => self.extension_name(e),
            Fun(f) => self.function_name(f),
            Imp(i) => self.implementation_name(i),
            Int(i) => self.interface_name(i),
            Rec(r) => self.record_name(r),
        }
    }

    /// Translates and registers a full-fledged item.
    pub fn item(&mut self, item: ast::Item)
        -> hir::Item
    {
        use self::ast::Item::*;

        match item {
            Enum(i) => self.enum_item(i),
            Ext(e) => self.extension_item(e),
            Fun(f) => self.fun_item(f),
            Imp(i) => self.implementation_item(i),
            Int(i) => self.interface_item(i),
            Rec(r) => self.rec_item(r),
        }
    }

    /// Translates a stand-alone expression.
    pub fn expression(&mut self, e: ast::ExpressionId) -> hir::Tree {
        use std::ops::Deref;

        let module = self.hir_module.borrow();
        let registry = Reg::new(&self.repository, module.deref());

        let tree = cell::RefCell::new(hir::Tree::default());

        let expr = self.symbol_mapper(self.scope, &registry, self.ast_tree, &tree)
            .value_of(e);

        tree.borrow_mut().set_root(expr);

        self.resolve(&tree);

        tree.into_inner()
    }

    /// Translates a function body.
    pub fn function(&mut self, f: ast::FunctionId, id: hir::FunctionId) -> hir::Tree {
        let function = self.ast_module.get_function(f);

        let module = self.hir_module.borrow();
        let registry = Reg::new(&self.repository, &*module);

        let signature = registry.get_function(id);
        let body = cell::RefCell::new(hir::Tree::default());

        self.within_scope(signature.scope, |scope| {
            body.borrow_mut().set_function(signature, &registry);

            {
                let body = body.borrow();
                let patterns = body.get_function_arguments().expect("Arguments");
    
                let patterns = body.get_pattern_ids(patterns);
                let names = registry.get_arguments(signature.arguments);
                debug_assert!(patterns.len() == names.len());

                for (&name, &pattern) in names.iter().zip(patterns) {
                    self.context.insert_value(name, pattern.into());
                }
            }

            let ast_tree =
                self.ast_module.get_function_body(f).as_ref().expect("Function Body");
            let ast_arguments = self.ast_module.get_arguments(function.arguments);

            let scope = self.function_scope(scope, ast_arguments);
            let expr = self.symbol_mapper(&scope, &registry, ast_tree, &body)
                .value_of(ast_tree.get_root_function().expect("Function Root").1);

            body.borrow_mut().set_root(expr);

            self.resolve(&body);
        });

        body.into_inner()
    }
}

//
//  Implementation Details
//
impl<'a> GraphBuilder<'a> {
    fn enum_name(&mut self, e: ast::EnumId) -> hir::Item {
        let id = self.hir_module.borrow_mut().push_enum_name(e);
        debug_assert!(hir::EnumId::from(e) == id);

        let e = self.ast_module.get_enum(e);

        for ev in self.ast_module.get_record_ids(e.variants) {
            self.record_name(*ev);
        }

        hir::Item::Enum(id)
    }

    fn extension_name(&mut self, e: ast::ExtensionId) -> hir::Item {
        let id = self.hir_module.borrow_mut().push_extension_name(e);
        debug_assert!(hir::ExtensionId::from(e) == id);

        hir::Item::Ext(id)
    }

    fn function_name(&mut self, f: ast::FunctionId) -> hir::Item {
        let id = self.hir_module.borrow_mut().push_function_name(f);
        debug_assert!(hir::FunctionId::from(f) == id);

        hir::Item::Fun(id)
    }

    fn implementation_name(&mut self, i: ast::ImplementationId) -> hir::Item {
        let id = self.hir_module.borrow_mut().push_implementation_name(i);
        debug_assert!(hir::ImplementationId::from(i) == id);

        hir::Item::Imp(id)
    }

    fn interface_name(&mut self, i: ast::InterfaceId) -> hir::Item {
        let id = self.hir_module.borrow_mut().push_interface_name(i);
        debug_assert!(hir::InterfaceId::from(i) == id);

        hir::Item::Int(id)
    }

    fn record_name(&mut self, r: ast::RecordId) -> hir::Item {
        let id = self.hir_module.borrow_mut().push_record_name(r);
        debug_assert!(hir::RecordId::from(r) == id);

        hir::Item::Rec(id)
    }

    fn enum_item(&mut self, e: ast::EnumId) -> hir::Item {
        let id = e.into();
        let e = self.ast_module.get_enum(e);

        let scope = {
            let mut scope = scp::TypeScope::new(self.scope, hir::Type::Enum(id));
            scope.enable_self();
            scope
        };

        let ast_variants = self.ast_module.get_record_ids(e.variants);
        let mut variants = Vec::with_capacity(ast_variants.len());

        for ev in ast_variants {
            let r = self.ast_module.get_record(*ev);
            let name = r.inner.name().unwrap_or_default();
            let v = self.rec_item_impl(&scope, (*ev).into(), name, r.inner, r.inner.span(), Some(id));
            variants.push(v);
        }

        let name = e.name.into();
        let range = Range::new(e.keyword as usize, 5).extend(e.name.span());
        let variants = self.hir_module.borrow_mut().push_record_ids(variants);

        let enum_ = hir::Enum { name, range, variants, };

        self.hir_module.borrow_mut().set_enum(id, enum_);

        hir::Item::Enum(id)
    }

    fn extension_item(&mut self, e: ast::ExtensionId) -> hir::Item {
        let id = e.into();
        let ext = self.ast_module.get_extension(e);

        let range = ext.span();

        let elaborate_extended =
            self.type_mapper(self.scope, self.ast_module, self.hir_module)
                .type_of(ext.extended);
        let extended = self.simplify(elaborate_extended);

        let ext = hir::Extension { range, extended, elaborate_extended, };
        self.hir_module.borrow_mut().set_extension(id, ext);

        hir::Item::Ext(id)
    }

    fn fun_item(&mut self, f: ast::FunctionId) -> hir::Item {
        let id = f.into();
        let fun = self.ast_module.get_function(f);

        let name = fun.name.into();

        let range = Range::new(
            fun.keyword as usize,
            self.ast_module.get_type_range(fun.result).end_offset() - (fun.keyword as usize)
        );

        let scope = self.ast_module.get_function_scope(f).into();

        let (arguments, argument_types, elaborate_argument_types) =
            self.within_scope(scope, |scope| {
                let ast_arguments = self.ast_module.get_arguments(fun.arguments);

                let mut elaborate_argument_types = Vec::with_capacity(ast_arguments.len());
                let mut argument_types = Vec::with_capacity(ast_arguments.len());
                let mut arguments: Vec<hir::ValueIdentifier> =
                    Vec::with_capacity(ast_arguments.len());

                for a in ast_arguments {
                    let type_ = self
                        .type_mapper(scope, self.ast_module, self.hir_module)
                        .type_of(a.type_);

                    arguments.push(a.name.into());
                    argument_types.push(self.simplify(type_));
                    elaborate_argument_types.push(type_);
                }

                let arguments = self.hir_module.borrow_mut()
                    .push_arguments(arguments);
                let argument_types = self.hir_module.borrow_mut()
                    .push_type_ids(argument_types);
                let elaborate_argument_types = self.hir_module.borrow_mut()
                    .push_elaborate_type_ids(elaborate_argument_types);

                (arguments, argument_types, elaborate_argument_types)
            });

        let elaborate_result = self.within_scope(scope, |scope| {
            self.type_mapper(scope, self.ast_module, self.hir_module)
                .type_of(fun.result)
        });
        let result = self.simplify(elaborate_result);

        let signature = hir::FunctionSignature {
            name,
            range,
            scope,
            arguments,
            argument_types,
            result,
            elaborate_argument_types,
            elaborate_result,
        };

        self.hir_module.borrow_mut().set_function(id, signature);

        hir::Item::Fun(id)
    }

    fn implementation_item(&mut self, i: ast::ImplementationId) -> hir::Item {
        let id = i.into();
        let imp = self.ast_module.get_implementation(i);

        let range = imp.span();

        let elaborate_implemented =
            self.type_mapper(self.scope, self.ast_module, self.hir_module)
                .type_of(imp.implemented);
        let implemented = self.simplify(elaborate_implemented);

        let elaborate_extended =
            self.type_mapper(self.scope, self.ast_module, self.hir_module)
                .type_of(imp.extended);
        let extended = self.simplify(elaborate_extended);

        let implemented = self.hir_module.borrow().get_type(implemented);
        let implemented = if let hir::Type::Int(i) = implemented {
            i
        } else {
            unreachable!("Unknown interface {:?}", implemented);
        };

        let imp = hir::Implementation {
            range,
            implemented,
            extended,
            elaborate_implemented,
            elaborate_extended,
        };
        self.hir_module.borrow_mut().set_implementation(id, imp);

        hir::Item::Imp(id)
    }

    fn interface_item(&mut self, i: ast::InterfaceId) -> hir::Item {
        let id = i.into();
        let int = self.ast_module.get_interface(i);

        let name = int.name.into();
        let range = int.span();

        let int = hir::Interface { name, range, };
        self.hir_module.borrow_mut().set_interface(id, int);

        hir::Item::Int(id)
    }

    fn rec_item(&mut self, r: ast::RecordId) -> hir::Item {
        let id = r.into();
        let r = self.ast_module.get_record(r);

        let scope = {
            let mut scope = scp::TypeScope::new(self.scope, hir::Type::Rec(id));
            scope.enable_self();
            scope
        };

        self.rec_item_impl(&scope, id, r.name(), r.inner, r.span(), None);

        hir::Item::Rec(id)
    }

    fn rec_item_impl(
        &mut self,
        scope: &dyn scp::Scope,
        id: hir::RecordId,
        name: ast::TypeIdentifier,
        record: ast::InnerRecord,
        range: Range,
        enum_: Option<hir::EnumId>,
    )
        -> hir::RecordId
    {
        use self::ast::InnerRecord::*;

        let name = name.into();

        let elaborate_definition = match record {
            Missing(_) | Unexpected(_) | Unit(_) => hir::Tuple::unit(),
            Tuple(_, tup) => 
                self.type_mapper(scope, self.ast_module, self.hir_module)
                    .tuple_of(tup),
        };
        let definition = self.simplify_tuple(elaborate_definition);

        let record = hir::Record { name, range, enum_, definition, elaborate_definition, };
        self.hir_module.borrow_mut().set_record(id, record);

        id
    }

    fn within_scope<F, R>(&self, s: hir::Scope, fun: F) -> R
        where
            F: FnOnce(&dyn scp::Scope) -> R
    {
        match s {
            hir::Scope::Module => fun(self.scope),
            hir::Scope::Ext(ext) => {
                let extended = {
                    let module = self.hir_module.borrow();
                    let registry = Reg::new(&self.repository, &*module);
                    let extended = registry.get_extension(ext).extended;
                    registry.get_type(extended)
                };
                let mut scope = scp::TypeScope::new(self.scope, extended);
                scope.enable_self();
                fun(&scope)
            },
            hir::Scope::Imp(imp) => {
                let extended = {
                    let module = self.hir_module.borrow();
                    let registry = Reg::new(&self.repository, &*module);
                    let extended = registry.get_implementation(imp).extended;
                    registry.get_type(extended)
                };
                let mut scope = scp::TypeScope::new(self.scope, extended);
                scope.enable_self();
                fun(&scope)
            },
            hir::Scope::Int(int) => {
                let mut scope = scp::TypeScope::new(self.scope, hir::Type::Int(int));
                scope.enable_self();
                fun(&scope)
            }
        }
    }

    fn resolve(&self, tree: &cell::RefCell<hir::Tree>) {
        use std::ops::Deref;

        let module = self.hir_module.borrow();
        let registry = Reg::new(&self.repository, module.deref());

        {
            let tree = tree.borrow();

            for id in tree.get_patterns() {
                self.context.link_gvns(&[id.into()]);
                self.context.push_unfetched(id.into());
                self.context.push_diverging(id.into());
            }

            for id in tree.get_expressions() {
                self.context.push_unfetched(id.into());
                self.context.push_diverging(id.into());
            }
        }

        for _ in 0..4 {
            self.context.next_iteration();

            self.nested(&registry, tree).fetch_all();

            self.unifier(&registry, tree).unify_all();

            //  Nothing left to resolve, done!
            if self.context.unfetched() == 0 && self.context.diverging() == 0 { break; }

            //  No progress made, no point in continuing.
            if self.context.fetched() == 0 && self.context.unified() == 0 { break; }
        }

        self.unifier(&registry, tree).finalize();
    }

    fn simplify(&self, t: hir::ElaborateTypeId) -> hir::TypeId {
        self.simplify_impl(&mut self.hir_module.borrow_mut(), t)
    }

    fn simplify_tuple(&self, t: hir::Tuple<hir::ElaborateTypeId>)
        -> hir::Tuple<hir::TypeId>
    {
        self.simplify_tuple_impl(&mut self.hir_module.borrow_mut(), t)
    }

    fn simplify_impl(&self, module: &mut hir::Module, t: hir::ElaborateTypeId)
        -> hir::TypeId
    {
        use self::hir::ElaborateType::*;

        match module.get_elaborate_type(t) {
            Alias(a) => a,
            Builtin(b) => hir::TypeId::from(b),
            Enum(e, ..) => module.push_type(hir::Type::Enum(e)),
            Int(i, ..) => module.push_type(hir::Type::Int(i)),
            Rec(r, ..) => module.push_type(hir::Type::Rec(r)),
            Tuple(t) => {
                let tuple = self.simplify_tuple_impl(module, t);
                module.push_type(hir::Type::Tuple(tuple))
            },
            Unresolved(..) => module.push_type(hir::Type::Unresolved),
        }
    }

    fn simplify_tuple_impl(
        &self,
        module: &mut hir::Module,
        tup: hir::Tuple<hir::ElaborateTypeId>,
    )
        -> hir::Tuple<hir::TypeId>
    {
        if tup.fields == hir::Id::empty() {
            return hir::Tuple::unit();
        }

        //  End borrow early.
        let elaborate: Vec<_> = module.get_elaborate_type_ids(tup.fields)
            .iter()
            .copied()
            .collect();

        //  End borrow early.
        let fields: Vec<_> = elaborate.into_iter()
            .map(|e| self.simplify_impl(module, e))
            .collect();

        let fields = module.push_type_ids(fields);
        hir::Tuple { fields, names: tup.names, }
    }

    fn function_scope<'b>(
        &'b self,
        parent: &'b dyn scp::Scope,
        patterns: &[ast::Argument],
    )
        -> scp::FunctionScope<'b>
    {
        scp::FunctionScope::new(parent, patterns.iter().map(|&a| a.name.into()))
    }

    fn symbol_mapper<'b>(
        &'b self,
        scope: &'b dyn scp::Scope,
        registry: &'b dyn hir::Registry,
        ast_tree: &'b ast::Tree,
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> sym::SymbolMapper<'b>
    {

        sym::SymbolMapper::new(scope, registry, self.context, ast_tree, tree)
    }

    fn type_mapper<'b, A, H>(
        &'b self,
        scope: &'b dyn scp::Scope,
        ast_store: &'b A,
        hir_store: &'b cell::RefCell<H>,
    )
        -> sym::TypeMapper<'b, A, H>
    {
        sym::TypeMapper::new(scope, ast_store, hir_store)
    }

    fn nested<'b>(
        &'b self,
        registry: &'b dyn hir::Registry,
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> nef::NestedEntityFetcher<'b>
    {
        nef::NestedEntityFetcher::new(self.context, self.scope, registry, tree)
    }

    fn unifier<'b>(
        &'b self,
        registry: &'b dyn hir::Registry,
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> tup::TypeUnifier<'b>
    {
        tup::TypeUnifier::new(self.context, registry, tree)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::{cell, convert, rc};
    use crate::basic::{com, mem};
    use crate::model::ast;
    use crate::model::ast::builder::Factory as SynFactory;
    use crate::model::hir;
    use crate::model::hir::builder::{Factory as SemFactory, RcModule, RcTree};
    use super::Context;
    use super::scp::mocks::MockScope;

    #[test]
    fn item_enum() {
        let env = Env::new(b":enum Simple { One, Two }");

        let ast = {
            let ast = env.ast();
            ast.item()
                .enum_(6, 6)
                .push_unit(15, 3)
                .push_unit(20, 3)
                .build()
        };

        {
            let i = env.hir().item();
            let name = env.item_id(6, 6);
            let mut enum_ = i.enum_(name);

            let (one, two) = (env.item_id(15, 3), env.item_id(20, 3));
            let one = i.unit_of_enum(one, enum_.id());
            let two = i.unit_of_enum(two, enum_.id());

            enum_.push(one).push(two).build();
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn item_rec_unit() {
        let env = Env::new(b":rec Simple;");

        let ast = {
            let ast = env.ast();
            ast.item().record(5, 6).build()
        };
        {
            let i = env.hir().item();

            i.rec(env.item_id(5, 6)).range(0, 12).build();
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn item_rec_tuple() {
        let env = Env::new(b":rec Tup(Int, String);");

        let ast = {
            let ast = env.ast();
            let (i, t) = (ast.item(), ast.type_module());
            i.record(5, 3)
                .tuple(
                    t.tuple()
                        .push(t.simple(9, 3))
                        .push(t.simple(14, 6))
                        .build_tuple()
                ).build()
        };
        {
            let hir = env.hir();
            let (i, t, te) = (hir.item(), hir.type_module(), hir.elaborate_type_module());

            i.rec(env.item_id(5, 3))
                .range(0, 22)
                .push(t.int())
                .push_elaborate(te.int())
                .push(t.string())
                .push_elaborate(te.string())
                .build();
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn item_fun() {
        let env = Env::new(b":fun add(a: Int, b: Int) -> Int { a + b }");

        let ast = {
            let ast = env.ast();
            let fun = ast.item().function(
                    5,
                    3,
                    ast.type_module().simple(28, 3),
                )
                .push(9, 1, ast.type_module().simple(12, 3))
                .push(17, 1, ast.type_module().simple(20, 3))
                .build();
            ast.expr().block(ast.expr().var(34, 5)).build_body(fun);
            fun
        };
        {
            let hir = env.hir();
            let (i, t) = (hir.item(), hir.type_module());
            i.fun(env.item_id(5, 3), t.int())
                .push(env.var_id(9, 1), t.int())
                .push(env.var_id(17, 1), t.int())
                .range(0, 31)
                .build();
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn item_fun_add() {
        let env = Env::new(b":fun add(a: Int, b: Int) -> Int { a + b }");

        let ast = {
            let f = env.ast();
            let (e, i, t) = (f.expr(), f.item(), f.type_module());

            let fun = i.function(
                5,
                3,
                t.simple(28, 3),
            )
                .push(9, 1, t.simple(12, 3))
                .push(17, 1, t.simple(20, 3))
                .build();
            e.block(e.bin_op(e.var(34, 1), e.var(38, 1)).build()).build_body(fun);
            fun
        };

        let hir = {
            let hir = env.hir();
            let add = env.item_id(5, 3);
            let (a, b) = (env.var_id(9, 1), env.var_id(17, 1));

            let fun = {
                let (i, td) = (hir.item(), hir.type_module());

                i.fun(add, td.int())
                    .push(a, td.int())
                    .push(b, td.int())
                    .range(0, 31)
                    .build()
            };

            env.arguments(fun);

            let body = {
                let (t, v) = (hir.type_(), hir.value());

                let a = v.name_ref(a, 34).pattern(0).type_(t.int()).build();
                let b = v.name_ref(b, 38).pattern(1).type_(t.int()).build();

                let call = v.call()
                    .builtin(hir::BuiltinFunction::Add, t.int())
                    .push(a)
                    .push(b)
                    .build();

                v.block(call).build_with_type()
            };

            env.body(body)
        };

        assert_eq!(env.function_of(ast), hir);
    }

    #[test]
    fn item_fun_tuple() {
        let env = Env::new(b":fun add() -> (Int, Int) { (1, 2) }");

        let ast = {
            let f = env.ast();
            let (e, i, t) = (f.expr(), f.item(), f.type_module());

            let fun = i.function(
                5,
                3,
                t.tuple()
                    .push(t.simple(15, 3))
                    .push(t.simple(20, 3))
                    .build(),
            )
                .build();
            e.block(
                e.tuple().push(e.int(1, 28)).push(e.int(2, 31)).build()
            ).build_body(fun);
            fun
        };

        let hir = {
            let hir = env.hir();
            let add = env.item_id(5, 3);

            let fun = {
                let (i, td) = (hir.item(), hir.type_module());

                i.fun(add, td.tuple().push(td.int()).push(td.int()).build())
                    .range(0, 24)
                    .build()
            };

            env.arguments(fun);

            let body = {
                let v = hir.value();

                v.block(
                    v.tuple()
                        .push(v.int(1, 28))
                        .push(v.int(2, 31))
                        .range(27, 6)
                        .build()
                ).build_with_type()
            };

            env.body(body)
        };

        assert_eq!(env.function_of(ast), hir);
    }

    struct Env {
        scope: MockScope,
        repository: hir::RepositorySnapshot,
        context: Context,
        ast_resolver: ast::interning::Resolver,
        hir_resolver: hir::interning::Resolver,
        ast_module: ast::builder::RcModule,
        ast_tree: ast::builder::RcTree,
        module: RcModule,
        tree: RcTree,
    }

    impl Env {
        fn new(fragment: &[u8]) -> Env {
            let interner = rc::Rc::new(mem::Interner::new());
            Env {
                scope: MockScope::new(),
                repository: hir::RepositorySnapshot::default(),
                context: Context::default(),
                ast_resolver: ast::interning::Resolver::new(fragment, interner.clone()),
                hir_resolver: hir::interning::Resolver::new(fragment, interner),
                ast_module: ast::builder::RcModule::default(),
                ast_tree: ast::builder::RcTree::default(),
                module: RcModule::default(),
                tree: RcTree::default(),
            }
        }

        fn ast(&self) -> SynFactory { SynFactory::new(self.ast_module.clone(), self.ast_tree.clone(), self.ast_resolver.clone()) }

        fn hir(&self) -> SemFactory { SemFactory::new(self.module.clone(), self.tree.clone()) }

        fn module(&self) -> hir::Module {
            let result = (*self.module).clone().into_inner();

            println!("module - {:#?}", result);
            println!();

            result
        }

        fn item_id(&self, pos: usize, len: usize) -> hir::ItemIdentifier {
            let range = range(pos, len);
            hir::ItemIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn var_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn builder<'a>(
            &'a self,
            ast_module: &'a ast::Module,
            ast_tree: &'a ast::Tree,
            hir_module: &'a cell::RefCell<hir::Module>,
        )
            -> super::GraphBuilder<'a>
        {
            super::GraphBuilder::new(
                &self.scope,
                &self.repository,
                &self.context,
                ast_module,
                ast_tree,
                hir_module,
            )
        }

        fn item_of<I: convert::Into<ast::Item>>(&self, item: I) -> hir::Module {
            let item: ast::Item = item.into();

            println!("item_of - {:#?}", item);
            println!();

            let result = {
                let ast_module = self.ast_module.borrow();
                let ast_tree = self.ast_tree.borrow();
                let hir_module = cell::RefCell::default();
                let mut builder = self.builder(&ast_module, &ast_tree, &hir_module);
                builder.name(item);
                builder.item(item);
                hir_module.into_inner()
            };

            println!("item_of - {:#?}", result);
            println!();

            result
        }

        fn function_of(&self, fun: ast::FunctionId) -> hir::Tree {
            println!("function_of - {:#?}", fun);
            println!();

            let result = {
                let ast_module = self.ast_module.borrow();
                let ast_tree = self.ast_tree.borrow();
                let hir_module = cell::RefCell::default();
                let mut builder = self.builder(&ast_module, &ast_tree, &hir_module);
                if let hir::Item::Fun(id) = builder.name(fun.into()) {
                    builder.item(fun.into());
                    builder.function(fun, id)
                } else {
                    unreachable!()
                }
            };

            println!("function_of - {:#?}", result);
            println!();

            result
        }

        fn arguments(&self, id: hir::FunctionId) {
            use crate::model::hir::Registry;

            let module = self.module.borrow();
            let signature = module.get_function(id);
            self.tree.borrow_mut().set_function(signature, &*module);
        }

        fn body(&self, expr: hir::ExpressionId) -> hir::Tree {
            let mut body = self.tree.borrow().clone();
            body.set_root(expr);

            println!("function - {:#?}", body);
            println!();

            body
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
