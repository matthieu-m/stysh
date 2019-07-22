//! Semantic passes, aka name resolution, type checking, ...
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use std::cell;

use basic::com::{Range, Span};

use model::{ast, hir};
use model::hir::Registry;

use super::Reg;
use super::{nef, tup, Context};
use super::sym::{self, scp};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'a> {
    scope: &'a scp::Scope,
    repository: &'a hir::RepositorySnapshot,
    context: &'a Context,
    ast_module: &'a ast::Module,
    ast_tree: &'a ast::Tree,
    hir_module: &'a cell::RefCell<hir::Module>,
}

impl<'a> GraphBuilder<'a> {
    /// Creates a new instance of the graph builder.
    pub fn new(
        scope: &'a scp::Scope,
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
            Fun(f) => self.function_name(f),
            Rec(r) => self.record_name(r),
        }
    }

    /// Extracts and registers the prototypes of an item.
    pub fn prototype(&mut self, item: ast::Item) -> hir::Prototype {
        use self::ast::Item::*;

        match item {
            Enum(e) => self.enum_prototype(e),
            Fun(fun) => self.fun_prototype(fun),
            Rec(r) => self.rec_prototype(r),
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

        tree.borrow_mut().set_root_expression(expr);

        self.resolve(&tree);

        tree.into_inner()
    }

    /// Translates and registers a full-fledged item.
    pub fn item(&mut self, item: ast::Item)
        -> hir::Item
    {
        use self::ast::Item::*;

        match item {
            Enum(i) => self.enum_item(i),
            Fun(f) => self.fun_item(f),
            Rec(r) => self.rec_item(r),
        }
    }
}

//
//  Implementation Details
//
impl<'a> GraphBuilder<'a> {
    fn enum_name(&mut self, e: ast::EnumId) -> hir::Item {
        let e = self.ast_module.get_enum(e);
        let id = self.hir_module.borrow_mut().push_enum_name(e.name.into());
        hir::Item::Enum(id)
    }

    fn record_name(&mut self, r: ast::RecordId) -> hir::Item {
        let r = self.ast_module.get_record(r);
        let id = self.hir_module.borrow_mut().push_record_name(r.name().into());
        hir::Item::Rec(id)
    }

    fn function_name(&mut self, f: ast::FunctionId) -> hir::Item {
        let f = self.ast_module.get_function(f);
        let id = self.hir_module.borrow_mut().push_function_name(f.name.into());
        hir::Item::Fun(id)
    }

    fn enum_prototype(&mut self, e: ast::EnumId) -> hir::Prototype {
        let e = self.ast_module.get_enum(e);

        let name = e.name.into();
        let range = Range::new(e.keyword as usize, 5).extend(e.name.span());
        let prototype = hir::EnumPrototype { name, range, };

        let id = self.hir_module.borrow().lookup_enum(name)
            .expect("Enum to be registered");
        self.hir_module.borrow_mut().set_enum_prototype(id, prototype);

        hir::Prototype::Enum(prototype)
    }

    fn fun_prototype(&mut self, fun: ast::FunctionId) -> hir::Prototype {
        let fun = self.ast_module.get_function(fun);
        let name = fun.name.into();

        let arguments = {
            let ast_arguments = self.ast_module.get_arguments(fun.arguments);

            let mut fields = Vec::with_capacity(ast_arguments.len());
            let mut names = Vec::with_capacity(ast_arguments.len());

            for a in ast_arguments {
                names.push(hir::ValueIdentifier(a.name.id(), a.name.span()));
                fields.push(self.type_mapper(self.scope, self.ast_module, self.hir_module)
                        .type_of(a.type_));
            }

            let names = self.hir_module.borrow_mut().push_names(names);
            let fields = self.hir_module.borrow_mut().push_type_ids(fields);

            hir::Tuple { fields, names, }
        };

        let result = self.type_mapper(self.scope, self.ast_module, self.hir_module)
            .type_of(fun.result);

        let range = Range::new(
            fun.keyword as usize,
            self.ast_module.get_type_range(fun.result).end_offset() - (fun.keyword as usize)
        );

        let prototype = hir::FunctionPrototype { name, arguments, result, range, };

        let id = self.hir_module.borrow().lookup_function(name)
            .expect("Function to be registered");
        self.hir_module.borrow_mut().set_function_prototype(id, prototype);

        hir::Prototype::Fun(prototype)
    }

    fn rec_prototype(&mut self, r: ast::RecordId) -> hir::Prototype {
        let r = self.ast_module.get_record(r);

        let name = r.name().into();
        let range = r.span();
        let enum_ = None;
        let prototype = hir::RecordPrototype { name, range, enum_, };

        let id = self.hir_module.borrow().lookup_record(name)
            .expect("Record to be registered");
        self.hir_module.borrow_mut().set_record_prototype(id, prototype);

        hir::Prototype::Rec(prototype)
    }

    fn enum_item(&mut self, e: ast::EnumId) -> hir::Item {
        let e = self.ast_module.get_enum(e);
        let id = self.hir_module.borrow().lookup_enum(e.name.into())
            .expect("Enum to be registered");

        let ast_variants = self.ast_module.get_inner_records(e.variants);
        let mut variants = Vec::with_capacity(ast_variants.len());

        for ev in ast_variants {
            use self::ast::InnerRecord::*;

            match *ev {
                Tuple(..) => unimplemented!("InnerRecord::Tuple"),
                Unit(name) => {
                    let name = name.into();
                    let range = ev.span();
                    let enum_ = Some(id);
                    let prototype = hir::RecordPrototype { name, range, enum_, };
                    let record = hir::Record { prototype, definition: hir::Tuple::unit() };

                    let mut module = self.hir_module.borrow_mut();
                    let id = module.push_record_name(name);
                    module.set_record_prototype(id, prototype);
                    module.set_record(id, record);

                    variants.push(id);
                },
                Missing(_) | Unexpected(_) => (),
            }
        }

        let prototype = self.hir_module.borrow().get_enum_prototype(id);
        let variants = self.hir_module.borrow_mut().push_record_ids(variants);

        let enum_ = hir::Enum { prototype, variants };
        self.hir_module.borrow_mut().set_enum(id, enum_);

        hir::Item::Enum(id)
    }

    fn fun_item(&mut self, f: ast::FunctionId) -> hir::Item {
        let function = self.ast_module.get_function(f);
        let id = self.hir_module.borrow().lookup_function(function.name.into())
            .expect("Function to be registered");

        let prototype = self.hir_module.borrow().get_function_prototype(id);
        let body = cell::RefCell::new(hir::Tree::default());

        {
            use std::ops::Deref;

            let module = self.hir_module.borrow();
            let registry = Reg::new(&self.repository, module.deref());

            let ast_tree = self.ast_module.get_function_body(f);
            let ast_arguments = self.ast_module.get_arguments(function.arguments);

            let arguments = {
                let types = registry.get_type_ids(prototype.arguments.fields);
                debug_assert!(ast_arguments.len() == types.len(),
                    "Expecting matching length: {:?} vs {:?}", ast_arguments, types);

                let mut arguments = Vec::with_capacity(ast_arguments.len());
                let mut names = Vec::with_capacity(ast_arguments.len());

                for (&a, &t) in ast_arguments.iter().zip(types) {
                    let ty = registry.get_type(t);
                    let name = a.name.into();

                    let pattern = hir::Pattern::Var(name);
                    let pattern = body.borrow_mut().push_pattern(ty, pattern, name.span());

                    arguments.push(pattern);
                    names.push(name);

                    self.context.insert_value(name, pattern.into());
                }

                let fields = body.borrow_mut().push_patterns(arguments);
                let names = body.borrow_mut().push_names(names);

                hir::Tuple { fields, names }
            };

            let result = prototype.result;

            let scope = self.function_scope(self.scope, ast_arguments);
            let expr = self.symbol_mapper(&scope, &registry, ast_tree, &body)
                .value_of(ast_tree.get_root_function().expect("Function Root").1);

            body.borrow_mut().set_root_function(prototype.name, arguments, result, expr);

            self.resolve(&body);
        }

        let function = hir::Function { prototype, body: body.into_inner() };
        self.hir_module.borrow_mut().set_function(id, function);

        hir::Item::Fun(id)
    }

    fn rec_item(&mut self, r: ast::RecordId)
        -> hir::Item
    {
        use self::ast::InnerRecord::*;

        let r = self.ast_module.get_record(r);
        let id = self.hir_module.borrow().lookup_record(r.name().into())
            .expect("Record to be registered");

        let prototype = self.hir_module.borrow().get_record_prototype(id);
        let definition = match r.inner {
            Missing(_) | Unexpected(_) | Unit(_) => hir::Tuple::unit(),
            Tuple(_, tup) => self.type_mapper(self.scope, self.ast_module, self.hir_module)
                .tuple_of(tup),
        };

        let record = hir::Record { prototype, definition, };
        self.hir_module.borrow_mut().set_record(id, record);

        hir::Item::Rec(id)
    }

    fn resolve(&self, tree: &cell::RefCell<hir::Tree>) {
        use std::ops::Deref;

        let module = self.hir_module.borrow();
        let registry = Reg::new(&self.repository, module.deref());

        {
            let tree = tree.borrow();

            for p in 0..(tree.len_patterns() as u32) {
                let id = hir::PatternId::new(p);
                self.context.push_unfetched(id.into());
                self.context.push_diverging(id.into());
            }

            for e in 0..(tree.len_expressions() as u32) {
                let id = hir::ExpressionId::new(e);
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
    }

    fn function_scope<'b>(
        &'b self,
        parent: &'b scp::Scope,
        patterns: &[ast::Argument],
    )
        -> scp::FunctionScope<'b>
    {
        scp::FunctionScope::new(parent, patterns.iter().map(|&a| a.name.into()))
    }

    fn symbol_mapper<'b>(
        &'b self,
        scope: &'b scp::Scope,
        registry: &'b hir::Registry,
        ast_tree: &'b ast::Tree,
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> sym::SymbolMapper<'b>
    {

        sym::SymbolMapper::new(scope, registry, self.context, ast_tree, tree)
    }

    fn type_mapper<'b, A, H>(
        &'b self,
        scope: &'b scp::Scope,
        ast_store: &'b A,
        hir_store: &'b cell::RefCell<H>,
    )
        -> sym::TypeMapper<'b, A, H>
    {
        sym::TypeMapper::new(scope, ast_store, hir_store)
    }

    fn nested<'b>(
        &'b self,
        registry: &'b hir::Registry,
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> nef::NestedEntityFetcher<'b>
    {
        nef::NestedEntityFetcher::new(self.context, self.scope, registry, tree)
    }

    fn unifier<'b>(
        &'b self,
        registry: &'b hir::Registry,
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
    use basic::{com, mem};
    use model::ast;
    use model::ast::builder::Factory as SynFactory;
    use model::hir;
    use model::hir::builder::{Factory as SemFactory, RcModule, RcTree};
    use super::Context;
    use super::scp::mocks::MockScope;

    #[test]
    fn prototype_enum() {
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
            let hir = env.hir();
            hir.proto().enum_(env.item_id(6, 6)).build();
        }

        assert_eq!(env.proto_of(ast), env.module());
    }

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
            let hir = env.hir();
            let (i, p) = (hir.item(), hir.proto());

            let e = p.enum_(env.item_id(6, 6)).build();
            let enum_id = env.enum_id(e.name);
            let (one, two) = (env.item_id(15, 3), env.item_id(20, 3));

            i.enum_(e)
                .push(i.unit_of_enum(one, enum_id))
                .push(i.unit_of_enum(two, enum_id))
                .build();
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn prototype_rec() {
        let env = Env::new(b":rec Simple;");

        let ast = {
            let ast = env.ast();
            ast.item().record(5, 6).build()
        };
        {
            let hir = env.hir();
            hir.proto().rec(env.item_id(5, 6), 0).range(0, 12).build();
        }

        assert_eq!(env.proto_of(ast), env.module());
    }

    #[test]
    fn item_rec_unit() {
        let env = Env::new(b":rec Simple;");

        let ast = {
            let ast = env.ast();
            ast.item().record(5, 6).build()
        };
        {
            let hir = env.hir();
            let (i, p) = (hir.item(), hir.proto());

            let r = p.rec(env.item_id(5, 6), 0).range(0, 12).build();
            i.rec(r).build();
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
            let (i, p, t) = (hir.item(), hir.proto(), hir.type_module());

            let r = p.rec(env.item_id(5, 3), 0).range(0, 22).build();
            i.rec(r).push(t.int()).push(t.string()).build();
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn prototype_fun() {
        let env = Env::new(b":fun add(a: Int, b: Int) -> Int { a + b }");

        let ast = {
            let ast = env.ast();
            ast.item().function(
                    5,
                    3,
                    ast.type_().simple(28, 3),
                    ast.expr().block(ast.expr().var(34, 5)).build(),
                )
                .push(9, 1, ast.type_().simple(12, 3))
                .push(17, 1, ast.type_().simple(20, 3))
                .build()
        };
        {
            let hir = env.hir();
            let (p, t) = (hir.proto(), hir.type_module());
            p.fun(env.item_id(5, 3), t.int())
                .push(env.var_id(9, 1), t.int())
                .push(env.var_id(17, 1), t.int())
                .range(0, 31)
                .build();
        }

        assert_eq!(env.proto_of(ast), env.module());
    }

    #[test]
    fn item_fun_add() {
        let env = Env::new(b":fun add(a: Int, b: Int) -> Int { a + b }");

        let ast = {
            let f = env.ast();
            let (e, i, t) = (f.expr(), f.item(), f.type_());

            i.function(
                5,
                3,
                t.simple(28, 3),
                e.block(e.bin_op(e.var(34, 1), e.var(38, 1)).build())
                    .build(),
            )
            .push(9, 1, t.simple(12, 3))
            .push(17, 1, t.simple(20, 3))
            .build()
        };

        {
            let hir = env.hir();
            let add = env.item_id(5, 3);
            let (a, b) = (env.var_id(9, 1), env.var_id(17, 1));

            let prototype = {
                let (p, td) = (hir.proto(), hir.type_module());

                p.fun(add, td.int())
                    .push(a, td.int())
                    .push(b, td.int())
                    .range(0, 31)
                    .build()
            };

            let arguments = env.arguments(prototype);

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

            hir.item().fun(prototype, env.function(prototype, arguments, body));
        }

        assert_eq!(env.item_of(ast), env.module());
    }

    #[test]
    fn item_fun_tuple() {
        let env = Env::new(b":fun add() -> (Int, Int) { (1, 2) }");

        let ast = {
            let f = env.ast();
            let (e, i, t) = (f.expr(), f.item(), f.type_());

            i.function(
                5,
                3,
                t.tuple()
                    .push(t.simple(15, 3))
                    .push(t.simple(20, 3))
                    .build(),
                e.block(
                    e.tuple().push(e.int(1, 28)).push(e.int(2, 31)).build()
                ).build(),
            )
                .build()
        };

        {
            let hir = env.hir();
            let add = env.item_id(5, 3);

            let prototype = {
                let (p, td) = (hir.proto(), hir.type_module());

                p.fun(add, td.tuple().push(td.int()).push(td.int()).build())
                    .range(0, 24)
                    .build()
            };

            let arguments = env.arguments(prototype);

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

            hir.item().fun(prototype, env.function(prototype, arguments, body));
        }

        assert_eq!(env.item_of(ast), env.module());
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

        fn enum_id(&self, name: hir::ItemIdentifier) -> hir::EnumId {
            self.module.borrow().lookup_enum(name).expect("Enum to be registered")
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

        fn proto_of<I: convert::Into<ast::Item>>(&self, item: I) -> hir::Module {
            let item: ast::Item = item.into();

            println!("proto_of - {:#?}", item);
            println!();

            let result = {
                let ast_module = self.ast_module.borrow();
                let ast_tree = self.ast_tree.borrow();
                let hir_module = cell::RefCell::default();
                let mut builder = self.builder(&ast_module, &ast_tree, &hir_module);
                builder.name(item);
                builder.prototype(item);
                hir_module.into_inner()
            };

            println!("proto_of - {:#?}", result);
            println!();

            result
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
                builder.prototype(item);
                builder.item(item);
                hir_module.into_inner()
            };

            println!("item_of - {:#?}", result);
            println!();

            result
        }

        fn arguments(&self, prototype: hir::FunctionPrototype) -> hir::Tuple<hir::PatternId> {
            use std::ops::Deref;
            use self::hir::Registry;

            let repository = Default::default();
            let module = self.module.borrow();
            let registry = super::Reg::new(&repository, module.deref());

            let mut body = self.tree.borrow_mut();

            let types = registry.get_type_ids(prototype.arguments.fields);
            let type_names = registry.get_names(prototype.arguments.names);
            debug_assert!(types.len() == types.len(),
                "Expecting matching lengths {:?} vs {:?}", types, type_names);

            let mut arguments = Vec::with_capacity(types.len());
            let mut names = Vec::with_capacity(types.len());

            for (&name, &ty) in type_names.iter().zip(types) {
                let ty = registry.get_type(ty);

                let pattern = hir::Pattern::Var(name);
                let pattern = body.push_pattern(ty, pattern, name.1);

                arguments.push(pattern);
                names.push(name);
            }

            let fields = body.push_patterns(arguments);
            let names = body.push_names(names);

            hir::Tuple { fields, names }
        }

        fn function(
            &self,
            prototype: hir::FunctionPrototype,
            arguments: hir::Tuple<hir::PatternId>,
            expr: hir::ExpressionId
        )
            -> hir::Tree
        {
            let mut body = self.tree.borrow().clone();
            body.set_root_function(prototype.name, arguments, prototype.result, expr);

            println!("function - {:#?}", body);
            println!();

            body
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
