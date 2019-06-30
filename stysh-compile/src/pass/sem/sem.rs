//! Semantic passes, aka name resolution, type checking, ...
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use std::cell;

use basic::{com, mem};
use basic::com::Span;

use model::{ast, hir};

use super::{nef, tup, Context};
use super::sym::{self, scp};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'a> {
    scope: &'a scp::Scope,
    registry: &'a hir::Registry,
    context: &'a Context,
    ast_module: &'a ast::Module,
    ast_tree: &'a ast::Tree,
}

impl<'a> GraphBuilder<'a> {
    /// Creates a new instance of the graph builder.
    pub fn new(
        scope: &'a scp::Scope,
        registry: &'a hir::Registry,
        context: &'a Context,
        ast_module: &'a ast::Module,
        ast_tree: &'a ast::Tree,
    )
        -> GraphBuilder<'a>
    {
        GraphBuilder { scope, registry, context, ast_module, ast_tree, }
    }

    /// Extracts the prototypes of an item.
    pub fn prototype(&mut self, item: ast::Item) -> hir::Prototype {
        match item {
            ast::Item::Enum(e) => self.enum_prototype(e),
            ast::Item::Fun(fun) => self.fun_prototype(fun),
            ast::Item::Rec(r) => self.rec_prototype(r),
        }
    }

    /// Translates a stand-alone expression.
    pub fn expression(&mut self, e: ast::ExpressionId) -> hir::Tree {
        let tree = cell::RefCell::new(hir::Tree::default());

        let expr = self.symbol_mapper(self.scope, self.ast_tree, &tree).value_of(e);

        tree.borrow_mut().set_root_expression(expr);

        self.resolve(&tree);

        tree.into_inner()
    }

    /// Translates a full-fledged item.
    pub fn item(&mut self, proto: hir::Prototype, item: ast::Item)
        -> hir::Item
    {
        use model::ast::Item;
        use model::hir::Prototype::*;

        match (item, proto) {
            (Item::Enum(i), Enum(p)) => self.enum_item(i, p),
            (Item::Fun(f), Fun(p)) => self.fun_item(f, p),
            (Item::Rec(r), Rec(p)) => self.rec_item(r, p),
            (Item::Enum(_), p) => panic!("Expected enum {:?}", p),
            (Item::Fun(_), p) => panic!("Expected function {:?}", p),
            (Item::Rec(_), p) => panic!("Expected record {:?}", p),
        }
    }
}

//
//  Implementation Details
//
impl<'a> GraphBuilder<'a> {
    fn enum_prototype(&mut self, e: ast::EnumId) -> hir::Prototype {
        let e = self.ast_module.get_enum(e);

        hir::Prototype::Enum(
            hir::EnumProto {
                name: e.name.into(),
                range: com::Range::new(e.keyword as usize, 5).extend(e.name.span()),
            }
        )
    }

    fn fun_prototype(&mut self, fun: ast::FunctionId) -> hir::Prototype {
        let fun = self.ast_module.get_function(fun);

        let ast_arguments = self.ast_module.get_arguments(fun.arguments);
        let arguments = mem::DynArray::with_capacity(ast_arguments.len());

        for a in ast_arguments {
            arguments.push(hir::Argument {
                name: hir::ValueIdentifier(a.name.id(), a.name.span()),
                type_: self.type_mapper(self.scope, self.ast_module).type_of(a.type_),
                range: a.span(),
            });
        }

        let range = com::Range::new(
            fun.keyword as usize,
            self.ast_module.get_type_range(fun.result).end_offset() - (fun.keyword as usize)
        );

        hir::Prototype::Fun(
            hir::FunctionProto {
                name: hir::ItemIdentifier(fun.name.id(), fun.name.span()),
                range,
                arguments,
                result: self.type_mapper(self.scope, self.ast_module).type_of(fun.result),
            }
        )
    }

    fn rec_prototype(&mut self, r: ast::RecordId) -> hir::Prototype {
        let r = self.ast_module.get_record(r);

        hir::Prototype::Rec(hir::RecordProto {
            name: r.name().into(),
            range: r.span(),
            enum_: hir::ItemIdentifier::unresolved(),
        })
    }

    fn enum_item(&mut self, e: ast::EnumId, prototype: hir::EnumProto)
        -> hir::Item
    {
        let e = self.ast_module.get_enum(e);

        let ast_variants = self.ast_module.get_inner_records(e.variants);
        let variants = mem::DynArray::with_capacity(ast_variants.len());

        for ev in ast_variants {
            use self::ast::InnerRecord::*;

            match *ev {
                Tuple(..) => unimplemented!("InnerRecord::Tuple"),
                Unit(name) => variants.push(hir::Record {
                    prototype: hir::RecordProto {
                        name: name.into(),
                        range: ev.span(),
                        enum_: prototype.name,
                    },
                    definition: hir::DynTuple::unit(),
                }),
                Missing(_) | Unexpected(_) => (),
            }
        }

        hir::Item::Enum(hir::Enum { prototype, variants })
    }

    fn fun_item(&mut self, f: ast::FunctionId, p: hir::FunctionProto)
        -> hir::Item
    {
        let ast_tree = self.ast_module.get_function_body(f);
        let body = cell::RefCell::new(hir::Tree::default());

        let arguments = {
            let mut arguments = vec!();
            let mut names = vec!();

            for a in &p.arguments {
                let ty = self.symbol_mapper(self.scope, ast_tree, &body)
                    .convert_type(&a.type_);
                let pattern = hir::Pattern::Var(a.name);

                let pattern = body.borrow_mut().push_pattern(ty, pattern, a.range);
                arguments.push(pattern);
                names.push(a.name);

                self.context.insert_value(a.name, pattern.into());
            }

            let arguments = body.borrow_mut().push_patterns(&arguments);
            let names = body.borrow_mut().push_names(&names);

            hir::Tuple { fields: arguments, names }
        };

        let result = body.borrow_mut().push_type_definition(&p.result);

        let scope = self.function_scope(self.scope, p.clone());
        let expr = self.symbol_mapper(&scope, ast_tree, &body)
            .value_of(ast_tree.get_root_function().expect("Function Root").1);

        body.borrow_mut().set_root_function(p.name, arguments, result, expr);

        self.resolve(&body);

        hir::Item::Fun(hir::Function { prototype: p, body: body.into_inner() })
    }

    fn rec_item(&mut self, r: ast::RecordId, p: hir::RecordProto)
        -> hir::Item
    {
        use self::ast::InnerRecord::*;

        let r = self.ast_module.get_record(r);

        let definition = match r.inner {
            Missing(_) | Unexpected(_) | Unit(_) => hir::DynTuple::unit(),
            Tuple(_, tup) => self.type_mapper(self.scope, self.ast_module)
                .tuple_of(tup),
        };

        hir::Item::Rec(hir::Record {
            prototype: p,
            definition: definition,
        })
    }

    fn resolve(&self, tree: &cell::RefCell<hir::Tree>) {
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

            self.nested(tree).fetch_all();

            self.unifier(tree).unify_all();

            //  Nothing left to resolve, done!
            if self.context.unfetched() == 0 && self.context.diverging() == 0 { break; }

            //  No progress made, no point in continuing.
            if self.context.fetched() == 0 && self.context.unified() == 0 { break; }
        }
    }

    fn function_scope<'b>(
        &'b self,
        parent: &'b scp::Scope,
        p: hir::FunctionProto,
    )
        -> scp::FunctionScope<'b>
    {
        scp::FunctionScope::new(parent, p)
    }

    fn symbol_mapper<'b>(
        &'b self,
        scope: &'b scp::Scope,
        ast_tree: &'b ast::Tree,
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> sym::SymbolMapper<'b>
    {
        sym::SymbolMapper::new(scope, self.context, ast_tree, tree)
    }

    fn type_mapper<'b, S>(&'b self, scope: &'b scp::Scope, store: &'b S)
        -> sym::TypeMapper<'b, S>
    {
        sym::TypeMapper::new(scope, store)
    }

    fn nested<'b>(&'b self, tree: &'b cell::RefCell<hir::Tree>)
        -> nef::NestedEntityFetcher<'b>
    {
        nef::NestedEntityFetcher::new(self.context, self.registry, tree)
    }

    fn unifier<'b>(&'b self, tree: &'b cell::RefCell<hir::Tree>)
        -> tup::TypeUnifier<'b>
    {
        tup::TypeUnifier::new(self.context, self.registry, tree)
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use std::{convert, rc};
    use basic::{com, mem};
    use model::ast;
    use model::ast::builder::Factory as SynFactory;
    use model::hir::{self, *};
    use model::hir::builder::{Factory as SemFactory, RcTree};
    use super::Context;
    use super::scp::mocks::MockScope;

    #[test]
    fn prototype_enum() {
        let env = Env::new(b":enum Simple { One, Two }");

        let ast = env.ast();
        let hir = env.hir();

        assert_eq!(
            env.proto_of(
                ast.item()
                    .enum_(6, 6)
                    .push_unit(15, 3)
                    .push_unit(20, 3)
                    .build(),
            ),
            hir.proto().enum_(env.item_id(6, 6)).build()
        );
    }

    #[test]
    fn item_enum() {
        let env = Env::new(b":enum Simple { One, Two }");

        let ast = env.ast();
        let hir = env.hir();
        let (i, p) = (hir.item(), hir.proto());

        let e: EnumProto = p.enum_(env.item_id(6, 6)).build();
        let (one, two) = (env.item_id(15, 3), env.item_id(20, 3));

        assert_eq!(
            env.item_of(
                Prototype::Enum(e),
                ast.item()
                    .enum_(6, 6)
                    .push_unit(15, 3)
                    .push_unit(20, 3)
                    .build(),
            ),
            i.enum_(e).push(i.unit(one)).push(i.unit(two)).build().into()
        );
    }

    #[test]
    fn prototype_rec() {
        let env = Env::new(b":rec Simple;");

        let ast = env.ast();
        let hir = env.hir();

        assert_eq!(
            env.proto_of(ast.item().record(5, 6).build()),
            hir.proto().rec(env.item_id(5, 6), 0).range(0, 12).build()
        );
    }

    #[test]
    fn item_rec_unit() {
        let env = Env::new(b":rec Simple;");

        let ast = env.ast();
        let hir = env.hir();
        let (i, p) = (hir.item(), hir.proto());

        let r: RecordProto = p.rec(env.item_id(5, 6), 0).range(0, 12).build();

        assert_eq!(
            env.item_of(
                Prototype::Rec(r.clone()),
                ast.item().record(5, 6).build(),
            ),
            i.rec(r).build().into()
        );
    }

    #[test]
    fn item_rec_tuple() {
        let env = Env::new(b":rec Tup(Int, String);");

        let ast = env.ast();
        let hir = env.hir();
        let (i, p, t) = (hir.item(), hir.proto(), hir.type_definition());

        let r: RecordProto = p.rec(env.item_id(5, 3), 0).range(0, 22).build();

        assert_eq!(
            env.item_of(
                Prototype::Rec(r.clone()),
                ast.item()
                    .record(5, 3)
                    .tuple(
                        ast.type_module().tuple()
                            .push(ast.type_module().simple(9, 3))
                            .push(ast.type_module().simple(14, 6))
                            .build_tuple()
                    ).build(),
            ),
            i.rec(r).push(t.int()).push(t.string()).build().into()
        );
    }

    #[test]
    fn prototype_fun() {
        let env = Env::new(b":fun add(a: Int, b: Int) -> Int { a + b }");

        let ast = env.ast();
        let hir = env.hir();
        let (p, t) = (hir.proto(), hir.type_definition());

        assert_eq!(
            env.proto_of(
                ast.item().function(
                    5,
                    3,
                    ast.type_().simple(28, 3),
                    ast.expr().block(ast.expr().var(34, 5)).build(),
                )
                .push(9, 1, ast.type_().simple(12, 3))
                .push(17, 1, ast.type_().simple(20, 3))
                .build()
            ),
            p.fun(env.item_id(5, 3), t.int())
                .push(env.var_id(9, 1), t.int())
                .arg_range(9, 7)
                .push(env.var_id(17, 1), t.int())
                .range(0, 31)
                .build()
                .into()
        );
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
            .push(17, 1, t.simple(17, 1))
            .build()
        };

        let add = env.item_id(5, 3);
        let (a, b) = (env.var_id(9, 1), env.var_id(17, 1));

        let prototype = {
            let f = env.hir();
            let (p, td) = (f.proto(), f.type_definition());

            p.fun(add, td.int())
                .push(a, td.int())
                .push(b, td.int())
                .range(0, 31)
                .build()
        };

        let arguments = {
            let f = env.hir();
            let (p, t) = (f.pat(), f.type_());

            let a = p.simple().var(a).range(9, 6).type_(t.int()).build();
            let b = p.simple().var(b).range(17, 6).type_(t.int()).build();

            env.arguments(&[a, b])
        };

        let result = env.result(env.hir().type_().int());

        let body = {
            let f = env.hir();
            let (t, v) = (f.type_(), f.value());

            let a = v.name_ref(a, 34).pattern(0).type_(t.int()).build();
            let b = v.name_ref(b, 38).pattern(1).type_(t.int()).build();

            let call = v.call()
                .builtin(hir::BuiltinFunction::Add, t.int())
                .push(a)
                .push(b)
                .build();

            v.block(call).build_with_type()
        };

        assert_eq!(
            env.item_of(Prototype::Fun(prototype.clone()), ast),
            env.function(prototype, arguments, result, body)
        );
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

        let prototype = {
            let f = env.hir();
            let (p, td) = (f.proto(), f.type_definition());

            let add = env.item_id(5, 3);

            p.fun(add, td.tuple().push(td.int()).push(td.int()).build())
                .range(0, 24)
                .build()
        };

        let arguments = Tuple::unit();

        let result = {
            let f = env.hir();
            let (t, ti) = (f.type_(), f.type_id());
            ti.tuple().push(t.int()).push(t.int()).build()
        };

        let body = {
            let v = env.hir().value();

            v.block(
                v.tuple()
                    .push(v.int(1, 28))
                    .push(v.int(2, 31))
                    .range(27, 6)
                    .build()
            ).build_with_type()
        };

        assert_eq!(
            env.item_of(Prototype::Fun(prototype.clone()), ast),
            env.function(prototype, arguments, result, body)
        );
    }

    struct Env {
        scope: MockScope,
        registry: hir::mocks::MockRegistry,
        context: Context,
        ast_resolver: ast::interning::Resolver,
        hir_resolver: hir::interning::Resolver,
        ast_module: ast::builder::RcModule,
        ast_tree: ast::builder::RcTree,
        tree: RcTree,
    }

    impl Env {
        fn new(fragment: &[u8]) -> Env {
            let interner = rc::Rc::new(mem::Interner::new());
            Env {
                scope: MockScope::new(),
                registry: hir::mocks::MockRegistry::new(),
                context: Context::default(),
                ast_resolver: ast::interning::Resolver::new(fragment, interner.clone()),
                hir_resolver: hir::interning::Resolver::new(fragment, interner),
                ast_module: ast::builder::RcModule::default(),
                ast_tree: ast::builder::RcTree::default(),
                tree: RcTree::default(),
            }
        }

        fn ast(&self) -> SynFactory { SynFactory::new(self.ast_module.clone(), self.ast_tree.clone(), self.ast_resolver.clone()) }

        fn hir(&self) -> SemFactory { SemFactory::new(self.tree.clone()) }

        fn item_id(&self, pos: usize, len: usize) -> hir::ItemIdentifier {
            let range = range(pos, len);
            hir::ItemIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn var_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn builder<'a>(&'a self, ast_module: &'a ast::Module, ast_tree: &'a ast::Tree) -> super::GraphBuilder<'a> {
            super::GraphBuilder::new(
                &self.scope,
                &self.registry,
                &self.context,
                ast_module,
                ast_tree,
            )
        }

        fn proto_of<I: convert::Into<ast::Item>>(&self, item: I) -> Prototype {
            let item: ast::Item = item.into();

            println!("proto_of - {:#?}", item);
            println!();

            let ast_module = self.ast_module.borrow();
            let ast_tree = self.ast_tree.borrow();
            let result = self.builder(&ast_module, &ast_tree).prototype(item);

            println!("proto_of - {:#?}", result);
            println!();

            result
        }

        fn item_of<I: convert::Into<ast::Item>>(&self, proto: Prototype, item: I) -> Item {
            let item: ast::Item = item.into();

            println!("item_of - {:#?}", item);
            println!();

            let ast_module = self.ast_module.borrow();
            let ast_tree = self.ast_tree.borrow();
            let result = self.builder(&ast_module, &ast_tree).item(proto, item);

            println!("item_of - {:#?}", result);
            println!();

            result
        }

        fn arguments(&self, arguments: &[PatternId]) -> Tuple<PatternId> {
            let mut names = vec!();
            for a in arguments {
                let pattern = self.tree.borrow().get_pattern(*a);
                if let Pattern::Var(name) = pattern {
                    names.push(name);
                } else {
                    unimplemented!();
                }
            }
            let fields = self.tree.borrow_mut().push_patterns(arguments);
            let names = self.tree.borrow_mut().push_names(&names);
            Tuple { fields, names, }
        }

        fn result(&self, typ: Type) -> TypeId {
            self.tree.borrow_mut().push_type(typ)
        }

        fn function(
            &self,
            prototype: FunctionProto,
            arguments: Tuple<PatternId>,
            result: TypeId,
            expr: ExpressionId
        )
            -> Item
        {
            let mut body = self.tree.borrow().clone();
            body.set_root_function(prototype.name, arguments, result, expr);

            println!("function - {:#?}", body);
            println!();

            hir::Item::Fun(hir::Function { prototype, body })
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
