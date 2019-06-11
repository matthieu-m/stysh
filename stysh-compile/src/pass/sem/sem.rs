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
}

impl<'a> GraphBuilder<'a> {
    /// Creates a new instance of the graph builder.
    pub fn new(
        scope: &'a scp::Scope,
        registry: &'a hir::Registry,
        context: &'a Context,
    )
        -> GraphBuilder<'a>
    {
        GraphBuilder { scope, registry, context }
    }

    /// Extracts the prototypes of an item.
    pub fn prototype(&mut self, item: &ast::Item) -> hir::Prototype {
        match *item {
            ast::Item::Enum(e) => self.enum_prototype(e),
            ast::Item::Fun(fun) => self.fun_prototype(fun),
            ast::Item::Rec(r) => self.rec_prototype(r),
        }
    }

    /// Translates a stand-alone expression.
    pub fn expression(&mut self, e: &ast::Expression) -> hir::Tree {
        let tree = cell::RefCell::new(hir::Tree::default());

        let expr = self.symbol_mapper(self.scope, &tree).value_of(e);

        tree.borrow_mut().set_root_expression(expr);

        self.resolve(&tree);

        tree.into_inner()
    }

    /// Translates a full-fledged item.
    pub fn item(&mut self, proto: hir::Prototype, item: &ast::Item)
        -> hir::Item
    {
        use model::ast::Item;
        use model::hir::Prototype::*;

        debug_assert!(
            item.span().offset() == proto.span().offset(),
            "Mismatched item and prototype: {} vs {}",
            item.span(),
            proto.span()
        );

        match (*item, proto) {
            (Item::Enum(i), Enum(p)) => self.enum_item(i, p),
            (Item::Fun(i), Fun(p)) => self.fun_item(i, p),
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
    fn enum_prototype(&mut self, e: ast::Enum) -> hir::Prototype {
        hir::Prototype::Enum(
            hir::EnumProto {
                name: e.name.into(),
                range: e.keyword().span().extend(e.name.span()),
            }
        )
    }

    fn fun_prototype(&mut self, fun: ast::Function) -> hir::Prototype {
        let arguments = mem::DynArray::with_capacity(fun.arguments.len());

        for a in fun.arguments {
            arguments.push(hir::Argument {
                name: hir::ValueIdentifier(a.name.id(), a.name.span()),
                type_: self.type_mapper(self.scope).type_of(&a.type_),
                range: a.span(),
            });
        }

        hir::Prototype::Fun(
            hir::FunctionProto {
                name: hir::ItemIdentifier(fun.name.id(), fun.name.span()),
                range: com::Range::new(
                    fun.keyword as usize,
                    fun.result.span().end_offset() - (fun.keyword as usize)
                ),
                arguments: arguments,
                result: self.type_mapper(self.scope).type_of(&fun.result),
            }
        )
    }

    fn rec_prototype(&mut self, r: ast::Record) -> hir::Prototype {
        hir::Prototype::Rec(hir::RecordProto {
            name: r.name().into(),
            range: r.span(),
            enum_: hir::ItemIdentifier::unresolved(),
        })
    }

    fn enum_item(&mut self, e: ast::Enum, prototype: hir::EnumProto)
        -> hir::Item
    {
        let variants = mem::DynArray::with_capacity(e.variants.len());

        for ev in e.variants {
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

    fn fun_item(&mut self, fun: ast::Function, p: hir::FunctionProto)
        -> hir::Item
    {
        let body = cell::RefCell::new(hir::Tree::default());

        let arguments = {
            let mut arguments = vec!();
            let mut names = vec!();

            for a in &p.arguments {
                let ty = self.symbol_mapper(self.scope, &body).convert_type(&a.type_);
                let pattern = hir::Pattern::Var(a.name);

                let pattern = body.borrow_mut().push_pattern(ty, pattern, a.range);
                arguments.push(pattern);
                names.push(a.name);

                self.context.insert_value(a.name, pattern.into());
                self.context.link_gvns(&[pattern.into()]);
                self.context.push_diverging(pattern.into());
            }

            let arguments = body.borrow_mut().push_patterns(&arguments);
            let names = body.borrow_mut().push_names(&names);

            hir::Tuple { fields: arguments, names }
        };

        let result = body.borrow_mut().push_type_definition(&p.result);

        let scope = self.function_scope(self.scope, p.clone());
        let expr =
            self.symbol_mapper(&scope, &body)
                .value_of(&ast::Expression::Block(&fun.body));

        body.borrow_mut().set_root_function(p.name, arguments, result, expr);

        self.resolve(&body);

        hir::Item::Fun(hir::Function { prototype: p, body: body.into_inner() })
    }

    fn rec_item(&mut self, r: ast::Record, p: hir::RecordProto)
        -> hir::Item
    {
        use self::ast::InnerRecord::*;

        let definition = match r.inner {
            Missing(_) | Unexpected(_) | Unit(_) => hir::DynTuple::unit(),
            Tuple(_, tup) => self.type_mapper(self.scope).tuple_of(&tup),
        };

        hir::Item::Rec(hir::Record {
            prototype: p,
            definition: definition,
        })
    }

    fn resolve(&self, tree: &cell::RefCell<hir::Tree>) {
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
        tree: &'b cell::RefCell<hir::Tree>,
    )
        -> sym::SymbolMapper<'b>
    {
        sym::SymbolMapper::new(scope, self.context, tree)
    }

    fn type_mapper<'b>(&'b self, scope: &'b scp::Scope)
        -> sym::TypeMapper<'b>
    {
        sym::TypeMapper::new(scope)
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
    use std::rc;
    use basic::{com, mem};
    use model::ast;
    use model::ast::builder::Factory as SynFactory;
    use model::hir::{self, *};
    use model::hir::builder::{Factory as SemFactory, RcTree};
    use super::Context;
    use super::scp::mocks::MockScope;

    #[test]
    fn prototype_enum() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":enum Simple { One, Two }", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(Default::default());

        assert_eq!(
            env.proto_of(
                &ast.item()
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
        let global_arena = mem::Arena::new();
        let env = Env::new(b":enum Simple { One, Two }", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(Default::default());
        let (i, p) = (hir.item(), hir.proto());

        let e: EnumProto = p.enum_(env.item_id(6, 6)).build();

        assert_eq!(
            env.item_of(
                Prototype::Enum(e.clone()),
                &ast.item()
                    .enum_(6, 6)
                    .push_unit(15, 3)
                    .push_unit(20, 3)
                    .build(),
            ),
            env.resolve_item(
                i.enum_(e).push(i.unit(15, 3)).push(i.unit(20, 3)).build().into()
            )
        );
    }

    #[test]
    fn prototype_rec() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Simple;", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(Default::default());

        assert_eq!(
            env.proto_of(&ast.item().record(5, 6).build()),
            hir.proto().rec(env.item_id(5, 6), 0).range(0, 12).build()
        );
    }

    #[test]
    fn item_rec_unit() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Simple;", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(Default::default());
        let (i, p) = (hir.item(), hir.proto());

        let r: RecordProto = p.rec(env.item_id(5, 6), 0).range(0, 12).build();

        assert_eq!(
            env.item_of(
                Prototype::Rec(r.clone()),
                &ast.item().record(5, 6).build(),
            ),
            env.resolve_item(i.rec(r).build().into())
        );
    }

    #[test]
    fn item_rec_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Tup(Int, String);", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(Default::default());
        let (i, p, t) = (hir.item(), hir.proto(), hir.type_definition());

        let r: RecordProto = p.rec(env.item_id(5, 3), 0).range(0, 22).build();

        assert_eq!(
            env.item_of(
                Prototype::Rec(r.clone()),
                &ast.item()
                    .record(5, 3)
                    .tuple(
                        ast.type_tuple()
                            .push(ast.type_().simple(9, 3))
                            .push(ast.type_().simple(14, 6))
                            .build()
                    ).build(),
            ),
            env.resolve_item(
                i.rec(r).push(t.int()).push(t.string()).build().into()
            )
        );
    }

    #[test]
    fn prototype_fun() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add(a: Int, b: Int) -> Int { a + b }",
            &global_arena
        );

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(Default::default());
        let (p, t) = (hir.proto(), hir.type_definition());

        assert_eq!(
            env.proto_of(
                &ast.item().function(
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
                .push(env.var_id(17, 1), t.int())
                .range(0, 31)
                .build()
                .into()
        );
    }

    #[test]
    fn item_fun_add() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add(a: Int, b: Int) -> Int { a + b }",
            &global_arena,
        );

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

            v.block(
                v.call()
                    .builtin(hir::BuiltinFunction::Add, t.int())
                    .push(a)
                    .push(b)
                    .build()
            )
                .build_with_type()
        };

        assert_eq!(
            env.item_of(Prototype::Fun(prototype.clone()), &ast),
            env.function(prototype, arguments, result, body)
        );
    }

    #[test]
    fn item_fun_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add() -> (Int, Int) { (1, 2) }",
            &global_arena,
        );

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
            ).build()
        };

        assert_eq!(
            env.item_of(Prototype::Fun(prototype.clone()), &ast),
            env.function(prototype, arguments, result, body)
        );
    }

    struct Env<'g> {
        scope: MockScope,
        registry: hir::mocks::MockRegistry,
        context: Context,
        global_arena: &'g mem::Arena,
        ast_resolver: ast::interning::Resolver<'g>,
        hir_resolver: hir::interning::Resolver<'g>,
        tree: RcTree,
    }

    impl<'g> Env<'g> {
        fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> Env<'g> {
            let interner = rc::Rc::new(mem::Interner::new());
            Env {
                scope: MockScope::new(),
                registry: hir::mocks::MockRegistry::new(),
                context: Context::default(),
                global_arena: arena,
                ast_resolver: ast::interning::Resolver::new(fragment, interner.clone(), arena),
                hir_resolver: hir::interning::Resolver::new(fragment, interner),
                tree: RcTree::default(),
            }
        }

        fn ast(&self) -> SynFactory<'g> { SynFactory::new(self.global_arena) }

        fn hir(&self) -> SemFactory { SemFactory::new(self.tree.clone()) }

        fn item_id(&self, pos: usize, len: usize) -> hir::ItemIdentifier {
            let range = range(pos, len);
            hir::ItemIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn var_id(&self, pos: usize, len: usize) -> hir::ValueIdentifier {
            let range = range(pos, len);
            hir::ValueIdentifier(self.hir_resolver.from_range(range), range)
        }

        fn builder<'a>(&'a self) -> super::GraphBuilder<'a> {
            super::GraphBuilder::new(
                &self.scope,
                &self.registry,
                &self.context,
            )
        }

        fn proto_of(&self, item: &ast::Item) -> Prototype {
            let item = self.ast_resolver.resolve_item(*item);
            println!("proto_of - {:#?}", item);
            println!();

            let result = self.builder().prototype(&item);

            println!("proto_of - {:#?}", result);
            println!();

            result
        }

        fn item_of(&self, proto: Prototype, item: &ast::Item) -> Item {
            let item = self.ast_resolver.resolve_item(*item);
            println!("item_of - {:#?}", item);
            println!();

            let result = self.builder().item(proto, &item);

            println!("item_of - {:#?}", result);
            println!();

            result
        }

        fn arguments(&self, arguments: &[PatternId]) -> Tuple<PatternId> {
            let mut names = vec!();
            for a in arguments {
                let pattern = *self.tree.borrow().get_pattern(*a);
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

        fn resolve_item(&self, item: Item) -> Item {
            self.hir_resolver.resolve_item(item)
        }
    }

    fn range(start: usize, length: usize) -> com::Range {
        com::Range::new(start, length)
    }
}
