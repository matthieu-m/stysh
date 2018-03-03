//! Semantic passes, aka name resolution, type checking, ...
//!
//! Let's start simple here. It'll get MUCH more complicated later.

use basic::{com, mem};
use basic::com::Span;

use model::{ast, hir};

use super::nmr::{self, scp};

/// The Stysh ASG builder.
///
/// Builds the Abstract Semantic Graph.
pub struct GraphBuilder<'a, 'g, 'local>
    where 'g: 'a
{
    code_fragment: com::CodeFragment,
    scope: &'a scp::Scope<'g>,
    registry: &'a hir::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &'local mem::Arena,
}

impl<'a, 'g, 'local> GraphBuilder<'a, 'g, 'local>
    where 'g: 'a
{
    /// Creates a new instance of the graph builder.
    ///
    /// The global arena sets the lifetime of the returned objects, while the
    /// local arena is used as a scratch buffer and can be reset immediately.
    pub fn new(
        source: com::CodeFragment,
        scope: &'a scp::Scope<'g>,
        registry: &'a hir::Registry<'g>,
        global: &'g mem::Arena,
        local: &'local mem::Arena
    )
        -> GraphBuilder<'a, 'g, 'local>
    {
        GraphBuilder {
            code_fragment: source,
            scope: scope,
            registry: registry,
            global_arena: global,
            local_arena: local
        }
    }

    /// Extracts the prototypes of an item.
    pub fn prototype(&mut self, item: &ast::Item) -> hir::Prototype<'g> {
        match *item {
            ast::Item::Enum(e) => self.enum_prototype(e),
            ast::Item::Fun(fun) => self.fun_prototype(fun),
            ast::Item::Rec(r) => self.rec_prototype(r),
        }
    }

    /// Translates a stand-alone expression.
    pub fn expression(&mut self, e: &ast::Expression) -> hir::Value<'g> {
        self.resolver(self.scope).value_of(e)
    }

    /// Translates a full-fledged item.
    pub fn item(&mut self, proto: &'g hir::Prototype<'g>, item: &ast::Item)
        -> hir::Item<'g>
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
            (Item::Enum(i), &Enum(ref p)) => self.enum_item(i, p),
            (Item::Fun(i), &Fun(ref p)) => self.fun_item(i, p),
            (Item::Rec(r), &Rec(ref p)) => self.rec_item(r, p),
            (Item::Enum(_), &p) => panic!("Expected enum {:?}", p),
            (Item::Fun(_), &p) => panic!("Expected function {:?}", p),
            (Item::Rec(_), &p) => panic!("Expected record {:?}", p),
        }
    }
}

//
//  Implementation Details
//
impl<'a, 'g, 'local> GraphBuilder<'a, 'g, 'local>
    where 'g: 'a
{
    fn enum_prototype(&mut self, e: ast::Enum) -> hir::Prototype<'g> {
        hir::Prototype::Enum(
            hir::EnumProto {
                name: e.name.into(),
                range: e.keyword().span().extend(e.name.span()),
            }
        )
    }

    fn fun_prototype(&mut self, fun: ast::Function) -> hir::Prototype<'g> {
        let mut arguments =
            mem::Array::with_capacity(fun.arguments.len(), self.global_arena);

        for a in fun.arguments {
            arguments.push(hir::Binding::Argument(
                hir::ValueIdentifier(a.name.0),
                Default::default(),
                self.resolver(self.scope).type_of(&a.type_),
                a.span()
            ));
        }

        hir::Prototype::Fun(
            hir::FunctionProto {
                name: hir::ItemIdentifier(fun.name.0),
                range: com::Range::new(
                    fun.keyword as usize,
                    fun.result.span().end_offset() - (fun.keyword as usize)
                ),
                arguments: arguments.into_slice(),
                result: self.resolver(self.scope).type_of(&fun.result),
            }
        )
    }

    fn rec_prototype(&mut self, r: ast::Record) -> hir::Prototype<'g> {
        hir::Prototype::Rec(hir::RecordProto {
            name: r.name().into(),
            range: r.span(),
            enum_: hir::ItemIdentifier::unresolved(),
        })
    }

    fn enum_item(&mut self, e: ast::Enum, p: &'g hir::EnumProto)
        -> hir::Item<'g>
    {
        let mut variants =
            mem::Array::with_capacity(e.variants.len(), self.global_arena);

        for ev in e.variants {
            use self::ast::InnerRecord::*;

            match *ev {
                Tuple(..) => unimplemented!("InnerRecord::Tuple"),
                Unit(name) => variants.push(hir::Record {
                    prototype: self.global_arena.insert(hir::RecordProto {
                        name: name.into(),
                        range: ev.span(),
                        enum_: p.name.into()
                    }),
                    fields: &[],
                }),
                Missing(_) | Unexpected(_) => (),
            }
        }

        hir::Item::Enum(hir::Enum {
            prototype: p,
            variants: variants.into_slice(),
        })
    }

    fn fun_item(&mut self, fun: ast::Function, p: &'g hir::FunctionProto<'g>)
        -> hir::Item<'g>
    {
        let scope = self.function_scope(self.scope, p);

        hir::Item::Fun(hir::Function {
            prototype: p,
            body:
                self.resolver(&scope)
                    .value_of(&ast::Expression::Block(&fun.body))
        })
    }

    fn rec_item(&mut self, r: ast::Record, p: &'g hir::RecordProto)
        -> hir::Item<'g>
    {
        use self::ast::InnerRecord::*;

        let fields: &'g [hir::Type<'g>] = match r.inner {
            Missing(_) | Unexpected(_) | Unit(_) => &[],
            Tuple(_, tup) => {
                let mut fields =
                    mem::Array::with_capacity(tup.len(), self.global_arena);
                
                for f in tup.fields {
                    fields.push(self.resolver(self.scope).type_of(f));
                }

                fields.into_slice()
            },
        };

        hir::Item::Rec(hir::Record { prototype: p, fields: fields })
    }

    fn function_scope<'b>(
        &'b self,
        parent: &'b scp::Scope<'g>,
        p: &'b hir::FunctionProto<'b>,
    )
        -> scp::FunctionScope<'b, 'g>
    {
        scp::FunctionScope::new(
            &*self.code_fragment,
            parent,
            p,
            self.global_arena,
            self.local_arena,
        )
    }

    fn resolver<'b>(&'b self, scope: &'b scp::Scope<'g>)
        -> nmr::NameResolver<'b, 'g, 'local>
    {
        nmr::NameResolver::new(
            &*self.code_fragment,
            scope,
            self.registry,
            self.global_arena,
            self.local_arena
        )
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::ast;
    use model::ast::builder::Factory as SynFactory;
    use model::hir::builder::Factory as SemFactory;
    use model::hir::*;
    use model::hir::mocks::MockRegistry;
    use super::scp::mocks::MockScope;

    #[test]
    fn prototype_enum() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":enum Simple { One, Two }", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(&global_arena);
        let (i, p) = (hir.item(), hir.proto());

        assert_eq!(
            env.proto_of(
                &ast.item()
                    .enum_(6, 6)
                    .push_unit(15, 3)
                    .push_unit(20, 3)
                    .build(),
            ),
            p.enum_(i.id(6, 6)).build()
        );
    }

    #[test]
    fn item_enum() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":enum Simple { One, Two }", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(&global_arena);
        let (i, p) = (hir.item(), hir.proto());

        let e = p.enum_(i.id(6, 6)).build();

        assert_eq!(
            env.item_of(
                &Prototype::Enum(e),
                &ast.item()
                    .enum_(6, 6)
                    .push_unit(15, 3)
                    .push_unit(20, 3)
                    .build(),
            ),
            i.enum_(e).push(i.unit(15, 3)).push(i.unit(20, 3)).build().into()
        );
    }

    #[test]
    fn prototype_rec() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Simple;", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(&global_arena);

        assert_eq!(
            env.proto_of(&ast.item().record(5, 6).build()),
            hir.proto().rec(hir.item().id(5, 6), 0).range(0, 12).build()
        );
    }

    #[test]
    fn item_rec_unit() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Simple;", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(&global_arena);
        let (i, p) = (hir.item(), hir.proto());

        let r = p.rec(i.id(5, 6), 0).range(0, 12).build();

        assert_eq!(
            env.item_of(
                &Prototype::Rec(r),
                &ast.item().record(5, 6).build(),
            ),
            i.rec(r).build().into()
        );
    }

    #[test]
    fn item_rec_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(b":rec Tup(Int, String);", &global_arena);

        let ast = SynFactory::new(&global_arena);
        let hir = SemFactory::new(&global_arena);
        let (i, p, t) = (hir.item(), hir.proto(), hir.type_());

        let r = p.rec(i.id(5, 3), 0).range(0, 22).build();

        assert_eq!(
            env.item_of(
                &Prototype::Rec(r),
                &ast.item()
                    .record(5, 3)
                    .tuple(
                        ast.type_tuple()
                            .push(ast.type_().simple(9, 3))
                            .push(ast.type_().simple(14, 6))
                            .build()
                    ).build(),
            ),
            i.rec(r).push(t.int()).push(t.string()).build().into()
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
        let hir = SemFactory::new(&global_arena);
        let (i, p, t, v) = (hir.item(), hir.proto(), hir.type_(), hir.value());

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
            p.fun(i.id(5, 3), t.int())
                .push(v.id(9, 1), t.int())
                .push(v.id(17, 1), t.int())
                .range(0, 31)
                .build()
                .into()
        );
    }

    #[test]
    fn item_fun() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add(a: Int, b: Int) -> Int { a + b }",
            &global_arena,
        );

        let ast = SynFactory::new(&global_arena);
        let e = ast.expr();
        let hir = SemFactory::new(&global_arena);
        let (i, p, t, v) = (hir.item(), hir.proto(), hir.type_(), hir.value());

        let f =
            p.fun(i.id(5, 3), t.int())
                .push(v.id(9, 1), t.int())
                .push(v.id(17, 1), t.int())
                .range(0, 31)
                .build();

        let body =
            v.call()
                .push(v.arg_ref(t.int(), v.id(9, 1), 34))
                .push(v.arg_ref(t.int(), v.id(17, 1), 38))
                .build();

        assert_eq!(
            env.item_of(
                &Prototype::Fun(f),
                &ast.item().function(
                    5,
                    3,
                    ast.type_().simple(28, 3),
                    e.block(e.bin_op(e.var(34, 1), e.var(38, 1)).build())
                        .build(),
                )
                .push(9, 1, ast.type_().simple(12, 3))
                .push(17, 1, ast.type_().simple(17, 1))
                .build()
            ),
            i.fun(f, v.block(body).build()).into()
        );
    }

    #[test]
    fn item_fun_tuple() {
        let global_arena = mem::Arena::new();
        let env = Env::new(
            b":fun add() -> (Int, Int) { (1, 2) }",
            &global_arena,
        );

        let ast = SynFactory::new(&global_arena);
        let e = ast.expr();
        let hir = SemFactory::new(&global_arena);
        let (i, p, t, v) = (hir.item(), hir.proto(), hir.type_(), hir.value());

        let f =
            p.fun(i.id(5, 3), t.tuple().push(t.int()).push(t.int()).build())
                .range(0, 24)
                .build();

        let body = v.tuple().push(v.int(1, 28)).push(v.int(2, 31)).build();

        assert_eq!(
            env.item_of(
                &Prototype::Fun(f),
                &ast.item().function(
                    5,
                    3,
                    ast.type_().tuple()
                        .push(ast.type_().simple(15, 3))
                        .push(ast.type_().simple(20, 3))
                        .build(),
                    e.block(
                        e.tuple().push(e.int(28, 1)).push(e.int(31, 1)).build()
                    ).build(),
                )
                .build()
            ),
            i.fun(f, v.block(body).build()).into()
        );
    }

    struct Env<'g> {
        scope: MockScope<'g>,
        registry: MockRegistry<'g>,
        fragment: &'g [u8],
        arena: &'g mem::Arena,
    }

    impl<'g> Env<'g> {
        fn new(fragment: &'g [u8], arena: &'g mem::Arena) -> Env<'g> {
            Env {
                scope: MockScope::new(fragment, arena),
                registry: MockRegistry::new(arena),
                fragment: fragment,
                arena: arena,
            }
        }

        fn builder<'a, 'local>(&'a self, local: &'local mem::Arena)
            -> super::GraphBuilder<'a, 'g, 'local>
        {
            super::GraphBuilder::new(
                com::CodeFragment::new(self.fragment.to_vec()),
                &self.scope,
                &self.registry, 
                self.arena, 
                local
            )
        }

        fn proto_of(&self, item: &ast::Item) -> Prototype<'g> {
            let mut local_arena = mem::Arena::new();
            let result = self.builder(&local_arena).prototype(item);
            local_arena.recycle();
            result
        }

        fn item_of(&self, proto: &Prototype, item: &ast::Item) -> Item<'g> {
            let mut local_arena = mem::Arena::new();
            let proto = self.arena.intern_ref(proto);
            let result = self.builder(&local_arena).item(proto, item);
            local_arena.recycle();
            result
        }
    }
}
