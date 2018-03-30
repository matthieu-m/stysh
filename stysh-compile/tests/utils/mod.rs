/// Utilities for the integration tests

use stysh_compile::basic::mem;
use stysh_compile::model::{ast, hir, sir};
use stysh_compile::pass::int;
use stysh_compile::pass::sem::{self, scp};

pub fn interpret<'g>(
    raw: &'g [u8],
    interner: &'g mem::Interner,
    arena: &'g mem::Arena
)
    -> hir::Value<'g>
{
    let scope_arena = mem::Arena::new();
    let builtin = scp::BuiltinScope::new();
    let mut scope = scp::BlockScope::new(&builtin, arena, &scope_arena);
    let mut def_registry = hir::mocks::MockRegistry::new(arena);
    let mut cfg_registry = int::SimpleRegistry::new(arena);

    interpret_impl(
        raw,
        &mut scope,
        &mut def_registry,
        &mut cfg_registry,
        interner,
        arena,
    )
}

//
//  Implementation Details
//
fn interpret_impl<'a, 'g, 's>(
    raw: &'g [u8],
    scope: &mut scp::BlockScope<'a, 'g, 's>,
    def_registry: &mut hir::mocks::MockRegistry<'g>,
    cfg_registry: &mut int::SimpleRegistry<'g>,
    interner: &'g mem::Interner,
    arena: &'g mem::Arena,
)
    -> hir::Value<'g>
where
    'g: 'a + 's
{
    let mut local_arena = mem::Arena::new();

    let nodes = create_ast(raw, interner, arena, &mut local_arena);

    //  Gather prototypes
    let mut prototypes = Vec::new();
    let mut expression = None;

    for &node in nodes {
        match node {
            ast::Node::Item(i) => {
                use self::hir::Prototype::*;

                let prototype =
                    create_prototype(&i, scope, def_registry, arena, &mut local_arena);
                prototypes.push((i, arena.insert(prototype)));

                match prototype {
                    Enum(_) => unimplemented!(),
                    Fun(fun) => scope.add_function(fun),
                    Rec(_) => unimplemented!(),
                };
            },
            ast::Node::Expr(expr) => {
                assert!(
                    expression.is_none(),
                    "Cannot replace {:?} by {:?}", expression, expr
                );
                expression = Some(expr);
            },
            ast::Node::Stmt(_) => panic!("No statement allowed at top-level"),
        }
    }

    //  Pull definitions together
    for (i, p) in prototypes {
        use self::hir::Item::*;

        let item = create_item(&i, p, scope, def_registry, arena, &mut local_arena);

        match item {
            Enum(_) => unimplemented!(),
            Fun(ref fun) => {
                let c = create_cfg_from_function(
                    fun,
                    arena,
                    &mut local_arena,
                    def_registry
                );
                cfg_registry.insert(fun.prototype.name, c);
            },
            Rec(_) => unimplemented!(),
        }
    }

    //  Finally, interpret the expression.
    let value = create_value(
        &expression.expect("One expression is necessary!"),
        scope,
        def_registry,
        arena,
        &mut local_arena
    );

    let cfg =
        create_cfg_from_value(&value, arena, &mut local_arena, def_registry);
    evaluate(&cfg, cfg_registry, arena, &mut local_arena)
}

fn create_ast<'g>(
    raw: &[u8],
    interner: &'g mem::Interner,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
)
    -> ast::List<'g>
{
    use stysh_compile::pass::syn::Parser;

    let result = Parser::new(global_arena, local_arena).parse(raw, interner);
    local_arena.recycle();

    result
}

fn create_prototype<'a, 'g>(
    item: &ast::Item,
    scope: &'a scp::Scope<'g>,
    registry: &'a hir::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> hir::Prototype<'g>
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context, global_arena, local_arena)
        .prototype(item);
    local_arena.recycle();

    println!("create_prototype => {:?}", result);

    result
}

fn create_item<'g>(
    item: &ast::Item,
    proto: &'g hir::Prototype<'g>,
    scope: &scp::Scope<'g>,
    registry: &hir::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> hir::Item<'g>
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context, global_arena, local_arena)
        .item(proto, item);
    local_arena.recycle();

    println!("create_item => {:?}", result);

    result
}

fn create_value<'a, 'g>(
    expr: &ast::Expression,
    scope: &'a scp::Scope<'g>,
    registry: &'a hir::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> hir::Value<'g>
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context, global_arena, local_arena)
        .expression(expr);
    local_arena.recycle();

    println!("create_value => {:?}", result);

    result
}

fn create_cfg_from_value<'g>(
    value: &hir::Value<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
    registry: &hir::Registry<'g>,
)
    -> sir::ControlFlowGraph<'g>
{
    use stysh_compile::pass::ssa::GraphBuilder;

    let result =
        GraphBuilder::new(global_arena, local_arena, registry)
            .from_value(&value);
    local_arena.recycle();

    println!("create_cfg_from_value => {}", result);

    result
}

fn create_cfg_from_function<'g>(
    fun: &hir::Function<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
    registry: &hir::Registry<'g>,
)
    -> sir::ControlFlowGraph<'g>
{
    use stysh_compile::pass::ssa::GraphBuilder;

    let result =
        GraphBuilder::new(global_arena, local_arena, registry)
            .from_function(&fun);
    local_arena.recycle();

    println!(
        "create_cfg_from_function {:?} =>\n{}",
        fun.prototype.name, result
    );

    result
}

fn evaluate<'a, 'g>(
    cfg: &sir::ControlFlowGraph<'g>,
    registry: &int::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
)
    -> hir::Value<'g>
{
    use stysh_compile::pass::int::Interpreter;

    let result = Interpreter::new(registry, global_arena, local_arena)
        .evaluate(cfg, &[]);
    local_arena.recycle();

    result
}
