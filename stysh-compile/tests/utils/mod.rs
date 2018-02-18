/// Utilities for the integration tests

use stysh_compile::basic::{com, mem};
use stysh_compile::model::{syn, sem, sir};
use stysh_compile::pass::int;
use stysh_compile::pass::sem::scp;

pub fn interpret<'g>(raw: &'g [u8], arena: &'g mem::Arena) -> sem::Value<'g> {
    let scope_arena = mem::Arena::new();
    let builtin = scp::BuiltinScope::new(raw);
    let fake = sem::mocks::MockRegistry::new(arena);
    let mut scope =
        scp::BlockScope::new(raw, &builtin, &fake, arena, &scope_arena);
    let mut def_registry = sem::mocks::MockRegistry::new(arena);
    let mut cfg_registry = int::SimpleRegistry::new(arena);

    interpret_impl(raw, &mut scope, &mut def_registry, &mut cfg_registry, &arena)
}

//
//  Implementation Details
//
fn interpret_impl<'a, 'g, 's>(
    raw: &'g [u8],
    scope: &mut scp::BlockScope<'a, 'g, 's>,
    def_registry: &mut sem::mocks::MockRegistry<'g>,
    cfg_registry: &mut int::SimpleRegistry<'g>,
    arena: &'g mem::Arena
)
    -> sem::Value<'g>
where
    'g: 'a + 's
{
    let code = com::CodeFragment::new(raw.to_vec());

    let mut local_arena = mem::Arena::new();

    let nodes = create_ast(raw, arena, &mut local_arena);

    //  Gather prototypes
    let mut prototypes = Vec::new();
    let mut expression = None;

    for &node in nodes {
        match node {
            syn::Node::Item(i) => {
                use self::sem::Prototype::*;

                let code = code.clone();
                let prototype =
                    create_prototype(&i, code, scope, def_registry, arena, &mut local_arena);
                prototypes.push((i, arena.insert(prototype)));

                match prototype {
                    Enum(_) => unimplemented!(),
                    Fun(fun) => scope.add_function(fun),
                    Rec(_) => unimplemented!(),
                };
            },
            syn::Node::Expr(expr) => {
                assert!(
                    expression.is_none(),
                    "Cannot replace {:?} by {:?}", expression, expr
                );
                expression = Some(expr);
            },
            syn::Node::Stmt(_) => panic!("No statement allowed at top-level"),
        }
    }

    //  Pull definitions together
    for (i, p) in prototypes {
        use self::sem::Item::*;

        let code = code.clone();
        let item = create_item(&i, p, code, scope, def_registry, arena, &mut local_arena);

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
        code,
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
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> syn::List<'g>
{
    use stysh_compile::pass::syn::Parser;

    let result = Parser::new(global_arena, local_arena).parse(raw);
    local_arena.recycle();

    result
}

fn create_prototype<'a, 'g>(
    item: &syn::Item,
    code: com::CodeFragment,
    scope: &'a scp::Scope<'g>,
    registry: &'a sem::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> sem::Prototype<'g>
{
    use stysh_compile::pass::sem::GraphBuilder;

    let result = GraphBuilder::new(code, scope, registry, global_arena, local_arena)
        .prototype(item);
    local_arena.recycle();

    result
}

fn create_item<'a, 'g>(
    item: &syn::Item,
    proto: &'g sem::Prototype<'g>,
    code: com::CodeFragment,
    scope: &'a scp::Scope<'g>,
    registry: &'a sem::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> sem::Item<'g>
{
    use stysh_compile::pass::sem::GraphBuilder;

    let result = GraphBuilder::new(code, scope, registry, global_arena, local_arena)
        .item(proto, item);
    local_arena.recycle();

    result
}

fn create_value<'a, 'g>(
    expr: &syn::Expression,
    code: com::CodeFragment,
    scope: &'a scp::Scope<'g>,
    registry: &'a sem::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena
)
    -> sem::Value<'g>
{
    use stysh_compile::pass::sem::GraphBuilder;

    let result = GraphBuilder::new(code, scope, registry, global_arena, local_arena)
        .expression(expr);
    local_arena.recycle();

    result
}

fn create_cfg_from_value<'g>(
    value: &sem::Value<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
    registry: &sem::Registry<'g>,
)
    -> sir::ControlFlowGraph<'g>
{
    use stysh_compile::pass::sem::GlobalValueNumberer;
    use stysh_compile::pass::ssa::GraphBuilder;

    let value =
        GlobalValueNumberer::new(global_arena, local_arena)
            .number_value(value);

    let result =
        GraphBuilder::new(global_arena, local_arena, registry)
            .from_value(&value);
    local_arena.recycle();

    result
}

fn create_cfg_from_function<'g>(
    fun: &sem::Function<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
    registry: &sem::Registry<'g>,
)
    -> sir::ControlFlowGraph<'g>
{
    use stysh_compile::pass::sem::GlobalValueNumberer;
    use stysh_compile::pass::ssa::GraphBuilder;

    let fun =
        GlobalValueNumberer::new(global_arena, local_arena)
            .number_function(fun);

    let result =
        GraphBuilder::new(global_arena, local_arena, registry)
            .from_function(&fun);
    local_arena.recycle();

    result
}

fn evaluate<'a, 'g>(
    cfg: &sir::ControlFlowGraph<'g>,
    registry: &int::Registry<'g>,
    global_arena: &'g mem::Arena,
    local_arena: &mut mem::Arena,
)
    -> sem::Value<'g>
{
    use stysh_compile::pass::int::Interpreter;

    let result = Interpreter::new(registry, global_arena, local_arena)
        .evaluate(cfg, &[]);
    local_arena.recycle();

    result
}
