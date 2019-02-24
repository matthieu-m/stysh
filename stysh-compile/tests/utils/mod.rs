/// Utilities for the integration tests

use stysh_compile::basic::mem;
use stysh_compile::model::{ast, hir, sir};
use stysh_compile::pass::int;
use stysh_compile::pass::sem::{self, scp};

pub fn interpret(
    raw: &[u8],
    interner: &mem::Interner,
)
    -> int::Value
{
    let builtin = scp::BuiltinScope::new();
    let mut scope = scp::BlockScope::new(&builtin);
    let mut def_registry = hir::mocks::MockRegistry::new();
    let mut cfg_registry = int::SimpleRegistry::new();

    interpret_impl(
        raw,
        &mut scope,
        &mut def_registry,
        &mut cfg_registry,
        interner,
    )
}

//
//  Implementation Details
//
fn interpret_impl<'a>(
    raw: &[u8],
    scope: &mut scp::BlockScope<'a>,
    def_registry: &mut hir::mocks::MockRegistry,
    cfg_registry: &mut int::SimpleRegistry,
    interner: &mem::Interner,
)
    -> int::Value
{
    let global_arena = mem::Arena::new();
    let nodes = create_ast(raw, interner, &global_arena);

    //  Gather prototypes
    let mut prototypes = Vec::new();
    let mut expression = None;

    for &node in nodes {
        match node {
            ast::Node::Item(i) => {
                use self::hir::Prototype::*;

                let prototype = create_prototype(&i, scope, def_registry);
                prototypes.push((i, prototype.clone()));

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

        let item = create_item(&i, &p, scope, def_registry);

        match item {
            Enum(_) => unimplemented!(),
            Fun(ref fun) => {
                let cfg = create_cfg_from_function(fun);
                cfg_registry.insert(fun.prototype.name, cfg);
            },
            Rec(_) => unimplemented!(),
        }
    }

    //  Finally, interpret the expression.
    let value = create_value(
        &expression.expect("One expression is necessary!"),
        scope,
        def_registry,
    );

    let cfg = create_cfg_from_value(&value);
    evaluate(&cfg, interner.snapshot(), cfg_registry)
}

fn create_ast<'g>(
    raw: &[u8],
    interner: &'g mem::Interner,
    global_arena: &'g mem::Arena,
)
    -> ast::List<'g>
{
    use stysh_compile::pass::syn::Parser;

    let mut local_arena = mem::Arena::new();
    let result = Parser::new(global_arena, &local_arena).parse(raw, interner);
    local_arena.recycle();

    result
}

fn create_prototype(
    item: &ast::Item,
    scope: &scp::Scope,
    registry: &hir::Registry,
)
    -> hir::Prototype
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context).prototype(item);

    println!("create_prototype - {:?}", result);
    println!("");

    result
}

fn create_item(
    item: &ast::Item,
    proto: &hir::Prototype,
    scope: &scp::Scope,
    registry: &hir::Registry,
)
    -> hir::Item
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context)
        .item(proto.clone(), item);

    println!("create_item - {:?}", result);
    println!("");

    result
}

fn create_value(
    expr: &ast::Expression,
    scope: &scp::Scope,
    registry: &hir::Registry,
)
    -> hir::Value
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context).expression(expr);

    println!("create_value - {:?}", result);
    println!("");

    result
}

fn create_cfg_from_value(value: &hir::Value) -> sir::ControlFlowGraph {
    use stysh_compile::pass::ssa::GraphBuilder;

    let result = GraphBuilder::new().from_value(value);

    println!("create_cfg_from_value - {}", result);
    println!("");

    result
}

fn create_cfg_from_function(fun: &hir::Function) -> sir::ControlFlowGraph {
    use stysh_compile::pass::ssa::GraphBuilder;

    let result = GraphBuilder::new().from_function(fun);

    println!(
        "create_cfg_from_function - {:?} =>\n{}",
        fun.prototype.name, result
    );
    println!("");

    result
}

fn evaluate(
    cfg: &sir::ControlFlowGraph,
    interner: mem::InternerSnapshot<'_>,
    registry: &int::Registry,
)
    -> int::Value
{
    use stysh_compile::pass::int::Interpreter;

    let result = Interpreter::new(interner, registry)
        .evaluate(cfg, Default::default());

    result
}
