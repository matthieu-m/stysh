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
    let (module, tree) = create_ast(raw, interner);

    //  Gather prototypes
    let mut prototypes = Vec::new();

    for i in 0..(module.len_functions() as u32) {
        let id = ast::Item::Fun(ast::FunctionId::new(i));

        let prototype = create_prototype(id, &module, &tree, scope, def_registry);
        prototypes.push((id, prototype.clone()));

        if let self::hir::Prototype::Fun(fun) = prototype {
            scope.add_function(fun);
        }
    }

    //  Pull definitions together
    for (i, p) in prototypes {
        use self::hir::Item::*;

        let item = create_item(i, &p, &module, &tree, scope, def_registry);

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
        tree.get_root_expression().expect("One expression is necessary!"),
        &module,
        &tree,
        scope,
        def_registry,
    );

    let cfg = create_cfg_from_value(&value);
    evaluate(&cfg, interner.snapshot(), cfg_registry)
}

fn create_ast(
    raw: &[u8],
    interner: &mem::Interner,
)
    -> (ast::Module, ast::Tree)
{
    use stysh_compile::pass::syn::Parser;

    Parser::new().parse(raw, interner)
}

fn create_prototype(
    item: ast::Item,
    module: &ast::Module,
    tree: &ast::Tree,
    scope: &scp::Scope,
    registry: &hir::Registry,
)
    -> hir::Prototype
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context, module, tree)
        .prototype(item);

    println!("create_prototype - {:?}", result);
    println!("");

    result
}

fn create_item(
    item: ast::Item,
    proto: &hir::Prototype,
    module: &ast::Module,
    tree: &ast::Tree,
    scope: &scp::Scope,
    registry: &hir::Registry,
)
    -> hir::Item
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context, module, tree)
        .item(proto.clone(), item);

    println!("create_item - {:#?}", result);
    println!("");

    result
}

fn create_value(
    expr: ast::ExpressionId,
    module: &ast::Module,
    tree: &ast::Tree,
    scope: &scp::Scope,
    registry: &hir::Registry,
)
    -> hir::Tree
{
    use self::sem::{Context, GraphBuilder};

    let context = Context::default();
    let result = GraphBuilder::new(scope, registry, &context, module, tree).expression(expr);

    println!("create_value - {:#?}", result);
    println!("");

    result
}

fn create_cfg_from_value(tree: &hir::Tree) -> sir::ControlFlowGraph {
    use stysh_compile::pass::ssa::GraphBuilder;

    let id = if let Some(hir::Root::Expression(e)) = tree.get_root() {
        e
    } else {
        unreachable!("Not an expression!");
    };

    let result = GraphBuilder::new().from_expression(tree, id);

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
