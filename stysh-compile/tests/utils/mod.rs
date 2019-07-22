/// Utilities for the integration tests

use std::cell;

use stysh_compile::basic::mem;
use stysh_compile::model::{ast, hir, sir};
use stysh_compile::pass::int;
use stysh_compile::pass::sem::{self, scp};

use self::hir::Registry;

pub fn interpret(raw: &[u8]) -> int::Value {
    let interner = mem::Interner::new();
    let builtin = scp::BuiltinScope::new();
    let mut scope = scp::BlockScope::new(&builtin);
    let mut cfg_registry = int::SimpleRegistry::new();

    interpret_impl(
        raw,
        &mut scope,
        &mut cfg_registry,
        &interner,
    )
}

//
//  Implementation Details
//
fn interpret_impl<'a>(
    raw: &[u8],
    scope: &mut scp::BlockScope<'a>,
    cfg_registry: &mut int::SimpleRegistry,
    interner: &mem::Interner,
)
    -> int::Value
{
    let (ast_module, tree) = create_ast(raw, interner);

    //  Create prototypes
    let hir_module = cell::RefCell::new(hir::Module::default());

    for i in 0..(ast_module.len_functions() as u32) {
        let id = ast::Item::Fun(ast::FunctionId::new(i));

        let prototype = create_prototype(id, &ast_module, &tree, scope, &hir_module);

        if let self::hir::Prototype::Fun(fun) = prototype {
            let id = hir_module.borrow().lookup_function(fun.name)
                .expect("Function to be registered");
            scope.add_function(fun.name, id);
        }
    }

    //  Pull definitions together
    for i in 0..(ast_module.len_functions() as u32) {
        use self::hir::Item::*;

        let id = ast::Item::Fun(ast::FunctionId::new(i));
        let item = create_item(id, &ast_module, &tree, scope, &hir_module);

        match item {
            Enum(_) => unimplemented!(),
            Fun(fun) => {
                let cfg = create_cfg_from_function(&hir_module.borrow(), fun);
                cfg_registry.insert(cfg.source(), cfg);
            },
            Rec(_) => unimplemented!(),
        }
    }

    //  Finally, interpret the expression.
    let value = create_value(
        tree.get_root_expression().expect("One expression is necessary!"),
        &ast_module,
        &tree,
        scope,
        &hir_module,
    );

    let cfg = create_cfg_from_value(&hir_module.borrow(), &value);
    return evaluate(&cfg, interner.snapshot(), &hir_module.borrow(), cfg_registry);
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
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &scp::Scope,
    hir_module: &cell::RefCell<hir::Module>,
)
    -> hir::Prototype
{
    use self::sem::{Context, GraphBuilder};

    let repository = hir::RepositorySnapshot::default();

    let context = Context::default();
    let mut builder = GraphBuilder::new(scope, &repository, &context, ast_module, tree, &hir_module);
    builder.name(item);
    let result = builder.prototype(item);

    println!("create_prototype - {:?}", result);
    println!("");

    result
}

fn create_item(
    item: ast::Item,
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &scp::Scope,
    hir_module: &cell::RefCell<hir::Module>,
)
    -> hir::Item
{
    use self::sem::{Context, GraphBuilder};

    let repository = hir::RepositorySnapshot::default();

    let context = Context::default();
    let result = GraphBuilder::new(scope, &repository, &context, ast_module, tree, &hir_module)
        .item(item);

    println!("create_item - {:#?}", result);
    println!("");

    result
}

fn create_value(
    expr: ast::ExpressionId,
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &scp::Scope,
    hir_module: &cell::RefCell<hir::Module>,
)
    -> hir::Tree
{
    use self::sem::{Context, GraphBuilder};

    let repository = hir::RepositorySnapshot::default();

    let context = Context::default();
    let result = GraphBuilder::new(scope, &repository, &context, ast_module, tree, hir_module)
        .expression(expr);

    println!("create_value - {:#?}", result);
    println!("");

    result
}

fn create_cfg_from_value(module: &hir::Module, tree: &hir::Tree)
    -> sir::Graph
{
    use stysh_compile::pass::ssa::GraphBuilder;

    let id = if let Some(hir::Root::Expression(e)) = tree.get_root() {
        e
    } else {
        unreachable!("Not an expression!");
    };

    let result = GraphBuilder::new(module).from_expression(tree, id);

    println!("create_cfg_from_value -\n");
    sir::display_graph(&result, module);
    println!("");

    result
}

fn create_cfg_from_function(module: &hir::Module, fun: hir::FunctionId)
    -> sir::Graph
{
    use stysh_compile::pass::ssa::GraphBuilder;

    let result = GraphBuilder::new(module).from_function(module, fun);

    println!("create_cfg_from_function - {:?} =>\n",
        module.get_function_prototype(fun).name);
    sir::display_graph(&result, module);
    println!("");

    result
}

fn evaluate(
    cfg: &sir::Graph,
    interner: mem::InternerSnapshot,
    module: &hir::Module,
    registry: &int::Registry,
)
    -> int::Value
{
    use stysh_compile::pass::int::Interpreter;

    let result = Interpreter::new(interner, module, registry)
        .evaluate(cfg, vec!());

    result
}
