/// Utilities for the integration tests

use std::cell;

use stysh_compile::basic::mem;
use stysh_compile::model::{ast, hir, sir};
use stysh_compile::pass::int;
use stysh_compile::pass::sem::{self, scp};

pub fn interpret(raw: &[u8]) -> int::Value {
    let interner = mem::Interner::new();
    let builtin = scp::BuiltinScope::new();
    let mut cfg_registry = int::SimpleRegistry::new();

    interpret_impl(
        raw,
        &builtin,
        &mut cfg_registry,
        &interner,
    )
}

//
//  Implementation Details
//
fn interpret_impl(
    raw: &[u8],
    builtin: &scp::BuiltinScope,
    cfg_registry: &mut int::SimpleRegistry,
    interner: &mem::Interner,
)
    -> int::Value
{
    let (ast_module, tree) = create_ast(raw, interner);
    let hir_module = cell::RefCell::new(hir::Module::default());

    {
        let mut scope = scp::BlockScope::new(builtin);

        //  Create names
        create_names(&ast_module, &tree, &mut scope, &hir_module);

        //  Create items
        create_items(&ast_module, &tree, &scope, &hir_module);
    }

    let mut repository = hir::Repository::default();
    repository.internalize_module(&hir_module.borrow());

    let scope = create_scope(builtin, &repository.snapshot());

    //  Create CFGs for functions.
    for id in ast_module.functions() {
        use self::hir::ItemId;

        //  Interface functions are declarations only.
        if ast_module.get_function_body(id).is_none() {
            continue;
        }

        let fun = hir::FunctionId::new_repository(id.value());
        let body = create_function(id, fun, &ast_module, &tree, &scope, &repository, &hir_module);

        let snapshot = repository.snapshot();

        let cfg = create_cfg_from_function(&snapshot, &body, fun);
        cfg_registry.insert(cfg.source(), cfg);
    }

    //  Finally, interpret the expression.
    let value = create_value(
        tree.get_root_expression().expect("One expression is necessary!"),
        &ast_module,
        &tree,
        &scope,
        &repository,
        &hir_module,
    );

    let snapshot = repository.snapshot();

    let cfg = create_cfg_from_value(&snapshot, &value);
    return evaluate(&cfg, interner.snapshot(), &snapshot, cfg_registry);
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

fn create_names<'a>(
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &mut scp::BlockScope<'a>,
    hir_module: &cell::RefCell<hir::Module>,
)
{
    for id in ast_module.enums() {
        let item = create_name(ast::Item::Enum(id), ast_module, tree, scope, hir_module);

        if let hir::Item::Enum(hir_id) = item {
            let e = ast_module.get_enum(id);
            scope.add_enum(e.name.into(), hir_id);
        }
    }

    for id in ast_module.records() {
        let item = create_name(ast::Item::Rec(id), ast_module, tree, scope, hir_module);

        if let hir::Item::Rec(hir_id) = item {
            let r = ast_module.get_record(id);
            scope.add_record(r.name().into(), hir_id);
        }
    }

    for id in ast_module.interfaces() {
        let item = create_name(ast::Item::Int(id), ast_module, tree, scope, hir_module);

        if let hir::Item::Int(hir_id) = item {
            let i = ast_module.get_interface(id);
            scope.add_interface(i.name.into(), hir_id);
        }
    }

    for id in ast_module.extensions() {
        create_name(ast::Item::Ext(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.implementations() {
        create_name(ast::Item::Imp(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.functions() {
        let item = create_name(ast::Item::Fun(id), ast_module, tree, scope, hir_module);

        //  Only register global scope functions and interface methods.
        match ast_module.get_function_scope(id) {
            ast::Scope::Module => {
                if let hir::Item::Fun(hir_id) = item {
                    let fun = ast_module.get_function(id);
                    scope.add_function(fun.name.into(), hir_id);
                };
            },
            ast::Scope::Int(_) => {
                if let hir::Item::Fun(hir_id) = item {
                    let fun = ast_module.get_function(id);
                    scope.add_method(fun.name.id(), hir_id);
                };
            },
            ast::Scope::Ext(_) | ast::Scope::Imp(_) => (),
        }
    }
}

fn create_name(
    item: ast::Item,
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &dyn scp::Scope,
    hir_module: &cell::RefCell<hir::Module>,
)
    -> hir::Item
{
    use self::sem::{Context, GraphBuilder};

    let repository = hir::RepositorySnapshot::default();

    let context = Context::default();
    GraphBuilder::new(scope, &repository, &context, ast_module, tree, &hir_module)
        .name(item)
}

fn create_items(
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &dyn scp::Scope,
    hir_module: &cell::RefCell<hir::Module>,
)
{
    for id in ast_module.enums() {
        create_item(ast::Item::Enum(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.records() {
        create_item(ast::Item::Rec(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.interfaces() {
        create_item(ast::Item::Int(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.extensions() {
        create_item(ast::Item::Ext(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.implementations() {
        create_item(ast::Item::Imp(id), ast_module, tree, scope, hir_module);
    }

    for id in ast_module.functions() {
        create_item(ast::Item::Fun(id), ast_module, tree, scope, hir_module);
    }
}

fn create_item(
    item: ast::Item,
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &dyn scp::Scope,
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

fn create_scope<'a>(
    builtin: &'a scp::BuiltinScope,
    registry: &dyn hir::Registry
)
    -> scp::BlockScope<'a>
{
    let mut scope = scp::BlockScope::new(builtin);

    for id in registry.enums() {
        let e = registry.get_enum(id);
        scope.add_enum(e.name, id);
    }

    for id in registry.records() {
        let r = registry.get_record(id);
        scope.add_record(r.name, id);
    }

    for id in registry.interfaces() {
        let i = registry.get_interface(id);
        scope.add_interface(i.name, id);
    }

    for id in registry.functions() {
        use hir::Scope::*;

        let f = registry.get_function(id);

        match f.scope {
            Module => scope.add_function(f.name, id),
            Int(_) => scope.add_method(f.name.id(), id),
            Ext(_) | Imp(_) => (),
        }
    }

    println!("create_scope - {:#?}", scope);
    println!("");

    scope
}

fn create_function(
    function: ast::FunctionId,
    hir_function: hir::FunctionId,
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &dyn scp::Scope,
    repository: &hir::Repository,
    hir_module: &cell::RefCell<hir::Module>,
)
    -> hir::Tree
{
    use self::sem::{Context, GraphBuilder};

    let snapshot = repository.snapshot();

    let context = Context::default();
    let mut result = GraphBuilder::new(scope, &snapshot, &context, ast_module, tree, &hir_module)
        .function(function, hir_function);

    repository.internalize_tree(&mut result);

    println!("create_function - {:#?}", result);
    println!("");

    result
}

fn create_value(
    expr: ast::ExpressionId,
    ast_module: &ast::Module,
    tree: &ast::Tree,
    scope: &dyn scp::Scope,
    repository: &hir::Repository,
    hir_module: &cell::RefCell<hir::Module>,
)
    -> hir::Tree
{
    use self::sem::{Context, GraphBuilder};

    let snapshot = repository.snapshot();

    let context = Context::default();
    let mut result = GraphBuilder::new(scope, &snapshot, &context, ast_module, tree, hir_module)
        .expression(expr);

    repository.internalize_tree(&mut result);

    println!("create_value - {:#?}", result);
    println!("");

    result
}

fn create_cfg_from_value(registry: &dyn hir::Registry, tree: &hir::Tree)
    -> sir::Graph
{
    use stysh_compile::pass::ssa::GraphBuilder;

    let result = GraphBuilder::new(registry).from_expression(tree);

    println!("create_cfg_from_value =>");
    println!("{}", sir::display_graph(&result, registry));
    println!("");

    result
}

fn create_cfg_from_function(registry: &dyn hir::Registry, tree: &hir::Tree, fun: hir::FunctionId)
    -> sir::Graph
{
    use stysh_compile::pass::ssa::GraphBuilder;

    let result = GraphBuilder::new(registry).from_function(tree, fun);

    println!("create_cfg_from_function - {:?} ({:?}) =>",
        fun, registry.get_function(fun).name);
    println!("{}", sir::display_graph(&result, registry));
    println!("");

    result
}

fn evaluate(
    cfg: &sir::Graph,
    interner: mem::InternerSnapshot,
    hir_registry: &dyn hir::Registry,
    registry: &dyn int::Registry,
)
    -> int::Value
{
    use stysh_compile::pass::int::Interpreter;

    let result = Interpreter::new(interner, hir_registry, registry)
        .evaluate(cfg, vec!());

    result
}
