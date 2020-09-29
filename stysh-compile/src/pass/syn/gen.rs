//! Syntactic passes, aka parsing.
//!
//! Generic Packs parser.

use std::cell;

use crate::basic::com::{Range, Span, Store, MultiStore};

use crate::model::tt::Kind;
use crate::model::ast::*;

use super::{com::RawParser, typ};

/// Attempts to parse a pack of generic parameters.
///
/// Returns the ID of the pack if successful, or the default ID otherwise.
pub fn try_parse_generic_parameters<'a, 'tree>(raw: &mut RawParser<'a, 'tree>) -> Option<Id<GenericParameterPack>> {
    let module = raw.module();
    try_parse_generic_pack(raw, module, try_parse_generic_parameter)
}

/// Attempts to parse a pack of generic variables.
///
/// Returns the ID of the pack if successful, or the default ID otherwise.
pub fn try_parse_generic_variables<'a, 'tree, S>(raw: &mut RawParser<'a, 'tree>, store: &cell::RefCell<S>)
    -> Option<Id<GenericVariablePack>>
    where
        S: Store<GenericVariablePack> + Store<Type> + MultiStore<GenericVariable> + MultiStore<Id<Type>> + MultiStore<Identifier> + MultiStore<u32>
{
    try_parse_generic_pack(raw, store, try_parse_generic_variable)
}


//
//  Implementation
//

fn try_parse_generic_pack<'a, 'tree, T, S>(
    raw: &mut RawParser<'a, 'tree>,
    store: &cell::RefCell<S>,
    try_parse_element: impl Fn(&mut RawParser<'a, 'tree>, &cell::RefCell<S>) -> Option<(T, Range)>,
)
    -> Option<Id<GenericPack<T>>>
    where
        S: Store<GenericPack<T>> + Store<Type> + MultiStore<T> + MultiStore<Id<Type>> + MultiStore<Identifier> + MultiStore<u32>
{
    raw.peek_braced(Kind::BracketOpen).map(|(open, ns, close)| {
        raw.pop_node();

        let (open, close) = (open.offset() as u32, close.offset() as u32);

        let mut parser = raw.spawn(ns);
        let mut elements = vec!();
        let mut commas = vec!();

        while let Some((element, range)) = try_parse_element(&mut parser, &store) {
            let comma = parser.pop_kind(Kind::SignComma)
                .map(|token| token.offset())
                .unwrap_or(range.end_offset() - 1);

            elements.push(element);
            commas.push(comma as u32);
        }

        let mut store = store.borrow_mut();
        let elements = store.push_slice(&elements);
        let commas = store.push_slice(&commas);

        let pack = GenericPack{ elements, commas, open, close, };
        let range = Range::half_open(open, close + 1);

        store.push(pack, range)
    })
}

fn try_parse_generic_parameter<'a, 'tree, S>(raw: &mut RawParser<'a, 'tree>, _: &cell::RefCell<S>)
    -> Option<(Identifier, Range)>
{
    raw.pop_kinds(&[Kind::NameType, Kind::NameValue]) 
        .map(|token| raw.resolve_identifier(token))
        .map(|identifier| (identifier, identifier.span()))
}

fn try_parse_generic_variable<'a, 'tree, S>(raw: &mut RawParser<'a, 'tree>, store: &cell::RefCell<S>)
    -> Option<(GenericVariable, Range)>
    where
        S: Store<GenericPack<GenericVariable>> + Store<Type> + MultiStore<GenericVariable> + MultiStore<Id<Type>> + MultiStore<Identifier> + MultiStore<u32>
{
    let path = raw.parse_path(store);

    let token = if let Some(token) = raw.peek_token() {
        if token.kind() != Kind::NameType {
            raw.pop_tokens(1);
        }
        token
    } else {
        return None;
    };

    match token.kind() {
        Kind::LitBoolFalse => {
            assert!(path.is_empty(), "A literal false should have no path: {:?}", path);

            Some((GenericVariable::Literal(Literal::Bool(false), token.span()), token.span()))
        },
        Kind::LitBoolTrue => {
            assert!(path.is_empty(), "A literal true should have no path: {:?}", path);

            Some((GenericVariable::Literal(Literal::Bool(true), token.span()), token.span()))
        },
        Kind::LitIntegral => {
            assert!(path.is_empty(), "A literal integral should have no path: {:?}", path);

            let source = raw.source(token);
            let int = parse_integral_impl(source);

            Some((GenericVariable::Literal(Literal::Integral(int), token.span()), token.span()))
        },
        Kind::NameValue => {
            assert!(path.is_empty(), "A value should have no path: {:?}", path);
    
            Some((GenericVariable::Value(raw.resolve_variable(token)), token.span()))
        },
        Kind::NameType => {
            let typ = typ::try_parse_type(raw, store, path).expect("Type!");
            let variable = GenericVariable::Type(typ);
            let range = variable.range(&*store.borrow());

            Some((variable, range))
        },
        _ => unreachable!("Unexpected token: {:?}", token),
    }
}

fn parse_integral_impl(raw: &[u8]) -> i64 {
    let mut value = 0;

    for byte in raw {
        match *byte {
            b'0'..=b'9' => {
                value *= 10;
                value += (byte - b'0') as i64;
            },
            b'_' => (),
            _ => unreachable!("Not an integral byte"),
        }
    }

    value
}

#[cfg(test)]
mod tests {
    use std::ops;
    use crate::model::ast::*;
    use super::super::com::tests::Env;

    #[test]
    fn generic_parameters_none() {
        let env = LocalEnv::new(b"{}");

        assert_eq!(env.actual_generic_parameters(), env.expected_module());
    }

    #[test]
    fn generic_parameters_empty() {
        let env = LocalEnv::new(b"[] {}");
        let g = env.factory().generic();

        g.parameters()
            .brackets(0, 1)
            .build();

        assert_eq!(env.actual_generic_parameters(), env.expected_module());
    }

    #[test]
    fn generic_parameters_types() {
        let env = LocalEnv::new(b"[T, N] {}");
        let g = env.factory().generic();

        g.parameters()
            .push(g.parameter(1, 1))
            .push(g.parameter(4, 1))
            .build();

        assert_eq!(env.actual_generic_parameters(), env.expected_module());
    }

    #[test]
    fn generic_parameters_values() {
        let env = LocalEnv::new(b" [t, n] {}");
        let g = env.factory().generic();

        g.parameters()
            .push(g.parameter(2, 1))
            .push(g.parameter(5, 1))
            .build();

        assert_eq!(env.actual_generic_parameters(), env.expected_module());
    }

    #[test]
    fn generic_variables_none() {
        let env = LocalEnv::new(b"{}");

        assert_eq!(env.actual_generic_variables(), env.expected_module());
    }

    #[test]
    fn generic_variables_empty() {
        let env = LocalEnv::new(b"[] {}");
        let g = env.factory().generic();

        g.variables()
            .brackets(0, 1)
            .build();

        assert_eq!(env.actual_generic_variables(), env.expected_module());
    }

    #[test]
    fn generic_variables_literals() {
        let env = LocalEnv::new(b"[true, false, 42] {}");
        let g = env.factory().generic();

        g.variables()
            .push(g.variable_literal(Literal::Bool(true), 1, 4))
            .push(g.variable_literal(Literal::Bool(false), 7, 5))
            .push(g.variable_literal(Literal::Integral(42), 14, 2))
            .build();

        assert_eq!(env.actual_generic_variables(), env.expected_module());
    }

    #[test]
    fn generic_variables_types() {
        let env = LocalEnv::new(b"[T, N] {}");
        let (_, g, _, _, _, t, _) = env.factories();

        g.variables()
            .push(g.variable_type(t.simple(1, 1)))
            .push(g.variable_type(t.simple(4, 1)))
            .build();

        assert_eq!(env.actual_generic_variables(), env.expected_module());
    }

    #[test]
    fn generic_variables_complex_types() {
        let env = LocalEnv::new(b"[std::option::Option[std::array::Array[T, n]] ,] {}");
        let (_, g, _, _, _, t, _) = env.factories();

        let option_path = g.path().push(1, 3).push(6, 6).build();
        let array_path = g.path().push(21, 3).push(26, 5).build();

        let kv = g.variables()
            .push(g.variable_type(t.simple(39, 1)))
            .push(g.variable_value(42, 1))
            .build();

        let array = g.variables()
            .push(g.variable_type(t.generic(33, 5).path(array_path).variables(kv).build()))
            .build();

        g.variables()
            .push(g.variable_type(t.generic(14, 6).path(option_path).variables(array).build()))
            .comma(46)
            .build();

        assert_eq!(env.actual_generic_variables(), env.expected_module());
    }

    struct LocalEnv { env: Env, }

    impl LocalEnv {
        fn new(source: &[u8]) -> LocalEnv {
            LocalEnv { env: Env::new(source), }
        }

        fn actual_generic_parameters(&self) -> Module {
            let mut raw = self.env.raw();
            super::try_parse_generic_parameters(&mut raw);
            let result = self.env.actual_module().borrow().clone();
            println!("actual_generic_parameters: {:#?}", result);
            println!();
            result
        }

        fn actual_generic_variables(&self) -> Module {
            let mut raw = self.env.raw();
            let raw = &mut raw;
            super::try_parse_generic_variables(raw, raw.module());
            let result = self.env.actual_module().borrow().clone();
            println!("actual_generic_variables: {:#?}", result);
            println!();
            result
        }

        fn expected_module(&self) -> Module {
            let result = self.env.expected_module().borrow().clone();
            println!("expected_module: {:#?}", result);
            println!();
            result
        }
    }

    impl ops::Deref for LocalEnv {
        type Target = Env;

        fn deref(&self) -> &Env { &self.env }
    }
}
