/// Integration Tests for the `pass::int` module.

extern crate stysh_compile;

use stysh_compile::basic::mem;

mod utils;

#[test]
fn fibonacci() {
    assert_eq!(
        interpret(
            b"
            :fun fib(current: Int, next: Int, count: Int) -> Int {
                :if count == 0 {
                    current
                } :else {
                    fib(next, current + next, count - 1)
                }
            }
            fib(0, 1, 8)
            "
        ),
        "BuiltinVal(Int(21))"
    )
}

fn interpret(raw: &[u8]) -> String {
    let arena = mem::Arena::new();
    let value = utils::interpret(raw, &arena);

    format!("{:?}", value.expr)
}
