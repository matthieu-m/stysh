//! Integration Tests for the `pass::int` module.

extern crate stysh_compile;

use stysh_compile::basic::mem;

mod utils;

#[test]
fn fibonacci_recursive() {
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
        "Int(21)"
    )
}

#[test]
fn fibonnacci_iterative() {
    assert_eq!(
        interpret(
            b"
            :fun fib(n: Int) -> Int {
                :var (current, next) := (0, 1);
                :loop {
                    :set n := :if n == 0 {
                        :return current;
                    } :else {
                        :set (current, next) := (next, current + next);
                        n - 1
                    };
                }
            }
            fib(8)
            "
        ),
        "Int(21)"
    )
}

fn interpret(raw: &[u8]) -> String {
    let interner = mem::Interner::new();
    let value = utils::interpret(raw, &interner);

    format!("{:?}", value)
}
