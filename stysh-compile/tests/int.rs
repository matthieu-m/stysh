//! Integration Tests for the `pass::int` module.

extern crate stysh_compile;

use stysh_compile::pass::int;

mod utils;

#[test]
fn fibonacci_recursive() {
    assert_eq!(
        utils::interpret(
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
        int::Value::Int(21)
    );
}

#[test]
fn fibonnacci_iterative() {
    assert_eq!(
        utils::interpret(
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
        int::Value::Int(21)
    );
}

#[test]
fn peano_function() {
    assert_eq!(
        utils::interpret(
            b"
            :rec Peano;

            :ext Peano {
                :fun zero() -> Int { 0 }
                :fun one() -> Int { 1 }
                :fun two() -> Int { one() + one() }
            }

            Peano::two()
            "
        ),
        int::Value::Int(2)
    );
}

#[test]
fn peano_method() {
    assert_eq!(
        utils::interpret(
            b"
            :rec Peano;

            :ext Peano {
                :fun zero(self: Peano) -> Int { 0 }
                :fun one(self: Peano) -> Int { 1 }
                :fun two(self: Peano) -> Int { self.one() + self.one() }
            }

            Peano().two()
            "
        ),
        int::Value::Int(2)
    );
}
