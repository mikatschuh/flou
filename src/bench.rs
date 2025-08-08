//! Benchmarking module of the flou compiler

use std::time::Instant;

/// Benchmarks a given function - and returns the average time of execution
pub fn bench(func: impl Fn()) -> f64 {
    let now = Instant::now();
    func();
    let time = now.elapsed().as_nanos();
    let number_of_executions = 1_000_000_000 / time;
    let now = Instant::now();
    (0..number_of_executions).for_each(|_| func());
    now.elapsed().as_secs_f64() / number_of_executions as f64
}
