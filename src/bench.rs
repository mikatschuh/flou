//! Benchmarking module of the flou compiler

#[cfg(test)]
/// Benchmarks a given function - and returns the average time of execution
pub fn bench(mut func: impl FnMut()) -> f64 {
    use std::time::Instant;
    let now = Instant::now();
    func();
    let time = now.elapsed().as_nanos();
    let number_of_executions = 10_000_000_000 / time;
    let now = Instant::now();
    (0..number_of_executions).for_each(|_| func());
    now.elapsed().as_secs_f64() / number_of_executions as f64
}
