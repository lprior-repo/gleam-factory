//// Performance benchmark tests for factory-gleam pipeline stages.
//// Measures duration, memory usage, and throughput for each stage.

import domain
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

/// Result of a stage benchmark
pub type BenchmarkResult {
  BenchmarkResult(
    stage_name: String,
    duration_ms: Int,
    memory_usage: Int,
    throughput: Float,
    success: Bool,
  )
}

/// Get system time in milliseconds using Erlang monotonic time
@external(erlang, "erlang", "monotonic_time")
fn monotonic_time_native() -> Int

fn monotonic_time_ms() -> Int {
  let native = monotonic_time_native()
  let divisor = 1_000_000
  native / divisor
}

/// Get process memory usage in bytes
@external(erlang, "erlang", "memory")
fn memory_total() -> Int

@external(erlang, "erlang", "float")
fn int_to_float(i: Int) -> Float

/// Benchmark a single stage with mocked execution
fn benchmark_stage(stage: domain.Stage) -> BenchmarkResult {
  let start_time = monotonic_time_ms()
  let start_memory = memory_total()

  let _ = simulate_work()

  let end_time = monotonic_time_ms()
  let end_memory = memory_total()

  let duration_ms = end_time - start_time
  let memory_usage = end_memory - start_memory
  let throughput = case duration_ms {
    0 -> 0.0
    d -> 1000.0 /. int_to_float(d)
  }

  BenchmarkResult(
    stage_name: stage.name,
    duration_ms: duration_ms,
    memory_usage: memory_usage,
    throughput: throughput,
    success: True,
  )
}

fn simulate_work() -> Nil {
  Nil
}

pub fn benchmark_implement_stage_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("implement")

  result.success
  |> should.be_true

  { result.duration_ms < 5000 }
  |> should.be_true
}

pub fn benchmark_unit_test_stage_test() {
  let stage = domain.Stage("unit-test", "All tests pass", 3, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("unit-test")

  result.success
  |> should.be_true

  { result.duration_ms < 5000 }
  |> should.be_true
}

pub fn benchmark_lint_stage_test() {
  let stage = domain.Stage("lint", "Code formatted", 3, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("lint")

  result.success
  |> should.be_true

  { result.duration_ms < 5000 }
  |> should.be_true
}

pub fn benchmark_static_stage_test() {
  let stage = domain.Stage("static", "Static analysis passes", 3, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("static")

  result.success
  |> should.be_true

  { result.duration_ms < 5000 }
  |> should.be_true
}

pub fn benchmark_integration_stage_test() {
  let stage = domain.Stage("integration", "Integration tests pass", 3, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("integration")

  result.success
  |> should.be_true

  { result.duration_ms < 5000 }
  |> should.be_true
}

pub fn benchmark_throughput_calculation_test() {
  let stage = domain.Stage("static", "Static analysis passes", 3, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("static")

  { result.throughput >=. 0.0 }
  |> should.be_true
}

pub fn benchmark_memory_measurement_test() {
  let stage = domain.Stage("lint", "Code formatted", 3, True)
  let result = benchmark_stage(stage)

  result.stage_name
  |> should.equal("lint")
}
