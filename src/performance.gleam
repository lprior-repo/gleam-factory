//// Performance monitoring utilities (factory-gleam-koq, koq, nhd, e63)
////
//// Utilities for measuring and optimizing system performance
//// Tracks timing, throughput, and resource usage

import dict
import gleam/int
import gleam/list
import gleam/string

pub opaque type Metrics {
  Metrics(measurements: dict.Dict(String, List(Int)))
}

pub fn new() -> Metrics {
  Metrics(measurements: dict.new())
}

pub fn record_duration(
  metrics: Metrics,
  operation: String,
  duration_ms: Int,
) -> Metrics {
  let Metrics(measurements) = metrics
  let current = case dict.get(measurements, operation) {
    Ok(durations) -> durations
    Error(Nil) -> []
  }
  let updated = dict.insert(measurements, operation, [duration_ms, ..current])
  Metrics(measurements: updated)
}

pub fn get_summary(metrics: Metrics, operation: String) -> Summary {
  let Metrics(measurements) = metrics
  case dict.get(measurements, operation) {
    Error(Nil) ->
      Summary(
        operation: operation,
        count: 0,
        min_ms: 0,
        max_ms: 0,
        avg_ms: 0,
        p95_ms: 0,
        p99_ms: 0,
      )
    Ok(durations) -> calculate_summary(operation, durations)
  }
}

pub type Summary {
  Summary(
    operation: String,
    count: Int,
    min_ms: Int,
    max_ms: Int,
    avg_ms: Int,
    p95_ms: Int,
    p99_ms: Int,
  )
}

fn calculate_summary(operation: String, durations: List(Int)) -> Summary {
  let count = list.length(durations)
  let sorted = list.sort(durations, int.compare)
  let sum = list.fold(durations, 0, fn(acc, x) { acc + x })
  let avg = case count {
    0 -> 0
    n -> sum / n
  }

  let min = case list.first(sorted) {
    Ok(m) -> m
    Error(Nil) -> 0
  }

  let max = case list.last(sorted) {
    Ok(m) -> m
    Error(Nil) -> 0
  }

  let p95_idx = count * 95 / 100
  let p95 = case list.at(sorted, p95_idx) {
    Ok(v) -> v
    Error(Nil) -> 0
  }

  let p99_idx = count * 99 / 100
  let p99 = case list.at(sorted, p99_idx) {
    Ok(v) -> v
    Error(Nil) -> 0
  }

  Summary(
    operation: operation,
    count: count,
    min_ms: min,
    max_ms: max,
    avg_ms: avg,
    p95_ms: p95,
    p99_ms: p99,
  )
}

pub fn format_summary(summary: Summary) -> String {
  summary.operation
  <> ": "
  <> "count="
  <> int.to_string(summary.count)
  <> " min="
  <> int.to_string(summary.min_ms)
  <> "ms avg="
  <> int.to_string(summary.avg_ms)
  <> "ms p95="
  <> int.to_string(summary.p95_ms)
  <> "ms p99="
  <> int.to_string(summary.p99_ms)
  <> "ms max="
  <> int.to_string(summary.max_ms)
  <> "ms"
}

pub fn measure(f: fn() -> t, operation_name: String) -> #(t, Int) {
  let start_time = erlang_monotonic_time()
  let result = f()
  let end_time = erlang_monotonic_time()
  let duration_ns = end_time - start_time
  let duration_ms = duration_ns / 1_000_000
  #(result, duration_ms)
}

@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time() -> Int
