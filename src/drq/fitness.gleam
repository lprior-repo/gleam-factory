import gleam/float
import gleam/int
import gleam/list
import gleam/result

pub type Order {
  Lt
  Eq
  Gt
}

pub type Fitness {
  Fitness(
    score: Float,
    test_pass_rate: Float,
    regression_count: Int,
    coverage: Float,
    performance: Float,
    stability: Float,
  )
}

pub type TestResult {
  TestResult(passed: Bool, duration_ms: Int, coverage_delta: Float)
}

pub type GatesResult {
  GatesResult(
    performance_gate_passed: Bool,
    performance_limit_ms: Int,
    actual_duration_ms: Int,
    coverage_gate_passed: Bool,
    coverage_threshold: Float,
    actual_coverage: Float,
  )
}

pub type FitnessHistory {
  FitnessHistory(iteration: Int, fitness: Fitness, timestamp: String)
}

pub fn calculate_fitness(
  test_results: List(TestResult),
  gates_result: GatesResult,
  history: List(FitnessHistory),
) -> Fitness {
  let test_pass_rate = calculate_test_pass_rate(test_results)
  let regression_count = calculate_regression_count(test_results, history)
  let coverage = gates_result.actual_coverage
  let performance = calculate_performance_score(gates_result)
  let stability = calculate_stability_score(history)

  let score =
    weighted_score([
      #("test_pass_rate", test_pass_rate, 0.35),
      #("coverage", coverage, 0.25),
      #("performance", performance, 0.2),
      #("stability", stability, 0.2),
    ])
    |> apply_regression_penalty(regression_count)

  Fitness(
    score:,
    test_pass_rate:,
    regression_count:,
    coverage:,
    performance:,
    stability:,
  )
}

fn calculate_test_pass_rate(results: List(TestResult)) -> Float {
  case results {
    [] -> 0.0
    _ -> {
      let passed = results |> list.filter(fn(r) { r.passed }) |> list.length
      let total = list.length(results)
      int.to_float(passed) /. int.to_float(total)
    }
  }
}

fn calculate_regression_count(
  results: List(TestResult),
  history: List(FitnessHistory),
) -> Int {
  case history {
    [] -> 0
    _ -> {
      let prev_count =
        history
        |> list.first
        |> result.map(fn(h) { h.fitness.regression_count })
        |> result.unwrap(0)

      let current_failures =
        results |> list.filter(fn(r) { !r.passed }) |> list.length
      int.max(current_failures - prev_count, 0)
    }
  }
}

fn calculate_performance_score(gates: GatesResult) -> Float {
  let ratio =
    int.to_float(gates.actual_duration_ms)
    /. int.to_float(gates.performance_limit_ms)

  case ratio <=. 1.0 {
    True -> {
      let margin = 1.0 -. ratio
      1.0 +. margin *. 0.5
    }
    False -> float.max(1.0 /. ratio, 0.0)
  }
  |> float.min(1.0)
}

fn calculate_stability_score(history: List(FitnessHistory)) -> Float {
  case list.length(history) {
    0 | 1 -> 1.0
    n -> {
      let take_count = int.min(n, 10)
      let recent =
        history |> list.take(take_count) |> list.map(fn(h) { h.fitness.score })
      let avg = average(recent)
      let variance = variance_calc(recent, avg)
      float.max(1.0 -. variance, 0.0)
    }
  }
}

fn apply_regression_penalty(score: Float, regressions: Int) -> Float {
  case regressions {
    0 -> score
    n -> {
      let penalty = case float.power(0.9, int.to_float(n)) {
        Ok(p) -> p
        Error(_) -> 0.9
      }
      score *. penalty
    }
  }
}

fn weighted_score(weights: List(#(String, Float, Float))) -> Float {
  weights
  |> list.map(fn(w) {
    let #(_, value, weight) = w
    value *. weight
  })
  |> list.fold(from: 0.0, with: fn(acc, x) { acc +. x })
}

fn average(values: List(Float)) -> Float {
  case values {
    [] -> 0.0
    _ -> {
      let sum = list.fold(values, from: 0.0, with: fn(acc, x) { acc +. x })
      sum /. int.to_float(list.length(values))
    }
  }
}

fn variance_calc(values: List(Float), avg: Float) -> Float {
  case values {
    [] -> 0.0
    _ -> {
      let sum_squared_diff =
        values
        |> list.map(fn(v) {
          let diff = v -. avg
          diff *. diff
        })
        |> list.fold(from: 0.0, with: fn(acc, x) { acc +. x })

      sum_squared_diff /. int.to_float(list.length(values))
    }
  }
}

pub fn compare_fitness(a: Fitness, b: Fitness) -> Order {
  let sa = a.score
  let sb = b.score

  case sa >. sb {
    True -> Gt
    False ->
      case sb >. sa {
        True -> Lt
        False -> Eq
      }
  }
}

pub fn is_improvement(old: Fitness, new: Fitness) -> Bool {
  new.score >. old.score
}

pub fn fitness_trend(history: List(FitnessHistory)) -> List(Float) {
  history |> list.map(fn(h) { h.fitness.score })
}

pub fn add_to_history(
  history: List(FitnessHistory),
  iteration: Int,
  fitness: Fitness,
  timestamp: String,
) -> List(FitnessHistory) {
  let entry = FitnessHistory(iteration:, fitness:, timestamp:)
  [entry, ..history]
}
