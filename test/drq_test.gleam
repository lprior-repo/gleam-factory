import drq/fitness
import drq/regression
import drq/types
import gleam/list
import gleeunit/should

pub fn new_test_bank_test() {
  let bank = types.new_test_bank()
  bank.tests |> should.equal([])
}

pub fn add_test_to_bank_test() {
  let bank = types.new_test_bank()
  let t =
    types.TestCase(
      id: "test1",
      name: "My Test",
      scenario: "does something",
      expected_outcome: "passes",
    )

  let updated = types.add_test(bank, t)
  updated.tests |> list.length |> should.equal(1)
}

pub fn fitness_score_ratio_test() {
  let score =
    types.FitnessScore(
      passed: 8,
      total: 10,
      coverage: 0.8,
      performance: 1.0,
      regressions_avoided: 0,
    )

  types.fitness_score_ratio(score) |> should.equal(0.8)
}

pub fn fitness_score_zero_total_test() {
  let score =
    types.FitnessScore(
      passed: 0,
      total: 0,
      coverage: 0.0,
      performance: 1.0,
      regressions_avoided: 0,
    )

  types.fitness_score_ratio(score) |> should.equal(0.0)
}

pub fn calculate_fitness_test() {
  let test_results = [
    fitness.TestResult(passed: True, duration_ms: 100, coverage_delta: 0.1),
    fitness.TestResult(passed: True, duration_ms: 100, coverage_delta: 0.1),
    fitness.TestResult(passed: False, duration_ms: 100, coverage_delta: 0.0),
  ]

  let gates =
    fitness.GatesResult(
      performance_gate_passed: True,
      performance_limit_ms: 1000,
      actual_duration_ms: 300,
      coverage_gate_passed: True,
      coverage_threshold: 0.8,
      actual_coverage: 0.85,
    )

  let result = fitness.calculate_fitness(test_results, gates, [])

  result.test_pass_rate |> should.equal(0.6666666666666666)
  result.score |> should.not_equal(0.0)
}

pub fn regression_detection_test() {
  let patch =
    regression.Patch(
      commit_hash: "abc123",
      patch_id: "patch1",
      timestamp: "2024-01-01",
    )

  let old_results = [
    regression.TestResult(
      test_case: regression.TestCase(
        test_name: "test1",
        file_path: "test.gleam",
        line_number: 10,
      ),
      passed: True,
      output: "ok",
      executed_at: "2024-01-01",
    ),
  ]

  let new_results = [
    regression.TestResult(
      test_case: regression.TestCase(
        test_name: "test1",
        file_path: "test.gleam",
        line_number: 10,
      ),
      passed: False,
      output: "Error: assertion failed",
      executed_at: "2024-01-02",
    ),
  ]

  let old_run = regression.TestRun(patch: patch, results: old_results)
  let new_run = regression.TestRun(patch: patch, results: new_results)

  let regressions = regression.detect_regressions(old_run, new_run)

  regressions |> list.length |> should.equal(1)
}

pub fn permanent_regression_test() {
  let patch =
    regression.Patch(
      commit_hash: "abc123",
      patch_id: "patch1",
      timestamp: "2024-01-01",
    )

  let failure =
    regression.TestResult(
      test_case: regression.TestCase(
        test_name: "test1",
        file_path: "test.gleam",
        line_number: 10,
      ),
      passed: False,
      output: "panic: fatal error",
      executed_at: "2024-01-01",
    )

  let reg = regression.promote_to_regression(failure, patch)

  regression.is_permanent(reg) |> should.be_true
}

pub fn minor_regression_test() {
  let patch =
    regression.Patch(
      commit_hash: "abc123",
      patch_id: "patch1",
      timestamp: "2024-01-01",
    )

  let failure =
    regression.TestResult(
      test_case: regression.TestCase(
        test_name: "test1",
        file_path: "test.gleam",
        line_number: 10,
      ),
      passed: False,
      output: "minor issue",
      executed_at: "2024-01-01",
    )

  let reg = regression.promote_to_regression(failure, patch)

  regression.is_permanent(reg) |> should.be_false
}

pub fn fitness_comparison_test() {
  let test_results = []
  let gates =
    fitness.GatesResult(
      performance_gate_passed: True,
      performance_limit_ms: 1000,
      actual_duration_ms: 100,
      coverage_gate_passed: True,
      coverage_threshold: 0.8,
      actual_coverage: 0.9,
    )

  let fit1 = fitness.calculate_fitness(test_results, gates, [])
  let fit2 = fitness.calculate_fitness(test_results, gates, [])

  fitness.compare_fitness(fit1, fit2) |> should.equal(fitness.Eq)
}

pub fn fitness_improvement_test() {
  let test_results = [
    fitness.TestResult(passed: True, duration_ms: 100, coverage_delta: 0.1),
  ]

  let gates =
    fitness.GatesResult(
      performance_gate_passed: True,
      performance_limit_ms: 1000,
      actual_duration_ms: 100,
      coverage_gate_passed: True,
      coverage_threshold: 0.8,
      actual_coverage: 0.9,
    )

  let old =
    fitness.Fitness(
      score: 0.5,
      test_pass_rate: 0.5,
      regression_count: 0,
      coverage: 0.5,
      performance: 0.5,
      stability: 0.5,
    )

  let new = fitness.calculate_fitness(test_results, gates, [])

  fitness.is_improvement(old, new) |> should.be_true
}
