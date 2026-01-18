import feedback_loop
import gleam/option.{None, Some}
import gleeunit/should

// Budget tests

pub fn new_budget_test() {
  let budget = feedback_loop.new_budget(10_000, 5)
  should.equal(budget.max_tokens, 10_000)
  should.equal(budget.used_tokens, 0)
  should.equal(budget.max_iterations, 5)
  should.equal(budget.current_iteration, 0)
}

pub fn has_budget_fresh_test() {
  let budget = feedback_loop.new_budget(10_000, 5)
  feedback_loop.has_budget(budget)
  |> should.be_true
}

pub fn has_budget_tokens_exhausted_test() {
  let budget =
    feedback_loop.TokenBudget(
      max_tokens: 100,
      used_tokens: 100,
      max_iterations: 5,
      current_iteration: 1,
    )
  feedback_loop.has_budget(budget)
  |> should.be_false
}

pub fn has_budget_iterations_exhausted_test() {
  let budget =
    feedback_loop.TokenBudget(
      max_tokens: 10_000,
      used_tokens: 500,
      max_iterations: 5,
      current_iteration: 5,
    )
  feedback_loop.has_budget(budget)
  |> should.be_false
}

pub fn spend_tokens_test() {
  let budget = feedback_loop.new_budget(10_000, 5)
  let updated = feedback_loop.spend_tokens(budget, 1500)
  should.equal(updated.used_tokens, 1500)
  should.equal(updated.current_iteration, 1)
}

pub fn budget_utilization_empty_test() {
  let budget = feedback_loop.new_budget(10_000, 5)
  feedback_loop.budget_utilization(budget)
  |> should.equal(0)
}

pub fn budget_utilization_half_test() {
  let budget =
    feedback_loop.TokenBudget(
      max_tokens: 10_000,
      used_tokens: 5000,
      max_iterations: 5,
      current_iteration: 2,
    )
  feedback_loop.budget_utilization(budget)
  |> should.equal(50)
}

pub fn budget_utilization_zero_max_test() {
  let budget =
    feedback_loop.TokenBudget(
      max_tokens: 0,
      used_tokens: 0,
      max_iterations: 5,
      current_iteration: 0,
    )
  feedback_loop.budget_utilization(budget)
  |> should.equal(100)
}

// Error extraction tests

pub fn extract_error_success_test() {
  let feedback = feedback_loop.extract_error("all tests passed", "", 0)
  should.equal(feedback.exit_code, 0)
}

pub fn extract_error_failure_test() {
  let stderr = "error: type mismatch\n  expected Int, got String"
  let feedback = feedback_loop.extract_error("", stderr, 1)
  should.equal(feedback.exit_code, 1)
  should.equal(feedback.error_summary, "error: type mismatch")
}

pub fn test_passed_success_test() {
  let feedback =
    feedback_loop.TestFeedback(
      stage: "test",
      exit_code: 0,
      stdout: "ok",
      stderr: "",
      error_summary: "",
    )
  feedback_loop.test_passed(feedback)
  |> should.be_true
}

pub fn test_passed_failure_test() {
  let feedback =
    feedback_loop.TestFeedback(
      stage: "test",
      exit_code: 1,
      stdout: "",
      stderr: "failed",
      error_summary: "failed",
    )
  feedback_loop.test_passed(feedback)
  |> should.be_false
}

// Format tests

pub fn format_for_llm_no_previous_test() {
  let feedback =
    feedback_loop.TestFeedback(
      stage: "test",
      exit_code: 1,
      stdout: "",
      stderr: "",
      error_summary: "undefined function foo",
    )
  let prompt = feedback_loop.format_for_llm(feedback, "add foo function", None)
  should.be_true(
    prompt
    |> contains("Task: add foo function"),
  )
  should.be_true(
    prompt
    |> contains("undefined function foo"),
  )
}

pub fn format_for_llm_with_previous_test() {
  let feedback =
    feedback_loop.TestFeedback(
      stage: "test",
      exit_code: 1,
      stdout: "",
      stderr: "",
      error_summary: "wrong return type",
    )
  let prompt =
    feedback_loop.format_for_llm(feedback, "fix types", Some("fn foo() { 42 }"))
  should.be_true(
    prompt
    |> contains("Previous attempt"),
  )
}

pub fn format_budget_test() {
  let budget =
    feedback_loop.TokenBudget(
      max_tokens: 10_000,
      used_tokens: 2500,
      max_iterations: 5,
      current_iteration: 2,
    )
  let formatted = feedback_loop.format_budget(budget)
  should.be_true(
    formatted
    |> contains("2/5"),
  )
  should.be_true(
    formatted
    |> contains("2500/10000"),
  )
  should.be_true(
    formatted
    |> contains("25%"),
  )
}

// Helper
fn contains(haystack: String, needle: String) -> Bool {
  case haystack {
    "" -> False
    _ -> {
      case needle {
        "" -> True
        _ -> {
          let h_len = string_length(haystack)
          let n_len = string_length(needle)
          case h_len < n_len {
            True -> False
            False -> check_contains(haystack, needle, 0, h_len - n_len + 1)
          }
        }
      }
    }
  }
}

fn check_contains(haystack: String, needle: String, pos: Int, max: Int) -> Bool {
  case pos >= max {
    True -> False
    False -> {
      let slice = string_slice(haystack, pos, string_length(needle))
      case slice == needle {
        True -> True
        False -> check_contains(haystack, needle, pos + 1, max)
      }
    }
  }
}

@external(erlang, "string", "length")
fn string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn string_slice(s: String, start: Int, len: Int) -> String
