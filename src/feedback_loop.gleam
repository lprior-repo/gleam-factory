//// Feedback Loop - The core auto-heal mechanism.
////
//// Test failure -> Extract error -> LLM retry -> Iterate until pass or budget exhausted.
//// This is the nervous system that connects tests to LLM to evolution.

import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import llm
import llm_router
import process as shell_process

/// Budget tracking for token efficiency
pub type TokenBudget {
  TokenBudget(
    max_tokens: Int,
    used_tokens: Int,
    max_iterations: Int,
    current_iteration: Int,
  )
}

/// Feedback from a failed test run
pub type TestFeedback {
  TestFeedback(
    stage: String,
    exit_code: Int,
    stdout: String,
    stderr: String,
    error_summary: String,
  )
}

/// Result of a feedback loop iteration
pub type IterationResult {
  Fixed(response: String, tokens_used: Int)
  StillFailing(feedback: TestFeedback, tokens_used: Int)
  BudgetExhausted(iterations: Int, tokens_used: Int)
  LLMError(reason: String)
}

/// Configuration for the feedback loop
pub type FeedbackConfig {
  FeedbackConfig(
    router_config: llm_router.RouterConfig,
    max_iterations: Int,
    max_tokens_per_task: Int,
    context_window: Int,
  )
}

/// Create a new token budget
pub fn new_budget(max_tokens: Int, max_iterations: Int) -> TokenBudget {
  TokenBudget(
    max_tokens:,
    used_tokens: 0,
    max_iterations:,
    current_iteration: 0,
  )
}

/// Check if budget allows another iteration
pub fn has_budget(budget: TokenBudget) -> Bool {
  budget.used_tokens < budget.max_tokens
  && budget.current_iteration < budget.max_iterations
}

/// Update budget after an LLM call
pub fn spend_tokens(budget: TokenBudget, tokens: Int) -> TokenBudget {
  TokenBudget(
    ..budget,
    used_tokens: budget.used_tokens + tokens,
    current_iteration: budget.current_iteration + 1,
  )
}

/// Extract actionable error from test output
pub fn extract_error(
  stdout: String,
  stderr: String,
  exit_code: Int,
) -> TestFeedback {
  let combined = stdout <> "\n" <> stderr
  let summary = extract_error_summary(combined)

  TestFeedback(
    stage: "test",
    exit_code:,
    stdout:,
    stderr:,
    error_summary: summary,
  )
}

/// Parse error output to find the key failure message
fn extract_error_summary(output: String) -> String {
  // Look for common error patterns
  let lines = string.split(output, "\n")

  // Find first line with error indicators
  let error_line = find_error_line(lines)

  case error_line {
    Some(line) -> line
    None -> truncate_output(output, 500)
  }
}

fn find_error_line(lines: List(String)) -> Option(String) {
  case lines {
    [] -> None
    [line, ..rest] -> {
      let lower = string.lowercase(line)
      case
        string.contains(lower, "error")
        || string.contains(lower, "failed")
        || string.contains(lower, "panic")
        || string.contains(lower, "exception")
        || string.contains(lower, "assertion")
      {
        True -> Some(string.trim(line))
        False -> find_error_line(rest)
      }
    }
  }
}

fn truncate_output(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len) <> "..."
    False -> s
  }
}

/// Format error context for LLM consumption (token-efficient)
pub fn format_for_llm(
  feedback: TestFeedback,
  task_spec: String,
  previous_attempt: Option(String),
) -> String {
  let base =
    "Task: " <> task_spec <> "\n\nTest failed with:\n" <> feedback.error_summary

  case previous_attempt {
    Some(prev) ->
      base
      <> "\n\nPrevious attempt that failed:\n"
      <> truncate_output(prev, 200)
      <> "\n\nFix the issue. Be precise."
    None -> base <> "\n\nWrite code to fix this. Be minimal."
  }
}

/// Run one iteration of the feedback loop
pub fn iterate(
  config: FeedbackConfig,
  task_spec: String,
  feedback: TestFeedback,
  previous_attempt: Option(String),
  role: llm.Role,
) -> IterationResult {
  let prompt = format_for_llm(feedback, task_spec, previous_attempt)
  let system = llm.system_prompt(role)

  let request =
    llm.new_request("default", prompt, config.context_window)
    |> llm.with_system_prompt(system)
    |> llm.with_temperature(0.3)
  // Lower temp for fixes

  case llm_router.call(config.router_config, request, role) {
    Ok(response) -> {
      let tokens = response.usage.total_tokens
      Fixed(response: response.content, tokens_used: tokens)
    }
    Error(llm.NetworkError(e)) -> LLMError("Network: " <> e)
    Error(llm.ParseError(e)) -> LLMError("Parse: " <> e)
    Error(llm.RateLimitError(wait)) ->
      LLMError("Rate limited, wait " <> int.to_string(wait) <> "s")
    Error(llm.AuthError(e)) -> LLMError("Auth: " <> e)
  }
}

/// Run a test command and capture output
pub fn run_test(
  command: String,
  args: List(String),
  cwd: String,
) -> TestFeedback {
  case shell_process.run_command(command, args, cwd) {
    Ok(shell_process.Success(stdout, stderr, code)) ->
      case code {
        0 ->
          TestFeedback(
            stage: "test",
            exit_code: 0,
            stdout:,
            stderr:,
            error_summary: "",
          )
        _ -> extract_error(stdout, stderr, code)
      }
    Ok(shell_process.Failure(err, code)) ->
      TestFeedback(
        stage: "test",
        exit_code: code,
        stdout: "",
        stderr: err,
        error_summary: err,
      )
    Error(e) ->
      TestFeedback(
        stage: "test",
        exit_code: 1,
        stdout: "",
        stderr: e,
        error_summary: e,
      )
  }
}

/// Check if test passed
pub fn test_passed(feedback: TestFeedback) -> Bool {
  feedback.exit_code == 0
}

/// The main feedback loop - iterate until pass or budget exhausted
pub fn run_loop(
  config: FeedbackConfig,
  task_spec: String,
  test_cmd: String,
  test_args: List(String),
  cwd: String,
  apply_fix: fn(String) -> Result(Nil, String),
) -> LoopResult {
  run_loop_internal(
    config,
    task_spec,
    test_cmd,
    test_args,
    cwd,
    apply_fix,
    new_budget(config.max_tokens_per_task, config.max_iterations),
    None,
  )
}

pub type LoopResult {
  Success(iterations: Int, total_tokens: Int)
  Failure(reason: String, iterations: Int, total_tokens: Int)
}

fn run_loop_internal(
  config: FeedbackConfig,
  task_spec: String,
  test_cmd: String,
  test_args: List(String),
  cwd: String,
  apply_fix: fn(String) -> Result(Nil, String),
  budget: TokenBudget,
  last_attempt: Option(String),
) -> LoopResult {
  // Check budget first
  case has_budget(budget) {
    False ->
      Failure("Budget exhausted", budget.current_iteration, budget.used_tokens)
    True -> {
      // Run tests
      let feedback = run_test(test_cmd, test_args, cwd)

      case test_passed(feedback) {
        True -> Success(budget.current_iteration, budget.used_tokens)
        False -> {
          // Get fix from LLM
          case
            iterate(config, task_spec, feedback, last_attempt, llm.Implementer)
          {
            Fixed(response, tokens) -> {
              let new_budget = spend_tokens(budget, tokens)
              // Apply the fix
              case apply_fix(response) {
                Ok(_) ->
                  // Recurse with new attempt
                  run_loop_internal(
                    config,
                    task_spec,
                    test_cmd,
                    test_args,
                    cwd,
                    apply_fix,
                    new_budget,
                    Some(response),
                  )
                Error(e) ->
                  Failure(
                    "Apply failed: " <> e,
                    new_budget.current_iteration,
                    new_budget.used_tokens,
                  )
              }
            }
            StillFailing(_, tokens) -> {
              let new_budget = spend_tokens(budget, tokens)
              Failure(
                "Still failing after fix",
                new_budget.current_iteration,
                new_budget.used_tokens,
              )
            }
            BudgetExhausted(iters, tokens) ->
              Failure("Budget exhausted", iters, tokens)
            LLMError(reason) ->
              Failure(
                "LLM error: " <> reason,
                budget.current_iteration,
                budget.used_tokens,
              )
          }
        }
      }
    }
  }
}

/// Get budget utilization as percentage
pub fn budget_utilization(budget: TokenBudget) -> Int {
  case budget.max_tokens {
    0 -> 100
    max -> budget.used_tokens * 100 / max
  }
}

/// Format budget status for logging
pub fn format_budget(budget: TokenBudget) -> String {
  "Iteration "
  <> int.to_string(budget.current_iteration)
  <> "/"
  <> int.to_string(budget.max_iterations)
  <> " | Tokens "
  <> int.to_string(budget.used_tokens)
  <> "/"
  <> int.to_string(budget.max_tokens)
  <> " ("
  <> int.to_string(budget_utilization(budget))
  <> "%)"
}
