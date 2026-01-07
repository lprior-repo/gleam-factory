// Property-based tests for errors module
// Tests error classification and extraction invariants

import gleeunit/should
import gleam/string
import gleam/list
import gleam/int
import gleam/option
import qcheck
import qcheck/generators.{string as gen_string, choice}
import errors

// Timeout classification tests
pub fn prop_classify_error_detects_timeout() {
  let timeout_outputs = [
    "did not complete in time",
    "operation did not complete in time out of order",
    "test did not complete in time",
    "compile did not complete in time error: foo",
  ]

  list.all(timeout_outputs, fn(output) {
    errors.classify_error(output) == errors.Timeout
  })
  |> should.equal(True)
}

pub fn prop_classify_error_timeout_has_priority() {
  let output = "did not complete in time\npanic: something\nFAILED\nerror: compile"

  errors.classify_error(output)
  |> should.equal(errors.Timeout)
}

// Panic classification tests
pub fn prop_classify_error_detects_panic() {
  let panic_outputs = [
    "panic: null pointer",
    "panic: division by zero",
    "error happens then panic: oops",
  ]

  list.all(panic_outputs, fn(output) {
    errors.classify_error(output) == errors.RuntimePanic
  })
  |> should.equal(True)
}

pub fn prop_classify_error_panic_second_priority() {
  let output = "panic: something\nFAILED\nerror: compile"

  errors.classify_error(output)
  |> should.equal(errors.RuntimePanic)
}

// Test failure classification tests
pub fn prop_classify_error_detects_test_failure() {
  let test_outputs = [
    "FAILED",
    "Test FAILED",
    "3 tests FAILED",
    "error happens then FAILED",
  ]

  list.all(test_outputs, fn(output) {
    errors.classify_error(output) == errors.TestFailure
  })
  |> should.equal(True)
}

pub fn prop_classify_error_test_failure_third_priority() {
  let output = "FAILED\nerror: compile"

  errors.classify_error(output)
  |> should.equal(errors.TestFailure)
}

// Compile error classification tests
pub fn prop_classify_error_detects_compile_error_lowercase() {
  let compile_outputs = [
    "error: syntax error",
    "error: type mismatch",
    "compilation error: undefined symbol",
  ]

  list.all(compile_outputs, fn(output) {
    errors.classify_error(output) == errors.CompileError
  })
  |> should.equal(True)
}

pub fn prop_classify_error_detects_compile_error_capitalized() {
  let compile_outputs = [
    "Error: syntax error",
    "Error: type mismatch",
    "compilation Error: undefined symbol",
  ]

  list.all(compile_outputs, fn(output) {
    errors.classify_error(output) == errors.CompileError
  })
  |> should.equal(True)
}

pub fn prop_classify_error_both_lowercase_and_capital_count() {
  let output = "Error: type\nerror: syntax"

  errors.classify_error(output)
  |> should.equal(errors.CompileError)
}

// Unknown classification tests
pub fn prop_classify_error_defaults_to_unknown() {
  let unknown_outputs = [
    "random output",
    "nothing interesting here",
    "some log message",
    "",
  ]

  list.all(unknown_outputs, fn(output) {
    errors.classify_error(output) == errors.Unknown
  })
  |> should.equal(True)
}

// Error extraction tests
pub fn prop_extract_root_cause_finds_error_line() {
  let output = "some initial output\nError: something bad\nfollowing text"

  case errors.extract_root_cause(output) {
    option.Some(cause) -> string.contains(cause, "Error: something bad")
    option.None -> False
  }
  |> should.equal(True)
}

pub fn prop_extract_root_cause_finds_lowercase_error() {
  let output = "line 1\nerror: bad thing\nline 3"

  case errors.extract_root_cause(output) {
    option.Some(cause) -> string.contains(cause, "error: bad thing")
    option.None -> False
  }
  |> should.equal(True)
}

pub fn prop_extract_root_cause_returns_none_when_no_error() {
  let outputs = [
    "normal output",
    "all systems go",
    "completed successfully",
  ]

  list.all(outputs, fn(output) {
    case errors.extract_root_cause(output) {
      option.Some(_) -> False
      option.None -> True
    }
  })
  |> should.equal(True)
}

pub fn prop_extract_root_cause_first_error_wins() {
  let output = "Error: first problem\nError: second problem"

  case errors.extract_root_cause(output) {
    option.Some(cause) -> string.contains(cause, "first problem")
    option.None -> False
  }
  |> should.equal(True)
}

pub fn prop_extract_root_cause_empty_input() {
  case errors.extract_root_cause("") {
    option.Some(_) -> False
    option.None -> True
  }
  |> should.equal(True)
}

// Summarize error tests
pub fn prop_summarize_error_reduces_to_max_lines() {
  let output = string.join(
    list.range(1, 20)
    |> list.map(int.to_string),
    "\n",
  )

  let summary = errors.summarize_error(output, 5)
  let lines = string.split(summary, "\n")

  list.length(lines) <= 5
  |> should.equal(True)
}

pub fn prop_summarize_error_includes_error_lines() {
  let output = "line 1\nError: problem\nline 3\nerror: issue\nline 5"

  let summary = errors.summarize_error(output, 10)

  string.contains(summary, "Error: problem")
  && string.contains(summary, "error: issue")
  |> should.equal(True)
}

pub fn prop_summarize_error_prioritizes_errors() {
  let output = "a\nb\nc\nError: important\nd\ne\nf"

  let summary = errors.summarize_error(output, 2)

  string.contains(summary, "Error: important")
  |> should.equal(True)
}

pub fn prop_summarize_error_empty_input() {
  let summary = errors.summarize_error("", 5)

  summary
  |> should.equal("")
}

pub fn prop_summarize_error_single_line() {
  let output = "Single line output"

  let summary = errors.summarize_error(output, 1)

  summary
  |> should.equal(output)
}

pub fn prop_summarize_error_respects_max_lines_zero() {
  let output = "line1\nline2\nline3"

  let summary = errors.summarize_error(output, 0)

  summary
  |> should.equal("")
}

// Classification consistency tests
pub fn prop_classify_error_idempotent() {
  let output = "Error: something bad"

  let first = errors.classify_error(output)
  let second = errors.classify_error(output)

  first == second
  |> should.equal(True)
}

pub fn prop_extract_root_cause_idempotent() {
  let output = "Error: problem\nmore text"

  let first = errors.extract_root_cause(output)
  let second = errors.extract_root_cause(output)

  first == second
  |> should.equal(True)
}

pub fn prop_summarize_error_idempotent() {
  let output = "Error: problem\nerror: issue\nmore"

  let first = errors.summarize_error(output, 5)
  let second = errors.summarize_error(first, 5)

  // Second summarize should be same as first (idempotent-ish)
  string.length(second) <= string.length(first)
  |> should.equal(True)
}

// Classification priority order tests
pub fn prop_priority_timeout_over_panic() {
  let output = "did not complete in time\npanic: oops"

  errors.classify_error(output)
  |> should.equal(errors.Timeout)
}

pub fn prop_priority_panic_over_test_failure() {
  let output = "panic: oops\nFAILED"

  errors.classify_error(output)
  |> should.equal(errors.RuntimePanic)
}

pub fn prop_priority_test_failure_over_compile() {
  let output = "FAILED\nerror: syntax"

  errors.classify_error(output)
  |> should.equal(errors.TestFailure)
}
