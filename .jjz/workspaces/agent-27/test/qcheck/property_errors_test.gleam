// Property-based tests for errors module using qcheck
// Tests error classification and extraction invariants

import errors
import gleam/option.{None, Some}
import gleam/string
import qcheck

// ERROR CLASSIFICATION - Property: timeout pattern always classifies as Timeout
pub fn prop_classify_timeout_keyword__test() {
  use len <- qcheck.given(qcheck.bounded_int(10, 100))
  let prefix = string.repeat("x", len)
  let output = prefix <> "did not complete in time"
  assert case errors.classify_error(output) {
    errors.Timeout -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: panic pattern always classifies as RuntimePanic
pub fn prop_classify_panic_keyword__test() {
  use len <- qcheck.given(qcheck.bounded_int(10, 100))
  let prefix = string.repeat("x", len)
  let output = prefix <> "panic: runtime error"
  assert case errors.classify_error(output) {
    errors.RuntimePanic -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: FAILED pattern always classifies as TestFailure
pub fn prop_classify_test_failure_keyword__test() {
  use len <- qcheck.given(qcheck.bounded_int(10, 100))
  let prefix = string.repeat("x", len)
  let output = prefix <> "FAILED: test did not pass"
  assert case errors.classify_error(output) {
    errors.TestFailure -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: error: pattern classifies as CompileError
pub fn prop_classify_error_lowercase__test() {
  use len <- qcheck.given(qcheck.bounded_int(10, 100))
  let prefix = string.repeat("x", len)
  let output = prefix <> "error: type mismatch"
  assert case errors.classify_error(output) {
    errors.CompileError -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: Error: pattern classifies as CompileError
pub fn prop_classify_error_uppercase__test() {
  use len <- qcheck.given(qcheck.bounded_int(10, 100))
  let prefix = string.repeat("x", len)
  let output = prefix <> "Error: file not found"
  assert case errors.classify_error(output) {
    errors.CompileError -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: unrecognized output classifies as Unknown
pub fn prop_classify_unknown__test() {
  use len <- qcheck.given(qcheck.bounded_int(10, 100))
  let output = string.repeat("random", len)
  assert case errors.classify_error(output) {
    errors.Unknown -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: empty string classifies as Unknown
pub fn prop_classify_empty_is_unknown__test() {
  assert case errors.classify_error("") {
    errors.Unknown -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: timeout takes priority over other keywords
pub fn prop_classify_timeout_priority__test() {
  let output = "did not complete in time\nError: something else"
  assert case errors.classify_error(output) {
    errors.Timeout -> True
    _ -> False
  }
}

// ERROR CLASSIFICATION - Property: panic takes priority over test failure
pub fn prop_classify_panic_priority__test() {
  let output = "panic: error\nFAILED: test"
  assert case errors.classify_error(output) {
    errors.RuntimePanic -> True
    _ -> False
  }
}

// ERROR EXTRACTION - Property: output with no error lines returns None
pub fn prop_extract_no_error_returns_none__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let output = string.repeat("line\n", len)
  assert case errors.extract_root_cause(output) {
    None -> True
    Some(_) -> False
  }
}

// ERROR EXTRACTION - Property: output with error line returns Some
pub fn prop_extract_with_error_returns_some__test() {
  let output = "some output\nError: this is the error\nmore output"
  assert case errors.extract_root_cause(output) {
    Some(msg) -> string.length(msg) > 0
    None -> False
  }
}

// ERROR EXTRACTION - Property: extracted error contains Error keyword
pub fn prop_extract_contains_error_keyword__test() {
  let output = "prefix\nerror: compilation failed\nsuffix"
  assert case errors.extract_root_cause(output) {
    Some(msg) -> string.contains(msg, "error")
    None -> False
  }
}

// ERROR EXTRACTION - Property: empty output returns None
pub fn prop_extract_empty_returns_none__test() {
  assert case errors.extract_root_cause("") {
    None -> True
    Some(_) -> False
  }
}

// ERROR EXTRACTION - Property: only whitespace returns None
pub fn prop_extract_whitespace_returns_none__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 20))
  let whitespace = string.repeat(" \n", len)
  assert case errors.extract_root_cause(whitespace) {
    None -> True
    Some(_) -> False
  }
}

// ERROR EXTRACTION - Property: multiple error lines returns first one
pub fn prop_extract_returns_first_error__test() {
  let output = "Error: first error\nmore content\nerror: second error"
  assert case errors.extract_root_cause(output) {
    Some(msg) -> string.contains(msg, "first")
    None -> False
  }
}

// ERROR SUMMARIZATION - Property: summary with 0 lines returns empty
pub fn prop_summarize_zero_lines_empty__test() {
  let output = "Error: some error\nmore stuff"
  let summary = errors.summarize_error(output, 0)
  assert summary == "" || summary == "\n"
}

// ERROR SUMMARIZATION - Property: summary preserves error lines
pub fn prop_summarize_includes_errors__test() {
  let output = "junk\nError: important\njunk\nerror: also important\njunk"
  let summary = errors.summarize_error(output, 10)
  assert string.contains(summary, "Error") || string.contains(summary, "error")
}

// ERROR SUMMARIZATION - Property: summary respects max_lines limit
pub fn prop_summarize_respects_limit__test() {
  use max <- qcheck.given(qcheck.bounded_int(1, 10))
  let lines = list_build_errors(20)
  let output = string.join(lines, "\n")
  let summary = errors.summarize_error(output, max)
  let summary_line_count =
    string.split(summary, "\n")
    |> list.length
  assert summary_line_count <= max + 1
  // +1 for edge case
}

// ERROR SUMMARIZATION - Property: empty output summarizes to empty
pub fn prop_summarize_empty_output__test() {
  let summary = errors.summarize_error("", 5)
  assert summary == ""
}

// ERROR SUMMARIZATION - Property: summarize is deterministic
pub fn prop_summarize_deterministic__test() {
  let output = "Error: first\nstuff\nerror: second\nmore"
  let summary1 = errors.summarize_error(output, 5)
  let summary2 = errors.summarize_error(output, 5)
  assert summary1 == summary2
}

// ERROR CONTEXT - Property: context lines must start with space
pub fn prop_extract_context_preserves_indentation__test() {
  let output = "Error: main error\n  context line\n  another context\nnew line"
  assert case errors.extract_root_cause(output) {
    Some(msg) -> string.contains(msg, "context")
    None -> False
  }
}

// HELPER FUNCTIONS
fn list_build_errors(count: Int) -> List(String) {
  case count {
    0 -> []
    n if n % 2 == 0 -> [
      "Error: " <> string.inspect(n),
      ..list_build_errors(n - 1)
    ]
    n -> ["output line " <> string.inspect(n), ..list_build_errors(n - 1)]
  }
}

import gleam/list
