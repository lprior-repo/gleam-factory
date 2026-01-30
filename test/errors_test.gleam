import errors
import gleam/option.{None, Some}
import gleeunit/should

// === Error Classification Behavior ===

pub fn classify_error_identifies_timeout_from_completion_message_test() {
  errors.classify_error("Command did not complete in time")
  |> should.equal(errors.Timeout)
}

pub fn classify_error_identifies_timeout_when_embedded_in_output_test() {
  errors.classify_error(
    "Starting...\nProcess did not complete in time\nAborted",
  )
  |> should.equal(errors.Timeout)
}

pub fn classify_error_identifies_runtime_panic_from_panic_keyword_test() {
  errors.classify_error("panic: runtime error: index out of range")
  |> should.equal(errors.RuntimePanic)
}

pub fn classify_error_identifies_panic_with_stack_trace_test() {
  let output =
    "goroutine 1 [running]:\npanic: nil pointer dereference\n\tat main.go:42"
  errors.classify_error(output)
  |> should.equal(errors.RuntimePanic)
}

pub fn classify_error_identifies_test_failure_from_failed_keyword_test() {
  errors.classify_error("Tests: 5 passed, 2 FAILED")
  |> should.equal(errors.TestFailure)
}

pub fn classify_error_identifies_compile_error_from_lowercase_error_test() {
  errors.classify_error("error: expected semicolon")
  |> should.equal(errors.CompileError)
}

pub fn classify_error_identifies_compile_error_from_capitalized_error_test() {
  errors.classify_error("Error: cannot find module")
  |> should.equal(errors.CompileError)
}

pub fn classify_error_returns_unknown_for_unrecognized_output_test() {
  errors.classify_error("Build completed successfully")
  |> should.equal(errors.Unknown)
}

pub fn classify_error_returns_unknown_for_empty_string_test() {
  errors.classify_error("")
  |> should.equal(errors.Unknown)
}

pub fn classify_error_prioritizes_timeout_over_panic_test() {
  let output = "panic: something\ndid not complete in time"
  errors.classify_error(output)
  |> should.equal(errors.Timeout)
}

pub fn classify_error_prioritizes_panic_over_test_failure_test() {
  let output = "FAILED\npanic: oops"
  errors.classify_error(output)
  |> should.equal(errors.RuntimePanic)
}

pub fn classify_error_prioritizes_test_failure_over_compile_error_test() {
  let output = "error: syntax\nFAILED"
  errors.classify_error(output)
  |> should.equal(errors.TestFailure)
}

// === Root Cause Extraction Behavior ===

pub fn extract_root_cause_finds_error_line_test() {
  let output = "Building...\nError: undefined variable\nDone"
  errors.extract_root_cause(output)
  |> should.equal(Some("Error: undefined variable"))
}

pub fn extract_root_cause_finds_lowercase_error_line_test() {
  let output = "Compiling...\nerror: expected type\nFinished"
  errors.extract_root_cause(output)
  |> should.equal(Some("error: expected type"))
}

pub fn extract_root_cause_includes_context_lines_starting_with_space_test() {
  let output =
    "Building...\nError: type mismatch\n  expected: Int\n  got: String\nDone"
  errors.extract_root_cause(output)
  |> should.equal(Some("Error: type mismatch\n  expected: Int\n  got: String"))
}

pub fn extract_root_cause_limits_context_to_three_lines_test() {
  let output =
    "Error: problem\n  line1\n  line2\n  line3\n  line4\n  line5\nDone"
  case errors.extract_root_cause(output) {
    Some(result) -> {
      // Should include Error line + 3 context lines max
      result
      |> should.equal("Error: problem\n  line1\n  line2\n  line3")
    }
    None -> should.fail()
  }
}

pub fn extract_root_cause_stops_at_non_indented_line_test() {
  let output = "Error: something\n  context\nNext step\n  more indented"
  case errors.extract_root_cause(output) {
    Some(result) -> {
      result
      |> should.equal("Error: something\n  context")
    }
    None -> should.fail()
  }
}

pub fn extract_root_cause_returns_none_when_no_error_test() {
  errors.extract_root_cause("Build successful\nAll tests passed")
  |> should.equal(None)
}

pub fn extract_root_cause_returns_none_for_empty_string_test() {
  errors.extract_root_cause("")
  |> should.equal(None)
}

// === Error Summarization Behavior ===

pub fn summarize_error_extracts_only_error_lines_test() {
  let output = "Line 1\nError here\nLine 3\nAnother error\nLine 5"
  errors.summarize_error(output, 10)
  |> should.equal("Error here\nAnother error")
}

pub fn summarize_error_respects_max_lines_limit_test() {
  let output = "error one\nerror two\nerror three\nerror four"
  errors.summarize_error(output, 2)
  |> should.equal("error one\nerror two")
}

pub fn summarize_error_finds_error_with_capital_e_test() {
  let output = "Start\nError: something\nEnd"
  errors.summarize_error(output, 5)
  |> should.equal("Error: something")
}

pub fn summarize_error_returns_empty_when_no_errors_test() {
  errors.summarize_error("All good\nNo problems\nSuccess", 10)
  |> should.equal("")
}

pub fn summarize_error_returns_empty_for_empty_input_test() {
  errors.summarize_error("", 10)
  |> should.equal("")
}

pub fn summarize_error_handles_zero_max_lines_test() {
  errors.summarize_error("error: problem", 0)
  |> should.equal("")
}
