// Errors module - Error extraction utilities
// Provides functions to extract error information from command output

import gleam/option.{type Option, None, Some}
import gleam/string

/// Error classification types
pub type ErrorType {
  CompileError
  TestFailure
  RuntimePanic
  Timeout
  Unknown
}

/// Classify the type of error from command output
pub fn classify_error(output: String) -> ErrorType {
  case string.contains(output, "did not complete in time") {
    True -> Timeout
    False ->
      case string.contains(output, "panic:") {
        True -> RuntimePanic
        False ->
          case string.contains(output, "FAILED") {
            True -> TestFailure
            False ->
              case
                string.contains(output, "error:")
                || string.contains(output, "Error:")
              {
                True -> CompileError
                False -> Unknown
              }
          }
      }
  }
}

/// Extract the root cause error from command output
/// Looks for lines containing "Error:" pattern
pub fn extract_root_cause(output: String) -> Option(String) {
  output
  |> string.split("\n")
  |> find_error_line
}

fn find_error_line(lines: List(String)) -> Option(String) {
  case lines {
    [] -> None
    [line, ..rest] ->
      case string.contains(line, "Error:") || string.contains(line, "error:") {
        True -> Some(line <> collect_context_lines(rest, 3))
        False -> find_error_line(rest)
      }
  }
}

fn collect_context_lines(lines: List(String), remaining: Int) -> String {
  case remaining, lines {
    0, _ -> ""
    _, [] -> ""
    _, [line, ..rest] ->
      case string.starts_with(line, " ") {
        True -> "\n" <> line <> collect_context_lines(rest, remaining - 1)
        False -> ""
      }
  }
}

/// Summarize error output to a maximum number of lines
/// Prioritizes lines containing error information
pub fn summarize_error(output: String, max_lines: Int) -> String {
  output
  |> string.split("\n")
  |> find_error_lines(max_lines, [])
  |> string.join("\n")
}

fn find_error_lines(
  lines: List(String),
  remaining: Int,
  acc: List(String),
) -> List(String) {
  case remaining, lines {
    0, _ -> list_reverse(acc)
    _, [] -> list_reverse(acc)
    _, [line, ..rest] ->
      case string.contains(line, "Error") || string.contains(line, "error") {
        True -> find_error_lines(rest, remaining - 1, [line, ..acc])
        False -> find_error_lines(rest, remaining, acc)
      }
  }
}

fn list_reverse(list: List(a)) -> List(a) {
  do_reverse(list, [])
}

fn do_reverse(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [x, ..rest] -> do_reverse(rest, [x, ..acc])
  }
}
