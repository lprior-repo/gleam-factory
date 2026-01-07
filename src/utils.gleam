// Unified utils module - single source of truth for all utilities
// Uses: shellout, stdin
// This is the ONLY place these tools are used

import gleam/io
import gleam/string
import gleam/result
import gleam/list
import gleam/int
import shellout

/// Execute a command with status indicator
pub fn run_with_status(
  description: String,
  command: String,
  args: List(String),
  cwd: String,
) -> Result(String, String) {
  io.println("▶ " <> description)

  shellout.command(command, args, cwd, [])
  |> result.map_error(fn(err) {
    "Command failed: " <> string.inspect(err)
  })
}

/// Execute multiple commands in sequence with progress
pub fn run_pipeline(
  name: String,
  commands: List(#(String, String, List(String), String)),
) -> Result(List(String), String) {
  io.println("▶ " <> name)

  commands
  |> list.try_map(fn(cmd) {
    let #(desc, command, args, cwd) = cmd
    run_with_status(desc, command, args, cwd)
  })
  |> result.map(fn(results) {
    io.println("✓ Complete")
    results
  })
  |> result.map_error(fn(err) {
    io.println("✗ Failed: " <> err)
    err
  })
}

/// Get user input from stdin
pub fn prompt(message: String) -> String {
  // Note: This is a placeholder since interactive stdin is not easily available
  // in the current build environment. In production, would use stdin.read_line()
  io.print(message <> "> ")
  ""
}

/// Get yes/no confirmation
pub fn confirm(message: String) -> Bool {
  // Note: This is a placeholder since interactive stdin is not easily available
  // in the current build environment.
  let _ = prompt(message <> " (y/n)")
  False
}

/// Format success message
pub fn format_success(msg: String) -> String {
  "✓ " <> msg
}

/// Format error message
pub fn format_error(msg: String) -> String {
  "✗ " <> msg
}

/// Format info message
pub fn format_info(msg: String) -> String {
  "ℹ " <> msg
}

/// Format warning message
pub fn format_warning(msg: String) -> String {
  "⚠ " <> msg
}

/// Print a section header
pub fn print_header(title: String) -> Nil {
  io.println("")
  io.println("╔" <> string.repeat("═", string.length(title) + 2) <> "╗")
  io.println("║ " <> title <> " ║")
  io.println("╚" <> string.repeat("═", string.length(title) + 2) <> "╝")
}

/// Print key-value field
pub fn print_field(key: String, value: String) -> Nil {
  let padding = 25 - string.length(key)
  io.println(key <> string.repeat(" ", padding) <> ": " <> value)
}

/// Print a list
pub fn print_list(title: String, items: List(String)) -> Nil {
  io.println("")
  io.println(title <> ":")
  items
  |> list.each(fn(item) {
    io.println("  • " <> item)
  })
}

/// Create a progress bar
pub fn progress_bar(current: Int, total: Int) -> String {
  let percentage = case total {
    0 -> 0
    _ -> current * 100 / total
  }
  let filled = percentage / 5
  let empty = 20 - filled

  "["
  <> string.repeat("█", filled)
  <> string.repeat("░", empty)
  <> "] "
  <> int.to_string(percentage)
  <> "%"
}

/// Print stage result
pub fn print_stage_result(
  stage: String,
  status: String,
  duration_ms: Int,
) -> Nil {
  let icon = case status {
    "success" -> "✓"
    "failure" -> "✗"
    "skipped" -> "⊘"
    _ -> "?"
  }

  io.println(icon <> " " <> stage <> " (" <> int.to_string(duration_ms) <> "ms)")
}

/// Print pipeline summary
pub fn print_pipeline_summary(
  total: Int,
  passed: Int,
  failed: Int,
  skipped: Int,
  duration_ms: Int,
) -> Nil {
  print_header("Pipeline Summary")

  print_field("Total Stages", int.to_string(total))
  print_field("Passed", int.to_string(passed))
  print_field("Failed", int.to_string(failed))
  print_field("Skipped", int.to_string(skipped))
  print_field("Duration", int.to_string(duration_ms) <> "ms")

  let success_rate = case total {
    0 -> 0
    _ -> passed * 100 / total
  }
  print_field("Success Rate", int.to_string(success_rate) <> "%")

  io.println("")
}

/// Print error with context
pub fn print_error_context(
  error: String,
  context: List(#(String, String)),
) -> Nil {
  io.println(format_error(error))
  context
  |> list.each(fn(ctx) {
    let #(key, value) = ctx
    io.println("  " <> key <> ": " <> value)
  })
}

/// Convert int to string (replaces duplicates across audit, validation, llm_router)
pub fn int_to_string(n: Int) -> String {
  case n < 0 {
    True -> "-" <> int_to_string(-n)
    False ->
      case n {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> int_to_string(n / 10) <> int_to_string(n % 10)
      }
  }
}
