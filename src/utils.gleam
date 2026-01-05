// Unified utils module - single source of truth for all utilities
// Uses: shellout, spinner, stdin, glitzer
// This is the ONLY place these tools are used

import gleam/io
import gleam/string
import gleam/result
import gleam/list
import spinner
import shellout
import stdin

/// Execute a command with a spinner
pub fn run_with_spinner(
  description: String,
  command: String,
  args: List(String),
  cwd: String,
) -> Result(String, String) {
  let spin = spinner.start(description)
  let result = shellout.command(command, args, cwd)
  spinner.stop(spin)

  result
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
    run_with_spinner(desc, command, args, cwd)
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
  io.print(message <> "> ")
  case stdin.read_line() {
    Ok(line) -> string.trim(line)
    Error(_) -> ""
  }
}

/// Get yes/no confirmation
pub fn confirm(message: String) -> Bool {
  case prompt(message <> " (y/n)") {
    "y" | "yes" | "Y" | "Yes" -> True
    _ -> False
  }
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
  let percentage = current * 100 / total
  let filled = percentage / 5
  let empty = 20 - filled

  "["
  <> string.repeat("█", filled)
  <> string.repeat("░", empty)
  <> "] "
  <> string.inspect(percentage)
  <> "%"
}

/// Print formatted table
pub fn print_table(
  headers: List(String),
  rows: List(List(String)),
) -> Nil {
  // Calculate column widths
  let widths =
    headers
    |> list.index_map(fn(header, idx) {
      let max_in_rows =
        rows
        |> list.map(fn(row) {
          case list.at(row, idx) {
            Ok(cell) -> string.length(cell)
            Error(_) -> 0
          }
        })
        |> list.fold(0, fn(acc, len) { int.max(acc, len) })

      int.max(string.length(header), max_in_rows)
    })

  // Print header
  let header_line =
    headers
    |> list.index_map(fn(header, idx) {
      case list.at(widths, idx) {
        Ok(width) -> string.pad_end(header, width, " ")
        Error(_) -> header
      }
    })
    |> string.join(" | ")

  io.println(header_line)
  io.println(string.repeat("─", string.length(header_line)))

  // Print rows
  rows
  |> list.each(fn(row) {
    let row_line =
      row
      |> list.index_map(fn(cell, idx) {
        case list.at(widths, idx) {
          Ok(width) -> string.pad_end(cell, width, " ")
          Error(_) -> cell
        }
      })
      |> string.join(" | ")

    io.println(row_line)
  })
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

  io.println(icon <> " " <> stage <> " (" <> string.inspect(duration_ms) <> "ms)")
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

  print_field("Total Stages", string.inspect(total))
  print_field("Passed", string.inspect(passed))
  print_field("Failed", string.inspect(failed))
  print_field("Skipped", string.inspect(skipped))
  print_field("Duration", string.inspect(duration_ms) <> "ms")

  let success_rate = case total {
    0 -> 0
    _ -> passed * 100 / total
  }
  print_field("Success Rate", string.inspect(success_rate) <> "%")

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
