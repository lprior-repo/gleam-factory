import gleam/dict
import gleam/string
import gleeunit
import gleeunit/should

pub type LogLevel {
  Debug
  Info
  Error
}

pub fn main() {
  gleeunit.main()
}

fn format_context(context: dict.Dict(String, String)) -> String {
  dict.to_list(context)
  |> list_map(fn(pair) {
    let #(key, value) = pair
    key <> "=" <> value
  })
  |> string_join(", ")
}

fn format_log(
  level: LogLevel,
  message: String,
  context: dict.Dict(String, String),
  timestamp: String,
) -> String {
  let level_str = case level {
    Debug -> "DEBUG"
    Info -> "INFO"
    Error -> "ERROR"
  }

  let context_str = format_context(context)

  case context_str {
    "" -> "[" <> timestamp <> "] " <> level_str <> ": " <> message
    _ ->
      "["
      <> timestamp
      <> "] "
      <> level_str
      <> ": "
      <> message
      <> " {"
      <> context_str
      <> "}"
  }
}

pub fn log_debug_outputs_with_level_test() {
  let context = dict.new()
  let message = "Debug message"
  let timestamp = "2026-01-09T10:00:00Z"

  let output = format_log(Debug, message, context, timestamp)

  string.contains(output, "DEBUG")
  |> should.be_true()

  string.contains(output, message)
  |> should.be_true()

  string.contains(output, timestamp)
  |> should.be_true()
}

pub fn log_info_includes_context_test() {
  let context =
    dict.new()
    |> dict.insert("task_id", "task-123")
    |> dict.insert("module", "factory")
    |> dict.insert("function", "run")

  let message = "Processing started"
  let timestamp = "2026-01-09T10:00:01Z"

  let output = format_log(Info, message, context, timestamp)

  string.contains(output, "INFO")
  |> should.be_true()

  string.contains(output, "task_id=task-123")
  |> should.be_true()

  string.contains(output, "module=factory")
  |> should.be_true()

  string.contains(output, "function=run")
  |> should.be_true()

  string.contains(output, message)
  |> should.be_true()
}

pub fn log_error_includes_timestamp_test() {
  let context =
    dict.new()
    |> dict.insert("error_code", "500")

  let message = "System error occurred"
  let timestamp = "2026-01-09T10:00:02Z"

  let output = format_log(Error, message, context, timestamp)

  string.contains(output, "ERROR")
  |> should.be_true()

  string.contains(output, message)
  |> should.be_true()

  string.contains(output, timestamp)
  |> should.be_true()

  string.contains(output, "error_code=500")
  |> should.be_true()
}

pub fn log_formats_structured_data_test() {
  let context =
    dict.new()
    |> dict.insert("task_id", "factory-001")
    |> dict.insert("module", "integration")
    |> dict.insert("function", "execute")
    |> dict.insert("duration_ms", "1500")

  let message = "Task completed"
  let timestamp = "2026-01-09T10:00:03Z"

  let output = format_log(Info, message, context, timestamp)

  string.contains(output, "task_id=factory-001")
  |> should.be_true()

  string.contains(output, "module=integration")
  |> should.be_true()

  string.contains(output, "function=execute")
  |> should.be_true()

  string.contains(output, "duration_ms=1500")
  |> should.be_true()

  string.contains(output, message)
  |> should.be_true()

  string.contains(output, "[2026-01-09T10:00:03Z]")
  |> should.be_true()
}

pub fn log_empty_context_test() {
  let context = dict.new()
  let message = "Simple message"
  let timestamp = "2026-01-09T10:00:04Z"

  let output = format_log(Debug, message, context, timestamp)

  output
  |> should.equal("[2026-01-09T10:00:04Z] DEBUG: Simple message")
}

pub fn log_preserves_message_content_test() {
  let context =
    dict.new()
    |> dict.insert("request_id", "req-789")

  let message = "User action: clicked button 'submit'"
  let timestamp = "2026-01-09T10:00:05Z"

  let output = format_log(Info, message, context, timestamp)

  string.contains(output, message)
  |> should.be_true()

  string.contains(output, "request_id=req-789")
  |> should.be_true()
}

pub fn log_levels_differentiation_test() {
  let context =
    dict.new()
    |> dict.insert("component", "core")

  let message = "State changed"
  let timestamp = "2026-01-09T10:00:06Z"

  let debug_output = format_log(Debug, message, context, timestamp)
  let info_output = format_log(Info, message, context, timestamp)
  let error_output = format_log(Error, message, context, timestamp)

  string.contains(debug_output, "DEBUG")
  |> should.be_true()

  string.contains(info_output, "INFO")
  |> should.be_true()

  string.contains(error_output, "ERROR")
  |> should.be_true()

  debug_output
  |> should.not_equal(info_output)

  info_output
  |> should.not_equal(error_output)
}

pub fn log_context_ordering_independent_test() {
  let context1 =
    dict.new()
    |> dict.insert("a", "1")
    |> dict.insert("b", "2")

  let context2 =
    dict.new()
    |> dict.insert("b", "2")
    |> dict.insert("a", "1")

  let message = "Test"
  let timestamp = "2026-01-09T10:00:07Z"

  let output1 = format_log(Info, message, context1, timestamp)
  let output2 = format_log(Info, message, context2, timestamp)

  string.contains(output1, "a=1")
  |> should.be_true()

  string.contains(output1, "b=2")
  |> should.be_true()

  string.contains(output2, "a=1")
  |> should.be_true()

  string.contains(output2, "b=2")
  |> should.be_true()
}

fn list_map(list: List(a), f: fn(a) -> b) -> List(b) {
  case list {
    [] -> []
    [head, ..tail] -> [f(head), ..list_map(tail, f)]
  }
}

fn string_join(items: List(String), sep: String) -> String {
  case items {
    [] -> ""
    [single] -> single
    [first, ..rest] -> first <> sep <> string_join(rest, sep)
  }
}
