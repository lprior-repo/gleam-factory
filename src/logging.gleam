import gleam/dict
import gleam/io

pub type LogLevel {
  Debug
  Info
  Error
}

pub fn log(
  level: LogLevel,
  message: String,
  context: dict.Dict(String, String),
) -> Nil {
  let timestamp = ""
  let output = format_log(level, message, context, timestamp)
  io.println(output)
}

pub fn format_log(
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

fn format_context(context: dict.Dict(String, String)) -> String {
  context
  |> dict.to_list()
  |> list_map(fn(pair) {
    let #(key, value) = pair
    key <> "=" <> value
  })
  |> string_join(", ")
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
