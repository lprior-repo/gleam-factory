import gleam/dict
import gleam/int
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
  let timestamp = get_iso_timestamp()
  let output = format_log(level, message, context, timestamp)
  io.println(output)
}

fn get_iso_timestamp() -> String {
  let ts = erlang_system_time_ms()
  let secs = ts / 1000
  let ms = ts % 1000
  format_timestamp(secs, ms)
}

fn format_timestamp(secs: Int, ms: Int) -> String {
  let #(year, month, day, hour, min, sec) = calendar_to_datetime(secs)
  int.to_string(year)
  <> "-"
  <> pad_zero(month)
  <> "-"
  <> pad_zero(day)
  <> "T"
  <> pad_zero(hour)
  <> ":"
  <> pad_zero(min)
  <> ":"
  <> pad_zero(sec)
  <> "."
  <> pad_ms(ms)
  <> "Z"
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

fn pad_ms(n: Int) -> String {
  case n < 10 {
    True -> "00" <> int.to_string(n)
    False ->
      case n < 100 {
        True -> "0" <> int.to_string(n)
        False -> int.to_string(n)
      }
  }
}

@external(erlang, "logging_ffi", "system_time_ms")
fn erlang_system_time_ms() -> Int

@external(erlang, "logging_ffi", "secs_to_datetime")
fn calendar_to_datetime(secs: Int) -> #(Int, Int, Int, Int, Int, Int)

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
