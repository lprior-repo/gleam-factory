import gleam/json
import gleam/result
import gleam/string
import process

pub fn send_message(api_key: String, prompt: String) -> Result(String, String) {
  let request_body = build_request(prompt)
  use json_response <- result.try(curl_post(api_key, request_body))
  parse_response(json_response)
}

fn build_request(prompt: String) -> String {
  let messages = [
    json.object([
      #("role", json.string("user")),
      #("content", json.string(prompt)),
    ]),
  ]
  json.object([
    #("model", json.string("claude-sonnet-4-5-20250929")),
    #("max_tokens", json.int(8192)),
    #("messages", json.array(messages, of: fn(x) { x })),
  ])
  |> json.to_string
}

fn curl_post(api_key: String, body: String) -> Result(String, String) {
  let args = [
    "-s",
    "-X",
    "POST",
    "-H",
    "Content-Type: application/json",
    "-H",
    "x-api-key: " <> api_key,
    "-H",
    "anthropic-version: 2023-06-01",
    "-d",
    body,
    "--max-time",
    "30",
    "https://api.anthropic.com/v1/messages",
  ]
  use result <- result.try(process.run_command("curl", args, ""))
  case result {
    process.Success(stdout, _, 0) -> Ok(stdout)
    process.Success(_, _, code) | process.Failure(_, code) ->
      Error("curl failed: " <> string.inspect(code))
  }
}

pub fn parse_response(json_str: String) -> Result(String, String) {
  use text <- result.try(extract_text(json_str))
  case string.trim(text) {
    "" -> Error("empty response text")
    t -> Ok(t)
  }
}

fn extract_text(json_str: String) -> Result(String, String) {
  json_str
  |> string.split("\"text\":\"")
  |> take_second
  |> result.try(fn(s) { s |> string.split("\"}") |> take_first })
  |> result.map(unescape_json)
}

fn take_first(lst: List(a)) -> Result(a, String) {
  case lst {
    [x, ..] -> Ok(x)
    [] -> Error("text field not found")
  }
}

fn take_second(lst: List(a)) -> Result(a, String) {
  case lst {
    [_, x, ..] -> Ok(x)
    _ -> Error("text field not found")
  }
}

fn unescape_json(s: String) -> String {
  s
  |> string.replace("\\n", "\n")
  |> string.replace("\\\"", "\"")
  |> string.replace("\\\\", "\\")
}
