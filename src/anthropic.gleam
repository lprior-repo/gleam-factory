import gleam/dynamic/decode
import gleam/json
import gleam/list
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
  // Anthropic API returns: {"content": [{"type": "text", "text": "..."}]}
  let text_decoder = {
    use text <- decode.field("text", decode.string)
    decode.success(text)
  }
  let content_decoder = decode.at(["content"], decode.list(text_decoder))
  case json.parse(json_str, content_decoder) {
    Ok(texts) ->
      case list.first(texts) {
        Ok(text) -> Ok(text)
        Error(_) -> extract_text_fallback(json_str)
      }
    Error(_) -> extract_text_fallback(json_str)
  }
}

fn extract_text_fallback(json_str: String) -> Result(String, String) {
  // Fallback for simpler responses or different formats
  let simple_decoder = decode.at(["content", "text"], decode.string)
  case json.parse(json_str, simple_decoder) {
    Ok(text) -> Ok(text)
    Error(_) -> Error("Failed to extract text from response")
  }
}
