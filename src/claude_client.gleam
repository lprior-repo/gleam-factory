//// Claude API client with streaming support.

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/result
import gleam/string

pub type ClaudeMessage {
  ClaudeMessage(role: String, content: String)
}

pub type ClaudeRequest {
  ClaudeRequest(
    model: String,
    max_tokens: Int,
    messages: List(ClaudeMessage),
    system: String,
    stream: Bool,
  )
}

pub type ClaudeResponse {
  ClaudeResponse(content: String, stop_reason: String)
}

pub type StreamChunk {
  ContentDelta(text: String)
  MessageComplete
  StreamError(reason: String)
}

pub fn new_request(
  model: String,
  prompt: String,
  system: String,
) -> ClaudeRequest {
  ClaudeRequest(
    model:,
    max_tokens: 8192,
    messages: [ClaudeMessage("user", prompt)],
    system:,
    stream: False,
  )
}

pub fn encode_request(req: ClaudeRequest) -> String {
  json.object([
    #("model", json.string(req.model)),
    #("max_tokens", json.int(req.max_tokens)),
    #(
      "messages",
      json.array(req.messages, fn(m) {
        json.object([
          #("role", json.string(m.role)),
          #("content", json.string(m.content)),
        ])
      }),
    ),
    #("system", json.string(req.system)),
    #("stream", json.bool(req.stream)),
  ])
  |> json.to_string
}

pub fn call_claude_sync(
  api_key: String,
  req: ClaudeRequest,
) -> Result(ClaudeResponse, String) {
  use http_req <- result.try(
    request.to("https://api.anthropic.com/v1/messages")
    |> result.replace_error("Invalid Claude API URL"),
  )

  let body = encode_request(req)
  let final_req =
    http_req
    |> request.set_method(http.Post)
    |> request.set_header("x-api-key", api_key)
    |> request.set_header("anthropic-version", "2023-06-01")
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  use resp <- result.try(
    httpc.send(final_req) |> result.replace_error("HTTP request failed"),
  )

  case resp.status {
    200 -> parse_response(resp.body)
    _ ->
      Error(
        "Claude API error: " <> string.inspect(resp.status) <> " " <> resp.body,
      )
  }
}

pub fn parse_response(body: String) -> Result(ClaudeResponse, String) {
  let text_decoder = {
    use text <- decode.field("text", decode.string)
    decode.success(text)
  }
  let response_decoder = {
    use texts <- decode.field("content", decode.list(text_decoder))
    use stop_reason <- decode.field("stop_reason", decode.string)
    let content = texts |> list.first |> result.unwrap("")
    decode.success(ClaudeResponse(content:, stop_reason:))
  }
  json.parse(body, response_decoder)
  |> result.replace_error("Failed to parse Claude response")
}

fn extract_stream_text(json_str: String) -> Result(String, String) {
  let text_decoder = decode.at(["delta", "text"], decode.string)
  json.parse(json_str, text_decoder)
  |> result.replace_error("No delta text")
}

pub fn call_claude_stream(
  api_key: String,
  req: ClaudeRequest,
  on_chunk: fn(StreamChunk) -> Nil,
) -> Result(Nil, String) {
  let stream_req = ClaudeRequest(..req, stream: True)
  use http_req <- result.try(
    request.to("https://api.anthropic.com/v1/messages")
    |> result.replace_error("Invalid Claude API URL"),
  )

  let body = encode_request(stream_req)
  let final_req =
    http_req
    |> request.set_method(http.Post)
    |> request.set_header("x-api-key", api_key)
    |> request.set_header("anthropic-version", "2023-06-01")
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  use resp <- result.try(
    httpc.send(final_req) |> result.replace_error("HTTP request failed"),
  )

  case resp.status {
    200 -> {
      parse_stream(resp.body, on_chunk)
      Ok(Nil)
    }
    _ ->
      Error(
        "Claude API error: " <> string.inspect(resp.status) <> " " <> resp.body,
      )
  }
}

fn parse_stream(body: String, on_chunk: fn(StreamChunk) -> Nil) -> Nil {
  body
  |> string.split("data: ")
  |> list.each(fn(chunk) {
    case string.contains(chunk, "content_block_delta") {
      True ->
        case extract_stream_text(chunk) {
          Ok(text) -> on_chunk(ContentDelta(text))
          Error(_) -> Nil
        }
      False ->
        case string.contains(chunk, "message_stop") {
          True -> on_chunk(MessageComplete)
          False -> Nil
        }
    }
  })
}
