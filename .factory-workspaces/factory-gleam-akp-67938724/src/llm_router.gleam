//// LLM Router - Routes requests to local or remote endpoints with GPU management.
////
//// Acquires GPU tickets before calling local endpoints, blocks when unavailable.

import gleam/int
import gleam/result
import llm
import process as shell_process
import types

pub type RouterConfig {
  RouterConfig(
    gpu_governor: types.GpuGovernor,
    local_url: String,
    anthropic_url: String,
    anthropic_key: String,
  )
}

pub fn new_config(
  gpu_governor: types.GpuGovernor,
  local_url: String,
  anthropic_key: String,
) -> RouterConfig {
  RouterConfig(
    gpu_governor:,
    local_url:,
    anthropic_url: "https://api.anthropic.com/v1/messages",
    anthropic_key:,
  )
}

pub fn call(
  config: RouterConfig,
  request: llm.LLMRequest,
  role: llm.Role,
) -> Result(llm.LLMResponse, llm.LLMError) {
  let endpoint = llm.route_request(role)
  case endpoint {
    llm.LocalEndpoint(_) -> call_local_with_ticket(config, request)
    llm.AnthropicEndpoint(_, _) -> call_anthropic(config, request)
  }
}

fn call_local_with_ticket(
  config: RouterConfig,
  request: llm.LLMRequest,
) -> Result(llm.LLMResponse, llm.LLMError) {
  use ticket <- result.try(
    types.request_gpu_ticket(config.gpu_governor)
    |> result.map_error(fn(_) { llm.NetworkError("GPU ticket unavailable") }),
  )
  let result = call_local(config.local_url, request)
  let _ = types.release_gpu_ticket(config.gpu_governor, ticket)
  result
}

fn call_local(
  url: String,
  request: llm.LLMRequest,
) -> Result(llm.LLMResponse, llm.LLMError) {
  let json_body = build_local_request_json(request)
  case
    shell_process.run_command(
      "curl",
      [
        "-s",
        "-X",
        "POST",
        "-H",
        "Content-Type: application/json",
        "-d",
        json_body,
        url,
      ],
      "",
    )
  {
    Ok(shell_process.Success(stdout, _, _)) -> parse_local_response(stdout)
    Ok(shell_process.Failure(err, _)) -> Error(llm.NetworkError(err))
    Error(e) -> Error(llm.NetworkError(e))
  }
}

fn call_anthropic(
  config: RouterConfig,
  request: llm.LLMRequest,
) -> Result(llm.LLMResponse, llm.LLMError) {
  let json_body = build_anthropic_request_json(request)
  case
    shell_process.run_command(
      "curl",
      [
        "-s",
        "-X",
        "POST",
        "-H",
        "Content-Type: application/json",
        "-H",
        "x-api-key: " <> config.anthropic_key,
        "-H",
        "anthropic-version: 2023-06-01",
        "-d",
        json_body,
        config.anthropic_url,
      ],
      "",
    )
  {
    Ok(shell_process.Success(stdout, _, _)) -> parse_anthropic_response(stdout)
    Ok(shell_process.Failure(err, _)) -> Error(llm.NetworkError(err))
    Error(e) -> Error(llm.NetworkError(e))
  }
}

fn build_local_request_json(request: llm.LLMRequest) -> String {
  "{\"prompt\":\""
  <> escape_json(request.prompt)
  <> "\",\"n_predict\":"
  <> int.to_string(request.max_tokens)
  <> ",\"temperature\":"
  <> float_to_string(request.temperature)
  <> "}"
}

fn build_anthropic_request_json(request: llm.LLMRequest) -> String {
  "{\"model\":\""
  <> request.model
  <> "\",\"max_tokens\":"
  <> int.to_string(request.max_tokens)
  <> ",\"messages\":[{\"role\":\"user\",\"content\":\""
  <> escape_json(request.prompt)
  <> "\"}]}"
}

fn parse_local_response(json: String) -> Result(llm.LLMResponse, llm.LLMError) {
  case extract_content_field(json) {
    Ok(content) ->
      Ok(llm.LLMResponse(
        content:,
        finish_reason: "stop",
        usage: llm.TokenUsage(
          prompt_tokens: 0,
          completion_tokens: 0,
          total_tokens: 0,
        ),
      ))
    Error(_) -> Error(llm.ParseError("Failed to parse local response"))
  }
}

fn parse_anthropic_response(
  json: String,
) -> Result(llm.LLMResponse, llm.LLMError) {
  case extract_content_field(json) {
    Ok(content) ->
      Ok(llm.LLMResponse(
        content:,
        finish_reason: "end_turn",
        usage: llm.TokenUsage(
          prompt_tokens: 0,
          completion_tokens: 0,
          total_tokens: 0,
        ),
      ))
    Error(_) -> Error(llm.ParseError("Failed to parse Anthropic response"))
  }
}

fn extract_content_field(json: String) -> Result(String, Nil) {
  case string_split(json, "\"content\":\"") {
    [_, rest, ..] ->
      case string_split(rest, "\"") {
        [content, ..] -> Ok(content)
        [] -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn escape_json(s: String) -> String {
  s
  |> string_replace("\\", "\\\\")
  |> string_replace("\"", "\\\"")
  |> string_replace("\n", "\\n")
  |> string_replace("\r", "\\r")
  |> string_replace("\t", "\\t")
}

@external(erlang, "string", "split")
fn string_split(s: String, sep: String) -> List(String)

@external(erlang, "string", "replace")
fn string_replace(s: String, from: String, to: String) -> String

fn float_to_string(f: Float) -> String {
  case f <. 1.0 && f >=. 0.0 {
    True -> "0." <> int.to_string(truncate(f *. 10.0))
    False -> int.to_string(truncate(f)) <> ".0"
  }
}

@external(erlang, "erlang", "trunc")
fn truncate(f: Float) -> Int
