//// LLM Router - Routes requests to local or remote endpoints with GPU management.
////
//// Acquires GPU tickets before calling local endpoints, blocks when unavailable.

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
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
  let env = [#("ANTHROPIC_API_KEY", config.anthropic_key)]
  case
    shell_process.run_command_with_env(
      "curl",
      [
        "-s",
        "-X",
        "POST",
        "-H",
        "Content-Type: application/json",
        "-H",
        "x-api-key: $ANTHROPIC_API_KEY",
        "-H",
        "anthropic-version: 2023-06-01",
        "-d",
        json_body,
        config.anthropic_url,
      ],
      "",
      env,
    )
  {
    Ok(shell_process.Success(stdout, _, _)) -> parse_anthropic_response(stdout)
    Ok(shell_process.Failure(err, _)) -> Error(llm.NetworkError(err))
    Error(e) -> Error(llm.NetworkError(e))
  }
}

pub fn build_local_request_json(request: llm.LLMRequest) -> String {
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

fn parse_local_response(
  json_str: String,
) -> Result(llm.LLMResponse, llm.LLMError) {
  case extract_content_field(json_str) {
    Ok(content) -> {
      let usage = extract_local_usage(json_str)
      Ok(llm.LLMResponse(content:, finish_reason: "stop", usage:))
    }
    Error(_) -> Error(llm.ParseError("Failed to parse local response"))
  }
}

fn parse_anthropic_response(
  json_str: String,
) -> Result(llm.LLMResponse, llm.LLMError) {
  case extract_content_field(json_str) {
    Ok(content) -> {
      let usage = extract_anthropic_usage(json_str)
      let finish_reason = extract_stop_reason(json_str)
      Ok(llm.LLMResponse(content:, finish_reason:, usage:))
    }
    Error(_) -> Error(llm.ParseError("Failed to parse Anthropic response"))
  }
}

fn extract_anthropic_usage(json_str: String) -> llm.TokenUsage {
  let usage_decoder =
    decode.at(["usage"], {
      use input <- decode.field("input_tokens", decode.int)
      use output <- decode.field("output_tokens", decode.int)
      decode.success(llm.TokenUsage(input, output, input + output))
    })
  case json.parse(json_str, usage_decoder) {
    Ok(usage) -> usage
    Error(_) -> llm.TokenUsage(0, 0, 0)
  }
}

fn extract_local_usage(json_str: String) -> llm.TokenUsage {
  let usage_decoder =
    decode.at(["usage"], {
      use prompt <- decode.field("prompt_tokens", decode.int)
      use completion <- decode.field("completion_tokens", decode.int)
      use total <- decode.field("total_tokens", decode.int)
      decode.success(llm.TokenUsage(prompt, completion, total))
    })
  case json.parse(json_str, usage_decoder) {
    Ok(usage) -> usage
    Error(_) -> {
      // Try llama.cpp format: tokens_predicted, tokens_evaluated
      let llama_decoder = {
        use pred <- decode.field("tokens_predicted", decode.int)
        use eval <- decode.field("tokens_evaluated", decode.int)
        decode.success(llm.TokenUsage(eval, pred, eval + pred))
      }
      case json.parse(json_str, llama_decoder) {
        Ok(usage) -> usage
        Error(_) -> llm.TokenUsage(0, 0, 0)
      }
    }
  }
}

fn extract_stop_reason(json_str: String) -> String {
  let decoder = decode.at(["stop_reason"], decode.string)
  case json.parse(json_str, decoder) {
    Ok(reason) -> reason
    Error(_) -> "end_turn"
  }
}

fn extract_content_field(json_str: String) -> Result(String, Nil) {
  // Try Anthropic format first: content array with text blocks
  let anthropic_decoder =
    decode.at(["content"], decode.list(decode.at(["text"], decode.string)))
  case json.parse(json_str, anthropic_decoder) {
    Ok(texts) ->
      case list.first(texts) {
        Ok(text) -> Ok(text)
        Error(_) -> extract_content_fallback(json_str)
      }
    Error(_) -> extract_content_fallback(json_str)
  }
}

fn extract_content_fallback(json_str: String) -> Result(String, Nil) {
  // Try local LLM format: direct content field
  let local_decoder = decode.at(["content"], decode.string)
  case json.parse(json_str, local_decoder) {
    Ok(content) -> Ok(content)
    Error(_) -> {
      // Last resort: try response field (some local models)
      let response_decoder = decode.at(["response"], decode.string)
      json.parse(json_str, response_decoder)
      |> result.replace_error(Nil)
    }
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

fn string_replace(s: String, from: String, to: String) -> String {
  binary_replace(s, from, to, [Global])
}

type ReplaceOption {
  Global
}

@external(erlang, "binary", "replace")
fn binary_replace(
  s: String,
  from: String,
  to: String,
  opts: List(ReplaceOption),
) -> String

fn float_to_string(f: Float) -> String {
  let s = float_to_binary(f, [#(Decimals, 2)])
  case s {
    "0.00" -> "0.0"
    _ -> trim_trailing_zeros(s)
  }
}

fn trim_trailing_zeros(s: String) -> String {
  case string_ends_with_binary(s, "0") && !string_ends_with_binary(s, ".0") {
    True -> trim_trailing_zeros(string.drop_end(s, 1))
    False -> s
  }
}

fn string_ends_with_binary(s: String, suffix: String) -> Bool {
  let s_len = byte_size(s)
  let suffix_len = byte_size(suffix)
  case s_len >= suffix_len {
    False -> False
    True -> binary_part(s, s_len - suffix_len, suffix_len) == suffix
  }
}

@external(erlang, "erlang", "byte_size")
fn byte_size(s: String) -> Int

@external(erlang, "binary", "part")
fn binary_part(s: String, start: Int, len: Int) -> String

type FloatOption =
  #(FloatOptionKey, Int)

type FloatOptionKey {
  Decimals
}

@external(erlang, "erlang", "float_to_binary")
fn float_to_binary(f: Float, opts: List(FloatOption)) -> String
