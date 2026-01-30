import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import llm

// === Role Type Behavior ===

pub fn role_auditor_exists_test() {
  let role = llm.Auditor
  case role {
    llm.Auditor -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn role_implementer_exists_test() {
  let role = llm.Implementer
  case role {
    llm.Implementer -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn role_architect_exists_test() {
  let role = llm.Architect
  case role {
    llm.Architect -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn role_reviewer_exists_test() {
  let role = llm.Reviewer
  case role {
    llm.Reviewer -> should.be_true(True)
    _ -> should.fail()
  }
}

// === LLMRequest Construction ===

pub fn new_request_sets_model_test() {
  let req = llm.new_request("claude-3", "prompt", 100)
  case req {
    llm.LLMRequest(model: "claude-3", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_request_sets_prompt_test() {
  let req = llm.new_request("m", "my prompt text", 100)
  case req {
    llm.LLMRequest(prompt: "my prompt text", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_request_sets_max_tokens_test() {
  let req = llm.new_request("m", "p", 4096)
  case req {
    llm.LLMRequest(max_tokens: 4096, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_request_has_no_system_prompt_by_default_test() {
  let req = llm.new_request("m", "p", 100)
  case req {
    llm.LLMRequest(system_prompt: None, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_request_has_default_temperature_test() {
  let req = llm.new_request("m", "p", 100)
  case req {
    llm.LLMRequest(temperature: 0.7, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Request Modification ===

pub fn with_system_prompt_adds_system_prompt_test() {
  let req =
    llm.new_request("m", "p", 100)
    |> llm.with_system_prompt("You are helpful")
  case req {
    llm.LLMRequest(system_prompt: Some("You are helpful"), ..) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn with_temperature_changes_temperature_test() {
  let req =
    llm.new_request("m", "p", 100)
    |> llm.with_temperature(0.0)
  case req {
    llm.LLMRequest(temperature: 0.0, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn modifiers_can_be_chained_test() {
  let req =
    llm.new_request("model", "prompt", 500)
    |> llm.with_system_prompt("system")
    |> llm.with_temperature(0.5)
  case req {
    llm.LLMRequest(
      model: "model",
      prompt: "prompt",
      max_tokens: 500,
      system_prompt: Some("system"),
      temperature: 0.5,
    ) -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Endpoint Routing ===

pub fn route_request_auditor_uses_local_endpoint_test() {
  llm.route_request(llm.Auditor)
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn route_request_implementer_uses_local_endpoint_test() {
  llm.route_request(llm.Implementer)
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn route_request_architect_uses_local_endpoint_test() {
  llm.route_request(llm.Architect)
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn route_request_reviewer_uses_anthropic_endpoint_test() {
  llm.route_request(llm.Reviewer)
  |> llm.is_local_endpoint
  |> should.be_false
}

// === Endpoint Properties ===

pub fn is_local_endpoint_returns_true_for_local_test() {
  llm.LocalEndpoint("http://localhost:8080")
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn is_local_endpoint_returns_false_for_anthropic_test() {
  llm.AnthropicEndpoint("https://api.anthropic.com", "key")
  |> llm.is_local_endpoint
  |> should.be_false
}

pub fn get_endpoint_url_extracts_local_url_test() {
  llm.LocalEndpoint("http://localhost:8080/completion")
  |> llm.get_endpoint_url
  |> should.equal("http://localhost:8080/completion")
}

pub fn get_endpoint_url_extracts_anthropic_url_test() {
  llm.AnthropicEndpoint("https://api.anthropic.com/v1/messages", "secret")
  |> llm.get_endpoint_url
  |> should.equal("https://api.anthropic.com/v1/messages")
}

// === System Prompts ===

pub fn system_prompt_auditor_mentions_test_focus_test() {
  llm.system_prompt(llm.Auditor)
  |> string.contains("test")
  |> should.be_true
}

pub fn system_prompt_auditor_mentions_read_only_src_test() {
  llm.system_prompt(llm.Auditor)
  |> string.contains("src/ is read-only")
  |> should.be_true
}

pub fn system_prompt_implementer_mentions_minimal_code_test() {
  llm.system_prompt(llm.Implementer)
  |> string.contains("MINIMAL")
  |> should.be_true
}

pub fn system_prompt_implementer_restricts_to_src_test() {
  llm.system_prompt(llm.Implementer)
  |> string.contains("src/ files only")
  |> should.be_true
}

pub fn system_prompt_architect_mentions_cupid_test() {
  llm.system_prompt(llm.Architect)
  |> string.contains("CUPID")
  |> should.be_true
}

pub fn system_prompt_architect_mentions_refactoring_test() {
  llm.system_prompt(llm.Architect)
  |> string.contains("refactoring")
  |> should.be_true
}

pub fn system_prompt_reviewer_mentions_acceptance_criteria_test() {
  llm.system_prompt(llm.Reviewer)
  |> string.contains("acceptance criteria")
  |> should.be_true
}

pub fn system_prompt_reviewer_mentions_verdict_test() {
  llm.system_prompt(llm.Reviewer)
  |> string.contains("pass/fail")
  |> should.be_true
}

// === Error Types ===

pub fn llm_error_network_error_captures_message_test() {
  let err = llm.NetworkError("connection refused")
  case err {
    llm.NetworkError("connection refused") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn llm_error_parse_error_captures_message_test() {
  let err = llm.ParseError("invalid json")
  case err {
    llm.ParseError("invalid json") -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn llm_error_rate_limit_captures_retry_after_test() {
  let err = llm.RateLimitError(60)
  case err {
    llm.RateLimitError(60) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn llm_error_auth_error_captures_message_test() {
  let err = llm.AuthError("invalid api key")
  case err {
    llm.AuthError("invalid api key") -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Response Types ===

pub fn llm_response_captures_all_fields_test() {
  let usage = llm.TokenUsage(prompt_tokens: 10, completion_tokens: 50, total_tokens: 60)
  let response = llm.LLMResponse(content: "Hello", finish_reason: "stop", usage: usage)
  case response {
    llm.LLMResponse(content: "Hello", finish_reason: "stop", usage: u) -> {
      case u {
        llm.TokenUsage(prompt_tokens: 10, completion_tokens: 50, total_tokens: 60) ->
          should.be_true(True)
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn token_usage_tracks_prompt_tokens_test() {
  let usage = llm.TokenUsage(prompt_tokens: 100, completion_tokens: 0, total_tokens: 100)
  case usage {
    llm.TokenUsage(prompt_tokens: 100, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn token_usage_tracks_completion_tokens_test() {
  let usage = llm.TokenUsage(prompt_tokens: 0, completion_tokens: 200, total_tokens: 200)
  case usage {
    llm.TokenUsage(completion_tokens: 200, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}
