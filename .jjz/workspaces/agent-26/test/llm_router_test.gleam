import gleam/option.{None}
import gleam/string
import gleeunit/should
import llm
import llm_router

pub fn route_request_auditor_uses_local_test() {
  llm.route_request(llm.Auditor)
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn route_request_implementer_uses_local_test() {
  llm.route_request(llm.Implementer)
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn route_request_architect_uses_local_test() {
  llm.route_request(llm.Architect)
  |> llm.is_local_endpoint
  |> should.be_true
}

pub fn route_request_reviewer_uses_anthropic_test() {
  llm.route_request(llm.Reviewer)
  |> llm.is_local_endpoint
  |> should.be_false
}

pub fn route_request_auditor_returns_endpoint_test() {
  let endpoint = llm.route_request(llm.Auditor)
  llm.get_endpoint_url(endpoint)
  |> should.equal("http://localhost:8080/completion")
}

pub fn route_request_reviewer_returns_anthropic_endpoint_test() {
  let endpoint = llm.route_request(llm.Reviewer)
  llm.get_endpoint_url(endpoint)
  |> should.equal("https://api.anthropic.com/v1/messages")
}

pub fn float_formatting_preserves_two_decimals_test() {
  let request =
    llm.LLMRequest(
      prompt: "test",
      model: "model",
      system_prompt: None,
      max_tokens: 100,
      temperature: 0.75,
    )
  let json = llm_router.build_local_request_json(request)
  string.contains(json, "\"temperature\":0.75")
  |> should.be_true
}

pub fn float_formatting_preserves_single_decimal_test() {
  let request =
    llm.LLMRequest(
      prompt: "test",
      model: "model",
      system_prompt: None,
      max_tokens: 100,
      temperature: 0.5,
    )
  let json = llm_router.build_local_request_json(request)
  string.contains(json, "\"temperature\":0.5")
  |> should.be_true
}

pub fn float_formatting_handles_zero_test() {
  let request =
    llm.LLMRequest(
      prompt: "test",
      model: "model",
      system_prompt: None,
      max_tokens: 100,
      temperature: 0.0,
    )
  let json = llm_router.build_local_request_json(request)
  string.contains(json, "\"temperature\":0.0")
  |> should.be_true
}
