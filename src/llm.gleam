//// LLM client types and routing for local and remote inference.
////
//// Provides unified interface for calling local llama.cpp or Anthropic API.

import gleam/option.{type Option, None, Some}

pub type Role {
  Implementer
  Architect
  Reviewer
}

pub type LLMRequest {
  LLMRequest(
    model: String,
    prompt: String,
    system_prompt: Option(String),
    max_tokens: Int,
    temperature: Float,
  )
}

pub type LLMResponse {
  LLMResponse(content: String, finish_reason: String, usage: TokenUsage)
}

pub type TokenUsage {
  TokenUsage(prompt_tokens: Int, completion_tokens: Int, total_tokens: Int)
}

pub type Endpoint {
  LocalEndpoint(url: String)
  AnthropicEndpoint(url: String, api_key: String)
}

pub type LLMError {
  NetworkError(String)
  ParseError(String)
  RateLimitError(Int)
  AuthError(String)
}

pub fn new_request(
  model: String,
  prompt: String,
  max_tokens: Int,
) -> LLMRequest {
  LLMRequest(
    model:,
    prompt:,
    system_prompt: None,
    max_tokens:,
    temperature: 0.7,
  )
}

pub fn with_system_prompt(req: LLMRequest, system: String) -> LLMRequest {
  LLMRequest(..req, system_prompt: Some(system))
}

pub fn with_temperature(req: LLMRequest, temp: Float) -> LLMRequest {
  LLMRequest(..req, temperature: temp)
}

pub fn route_request(role: Role) -> Endpoint {
  case role {
    Implementer -> LocalEndpoint("http://localhost:8080/completion")
    Architect -> LocalEndpoint("http://localhost:8080/completion")
    Reviewer -> AnthropicEndpoint("https://api.anthropic.com/v1/messages", "")
  }
}

pub fn is_local_endpoint(endpoint: Endpoint) -> Bool {
  case endpoint {
    LocalEndpoint(_) -> True
    AnthropicEndpoint(_, _) -> False
  }
}

pub fn get_endpoint_url(endpoint: Endpoint) -> String {
  case endpoint {
    LocalEndpoint(url) -> url
    AnthropicEndpoint(url, _) -> url
  }
}

pub fn system_prompt(role: Role) -> String {
  case role {
    Implementer -> implementer_system_prompt()
    Architect -> architect_system_prompt()
    Reviewer -> reviewer_system_prompt()
  }
}

fn implementer_system_prompt() -> String {
  "You are an IMPLEMENTER in a TCR loop. Write MINIMAL code to pass tests. Rules: src/ files only, if tests fail ALL changes are REVERTED, follow existing patterns, no refactoring, no extra features. Types use PascalCase, functions use snake_case, use Result for errors, pattern matching over conditionals."
}

fn architect_system_prompt() -> String {
  "You are an ARCHITECT reviewing code for refactoring. Apply CUPID principles: Composable, Unix philosophy, Predictable, Idiomatic, Domain-based. Keep functions under 30 lines, one responsibility each, no premature abstraction. Suggest deletions, simplifications, and extractions."
}

fn reviewer_system_prompt() -> String {
  "You are a REVIEWER validating implementation against acceptance criteria. Check: requirements implemented, tests pass and cover edge cases, code is clean and idiomatic, no security issues (injection, leaks), API is intuitive, types are effective. Provide pass/fail verdict with evidence."
}
