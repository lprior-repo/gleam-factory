import anthropic
import gleam/string
import gleeunit/should

// === Response Parsing Behavior ===

pub fn parse_response_extracts_text_from_valid_response_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}"
  anthropic.parse_response(json)
  |> should.be_ok
  |> should.equal("Hello world")
}

pub fn parse_response_trims_whitespace_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"  trimmed  \"}]}"
  anthropic.parse_response(json)
  |> should.be_ok
  |> should.equal("trimmed")
}

pub fn parse_response_errors_on_empty_text_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"\"}]}"
  anthropic.parse_response(json)
  |> should.be_error
}

pub fn parse_response_errors_on_whitespace_only_text_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"   \"}]}"
  anthropic.parse_response(json)
  |> should.be_error
}

pub fn parse_response_errors_on_invalid_json_test() {
  anthropic.parse_response("not json")
  |> should.be_error
}

pub fn parse_response_errors_on_missing_content_field_test() {
  anthropic.parse_response("{\"result\":\"ok\"}")
  |> should.be_error
}

pub fn parse_response_errors_on_empty_content_array_test() {
  anthropic.parse_response("{\"content\":[]}")
  |> should.be_error
}

pub fn parse_response_extracts_first_text_from_multiple_blocks_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"first\"},{\"type\":\"text\",\"text\":\"second\"}]}"
  anthropic.parse_response(json)
  |> should.be_ok
  |> should.equal("first")
}

// === Response Text Content ===

pub fn parse_response_preserves_newlines_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"line1\\nline2\"}]}"
  case anthropic.parse_response(json) {
    Ok(text) -> {
      text
      |> string.contains("\n")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_response_preserves_unicode_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"Hello 世界\"}]}"
  anthropic.parse_response(json)
  |> should.be_ok
  |> should.equal("Hello 世界")
}

pub fn parse_response_preserves_code_blocks_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"```gleam\\npub fn main() {}\\n```\"}]}"
  case anthropic.parse_response(json) {
    Ok(text) -> {
      text
      |> string.contains("```gleam")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

// === Error Response Handling ===

pub fn parse_response_errors_on_api_error_response_test() {
  let json = "{\"error\":{\"type\":\"invalid_request\",\"message\":\"bad\"}}"
  anthropic.parse_response(json)
  |> should.be_error
}

pub fn parse_response_errors_on_malformed_content_test() {
  let json = "{\"content\":\"not an array\"}"
  anthropic.parse_response(json)
  |> should.be_error
}

// === Edge Cases ===

pub fn parse_response_handles_nested_json_in_text_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"{\\\"key\\\":\\\"value\\\"}\"}]}"
  case anthropic.parse_response(json) {
    Ok(text) -> {
      text
      |> string.contains("key")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_response_handles_special_characters_test() {
  let json = "{\"content\":[{\"type\":\"text\",\"text\":\"<>&\\\"'\"}]}"
  anthropic.parse_response(json)
  |> should.be_ok
}
