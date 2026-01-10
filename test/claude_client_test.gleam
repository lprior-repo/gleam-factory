import claude_client
import gleeunit/should

pub fn parse_response_with_escaped_quotes_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"The user said \\\"hello\\\"\"}],\"stop_reason\":\"end_turn\"}"
  claude_client.parse_response(json)
  |> should.be_ok
  |> fn(resp) { resp.content }
  |> should.equal("The user said \"hello\"")
}

pub fn parse_response_with_newlines_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"Line1\\nLine2\"}],\"stop_reason\":\"end_turn\"}"
  claude_client.parse_response(json)
  |> should.be_ok
  |> fn(resp) { resp.content }
  |> should.equal("Line1\nLine2")
}

pub fn parse_response_with_backslashes_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"Path: C:\\\\Users\"}],\"stop_reason\":\"end_turn\"}"
  claude_client.parse_response(json)
  |> should.be_ok
  |> fn(resp) { resp.content }
  |> should.equal("Path: C:\\Users")
}

pub fn parse_response_with_unicode_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"Hello \\u0048\\u0049\"}],\"stop_reason\":\"end_turn\"}"
  claude_client.parse_response(json)
  |> should.be_ok
  |> fn(resp) { resp.content }
  |> should.equal("Hello HI")
}

pub fn parse_response_simple_text_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}],\"stop_reason\":\"end_turn\"}"
  claude_client.parse_response(json)
  |> should.be_ok
  |> fn(resp) { resp.content }
  |> should.equal("Hello world")
}

pub fn parse_response_extracts_stop_reason_test() {
  let json =
    "{\"content\":[{\"type\":\"text\",\"text\":\"test\"}],\"stop_reason\":\"max_tokens\"}"
  claude_client.parse_response(json)
  |> should.be_ok
  |> fn(resp) { resp.stop_reason }
  |> should.equal("max_tokens")
}
