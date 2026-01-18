import gleeunit
import gleeunit/should
import llama_client

pub fn main() {
  gleeunit.main()
}

pub fn escape_json_tab_test() {
  llama_client.escape_json("hello\tworld")
  |> should.equal("hello\\tworld")
}

pub fn escape_json_carriage_return_test() {
  llama_client.escape_json("line1\rline2")
  |> should.equal("line1\\rline2")
}

pub fn escape_json_windows_line_endings_test() {
  llama_client.escape_json("line1\r\nline2")
  |> should.equal("line1\\r\\nline2")
}

pub fn escape_json_newline_test() {
  llama_client.escape_json("hello\nworld")
  |> should.equal("hello\\nworld")
}

pub fn escape_json_backslash_test() {
  llama_client.escape_json("path\\to\\file")
  |> should.equal("path\\\\to\\\\file")
}

pub fn escape_json_quote_test() {
  llama_client.escape_json("say \"hello\"")
  |> should.equal("say \\\"hello\\\"")
}

pub fn escape_json_combined_test() {
  llama_client.escape_json("line1\n\tindented\r\nquote:\"val\"")
  |> should.equal("line1\\n\\tindented\\r\\nquote:\\\"val\\\"")
}
