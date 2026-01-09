import gleam/json
import gleam/result
import gleam/string
import gleam/dynamic/decode
import process

pub fn send_completion(
  url: String,
  prompt: String,
  max_tokens: Int,
) -> Result(String, String) {
  let json_body = build_request(prompt, max_tokens)
  use cmd_result <- result.try(process.run_command(
    "curl",
    ["-s", "-X", "POST", "-H", "Content-Type: application/json", "-d", json_body, "--max-time", "15", url],
    "",
  ))
  use response_json <- result.try(case cmd_result {
    process.Success(stdout, _, 0) -> Ok(stdout)
    process.Success(_, _, code) | process.Failure(_, code) ->
      Error("curl failed with code " <> string.inspect(code))
  })
  parse_response(response_json)
}

pub fn parse_response(json_str: String) -> Result(String, String) {
  let decoder = {
    use content <- decode.field("content", decode.string)
    decode.success(content)
  }
  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "invalid llama response JSON" })
}

fn build_request(prompt: String, max_tokens: Int) -> String {
  "{\"prompt\":\""
  <> escape_json(prompt)
  <> "\",\"n_predict\":"
  <> string.inspect(max_tokens)
  <> ",\"temperature\":0.7}"
}

fn escape_json(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}
