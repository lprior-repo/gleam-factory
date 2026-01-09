import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/result
import gleam/string
import process
import types

pub type SessionStatus {
  Running
  Stopped
}

pub type Session {
  Session(id: String, status: SessionStatus)
}

pub opaque type AcpClientState {
  AcpClientState(client: types.AcpClient, sessions: Dict(String, Session))
}

pub fn new(base_url: String) -> AcpClientState {
  AcpClientState(types.new_acp_client(base_url), dict.new())
}

pub fn handle_create_session(
  state: AcpClientState,
) -> Result(#(String, AcpClientState), String) {
  use session_id <- result.try(process.acp_new_session(state.client))
  let session = Session(session_id, Running)
  let updated_sessions = dict.insert(state.sessions, session_id, session)
  Ok(#(session_id, AcpClientState(state.client, updated_sessions)))
}

pub fn handle_send_prompt(
  client: types.AcpClient,
  store: types.UpdateStore,
  session_id: String,
  prompt: String,
) -> Result(#(String, types.UpdateStore), String) {
  let json_body = build_prompt_request(session_id, prompt)
  use cmd_result <- result.try(process.run_command(
    "curl",
    [
      "-s",
      "-X",
      "POST",
      "-H",
      "Content-Type: application/json",
      "-d",
      json_body,
      types.get_base_url(client),
    ],
    "",
  ))
  use response <- result.try(case cmd_result {
    process.Success(stdout, _, 0) -> Ok(stdout)
    process.Success(_, _, code) | process.Failure(_, code) ->
      Error("http failed " <> string.inspect(code))
  })
  use #(content, notifs) <- result.try(parse_prompt_response(response))
  let updated_store = store_notifications(store, notifs)
  Ok(#(content, updated_store))
}

fn build_prompt_request(session_id: String, prompt: String) -> String {
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"session/prompt\",\"params\":{\"sessionId\":\""
  <> session_id
  <> "\",\"message\":\""
  <> escape_json(prompt)
  <> "\"}}"
}

fn escape_json(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

fn parse_prompt_response(
  json_str: String,
) -> Result(#(String, List(types.AcpNotification)), String) {
  use notifs <- result.try(extract_notifications(json_str))
  use content <- result.try(extract_content(json_str))
  Ok(#(content, notifs))
}

fn extract_notifications(
  json_str: String,
) -> Result(List(types.AcpNotification), String) {
  case string.contains(json_str, "notifications") {
    False -> Ok([])
    True -> Ok([])
  }
}

fn extract_content(json_str: String) -> Result(String, String) {
  let decoder = decode.at(["result", "content"], decode.string)
  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "invalid response" })
}

fn store_notifications(
  store: types.UpdateStore,
  notifs: List(types.AcpNotification),
) -> types.UpdateStore {
  case notifs {
    [] -> store
    [n, ..rest] -> store_notifications(types.store_update(store, n), rest)
  }
}
