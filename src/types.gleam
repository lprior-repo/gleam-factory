//// Type definitions for process management and workspace state.

import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/string

/// ProcessId wraps a process identifier to provide type-safe process references.
pub opaque type ProcessId {
  ProcessId(pid: Pid)
}

/// Converts a process Pid to a ProcessId opaque type.
pub fn from_pid(pid: Pid) -> ProcessId {
  ProcessId(pid)
}

/// Converts a ProcessId back to its underlying Pid.
pub fn to_pid(process_id: ProcessId) -> Pid {
  let ProcessId(pid) = process_id
  pid
}

/// WorkspaceType represents the classification of a workspace.
pub type WorkspaceType {
  Jj
  Reflink
}

/// WorkspaceId provides type-safe workspace identification.
pub opaque type WorkspaceId {
  WorkspaceId(String)
}

/// Converts a string to a WorkspaceId opaque type.
pub fn new_workspace_id(id: String) -> WorkspaceId {
  WorkspaceId(id)
}

/// Workspace represents a managed workspace with metadata.
pub type Workspace {
  Workspace(
    id: WorkspaceId,
    path: String,
    workspace_type: WorkspaceType,
    owner_pid: ProcessId,
    created_at: String,
  )
}

/// GitHash is an opaque type wrapping a validated git SHA-1 hash string.
/// The hash is guaranteed to be exactly 40 lowercase hexadecimal characters.
pub opaque type GitHash {
  GitHash(hash: String)
}

/// Parses a string into a GitHash, validating that it is exactly 40 lowercase
/// hexadecimal characters (valid git SHA-1 hash format).
///
/// Returns Ok(GitHash) if the string is valid, Error(String) with a descriptive
/// message if validation fails.
///
/// Validation rules:
/// - Must be exactly 40 characters long
/// - Must contain only lowercase hexadecimal characters (0-9, a-f)
/// - Uppercase letters (A-F) are rejected to ensure consistency with git output
pub fn git_hash_parse(input: String) -> Result(GitHash, String) {
  let trimmed = string.trim(input)
  let length = string.length(trimmed)

  case length == 40 {
    False ->
      Error(
        "Invalid git hash: expected 40 characters, got "
        <> int.to_string(length),
      )
    True -> {
      // Validate that all characters are lowercase hexadecimal (0-9, a-f)
      case is_valid_hex(trimmed) {
        True -> Ok(GitHash(trimmed))
        False ->
          Error(
            "Invalid git hash: must contain only lowercase hexadecimal characters (0-9, a-f)",
          )
      }
    }
  }
}

/// Converts a GitHash back to its string representation.
/// This unwraps the opaque type to access the underlying hash string.
///
/// This function is needed for serialization, display, and comparison operations
/// where we need to work with the raw hash value.
pub fn git_hash_to_string(git_hash: GitHash) -> String {
  let GitHash(hash) = git_hash
  hash
}

/// Internal helper to validate that a string contains only lowercase hex characters.
/// Returns True if all characters are in [0-9a-f], False otherwise.
fn is_valid_hex(input: String) -> Bool {
  input
  |> string.split("")
  |> list.all(fn(char) {
    case char {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      "a" | "b" | "c" | "d" | "e" | "f" -> True
      _ -> False
    }
  })
}

/// AcpClient represents an Agent Communication Protocol HTTP client.
pub type AcpClient {
  AcpClient(base_url: String)
  AcpClientWithCaps(base_url: String, capabilities: List(String))
}

/// Creates a new AcpClient with no capabilities set.
pub fn new_acp_client(base_url: String) -> AcpClient {
  AcpClient(base_url:)
}

/// Creates AcpClient with capabilities.
pub fn new_acp_client_with_capabilities(base_url: String, capabilities: List(String)) -> AcpClient {
  AcpClientWithCaps(base_url:, capabilities:)
}

/// AcpNotification represents an Agent Communication Protocol notification.
pub type AcpNotification {
  AcpNotification(session_id: String, method: String)
}

/// Encodes AcpNotification to JSON string.
pub fn encode_acp_notification(
  notification: AcpNotification,
) -> Result(String, Nil) {
  let AcpNotification(session_id, method) = notification
  json.object([
    #("jsonrpc", json.string("2.0")),
    #("method", json.string(method)),
    #(
      "params",
      json.object([#("meta", json.object([#("sessionId", json.string(session_id))]))]),
    ),
  ])
  |> json.to_string
  |> Ok
}

/// SessionStatus represents the state of an ACP session.
pub type SessionStatus {
  Running
  Complete
  Paused
}

/// AcpSessionTracker tracks session states for cancellation logic.
pub opaque type AcpSessionTracker {
  AcpSessionTracker(sessions: List(#(String, SessionStatus)))
}

/// Creates a new empty AcpSessionTracker.
pub fn new_acp_session_tracker() -> AcpSessionTracker {
  AcpSessionTracker([])
}

/// Registers a session with its status.
pub fn register_session(
  tracker: AcpSessionTracker,
  session_id: String,
  status: SessionStatus,
) -> AcpSessionTracker {
  let AcpSessionTracker(sessions) = tracker
  AcpSessionTracker([#(session_id, status), ..sessions])
}

/// Checks if a session can be cancelled (only Running sessions can be cancelled).
pub fn can_cancel(
  tracker: AcpSessionTracker,
  session_id: String,
) -> Result(Bool, String) {
  let AcpSessionTracker(sessions) = tracker
  case list.key_find(sessions, session_id) {
    Ok(Running) -> Ok(True)
    Ok(_) -> Ok(False)
    Error(_) -> Error("Session not found")
  }
}


/// Extracts base_url from AcpClient.
pub fn get_base_url(client: AcpClient) -> String {
  case client {
    AcpClient(base_url:) -> base_url
    AcpClientWithCaps(base_url:, ..) -> base_url
  }
}

/// Extracts capabilities from AcpClient.
pub fn get_capabilities(client: AcpClient) -> option.Option(List(String)) {
  case client {
    AcpClient(..) -> option.None
    AcpClientWithCaps(capabilities:, ..) -> option.Some(capabilities)
  }
}

/// Stores capabilities in AcpClient, returning updated client.
pub fn store_capabilities(client: AcpClient, caps: List(String)) -> AcpClient {
  AcpClientWithCaps(base_url: get_base_url(client), capabilities: caps)
}

/// Parses ACP initialize response JSON extracting capabilities list.
pub fn parse_initialize_result(json_str: String) -> Result(List(String), String) {
  case string.contains(json_str, "capabilities") {
    False -> Error("No capabilities field")
    True -> {
      json_str
      |> extract_capabilities_array
      |> Ok
    }
  }
}

fn extract_capabilities_array(json_str: String) -> List(String) {
  json_str
  |> string.split("\"capabilities\":[")
  |> list.last
  |> option.from_result
  |> option.then(fn(s) { string.split(s, "]") |> list.first |> option.from_result })
  |> option.map(parse_json_string_array)
  |> option.unwrap([])
}

fn parse_json_string_array(s: String) -> List(String) {
  s
  |> string.replace("\"", "")
  |> string.split(",")
  |> list.filter(fn(x) { string.length(string.trim(x)) > 0 })
}

/// Encodes MCP initialize request to JSON string.
pub fn encode_initialize_request(
  protocol_version: String,
  client_name: String,
  client_version: String,
) -> Result(String, Nil) {
  json.object([
    #("jsonrpc", json.string("2.0")),
    #("method", json.string("initialize")),
    #(
      "params",
      json.object([
        #("protocolVersion", json.string(protocol_version)),
        #(
          "clientInfo",
          json.object([
            #("name", json.string(client_name)),
            #("version", json.string(client_version)),
          ]),
        ),
      ]),
    ),
  ])
  |> json.to_string
  |> Ok
}
