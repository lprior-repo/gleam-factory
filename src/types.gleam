//// Type definitions for process management and workspace state.

import gleam/erlang/process.{type Pid, type Subject}
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


/// AcpNotification represents an Agent Communication Protocol notification.
pub type AcpNotification {
  AcpNotification(session_id: String, method: String)
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
    True ->
      json_str
      |> string.split("\"capabilities\":[")
      |> list.last
      |> option.from_result
      |> option.then(fn(s) { string.split(s, "]") |> list.first |> option.from_result })
      |> option.map(fn(s) {
        s
        |> string.replace("\"", "")
        |> string.split(",")
        |> list.filter(fn(x) { string.length(string.trim(x)) > 0 })
      })
      |> option.unwrap([])
      |> Ok
  }
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

@external(erlang, "erlang", "phash2")
fn hash_pid(pid: process.Pid) -> Int

pub opaque type GpuTicket {
  GpuTicket(gov_id: Int, id: Int)
}

pub opaque type GpuGovernor {
  GpuGovernor(gov_id: Int, subject: Subject(GpuMessage))
}

type GpuMessage {
  Request(reply: Subject(Result(GpuTicket, Nil)))
  Release(ticket: GpuTicket, reply: Subject(Result(Nil, Nil)))
}

type GpuState {
  GpuState(gov_id: Int, limit: Int, next: Int, issued: List(Int), waiters: List(Subject(Result(GpuTicket, Nil))))
}

pub fn new_gpu_governor(limit: Int) -> Result(GpuGovernor, Nil) {
  case limit > 0 {
    False -> Error(Nil)
    True -> {
      let parent_subject = process.new_subject()
      process.spawn(fn() {
        let child_subject = process.new_subject()
        let assert Ok(pid) = process.subject_owner(child_subject)
        let gov_id = hash_pid(pid)
        let initial = GpuState(gov_id:, limit:, next: 0, issued: [], waiters: [])
        process.send(parent_subject, #(gov_id, child_subject))
        let selector = process.new_selector() |> process.select(child_subject)
        gpu_loop(initial, selector)
      })
      case process.receive(parent_subject, 5000) {
        Ok(#(gov_id, child_subject)) -> Ok(GpuGovernor(gov_id, child_subject))
        Error(_) -> Error(Nil)
      }
    }
  }
}

fn gpu_loop(state: GpuState, selector: process.Selector(GpuMessage)) -> Nil {
  case process.selector_receive_forever(selector) {
    Request(reply:) ->
      case list.length(state.issued) < state.limit {
        True -> {
          let ticket = GpuTicket(state.gov_id, state.next)
          process.send(reply, Ok(ticket))
          gpu_loop(GpuState(..state, next: state.next + 1, issued: [state.next, ..state.issued]), selector)
        }
        False -> gpu_loop(GpuState(..state, waiters: [reply, ..state.waiters]), selector)
      }
    Release(ticket, reply:) ->
      case ticket.gov_id == state.gov_id && list.contains(state.issued, ticket.id) {
        False -> {
          process.send(reply, Error(Nil))
          gpu_loop(state, selector)
        }
        True -> {
          let new_issued = list.filter(state.issued, fn(id) { id != ticket.id })
          case state.waiters {
            [] -> {
              process.send(reply, Ok(Nil))
              gpu_loop(GpuState(..state, issued: new_issued), selector)
            }
            [w, ..rest] -> {
              let new_ticket = GpuTicket(state.gov_id, state.next)
              process.send(w, Ok(new_ticket))
              process.send(reply, Ok(Nil))
              gpu_loop(GpuState(..state, next: state.next + 1, issued: [state.next, ..new_issued], waiters: rest), selector)
            }
          }
        }
      }
  }
}

pub fn request_gpu_ticket(gov: GpuGovernor) -> Result(GpuTicket, Nil) {
  let GpuGovernor(_, subj) = gov
  let reply_subj = process.new_subject()
  process.send(subj, Request(reply_subj))
  case process.receive(reply_subj, 5000) {
    Ok(Ok(ticket)) -> Ok(ticket)
    Ok(Error(e)) -> Error(e)
    Error(e) -> Error(e)
  }
}

pub fn release_gpu_ticket(
  gov: GpuGovernor,
  ticket: GpuTicket,
) -> Result(Nil, Nil) {
  let GpuGovernor(_, subj) = gov
  let reply_subj = process.new_subject()
  process.send(subj, Release(ticket, reply_subj))
  case process.receive(reply_subj, 5000) {
    Ok(Ok(v)) -> Ok(v)
    Ok(Error(e)) -> Error(e)
    Error(e) -> Error(e)
  }
}
