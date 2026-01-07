//// Type definitions for process management and workspace state.

import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// ProcessId wraps a process identifier to provide type-safe process references.
pub opaque type ProcessId {
  ProcessId(pid: Pid)
}

/// Converts a process Pid to a ProcessId opaque type.
pub fn from_pid(pid: Pid) -> ProcessId {
  ProcessId(pid)
}

/// Unwraps ProcessId to Pid.
pub fn to_pid(pid: ProcessId) -> Pid {
  pid.pid
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

/// Parses string to GitHash (40 lowercase hex chars).
pub fn git_hash_parse(input: String) -> Result(GitHash, String) {
  let trimmed = string.trim(input)
  case string.length(trimmed), is_valid_hex(trimmed) {
    40, True -> Ok(GitHash(trimmed))
    40, False -> Error("Invalid git hash: must contain only lowercase hexadecimal characters (0-9, a-f)")
    len, _ -> Error("Invalid git hash: expected 40 characters, got " <> int.to_string(len))
  }
}

/// Unwraps GitHash to string.
pub fn git_hash_to_string(hash: GitHash) -> String {
  hash.hash
}

fn is_valid_hex(input: String) -> Bool {
  input
  |> string.split("")
  |> list.all(fn(c) {
    case c {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "a" | "b" | "c" | "d" | "e" | "f" -> True
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

/// Parses JSON string to AcpNotification extracting method and session_id.
pub fn parse_acp_notification(json_str: String) -> Result(AcpNotification, String) {
  case extract_field(json_str, "method"), extract_field(json_str, "session_id") {
    Ok(method), Ok(session_id) -> Ok(AcpNotification(session_id:, method:))
    Error(_), _ -> Error("Missing method field")
    _, Error(_) -> Error("Missing session_id in params")
  }
}

fn extract_field(json_str: String, field: String) -> Result(String, Nil) {
  json_str
  |> string.split("\"" <> field <> "\":\"")
  |> list.last
  |> result.try(fn(s) { string.split(s, "\"") |> list.first })
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
    AcpClient(base_url:) | AcpClientWithCaps(base_url:, ..) -> base_url
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
  case client {
    AcpClient(base_url:) | AcpClientWithCaps(base_url:, ..) ->
      AcpClientWithCaps(base_url:, capabilities: caps)
  }
}

/// Parses ACP initialize response JSON extracting capabilities list.
pub fn parse_initialize_result(json_str: String) -> Result(List(String), String) {
  case string.contains(json_str, "capabilities") {
    False -> Error("No capabilities field")
    True -> {
      use after_cap <- result.try(json_str |> string.split("\"capabilities\":[") |> list.last |> result.replace_error(""))
      use caps_str <- result.try(after_cap |> string.split("]") |> list.first |> result.replace_error(""))
      caps_str
        |> string.replace("\"", "")
        |> string.split(",")
        |> list.filter(fn(x) { string.length(string.trim(x)) > 0 })
        |> Ok
    }
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
  GpuState(limit: Int, next: Int, issued: List(Int), waiters: List(Subject(Result(GpuTicket, Nil))))
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
        let initial = GpuState(limit:, next: 0, issued: [], waiters: [])
        process.send(parent_subject, #(gov_id, child_subject))
        let selector = process.new_selector() |> process.select(child_subject)
        gpu_loop(gov_id, initial, selector)
      })
      case process.receive(parent_subject, 5000) {
        Ok(#(gov_id, child_subject)) -> Ok(GpuGovernor(gov_id, child_subject))
        Error(_) -> Error(Nil)
      }
    }
  }
}

fn gpu_loop(gov_id: Int, state: GpuState, selector: process.Selector(GpuMessage)) -> Nil {
  case process.selector_receive_forever(selector) {
    Request(reply:) -> handle_request(gov_id, state, reply, selector)
    Release(ticket, reply:) -> handle_release(gov_id, state, ticket, reply, selector)
  }
}

fn handle_request(gov_id: Int, state: GpuState, reply: Subject(Result(GpuTicket, Nil)), selector: process.Selector(GpuMessage)) -> Nil {
  case list.length(state.issued) < state.limit {
    True -> {
      process.send(reply, Ok(GpuTicket(gov_id, state.next)))
      gpu_loop(gov_id, GpuState(..state, next: state.next + 1, issued: [state.next, ..state.issued]), selector)
    }
    False -> gpu_loop(gov_id, GpuState(..state, waiters: [reply, ..state.waiters]), selector)
  }
}

fn handle_release(gov_id: Int, state: GpuState, ticket: GpuTicket, reply: Subject(Result(Nil, Nil)), selector: process.Selector(GpuMessage)) -> Nil {
  case ticket.gov_id == gov_id && list.contains(state.issued, ticket.id) {
    False -> {
      process.send(reply, Error(Nil))
      gpu_loop(gov_id, state, selector)
    }
    True -> release_valid_ticket(gov_id, state, ticket, reply, selector)
  }
}

fn release_valid_ticket(gov_id: Int, state: GpuState, ticket: GpuTicket, reply: Subject(Result(Nil, Nil)), selector: process.Selector(GpuMessage)) -> Nil {
  let new_issued = list.filter(state.issued, fn(id) { id != ticket.id })
  process.send(reply, Ok(Nil))
  case list.reverse(state.waiters) {
    [] -> gpu_loop(gov_id, GpuState(..state, issued: new_issued, waiters: []), selector)
    [w, ..rest] -> {
      process.send(w, Ok(GpuTicket(gov_id, ticket.id)))
      gpu_loop(gov_id, GpuState(..state, issued: new_issued, waiters: list.reverse(rest)), selector)
    }
  }
}

pub fn request_gpu_ticket(gov: GpuGovernor) -> Result(GpuTicket, Nil) {
  let GpuGovernor(_, subj) = gov
  let reply_subj = process.new_subject()
  process.send(subj, Request(reply_subj))
  result.flatten(process.receive(reply_subj, 5000))
}

pub fn release_gpu_ticket(
  gov: GpuGovernor,
  ticket: GpuTicket,
) -> Result(Nil, Nil) {
  let GpuGovernor(_, subj) = gov
  let reply_subj = process.new_subject()
  process.send(subj, Release(ticket, reply_subj))
  result.flatten(process.receive(reply_subj, 5000))
}

pub fn with_gpu_ticket(
  gov: GpuGovernor,
  work_fn: fn(GpuTicket) -> Result(a, Nil),
) -> Result(a, Nil) {
  use ticket <- result.try(request_gpu_ticket(gov))
  let work_result = work_fn(ticket)
  let _ = release_gpu_ticket(gov, ticket)
  work_result
}

import gleam/dict

pub opaque type UpdateStore {
  UpdateStore(store: dict.Dict(String, List(AcpNotification)))
}

pub fn new_update_store() -> UpdateStore {
  UpdateStore(dict.new())
}

pub fn store_update(store: UpdateStore, notif: AcpNotification) -> UpdateStore {
  let UpdateStore(d) = store
  UpdateStore(dict.upsert(d, notif.session_id, fn(opt) {
    case opt {
      option.Some(ns) -> [notif, ..ns]
      option.None -> [notif]
    }
  }))
}

pub fn query_updates(store: UpdateStore, sid: String) -> List(AcpNotification) {
  let UpdateStore(d) = store
  dict.get(d, sid) |> result.unwrap([]) |> list.reverse
}

pub fn filter_by_method(
  notifs: List(AcpNotification),
  method: String,
) -> List(AcpNotification) {
  list.filter(notifs, fn(n) { n.method == method })
}

pub fn query_all_updates(store: UpdateStore) -> List(AcpNotification) {
  let UpdateStore(d) = store
  dict.values(d) |> list.flatten |> list.reverse
}
