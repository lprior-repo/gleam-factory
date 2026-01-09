// Audit module - Track all task state changes and decisions
// Provides full audit trail for compliance and debugging
// Inspired by beads' actor tracking and timestamp system

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile

/// Type of audit event
pub type AuditEventType {
  TaskCreated
  TaskUpdated
  StageStarted
  StagePassed
  StageFailed
  StageRetried
  TaskApproved
  TaskRejected
  DeploymentStarted
  DeploymentCompleted
  DeploymentRolledBack
}

/// Single audit entry with full context
pub type AuditEntry {
  AuditEntry(
    timestamp: String,
    event_type: AuditEventType,
    task_slug: String,
    actor: String,
    details: String,
    metadata: List(#(String, String)),
  )
}

/// Audit log containing all entries for a task
pub type AuditLog {
  AuditLog(task_slug: String, entries: List(AuditEntry))
}

/// Convert event type to string
pub fn event_type_to_string(event_type: AuditEventType) -> String {
  case event_type {
    TaskCreated -> "task_created"
    TaskUpdated -> "task_updated"
    StageStarted -> "stage_started"
    StagePassed -> "stage_passed"
    StageFailed -> "stage_failed"
    StageRetried -> "stage_retried"
    TaskApproved -> "task_approved"
    TaskRejected -> "task_rejected"
    DeploymentStarted -> "deployment_started"
    DeploymentCompleted -> "deployment_completed"
    DeploymentRolledBack -> "deployment_rolled_back"
  }
}

/// Parse event type from string
pub fn string_to_event_type(s: String) -> Result(AuditEventType, String) {
  case s {
    "task_created" -> Ok(TaskCreated)
    "task_updated" -> Ok(TaskUpdated)
    "stage_started" -> Ok(StageStarted)
    "stage_passed" -> Ok(StagePassed)
    "stage_failed" -> Ok(StageFailed)
    "stage_retried" -> Ok(StageRetried)
    "task_approved" -> Ok(TaskApproved)
    "task_rejected" -> Ok(TaskRejected)
    "deployment_started" -> Ok(DeploymentStarted)
    "deployment_completed" -> Ok(DeploymentCompleted)
    "deployment_rolled_back" -> Ok(DeploymentRolledBack)
    other -> Error("Unknown event type: " <> other)
  }
}

/// Get current timestamp in ISO format
/// Uses Erlang os:system_time for millisecond precision
@external(erlang, "audit_ffi", "get_timestamp")
fn get_timestamp_ffi() -> String

/// Get current timestamp (wrapper with fallback)
pub fn get_timestamp() -> String {
  get_timestamp_ffi()
}

/// Get current actor from environment or default
@external(erlang, "audit_ffi", "get_actor")
fn get_actor_ffi() -> String

/// Get current actor (wrapper with fallback)
pub fn get_actor() -> String {
  get_actor_ffi()
}

/// Create new audit entry
pub fn create_entry(
  event_type: AuditEventType,
  task_slug: String,
  details: String,
  metadata: List(#(String, String)),
) -> AuditEntry {
  AuditEntry(
    timestamp: get_timestamp(),
    event_type: event_type,
    task_slug: task_slug,
    actor: get_actor(),
    details: details,
    metadata: metadata,
  )
}

/// Audit log file path
pub fn audit_file_path(repo_root: String, task_slug: String) -> String {
  repo_root <> "/.factory/audit/" <> task_slug <> ".jsonl"
}

/// Append audit entry to log file (JSONL format for git-friendly diffs)
pub fn log_event(
  repo_root: String,
  event_type: AuditEventType,
  task_slug: String,
  details: String,
  metadata: List(#(String, String)),
) -> Result(Nil, String) {
  let entry = create_entry(event_type, task_slug, details, metadata)
  let file_path = audit_file_path(repo_root, task_slug)
  let audit_dir = repo_root <> "/.factory/audit"

  // Ensure audit directory exists
  case simplifile.create_directory_all(audit_dir) {
    Error(_) -> Error("Could not create audit directory")
    Ok(Nil) -> {
      let json_line = entry_to_json(entry) <> "\n"

      // Append to file (create if not exists)
      case simplifile.append(file_path, json_line) {
        Ok(Nil) -> Ok(Nil)
        Error(_) -> Error("Could not write audit entry")
      }
    }
  }
}

/// Read all audit entries for a task
pub fn read_audit_log(
  repo_root: String,
  task_slug: String,
) -> Result(AuditLog, String) {
  let file_path = audit_file_path(repo_root, task_slug)

  case simplifile.read(file_path) {
    Error(_) -> Ok(AuditLog(task_slug: task_slug, entries: []))
    Ok(content) -> {
      let lines =
        content
        |> string.split("\n")
        |> list.filter(fn(line) { string.length(line) > 0 })

      let entries = list.filter_map(lines, json_to_entry)
      Ok(AuditLog(task_slug: task_slug, entries: entries))
    }
  }
}

/// Get last N audit entries for a task
pub fn get_recent_entries(
  repo_root: String,
  task_slug: String,
  count: Int,
) -> Result(List(AuditEntry), String) {
  use log <- result.try(read_audit_log(repo_root, task_slug))
  let recent =
    log.entries
    |> list.reverse
    |> list.take(count)
    |> list.reverse
  Ok(recent)
}

/// Filter audit entries by event type
pub fn filter_by_type(
  log: AuditLog,
  event_type: AuditEventType,
) -> List(AuditEntry) {
  list.filter(log.entries, fn(entry) { entry.event_type == event_type })
}

/// Get all stage events for a task
pub fn get_stage_history(log: AuditLog) -> List(AuditEntry) {
  list.filter(log.entries, fn(entry) {
    case entry.event_type {
      StageStarted | StagePassed | StageFailed | StageRetried -> True
      _ -> False
    }
  })
}

/// Get deployment events for a task
pub fn get_deployment_history(log: AuditLog) -> List(AuditEntry) {
  list.filter(log.entries, fn(entry) {
    case entry.event_type {
      DeploymentStarted | DeploymentCompleted | DeploymentRolledBack -> True
      _ -> False
    }
  })
}

// ============================================================================
// CONVENIENCE LOGGING FUNCTIONS
// ============================================================================

/// Log task creation
pub fn log_task_created(
  repo_root: String,
  task_slug: String,
  language: String,
  branch: String,
) -> Result(Nil, String) {
  log_event(repo_root, TaskCreated, task_slug, "Task created", [
    #("language", language),
    #("branch", branch),
  ])
}

/// Log stage start
pub fn log_stage_started(
  repo_root: String,
  task_slug: String,
  stage_name: String,
  attempt: Int,
) -> Result(Nil, String) {
  log_event(
    repo_root,
    StageStarted,
    task_slug,
    "Stage started: " <> stage_name,
    [
      #("stage", stage_name),
      #("attempt", int.to_string(attempt)),
    ],
  )
}

/// Log stage pass
pub fn log_stage_passed(
  repo_root: String,
  task_slug: String,
  stage_name: String,
  duration_ms: Int,
) -> Result(Nil, String) {
  log_event(repo_root, StagePassed, task_slug, "Stage passed: " <> stage_name, [
    #("stage", stage_name),
    #("duration_ms", int.to_string(duration_ms)),
  ])
}

/// Log stage failure
pub fn log_stage_failed(
  repo_root: String,
  task_slug: String,
  stage_name: String,
  error: String,
) -> Result(Nil, String) {
  log_event(repo_root, StageFailed, task_slug, "Stage failed: " <> stage_name, [
    #("stage", stage_name),
    #("error", error),
  ])
}

/// Log task approval
pub fn log_task_approved(
  repo_root: String,
  task_slug: String,
  strategy: String,
) -> Result(Nil, String) {
  log_event(repo_root, TaskApproved, task_slug, "Task approved for deployment", [
    #("strategy", strategy),
  ])
}

/// Log deployment start
pub fn log_deployment_started(
  repo_root: String,
  task_slug: String,
  rollout_percentage: Int,
) -> Result(Nil, String) {
  log_event(
    repo_root,
    DeploymentStarted,
    task_slug,
    "Deployment started at " <> int.to_string(rollout_percentage) <> "%",
    [#("rollout_percentage", int.to_string(rollout_percentage))],
  )
}

/// Log deployment completion
pub fn log_deployment_completed(
  repo_root: String,
  task_slug: String,
) -> Result(Nil, String) {
  log_event(
    repo_root,
    DeploymentCompleted,
    task_slug,
    "Deployment completed successfully",
    [],
  )
}

/// Log deployment rollback
pub fn log_deployment_rolled_back(
  repo_root: String,
  task_slug: String,
  reason: String,
) -> Result(Nil, String) {
  log_event(
    repo_root,
    DeploymentRolledBack,
    task_slug,
    "Deployment rolled back: " <> reason,
    [#("reason", reason)],
  )
}

/// Log agent execution (for TCR loop tracking)
pub fn log_agent_run(
  task_slug: String,
  iteration: Int,
  role: String,
  output: String,
) -> Result(Nil, String) {
  let audit_path = ".factory/audit/" <> task_slug <> ".jsonl"
  let entry =
    json.object([
      #("timestamp", json.string(get_timestamp())),
      #("iteration", json.int(iteration)),
      #("role", json.string(role)),
      #("output", json.string(output)),
      #("mode", json.string("api")),
    ])
    |> json.to_string

  case simplifile.append(audit_path, entry <> "\n") {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to append audit log")
  }
}

/// Log CLI tool execution (Claude Code, etc.)
pub fn log_cli_run(
  task_slug: String,
  iteration: Int,
  role: String,
  output: String,
  turns_used: Int,
) -> Result(Nil, String) {
  let audit_path = ".factory/audit/" <> task_slug <> ".jsonl"
  let entry =
    json.object([
      #("timestamp", json.string(get_timestamp())),
      #("iteration", json.int(iteration)),
      #("role", json.string(role)),
      #("output", json.string(output)),
      #("mode", json.string("cli")),
      #("turns_used", json.int(turns_used)),
    ])
    |> json.to_string

  case simplifile.append(audit_path, entry <> "\n") {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to append audit log")
  }
}

// ============================================================================
// JSON SERIALIZATION
// ============================================================================

/// Convert metadata list to JSON
fn metadata_to_json(metadata: List(#(String, String))) -> json.Json {
  json.object(list.map(metadata, fn(pair) { #(pair.0, json.string(pair.1)) }))
}

/// Convert audit entry to JSON string
fn entry_to_json(entry: AuditEntry) -> String {
  json.object([
    #("timestamp", json.string(entry.timestamp)),
    #("event_type", json.string(event_type_to_string(entry.event_type))),
    #("task_slug", json.string(entry.task_slug)),
    #("actor", json.string(entry.actor)),
    #("details", json.string(entry.details)),
    #("metadata", metadata_to_json(entry.metadata)),
  ])
  |> json.to_string
}

/// Decoder for metadata
fn metadata_decoder() -> decode.Decoder(List(#(String, String))) {
  decode.dict(decode.string, decode.string)
  |> decode.map(fn(d) {
    d
    |> dict_to_list
  })
}

/// Convert dict to list of tuples
@external(erlang, "maps", "to_list")
fn dict_to_list(dict: a) -> List(#(String, String))

/// Decoder for audit entry
fn entry_decoder() -> decode.Decoder(AuditEntry) {
  use timestamp <- decode.field("timestamp", decode.string)
  use event_type_str <- decode.field("event_type", decode.string)
  use task_slug <- decode.field("task_slug", decode.string)
  use actor <- decode.field("actor", decode.string)
  use details <- decode.field("details", decode.string)
  use metadata <- decode.field("metadata", metadata_decoder())

  case string_to_event_type(event_type_str) {
    Ok(event_type) ->
      decode.success(AuditEntry(
        timestamp: timestamp,
        event_type: event_type,
        task_slug: task_slug,
        actor: actor,
        details: details,
        metadata: metadata,
      ))
    Error(_) ->
      decode.success(AuditEntry(
        timestamp: timestamp,
        event_type: TaskUpdated,
        task_slug: task_slug,
        actor: actor,
        details: details,
        metadata: metadata,
      ))
  }
}

/// Parse JSON string to audit entry
fn json_to_entry(json_string: String) -> Result(AuditEntry, Nil) {
  case json.parse(json_string, entry_decoder()) {
    Ok(entry) -> Ok(entry)
    Error(_) -> Error(Nil)
  }
}
