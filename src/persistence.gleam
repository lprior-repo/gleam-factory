// Persistence module - Save/load task status as JSON
// Tracks which stages passed/failed for each task

import domain
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile

const branch_prefix = "feat/"

const colon_escape = "\\c"

fn encode_reason(reason: String) -> String {
  string.replace(reason, ":", colon_escape)
}

fn decode_reason(encoded: String) -> String {
  string.replace(encoded, colon_escape, ":")
}

fn parse_status(status_str: String) -> domain.TaskStatus {
  case string.split(status_str, ":") {
    ["created"] -> domain.Created
    ["passed"] -> domain.PassedPipeline
    ["integrated"] -> domain.Integrated
    ["in_progress", stage] -> domain.InProgress(stage)
    ["in_progress"] -> domain.InProgress("")
    ["failed", stage, ..rest] -> {
      let reason = rest |> string.join(":") |> decode_reason
      domain.FailedPipeline(stage, reason)
    }
    ["failed"] -> domain.FailedPipeline("", "")
    _ -> domain.Created
  }
}

pub type StageResult {
  StagePassed
  StageFailed
}

/// Stage status record
pub type StageRecord {
  StageRecord(
    stage_name: String,
    status: String,
    attempts: Int,
    last_error: String,
  )
}

/// Complete task record for persistence
pub type TaskRecord {
  TaskRecord(
    slug: String,
    language: String,
    status: String,
    priority: String,
    created_at: String,
    updated_at: String,
    stages: List(StageRecord),
    worktree_path: String,
  )
}

/// Status file location (.factory/tasks.json)
pub fn status_file_path(repo_root: String) -> String {
  repo_root <> "/.factory/tasks.json"
}

@external(erlang, "calendar", "universal_time")
fn universal_time() -> #(#(Int, Int, Int), #(Int, Int, Int))

fn int_to_padded_string(num: Int, width: Int) -> String {
  let str = int.to_string(num)
  let len = string.length(str)
  case len < width {
    True -> string.repeat("0", width - len) <> str
    False -> str
  }
}

fn current_timestamp() -> String {
  let #(#(year, month, day), #(hour, min, sec)) = universal_time()

  int_to_padded_string(year, 4)
  <> "-"
  <> int_to_padded_string(month, 2)
  <> "-"
  <> int_to_padded_string(day, 2)
  <> "T"
  <> int_to_padded_string(hour, 2)
  <> ":"
  <> int_to_padded_string(min, 2)
  <> ":"
  <> int_to_padded_string(sec, 2)
  <> "Z"
}

/// Create task record from domain task
pub fn task_to_record(task: domain.Task) -> TaskRecord {
  let language_str = case task.language {
    domain.Go -> "go"
    domain.Gleam -> "gleam"
    domain.Rust -> "rust"
    domain.Python -> "python"
  }

  let status_str = case task.status {
    domain.Created -> "created"
    domain.InProgress(stage) -> "in_progress:" <> stage
    domain.PassedPipeline -> "passed"
    domain.FailedPipeline(stage, reason) ->
      "failed:" <> stage <> ":" <> encode_reason(reason)
    domain.Integrated -> "integrated"
  }

  let priority_str = domain.priority_to_string(task.priority)

  let timestamp = current_timestamp()

  TaskRecord(
    slug: domain.slug_to_string(task.slug),
    language: language_str,
    status: status_str,
    priority: priority_str,
    created_at: timestamp,
    updated_at: timestamp,
    stages: [],
    worktree_path: task.worktree_path,
  )
}

/// Create domain task from record
pub fn record_to_task(record: TaskRecord) -> Result(domain.Task, String) {
  use lang <- result.try(case record.language {
    "go" -> Ok(domain.Go)
    "gleam" -> Ok(domain.Gleam)
    "rust" -> Ok(domain.Rust)
    "python" -> Ok(domain.Python)
    other -> Error("Unknown language: " <> other)
  })

  let status = parse_status(record.status)

  let priority = case domain.parse_priority(record.priority) {
    Ok(p) -> p
    Error(_) -> domain.P2
  }

  use slug <- result.try(domain.validate_slug(record.slug))

  Ok(domain.Task(
    slug: slug,
    language: lang,
    status: status,
    priority: priority,
    worktree_path: record.worktree_path,
    branch: branch_prefix <> record.slug,
  ))
}

/// Save task record to .factory/tasks.json
pub fn save_task_record(
  task: domain.Task,
  repo_root: String,
) -> Result(Nil, String) {
  let file_path = status_file_path(repo_root)
  let record = task_to_record(task)

  // Create .factory directory if it doesn't exist
  let factory_dir = repo_root <> "/.factory"
  case simplifile.create_directory_all(factory_dir) {
    Error(_) -> Error("Could not create .factory directory")
    Ok(Nil) -> {
      // Convert record to JSON string
      let json = record_to_json(record)

      // Write to file
      case simplifile.write(file_path, json) {
        Ok(Nil) -> Ok(Nil)
        Error(_) -> Error("Could not write task status file")
      }
    }
  }
}

/// Load task record from .factory/tasks.json
pub fn load_task_record(
  slug: String,
  repo_root: String,
) -> Result(domain.Task, String) {
  let file_path = status_file_path(repo_root)

  use content <- result.try(case simplifile.read(file_path) {
    Ok(content) -> Ok(content)
    Error(_) -> Error("Task status file not found")
  })

  use record <- result.try(json_to_record(content, slug))
  record_to_task(record)
}

/// List all tasks from .factory/tasks.json
pub fn list_all_tasks(repo_root: String) -> Result(List(domain.Task), String) {
  let file_path = status_file_path(repo_root)

  use content <- result.try(case simplifile.read(file_path) {
    Ok(content) -> Ok(content)
    Error(_) -> Error("Task status file not found")
  })

  use records <- result.try(json_to_all_records(content))
  list.try_map(records, record_to_task)
}

fn stage_result_to_string(result: StageResult) -> String {
  case result {
    StagePassed -> "passed"
    StageFailed -> "failed"
  }
}

pub fn build_stage_record(
  stage_name: String,
  result: StageResult,
  attempts: Int,
  error: String,
) -> StageRecord {
  StageRecord(
    stage_name: stage_name,
    status: stage_result_to_string(result),
    attempts: attempts,
    last_error: error,
  )
}

pub fn update_or_append_stage(
  stages: List(StageRecord),
  new_stage: StageRecord,
) -> List(StageRecord) {
  case list.find(stages, fn(s) { s.stage_name == new_stage.stage_name }) {
    Ok(_) ->
      list.map(stages, fn(s) {
        case s.stage_name == new_stage.stage_name {
          True -> new_stage
          False -> s
        }
      })
    Error(Nil) -> list.append(stages, [new_stage])
  }
}

/// Update stage status in task record
pub fn update_stage_status(
  task: domain.Task,
  stage_name: String,
  result: StageResult,
  attempts: Int,
  error: String,
  repo_root: String,
) -> Result(Nil, String) {
  let factory_dir = repo_root <> "/.factory"
  use Nil <- result.try(case simplifile.create_directory_all(factory_dir) {
    Ok(Nil) -> Ok(Nil)
    Error(_) -> Error("Could not create .factory directory")
  })

  let slug_str = domain.slug_to_string(task.slug)
  let task_record = task_to_record(task)

  use existing_stages <- result.try(
    case load_task_record(slug_str, repo_root) {
      Ok(_loaded_task) -> {
        use json_str <- result.try(
          simplifile.read(status_file_path(repo_root))
          |> result.map_error(fn(_) { "Could not read task file" })
        )
        use loaded_record <- result.try(json_to_record(json_str, slug_str))
        Ok(loaded_record.stages)
      }
      Error(_) -> Ok([])
    }
  )

  let new_stage = build_stage_record(stage_name, result, attempts, error)
  let updated_stages = update_or_append_stage(existing_stages, new_stage)
  let updated_record = TaskRecord(..task_record, stages: updated_stages)

  save_task_record_direct(updated_record, repo_root)
}

/// Save record directly (internal use)
fn save_task_record_direct(
  record: TaskRecord,
  repo_root: String,
) -> Result(Nil, String) {
  let file_path = status_file_path(repo_root)
  let json = record_to_json(record)

  case simplifile.write(file_path, json) {
    Ok(Nil) -> Ok(Nil)
    Error(_) -> Error("Could not write task status file")
  }
}

// ============================================================================
// JSON SERIALIZATION AND DESERIALIZATION
// ============================================================================

/// Convert stage record to JSON
fn stage_to_json(stage: StageRecord) -> json.Json {
  json.object([
    #("stage_name", json.string(stage.stage_name)),
    #("status", json.string(stage.status)),
    #("attempts", json.int(stage.attempts)),
    #("last_error", json.string(stage.last_error)),
  ])
}

/// Convert task record to JSON string
pub fn record_to_json(record: TaskRecord) -> String {
  let stages_json = json.array(record.stages, stage_to_json)

  json.object([
    #("slug", json.string(record.slug)),
    #("language", json.string(record.language)),
    #("status", json.string(record.status)),
    #("priority", json.string(record.priority)),
    #("created_at", json.string(record.created_at)),
    #("updated_at", json.string(record.updated_at)),
    #("stages", stages_json),
    #("worktree_path", json.string(record.worktree_path)),
  ])
  |> json.to_string
}

/// Decoder for a stage record
fn stage_decoder() -> decode.Decoder(StageRecord) {
  use stage_name <- decode.field("stage_name", decode.string)
  use status <- decode.field("status", decode.string)
  use attempts <- decode.field("attempts", decode.int)
  use last_error <- decode.field("last_error", decode.string)
  decode.success(StageRecord(
    stage_name: stage_name,
    status: status,
    attempts: attempts,
    last_error: last_error,
  ))
}

/// Decoder for a task record
fn task_record_decoder() -> decode.Decoder(TaskRecord) {
  use slug <- decode.field("slug", decode.string)
  use language <- decode.field("language", decode.string)
  use status <- decode.field("status", decode.string)
  use priority <- decode.optional_field("priority", "P2", decode.string)
  use created_at <- decode.field("created_at", decode.string)
  use updated_at <- decode.field("updated_at", decode.string)
  use stages <- decode.field("stages", decode.list(stage_decoder()))
  use worktree_path <- decode.optional_field("worktree_path", "", decode.string)
  decode.success(TaskRecord(
    slug: slug,
    language: language,
    status: status,
    priority: priority,
    created_at: created_at,
    updated_at: updated_at,
    stages: stages,
    worktree_path: worktree_path,
  ))
}

/// Convert JSON string to task record
pub fn json_to_record(
  json_string: String,
  _slug: String,
) -> Result(TaskRecord, String) {
  case json.parse(json_string, task_record_decoder()) {
    Ok(record) -> Ok(record)
    Error(_) -> Error("Could not parse JSON")
  }
}

/// Convert JSON string to all task records
fn json_to_all_records(json_string: String) -> Result(List(TaskRecord), String) {
  let list_decoder = decode.list(task_record_decoder())
  case json.parse(json_string, list_decoder) {
    Ok(records) -> Ok(records)
    Error(_) -> {
      // Try parsing as single object wrapped in a list
      case json_to_record(json_string, "") {
        Ok(record) -> Ok([record])
        Error(e) -> Error(e)
      }
    }
  }
}
