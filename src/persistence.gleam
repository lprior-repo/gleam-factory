// Persistence module - Save/load task status as JSON
// Tracks which stages passed/failed for each task

import gleam/string
import gleam/list
import gleam/result
import simplifile
import domain

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
    created_at: String,
    updated_at: String,
    stages: List(StageRecord),
  )
}

/// Status file location (.factory/tasks.json)
pub fn status_file_path(repo_root: String) -> String {
  repo_root <> "/.factory/tasks.json"
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
    domain.InProgress(_) -> "in_progress"
    domain.PassedPipeline -> "passed"
    domain.FailedPipeline(_, _) -> "failed"
    domain.Integrated -> "integrated"
  }

  TaskRecord(
    slug: task.slug,
    language: language_str,
    status: status_str,
    created_at: "2025-01-04T00:00:00Z",
    updated_at: "2025-01-04T00:00:00Z",
    stages: [],
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

  let status = case record.status {
    "created" -> domain.Created
    "in_progress" -> domain.InProgress("")
    "passed" -> domain.PassedPipeline
    "failed" -> domain.FailedPipeline("", "")
    "integrated" -> domain.Integrated
    _ -> domain.Created
  }

  Ok(domain.Task(
    slug: record.slug,
    language: lang,
    status: status,
    worktree_path: "",
    branch: "feat/" <> record.slug,
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

/// Update stage status in task record
pub fn update_stage_status(
  task: domain.Task,
  stage_name: String,
  passed: Bool,
  attempts: Int,
  error: String,
  repo_root: String,
) -> Result(Nil, String) {
  let record = task_to_record(task)

  let new_stage = StageRecord(
    stage_name: stage_name,
    status: case passed {
      True -> "passed"
      False -> "failed"
    },
    attempts: attempts,
    last_error: error,
  )

  let updated_stages = case
    list.find(record.stages, fn(s) { s.stage_name == stage_name })
  {
    Ok(_) ->
      list.map(record.stages, fn(s) {
        case s.stage_name == stage_name {
          True -> new_stage
          False -> s
        }
      })
    Error(Nil) -> list.append(record.stages, [new_stage])
  }

  let updated_record =
    TaskRecord(
      ..record,
      stages: updated_stages,
    )

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
// JSON SERIALIZATION
// ============================================================================

/// Convert task record to JSON string (simplified)
fn record_to_json(record: TaskRecord) -> String {
  let stages_json =
    list.map(record.stages, fn(stage) {
      "{"
      <> "\"stage_name\":\"" <> stage.stage_name <> "\","
      <> "\"status\":\"" <> stage.status <> "\","
      <> "\"attempts\":" <> string.inspect(stage.attempts) <> ","
      <> "\"last_error\":\"" <> stage.last_error <> "\""
      <> "}"
    })
    |> string.join(",")

  "{"
  <> "\"slug\":\"" <> record.slug <> "\","
  <> "\"language\":\"" <> record.language <> "\","
  <> "\"status\":\"" <> record.status <> "\","
  <> "\"created_at\":\"" <> record.created_at <> "\","
  <> "\"updated_at\":\"" <> record.updated_at <> "\","
  <> "\"stages\":[" <> stages_json <> "]"
  <> "}"
}

/// Convert JSON string to task record (simplified)
fn json_to_record(
  _json: String,
  _slug: String,
) -> Result(TaskRecord, String) {
  // Simplified: return a stub for now
  // In production would use proper JSON parsing library
  Error("JSON parsing not yet implemented")
}

/// Convert JSON string to all task records
fn json_to_all_records(_json: String) -> Result(List(TaskRecord), String) {
  // Simplified: return empty list for now
  Ok([])
}
