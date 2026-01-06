// Persistence module - Save/load task status as JSON
// Tracks which stages passed/failed for each task

import gleam/string
import gleam/list
import gleam/result
import gleam/int
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
// JSON SERIALIZATION AND DESERIALIZATION
// ============================================================================

/// Escape JSON string - handle special characters properly
fn escape_json_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}


/// Convert stage record to JSON object string
fn stage_to_json(stage: StageRecord) -> String {
  "{"
  <> "\"stage_name\":\"" <> escape_json_string(stage.stage_name) <> "\","
  <> "\"status\":\"" <> escape_json_string(stage.status) <> "\","
  <> "\"attempts\":" <> string.inspect(stage.attempts) <> ","
  <> "\"last_error\":\"" <> escape_json_string(stage.last_error) <> "\""
  <> "}"
}

/// Convert task record to JSON string
fn record_to_json(record: TaskRecord) -> String {
  let stages_json =
    record.stages
    |> list.map(stage_to_json)
    |> string.join(",")

  "{"
  <> "\"slug\":\"" <> escape_json_string(record.slug) <> "\","
  <> "\"language\":\"" <> escape_json_string(record.language) <> "\","
  <> "\"status\":\"" <> escape_json_string(record.status) <> "\","
  <> "\"created_at\":\"" <> escape_json_string(record.created_at) <> "\","
  <> "\"updated_at\":\"" <> escape_json_string(record.updated_at) <> "\","
  <> "\"stages\":[" <> stages_json <> "]"
  <> "}"
}

/// Extract a JSON string value by key, handling escaping properly
fn extract_json_string(json: String, key: String) -> Result(String, String) {
  let search = "\"" <> key <> "\":\""
  case string.contains(json, search) {
    False -> Error("Key not found: " <> key)
    True -> {
      case string.split_once(json, search) {
        Error(Nil) -> Error("Could not parse JSON")
        Ok(#(_, rest)) -> {
          // Extract until we find an unescaped quote
          extract_string_value(rest, "")
        }
      }
    }
  }
}

/// Extract a JSON integer value by key
fn extract_json_int(json: String, key: String) -> Result(Int, String) {
  let search = "\"" <> key <> "\":"
  case string.contains(json, search) {
    False -> Error("Key not found: " <> key)
    True -> {
      case string.split_once(json, search) {
        Error(Nil) -> Error("Could not parse JSON")
        Ok(#(_, rest)) -> {
          let trimmed = skip_whitespace(rest, 0)
          case extract_number_value(trimmed, "") {
            Ok(num_str) ->
              case int.parse(num_str) {
                Ok(n) -> Ok(n)
                Error(Nil) -> Error("Could not parse integer: " <> num_str)
              }
            Error(e) -> Error(e)
          }
        }
      }
    }
  }
}

/// Skip whitespace in string from position
fn skip_whitespace(json: String, pos: Int) -> String {
  case string.first(json) {
    Ok(" ") | Ok("\t") | Ok("\n") | Ok("\r") -> {
      skip_whitespace(string.slice(json, 1, string.length(json) - 1), pos + 1)
    }
    _ -> json
  }
}

/// Extract string value from position after opening quote
fn extract_string_value(json: String, acc: String) -> Result(String, String) {
  case string.first(json) {
    Ok("\\") -> {
      // Escape sequence - read next character
      case string.slice(json, 1, string.length(json) - 1) |> string.first {
        Ok(next_char) -> {
          let escaped = case next_char {
            "n" -> "\n"
            "r" -> "\r"
            "t" -> "\t"
            "\"" -> "\""
            "\\" -> "\\"
            other -> "\\" <> other
          }
          let rest = string.slice(json, 2, string.length(json) - 2)
          extract_string_value(rest, acc <> escaped)
        }
        Error(Nil) -> Error("Unexpected end of string")
      }
    }
    Ok("\"") -> {
      // End of string
      Ok(acc)
    }
    Ok(char) -> {
      let rest = string.slice(json, 1, string.length(json) - 1)
      extract_string_value(rest, acc <> char)
    }
    Error(Nil) -> Error("Unexpected end of string")
  }
}

/// Extract number value (int or float)
fn extract_number_value(json: String, acc: String) -> Result(String, String) {
  case string.first(json) {
    Ok(char) -> {
      case char {
        "," | "}" | "]" -> {
          case string.length(acc) {
            0 -> Error("No number found")
            _ -> Ok(acc)
          }
        }
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "-" -> {
          let rest = string.slice(json, 1, string.length(json) - 1)
          extract_number_value(rest, acc <> char)
        }
        _ -> {
          case string.length(acc) {
            0 -> Error("Invalid number")
            _ -> Ok(acc)
          }
        }
      }
    }
    Error(Nil) -> {
      case string.length(acc) {
        0 -> Error("No number found")
        _ -> Ok(acc)
      }
    }
  }
}

/// Parse JSON array of stage objects
fn parse_json_stages(json: String) -> Result(List(StageRecord), String) {
  // Extract stages array
  let search = "\"stages\":["
  case string.split_once(json, search) {
    Error(Nil) -> Ok([])
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "]") {
        Error(Nil) -> Error("Unclosed stages array")
        Ok(#(stages_str, _)) -> {
          parse_stage_objects(stages_str)
        }
      }
    }
  }
}

/// Parse individual stage objects from string
fn parse_stage_objects(stages_str: String) -> Result(List(StageRecord), String) {
  let trimmed = string.trim(stages_str)
  case string.length(trimmed) {
    0 -> Ok([])
    _ -> {
      // Split by }, { but preserve the content
      parse_stage_list(trimmed, [])
    }
  }
}

/// Parse stage list recursively
fn parse_stage_list(
  json: String,
  acc: List(StageRecord),
) -> Result(List(StageRecord), String) {
  let trimmed = string.trim(json)
  case string.length(trimmed) {
    0 -> Ok(list.reverse(acc))
    _ -> {
      case string.starts_with(trimmed, "{") {
        True -> {
          case find_closing_brace(trimmed, 0) {
            Ok(end_pos) -> {
              let obj_str = string.slice(trimmed, 0, end_pos + 1)
              use stage <- result.try(parse_stage_object(obj_str))
              let rest = string.slice(trimmed, end_pos + 1, string.length(trimmed) - end_pos - 1)
              let rest_trimmed = skip_whitespace(rest, 0)
              let rest_trimmed = case string.starts_with(rest_trimmed, ",") {
                True -> {
                  let after_comma = string.slice(rest_trimmed, 1, string.length(rest_trimmed) - 1)
                  skip_whitespace(after_comma, 0)
                }
                False -> rest_trimmed
              }
              parse_stage_list(rest_trimmed, [stage, ..acc])
            }
            Error(e) -> Error(e)
          }
        }
        False -> Error("Expected { in stages array")
      }
    }
  }
}

/// Parse single stage object
fn parse_stage_object(json: String) -> Result(StageRecord, String) {
  use stage_name <- result.try(extract_json_string(json, "stage_name"))
  use status <- result.try(extract_json_string(json, "status"))
  use attempts <- result.try(extract_json_int(json, "attempts"))
  use last_error <- result.try(extract_json_string(json, "last_error"))

  Ok(StageRecord(
    stage_name: stage_name,
    status: status,
    attempts: attempts,
    last_error: last_error,
  ))
}

/// Find closing brace position, handling nesting
fn find_closing_brace(json: String, depth: Int) -> Result(Int, String) {
  find_closing_brace_helper(json, 0, depth)
}

fn find_closing_brace_helper(
  json: String,
  pos: Int,
  depth: Int,
) -> Result(Int, String) {
  case string.first(json) {
    Ok("{") -> {
      let rest = string.slice(json, 1, string.length(json) - 1)
      find_closing_brace_helper(rest, pos + 1, depth + 1)
    }
    Ok("}") -> {
      case depth {
        1 -> Ok(pos)
        _ -> {
          let rest = string.slice(json, 1, string.length(json) - 1)
          find_closing_brace_helper(rest, pos + 1, depth - 1)
        }
      }
    }
    Ok(_) -> {
      let rest = string.slice(json, 1, string.length(json) - 1)
      find_closing_brace_helper(rest, pos + 1, depth)
    }
    Error(Nil) -> Error("Unclosed brace")
  }
}

/// Convert JSON string to task record
fn json_to_record(
  json: String,
  _slug: String,
) -> Result(TaskRecord, String) {
  use slug <- result.try(extract_json_string(json, "slug"))
  use language <- result.try(extract_json_string(json, "language"))
  use status <- result.try(extract_json_string(json, "status"))
  use created_at <- result.try(extract_json_string(json, "created_at"))
  use updated_at <- result.try(extract_json_string(json, "updated_at"))
  use stages <- result.try(parse_json_stages(json))

  Ok(TaskRecord(
    slug: slug,
    language: language,
    status: status,
    created_at: created_at,
    updated_at: updated_at,
    stages: stages,
  ))
}

/// Convert JSON string to all task records
fn json_to_all_records(json: String) -> Result(List(TaskRecord), String) {
  let trimmed = string.trim(json)
  case string.starts_with(trimmed, "[") {
    True -> {
      // Array format
      case string.ends_with(trimmed, "]") {
        True -> {
          let inner = string.slice(trimmed, 1, string.length(trimmed) - 2)
          case string.length(string.trim(inner)) {
            0 -> Ok([])
            _ -> parse_task_list(inner, [])
          }
        }
        False -> Error("Unclosed array")
      }
    }
    False -> {
      // Single object format
      use record <- result.try(json_to_record(trimmed, ""))
      Ok([record])
    }
  }
}

/// Parse list of task objects
fn parse_task_list(
  json: String,
  acc: List(TaskRecord),
) -> Result(List(TaskRecord), String) {
  let trimmed = string.trim(json)
  case string.length(trimmed) {
    0 -> Ok(list.reverse(acc))
    _ -> {
      case string.starts_with(trimmed, "{") {
        True -> {
          case find_closing_brace(trimmed, 0) {
            Ok(end_pos) -> {
              let obj_str = string.slice(trimmed, 0, end_pos + 1)
              use task <- result.try(json_to_record(obj_str, ""))
              let rest = string.slice(trimmed, end_pos + 1, string.length(trimmed) - end_pos - 1)
              let rest_trimmed = skip_whitespace(rest, 0)
              let rest_trimmed = case string.starts_with(rest_trimmed, ",") {
                True -> {
                  let after_comma = string.slice(rest_trimmed, 1, string.length(rest_trimmed) - 1)
                  skip_whitespace(after_comma, 0)
                }
                False -> rest_trimmed
              }
              parse_task_list(rest_trimmed, [task, ..acc])
            }
            Error(e) -> Error(e)
          }
        }
        False -> Error("Expected { in task array")
      }
    }
  }
}
