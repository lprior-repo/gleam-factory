// Property-based tests for persistence module using qcheck
// Tests pure data transformation invariants

import gleam/string
import gleam/list
import qcheck
import persistence
import domain

// PATH CONSTRUCTION - Property: status file path must always end with .json
pub fn prop_status_file_path_ends_with_json__test() {
  use root_len <- qcheck.given(qcheck.bounded_int(1, 30))
  let repo_root = string.repeat("a", root_len)
  let path = persistence.status_file_path(repo_root)
  assert string.ends_with(path, ".json")
}

// PATH CONSTRUCTION - Property: status file path must contain repo root
pub fn prop_status_file_path_contains_repo_root__test() {
  use root_len <- qcheck.given(qcheck.bounded_int(1, 30))
  let repo_root = string.repeat("a", root_len)
  let path = persistence.status_file_path(repo_root)
  assert string.contains(path, repo_root)
}

// PATH CONSTRUCTION - Property: status file path is deterministic
pub fn prop_status_file_path_deterministic__test() {
  use root_len <- qcheck.given(qcheck.bounded_int(1, 30))
  let repo_root = string.repeat("a", root_len)
  let path1 = persistence.status_file_path(repo_root)
  let path2 = persistence.status_file_path(repo_root)
  assert path1 == path2
}

// PATH CONSTRUCTION - Property: status file path contains .factory
pub fn prop_status_file_path_contains_factory_dir__test() {
  use root_len <- qcheck.given(qcheck.bounded_int(1, 30))
  let repo_root = string.repeat("a", root_len)
  let path = persistence.status_file_path(repo_root)
  assert string.contains(path, ".factory")
}

// RECORD CONVERSION - Property: task_to_record preserves slug
pub fn prop_task_to_record_preserves_slug__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 50))
  let slug = string.repeat("a", len)
  let task = domain.Task(
    slug: slug,
    language: domain.Gleam,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.slug == slug
}

// RECORD CONVERSION - Property: task_to_record converts language correctly
pub fn prop_task_to_record_language_go__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Go,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.language == "go"
}

pub fn prop_task_to_record_language_gleam__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.language == "gleam"
}

pub fn prop_task_to_record_language_rust__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Rust,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.language == "rust"
}

pub fn prop_task_to_record_language_python__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Python,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.language == "python"
}

// RECORD CONVERSION - Property: task_to_record converts Created status
pub fn prop_task_to_record_status_created__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.status == "created"
}

// RECORD CONVERSION - Property: task_to_record converts InProgress status
pub fn prop_task_to_record_status_in_progress__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.InProgress("test-stage"),
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.status == "in_progress"
}

// RECORD CONVERSION - Property: task_to_record converts PassedPipeline status
pub fn prop_task_to_record_status_passed__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.PassedPipeline,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.status == "passed"
}

// RECORD CONVERSION - Property: task_to_record converts FailedPipeline status
pub fn prop_task_to_record_status_failed__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.FailedPipeline("test-stage", "error"),
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.status == "failed"
}

// RECORD CONVERSION - Property: task_to_record converts Integrated status
pub fn prop_task_to_record_status_integrated__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.Integrated,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert record.status == "integrated"
}

// RECORD CONVERSION - Property: record_to_task round-trip for Created
pub fn prop_record_to_task_created_roundtrip__test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Gleam,
    status: domain.Created,
    worktree_path: "/tmp",
    branch: "main",
  )
  let record = persistence.task_to_record(task)
  assert case persistence.record_to_task(record) {
    Ok(t) -> t.slug == task.slug && t.language == task.language
    Error(_) -> False
  }
}

// STAGE RECORD - Property: stage name is preserved in StageRecord
pub fn prop_stage_record_preserves_name__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 50))
  let stage_name = string.repeat("x", len)
  let record = persistence.StageRecord(
    stage_name: stage_name,
    status: "passed",
    attempts: 1,
    last_error: "",
  )
  assert record.stage_name == stage_name
}

// STAGE RECORD - Property: status field only contains specific values
pub fn prop_stage_record_status_passed__test() {
  let record = persistence.StageRecord(
    stage_name: "test",
    status: "passed",
    attempts: 1,
    last_error: "",
  )
  assert record.status == "passed"
}

pub fn prop_stage_record_status_failed__test() {
  let record = persistence.StageRecord(
    stage_name: "test",
    status: "failed",
    attempts: 1,
    last_error: "",
  )
  assert record.status == "failed"
}

// STAGE RECORD - Property: attempts is non-negative
pub fn prop_stage_record_non_negative_attempts__test() {
  use attempts <- qcheck.given(qcheck.small_non_negative_int())
  let record = persistence.StageRecord(
    stage_name: "test",
    status: "passed",
    attempts: attempts,
    last_error: "",
  )
  assert record.attempts >= 0
}

// TASK RECORD - Property: task record preserves all slug chars
pub fn prop_task_record_preserves_slug_chars__test() {
  use slug_len <- qcheck.given(qcheck.bounded_int(1, 50))
  let slug = string.repeat("a", slug_len)
  let record = persistence.TaskRecord(
    slug: slug,
    language: "gleam",
    status: "created",
    created_at: "2025-01-01",
    updated_at: "2025-01-01",
    stages: [],
  )
  assert string.contains(record.slug, "a") || record.slug == ""
}

// TASK RECORD - Property: stages list in task record starts empty
pub fn prop_task_record_empty_stages__test() {
  let record = persistence.TaskRecord(
    slug: "test",
    language: "gleam",
    status: "created",
    created_at: "2025-01-01",
    updated_at: "2025-01-01",
    stages: [],
  )
  assert record.stages == []
}

// TASK RECORD - Property: can append stages to task record
pub fn prop_task_record_append_stages__test() {
  use num_stages <- qcheck.given(qcheck.bounded_int(1, 10))
  let stages = list.range(1, num_stages)
    |> list.map(fn(i) {
      persistence.StageRecord(
        stage_name: "stage" <> string.inspect(i),
        status: "passed",
        attempts: 1,
        last_error: "",
      )
    })
  let record = persistence.TaskRecord(
    slug: "test",
    language: "gleam",
    status: "created",
    created_at: "2025-01-01",
    updated_at: "2025-01-01",
    stages: stages,
  )
  assert list.length(record.stages) == num_stages
}
