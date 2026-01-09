import domain
import gleeunit
import gleeunit/should
import persistence

pub fn main() {
  gleeunit.main()
}

// task_to_record/record_to_task roundtrip tests for all TaskStatus variants

pub fn roundtrip_created_test() {
  let assert Ok(slug) = domain.validate_slug("test-task")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.Created,
      worktree_path: "/path/to/worktree",
      branch: "feat/test-task",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)

  restored.slug
  |> domain.slug_to_string
  |> should.equal("test-task")

  restored.language
  |> should.equal(domain.Gleam)

  case restored.status {
    domain.Created -> True
    _ -> False
  }
  |> should.equal(True)
}

pub fn roundtrip_in_progress_test() {
  let assert Ok(slug) = domain.validate_slug("task-progress")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Go,
      status: domain.InProgress("implement"),
      worktree_path: "/path",
      branch: "feat/task-progress",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)

  case restored.status {
    domain.InProgress(_) -> True
    _ -> False
  }
  |> should.equal(True)
}

pub fn roundtrip_passed_pipeline_test() {
  let assert Ok(slug) = domain.validate_slug("task-passed")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Rust,
      status: domain.PassedPipeline,
      worktree_path: "/path",
      branch: "feat/task-passed",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)

  case restored.status {
    domain.PassedPipeline -> True
    _ -> False
  }
  |> should.equal(True)
}

pub fn roundtrip_failed_pipeline_test() {
  let assert Ok(slug) = domain.validate_slug("task-failed")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Python,
      status: domain.FailedPipeline("lint", "formatting error"),
      worktree_path: "/path",
      branch: "feat/task-failed",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)

  case restored.status {
    domain.FailedPipeline(_, _) -> True
    _ -> False
  }
  |> should.equal(True)
}

pub fn roundtrip_integrated_test() {
  let assert Ok(slug) = domain.validate_slug("task-integrated")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.Integrated,
      worktree_path: "/path",
      branch: "feat/task-integrated",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)

  case restored.status {
    domain.Integrated -> True
    _ -> False
  }
  |> should.equal(True)
}

pub fn roundtrip_all_languages_go_test() {
  let assert Ok(slug) = domain.validate_slug("go-task")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Go,
      status: domain.Created,
      worktree_path: "/path",
      branch: "feat/go-task",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)
  restored.language |> should.equal(domain.Go)
}

pub fn roundtrip_all_languages_rust_test() {
  let assert Ok(slug) = domain.validate_slug("rust-task")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Rust,
      status: domain.Created,
      worktree_path: "/path",
      branch: "feat/rust-task",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)
  restored.language |> should.equal(domain.Rust)
}

pub fn roundtrip_all_languages_python_test() {
  let assert Ok(slug) = domain.validate_slug("python-task")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Python,
      status: domain.Created,
      worktree_path: "/path",
      branch: "feat/python-task",
    )

  let record = persistence.task_to_record(task)
  let assert Ok(restored) = persistence.record_to_task(record)
  restored.language |> should.equal(domain.Python)
}

// JSON serialization tests

pub fn json_roundtrip_basic_test() {
  let record =
    persistence.TaskRecord(
      slug: "json-test",
      language: "gleam",
      status: "created",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [],
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "json-test")

  restored.slug |> should.equal("json-test")
  restored.language |> should.equal("gleam")
  restored.status |> should.equal("created")
}

pub fn json_roundtrip_with_stages_test() {
  let stages = [
    persistence.StageRecord(
      stage_name: "implement",
      status: "passed",
      attempts: 3,
      last_error: "",
    ),
    persistence.StageRecord(
      stage_name: "unit-test",
      status: "failed",
      attempts: 2,
      last_error: "test failure",
    ),
  ]

  let record =
    persistence.TaskRecord(
      slug: "staged-task",
      language: "go",
      status: "in_progress",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:05:00Z",
      stages: stages,
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "staged-task")

  restored.stages
  |> should.equal(stages)
}

pub fn json_all_fields_preserved_test() {
  let record =
    persistence.TaskRecord(
      slug: "complete-task",
      language: "rust",
      status: "passed",
      created_at: "2026-01-09T08:30:00Z",
      updated_at: "2026-01-09T09:45:00Z",
      stages: [
        persistence.StageRecord(
          stage_name: "review",
          status: "passed",
          attempts: 1,
          last_error: "",
        ),
      ],
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "complete-task")

  restored.slug |> should.equal("complete-task")
  restored.language |> should.equal("rust")
  restored.status |> should.equal("passed")
  restored.created_at |> should.equal("2026-01-09T08:30:00Z")
  restored.updated_at |> should.equal("2026-01-09T09:45:00Z")
  restored.stages
  |> should.equal([
    persistence.StageRecord(
      stage_name: "review",
      status: "passed",
      attempts: 1,
      last_error: "",
    ),
  ])
}

// Stage status update tests

pub fn update_or_append_stage_new_test() {
  let stages = [
    persistence.StageRecord(
      stage_name: "implement",
      status: "passed",
      attempts: 1,
      last_error: "",
    ),
  ]

  let new_stage =
    persistence.StageRecord(
      stage_name: "unit-test",
      status: "failed",
      attempts: 2,
      last_error: "timeout",
    )

  let updated = persistence.update_or_append_stage(stages, new_stage)
  updated
  |> should.equal([
    persistence.StageRecord(
      stage_name: "implement",
      status: "passed",
      attempts: 1,
      last_error: "",
    ),
    persistence.StageRecord(
      stage_name: "unit-test",
      status: "failed",
      attempts: 2,
      last_error: "timeout",
    ),
  ])
}

pub fn update_or_append_stage_existing_test() {
  let stages = [
    persistence.StageRecord(
      stage_name: "implement",
      status: "passed",
      attempts: 1,
      last_error: "",
    ),
    persistence.StageRecord(
      stage_name: "unit-test",
      status: "failed",
      attempts: 1,
      last_error: "test1 failed",
    ),
  ]

  let updated_stage =
    persistence.StageRecord(
      stage_name: "unit-test",
      status: "passed",
      attempts: 2,
      last_error: "",
    )

  let result = persistence.update_or_append_stage(stages, updated_stage)
  result
  |> should.equal([
    persistence.StageRecord(
      stage_name: "implement",
      status: "passed",
      attempts: 1,
      last_error: "",
    ),
    persistence.StageRecord(
      stage_name: "unit-test",
      status: "passed",
      attempts: 2,
      last_error: "",
    ),
  ])
}

pub fn update_or_append_stage_empty_list_test() {
  let new_stage =
    persistence.StageRecord(
      stage_name: "implement",
      status: "passed",
      attempts: 1,
      last_error: "",
    )

  let result = persistence.update_or_append_stage([], new_stage)
  result
  |> should.equal([new_stage])
}

// Error case tests

pub fn record_to_task_invalid_language_test() {
  let record =
    persistence.TaskRecord(
      slug: "test-task",
      language: "javascript",
      status: "created",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [],
    )

  persistence.record_to_task(record)
  |> should.be_error()
}

pub fn record_to_task_invalid_slug_test() {
  let record =
    persistence.TaskRecord(
      slug: "INVALID SLUG",
      language: "gleam",
      status: "created",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [],
    )

  persistence.record_to_task(record)
  |> should.be_error()
}

pub fn json_to_record_malformed_json_test() {
  persistence.json_to_record("{not valid json", "test")
  |> should.be_error()
}

pub fn json_to_record_missing_fields_test() {
  let incomplete_json = "{\"slug\": \"test\"}"
  persistence.json_to_record(incomplete_json, "test")
  |> should.be_error()
}

// StageResult to string conversion

pub fn stage_result_passed_test() {
  let result = persistence.StagePassed
  let stage_record =
    persistence.build_stage_record("test", result, 1, "")
  stage_record.status |> should.equal("passed")
}

pub fn stage_result_failed_test() {
  let result = persistence.StageFailed
  let stage_record =
    persistence.build_stage_record("test", result, 1, "error")
  stage_record.status |> should.equal("failed")
}
