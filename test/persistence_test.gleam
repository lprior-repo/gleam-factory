import domain
import gleeunit
import gleeunit/should
import persistence
import gleam/list
import simplifile
import gleam/string

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

// File I/O tests - save and load roundtrip
pub fn save_and_load_task_roundtrip_test() {
  let test_dir = "/tmp/persistence_test_io"
  let assert Ok(_) = simplifile.create_directory_all(test_dir)

  let assert Ok(slug) = domain.validate_slug("file-io-test")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.Created,
      worktree_path: "/tmp/wt",
      branch: "feat/file-io-test",
    )

  let assert Ok(Nil) = persistence.save_task_record(task, test_dir)
  let assert Ok(loaded) = persistence.load_task_record("file-io-test", test_dir)

  loaded.slug
  |> domain.slug_to_string
  |> should.equal("file-io-test")

  let assert Ok(_) = simplifile.delete(test_dir <> "/.factory/tasks.json")
  let assert Ok(_) = simplifile.delete(test_dir <> "/.factory")
  let assert Ok(_) = simplifile.delete(test_dir)
}

// File I/O test - directory creation
pub fn save_creates_factory_directory_test() {
  let test_dir = "/tmp/persistence_factory_dir"
  let factory_dir = test_dir <> "/.factory"

  let assert Ok(_) = simplifile.create_directory_all(test_dir)
  let assert Ok(slug) = domain.validate_slug("factory-test")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Go,
      status: domain.Created,
      worktree_path: "/tmp",
      branch: "feat/factory-test",
    )

  let assert Ok(Nil) = persistence.save_task_record(task, test_dir)

  case simplifile.verify_is_directory(factory_dir) {
    Ok(_) -> True
    _ -> False
  }
  |> should.equal(True)

  let assert Ok(_) = simplifile.delete(factory_dir <> "/tasks.json")
  let assert Ok(_) = simplifile.delete(factory_dir)
  let assert Ok(_) = simplifile.delete(test_dir)
}

// File I/O test - missing file handling
pub fn load_missing_file_test() {
  let test_dir = "/tmp/persistence_missing"
  let assert Ok(_) = simplifile.create_directory_all(test_dir)

  persistence.load_task_record("nonexistent", test_dir)
  |> should.be_error()

  let assert Ok(_) = simplifile.delete(test_dir)
}

// Invalid JSON tests - empty and malformed
pub fn json_empty_string_test() {
  persistence.json_to_record("", "test") |> should.be_error()
}

pub fn json_whitespace_only_test() {
  persistence.json_to_record("   \n\t  ", "test") |> should.be_error()
}

pub fn json_array_instead_of_object_test() {
  persistence.json_to_record("[]", "test") |> should.be_error()
}

pub fn json_null_value_test() {
  persistence.json_to_record("null", "test") |> should.be_error()
}

// Invalid JSON tests - missing required fields
pub fn json_missing_slug_test() {
  let incomplete =
    "{\"language\":\"gleam\",\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(incomplete, "test") |> should.be_error()
}

pub fn json_missing_language_test() {
  let incomplete =
    "{\"slug\":\"test\",\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(incomplete, "test") |> should.be_error()
}

pub fn json_missing_status_test() {
  let incomplete =
    "{\"slug\":\"test\",\"language\":\"gleam\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(incomplete, "test") |> should.be_error()
}

pub fn json_missing_created_at_test() {
  let incomplete =
    "{\"slug\":\"test\",\"language\":\"gleam\",\"status\":\"created\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(incomplete, "test") |> should.be_error()
}

pub fn json_missing_updated_at_test() {
  let incomplete =
    "{\"slug\":\"test\",\"language\":\"gleam\",\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(incomplete, "test") |> should.be_error()
}

pub fn json_missing_stages_test() {
  let incomplete =
    "{\"slug\":\"test\",\"language\":\"gleam\",\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\"}"
  persistence.json_to_record(incomplete, "test") |> should.be_error()
}

// Invalid JSON tests - wrong types
pub fn json_wrong_type_slug_test() {
  let wrong_type =
    "{\"slug\":123,\"language\":\"gleam\",\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(wrong_type, "test") |> should.be_error()
}

pub fn json_wrong_type_language_test() {
  let wrong_type =
    "{\"slug\":\"test\",\"language\":123,\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[]}"
  persistence.json_to_record(wrong_type, "test") |> should.be_error()
}

pub fn json_extra_fields_ignored_test() {
  let with_extra =
    "{\"slug\":\"test\",\"language\":\"gleam\",\"status\":\"created\",\"created_at\":\"2026-01-09T10:00:00Z\",\"updated_at\":\"2026-01-09T10:00:00Z\",\"stages\":[],\"extra\":\"ignored\"}"
  let assert Ok(_) = persistence.json_to_record(with_extra, "test")
  True |> should.equal(True)
}

// Stage update tests
pub fn update_stage_status_file_write_test() {
  let test_dir = "/tmp/persistence_stage_write"
  let assert Ok(_) = simplifile.create_directory_all(test_dir)

  let assert Ok(slug) = domain.validate_slug("stage-write-test")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Rust,
      status: domain.Created,
      worktree_path: "/tmp",
      branch: "feat/stage-write-test",
    )

  let assert Ok(Nil) =
    persistence.update_stage_status(
      task,
      "implement",
      persistence.StagePassed,
      1,
      "",
      test_dir,
    )

  let assert Ok(content) = simplifile.read(test_dir <> "/.factory/tasks.json")
  content |> string.contains("implement") |> should.equal(True)

  let assert Ok(_) = simplifile.delete(test_dir <> "/.factory/tasks.json")
  let assert Ok(_) = simplifile.delete(test_dir <> "/.factory")
  let assert Ok(_) = simplifile.delete(test_dir)
}

pub fn update_stage_status_appends_stages_test() {
  let test_dir = "/tmp/persistence_stage_append"
  let assert Ok(_) = simplifile.create_directory_all(test_dir)

  let assert Ok(slug) = domain.validate_slug("append-stages-test")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Python,
      status: domain.Created,
      worktree_path: "/tmp",
      branch: "feat/append-stages-test",
    )

  let assert Ok(Nil) =
    persistence.update_stage_status(
      task,
      "implement",
      persistence.StagePassed,
      1,
      "",
      test_dir,
    )

  let assert Ok(Nil) =
    persistence.update_stage_status(
      task,
      "unit-test",
      persistence.StageFailed,
      2,
      "timeout",
      test_dir,
    )

  let assert Ok(content) = simplifile.read(test_dir <> "/.factory/tasks.json")
  content |> string.contains("implement") |> should.equal(True)
  content |> string.contains("unit-test") |> should.equal(True)

  let assert Ok(_) = simplifile.delete(test_dir <> "/.factory/tasks.json")
  let assert Ok(_) = simplifile.delete(test_dir <> "/.factory")
  let assert Ok(_) = simplifile.delete(test_dir)
}

// JSON serialization edge cases
pub fn json_special_chars_in_error_test() {
  let error_msg = "Error: \"quotes\" and 'apostrophe' & <tags>"
  let stage =
    persistence.StageRecord(
      stage_name: "special",
      status: "failed",
      attempts: 1,
      last_error: error_msg,
    )

  let record =
    persistence.TaskRecord(
      slug: "special-test",
      language: "gleam",
      status: "failed",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [stage],
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "special-test")

  let assert Ok(restored_stage) = restored.stages |> list.first
  restored_stage.last_error |> should.equal(error_msg)
}

pub fn json_multiline_error_test() {
  let error_msg = "line1\nline2\nline3"
  let stage =
    persistence.StageRecord(
      stage_name: "multiline",
      status: "failed",
      attempts: 1,
      last_error: error_msg,
    )

  let record =
    persistence.TaskRecord(
      slug: "multiline-test",
      language: "rust",
      status: "failed",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [stage],
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "multiline-test")

  let assert Ok(restored_stage) = restored.stages |> list.first
  restored_stage.last_error |> should.equal(error_msg)
}

pub fn all_languages_roundtrip_test() {
  let languages = ["go", "gleam", "rust", "python"]

  list.each(languages, fn(lang) {
    let record =
      persistence.TaskRecord(
        slug: lang <> "-test",
        language: lang,
        status: "created",
        created_at: "2026-01-09T10:00:00Z",
        updated_at: "2026-01-09T10:00:00Z",
        stages: [],
      )

    let json_str = persistence.record_to_json(record)
    let assert Ok(restored) = persistence.json_to_record(json_str, lang <> "-test")

    restored.language |> should.equal(lang)
  })
}

pub fn all_status_roundtrip_test() {
  let statuses = ["created", "in_progress", "passed", "failed", "integrated"]

  list.each(statuses, fn(status) {
    let record =
      persistence.TaskRecord(
        slug: status <> "-test",
        language: "gleam",
        status: status,
        created_at: "2026-01-09T10:00:00Z",
        updated_at: "2026-01-09T10:00:00Z",
        stages: [],
      )

    let json_str = persistence.record_to_json(record)
    let assert Ok(restored) = persistence.json_to_record(json_str, status <> "-test")

    restored.status |> should.equal(status)
  })
}

pub fn multiple_stages_roundtrip_test() {
  let stages = [
    persistence.StageRecord(
      stage_name: "s1",
      status: "passed",
      attempts: 1,
      last_error: "",
    ),
    persistence.StageRecord(
      stage_name: "s2",
      status: "failed",
      attempts: 2,
      last_error: "err2",
    ),
  ]

  let record =
    persistence.TaskRecord(
      slug: "multi-stages",
      language: "go",
      status: "in_progress",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: stages,
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "multi-stages")

  restored.stages |> list.length |> should.equal(2)
}

pub fn stage_zero_attempts_test() {
  let stage =
    persistence.StageRecord(
      stage_name: "zero",
      status: "passed",
      attempts: 0,
      last_error: "",
    )

  let record =
    persistence.TaskRecord(
      slug: "zero-attempts",
      language: "python",
      status: "created",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [stage],
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "zero-attempts")

  let assert Ok(s) = restored.stages |> list.first
  s.attempts |> should.equal(0)
}

pub fn stage_large_attempts_test() {
  let stage =
    persistence.StageRecord(
      stage_name: "many",
      status: "failed",
      attempts: 9999,
      last_error: "persistent",
    )

  let record =
    persistence.TaskRecord(
      slug: "many-attempts",
      language: "gleam",
      status: "failed",
      created_at: "2026-01-09T10:00:00Z",
      updated_at: "2026-01-09T10:00:00Z",
      stages: [stage],
    )

  let json_str = persistence.record_to_json(record)
  let assert Ok(restored) = persistence.json_to_record(json_str, "many-attempts")

  let assert Ok(s) = restored.stages |> list.first
  s.attempts |> should.equal(9999)
}
