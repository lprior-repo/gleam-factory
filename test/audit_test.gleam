import audit
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

const test_repo_root = "/tmp/factory-gleam-audit-test"
const test_task_slug = "test-task-123"

fn setup_test_dir() -> Nil {
  let _ = simplifile.delete(test_repo_root)
  let assert Ok(_) = simplifile.create_directory_all(test_repo_root)
  Nil
}

fn cleanup_test_dir() -> Nil {
  let _ = simplifile.delete(test_repo_root)
  Nil
}

// Event type conversion tests
pub fn event_type_to_string_task_created_test() {
  audit.event_type_to_string(audit.TaskCreated)
  |> should.equal("task_created")
}

pub fn event_type_to_string_stage_started_test() {
  audit.event_type_to_string(audit.StageStarted)
  |> should.equal("stage_started")
}

pub fn event_type_to_string_stage_passed_test() {
  audit.event_type_to_string(audit.StagePassed)
  |> should.equal("stage_passed")
}

pub fn event_type_to_string_stage_failed_test() {
  audit.event_type_to_string(audit.StageFailed)
  |> should.equal("stage_failed")
}

pub fn event_type_to_string_task_approved_test() {
  audit.event_type_to_string(audit.TaskApproved)
  |> should.equal("task_approved")
}

pub fn string_to_event_type_task_created_test() {
  audit.string_to_event_type("task_created")
  |> should.equal(Ok(audit.TaskCreated))
}

pub fn string_to_event_type_stage_started_test() {
  audit.string_to_event_type("stage_started")
  |> should.equal(Ok(audit.StageStarted))
}

pub fn string_to_event_type_stage_passed_test() {
  audit.string_to_event_type("stage_passed")
  |> should.equal(Ok(audit.StagePassed))
}

pub fn string_to_event_type_stage_failed_test() {
  audit.string_to_event_type("stage_failed")
  |> should.equal(Ok(audit.StageFailed))
}

pub fn string_to_event_type_task_approved_test() {
  audit.string_to_event_type("task_approved")
  |> should.equal(Ok(audit.TaskApproved))
}

pub fn string_to_event_type_unknown_test() {
  audit.string_to_event_type("unknown_event")
  |> should.be_error()
}

// Audit entry creation tests
pub fn create_entry_test() {
  let entry = audit.create_entry(
    audit.TaskCreated,
    test_task_slug,
    "Test details",
    [#("key", "value")],
  )

  entry.event_type
  |> should.equal(audit.TaskCreated)

  entry.task_slug
  |> should.equal(test_task_slug)

  entry.details
  |> should.equal("Test details")

  entry.metadata
  |> should.equal([#("key", "value")])
}

// Log task created tests
pub fn log_task_created_test() {
  setup_test_dir()

  let result = audit.log_task_created(
    test_repo_root,
    test_task_slug,
    "gleam",
    "main",
  )

  result
  |> should.be_ok()

  let file_path = audit.audit_file_path(test_repo_root, test_task_slug)
  let assert Ok(content) = simplifile.read(file_path)

  content
  |> should.not_equal("")

  cleanup_test_dir()
}

pub fn log_task_created_contains_metadata_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_task_created(
    test_repo_root,
    test_task_slug,
    "rust",
    "feature-branch",
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.metadata
  |> list_contains(#("language", "rust"))
  |> should.be_true()

  entry.metadata
  |> list_contains(#("branch", "feature-branch"))
  |> should.be_true()

  cleanup_test_dir()
}

// Log stage started tests
pub fn log_stage_started_test() {
  setup_test_dir()

  let result = audit.log_stage_started(
    test_repo_root,
    test_task_slug,
    "implement",
    1,
  )

  result
  |> should.be_ok()

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.event_type
  |> should.equal(audit.StageStarted)

  entry.details
  |> should.equal("Stage started: implement")

  cleanup_test_dir()
}

pub fn log_stage_started_metadata_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_stage_started(
    test_repo_root,
    test_task_slug,
    "test",
    3,
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.metadata
  |> list_contains(#("stage", "test"))
  |> should.be_true()

  entry.metadata
  |> list_contains(#("attempt", "3"))
  |> should.be_true()

  cleanup_test_dir()
}

// Log stage passed tests
pub fn log_stage_passed_test() {
  setup_test_dir()

  let result = audit.log_stage_passed(
    test_repo_root,
    test_task_slug,
    "implement",
    1500,
  )

  result
  |> should.be_ok()

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.event_type
  |> should.equal(audit.StagePassed)

  entry.details
  |> should.equal("Stage passed: implement")

  cleanup_test_dir()
}

pub fn log_stage_passed_metadata_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_stage_passed(
    test_repo_root,
    test_task_slug,
    "test",
    2500,
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.metadata
  |> list_contains(#("stage", "test"))
  |> should.be_true()

  entry.metadata
  |> list_contains(#("duration_ms", "2500"))
  |> should.be_true()

  cleanup_test_dir()
}

// Log stage failed tests
pub fn log_stage_failed_test() {
  setup_test_dir()

  let result = audit.log_stage_failed(
    test_repo_root,
    test_task_slug,
    "implement",
    "compilation error",
  )

  result
  |> should.be_ok()

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.event_type
  |> should.equal(audit.StageFailed)

  entry.details
  |> should.equal("Stage failed: implement")

  cleanup_test_dir()
}

pub fn log_stage_failed_metadata_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_stage_failed(
    test_repo_root,
    test_task_slug,
    "test",
    "test timeout",
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.metadata
  |> list_contains(#("stage", "test"))
  |> should.be_true()

  entry.metadata
  |> list_contains(#("error", "test timeout"))
  |> should.be_true()

  cleanup_test_dir()
}

// Log task approved tests
pub fn log_task_approved_test() {
  setup_test_dir()

  let result = audit.log_task_approved(
    test_repo_root,
    test_task_slug,
    "canary",
  )

  result
  |> should.be_ok()

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.event_type
  |> should.equal(audit.TaskApproved)

  entry.details
  |> should.equal("Task approved for deployment")

  cleanup_test_dir()
}

pub fn log_task_approved_metadata_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_task_approved(
    test_repo_root,
    test_task_slug,
    "blue-green",
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let assert Ok(entry) = list_first(log.entries)

  entry.metadata
  |> list_contains(#("strategy", "blue-green"))
  |> should.be_true()

  cleanup_test_dir()
}

// Multiple events test
pub fn log_multiple_events_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_task_created(
    test_repo_root,
    test_task_slug,
    "gleam",
    "main",
  )
  let assert Ok(_) = audit.log_stage_started(
    test_repo_root,
    test_task_slug,
    "implement",
    1,
  )
  let assert Ok(_) = audit.log_stage_passed(
    test_repo_root,
    test_task_slug,
    "implement",
    1000,
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)

  list_length(log.entries)
  |> should.equal(3)

  cleanup_test_dir()
}

// Filter by type tests
pub fn filter_by_type_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_task_created(
    test_repo_root,
    test_task_slug,
    "gleam",
    "main",
  )
  let assert Ok(_) = audit.log_stage_started(
    test_repo_root,
    test_task_slug,
    "implement",
    1,
  )
  let assert Ok(_) = audit.log_stage_passed(
    test_repo_root,
    test_task_slug,
    "implement",
    1000,
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let stage_events = audit.filter_by_type(log, audit.StageStarted)

  list_length(stage_events)
  |> should.equal(1)

  let assert Ok(entry) = list_first(stage_events)
  entry.event_type
  |> should.equal(audit.StageStarted)

  cleanup_test_dir()
}

// Get stage history tests
pub fn get_stage_history_test() {
  setup_test_dir()

  let assert Ok(_) = audit.log_task_created(
    test_repo_root,
    test_task_slug,
    "gleam",
    "main",
  )
  let assert Ok(_) = audit.log_stage_started(
    test_repo_root,
    test_task_slug,
    "implement",
    1,
  )
  let assert Ok(_) = audit.log_stage_passed(
    test_repo_root,
    test_task_slug,
    "implement",
    1000,
  )
  let assert Ok(_) = audit.log_task_approved(
    test_repo_root,
    test_task_slug,
    "canary",
  )

  let assert Ok(log) = audit.read_audit_log(test_repo_root, test_task_slug)
  let stage_events = audit.get_stage_history(log)

  list_length(stage_events)
  |> should.equal(2)

  cleanup_test_dir()
}

// Read empty audit log test
pub fn read_empty_audit_log_test() {
  setup_test_dir()

  let result = audit.read_audit_log(test_repo_root, "nonexistent-task")

  result
  |> should.be_ok()

  let assert Ok(log) = result
  list_length(log.entries)
  |> should.equal(0)

  cleanup_test_dir()
}

// Audit file path test
pub fn audit_file_path_test() {
  let path = audit.audit_file_path("/repo", "my-task")
  path
  |> should.equal("/repo/.factory/audit/my-task.jsonl")
}

// Helper functions
fn list_first(list: List(a)) -> Result(a, Nil) {
  case list {
    [first, ..] -> Ok(first)
    [] -> Error(Nil)
  }
}

fn list_length(list: List(a)) -> Int {
  do_list_length(list, 0)
}

fn do_list_length(list: List(a), acc: Int) -> Int {
  case list {
    [] -> acc
    [_, ..rest] -> do_list_length(rest, acc + 1)
  }
}

fn list_contains(list: List(#(a, b)), target: #(a, b)) -> Bool {
  case list {
    [] -> False
    [head, ..rest] ->
      case head == target {
        True -> True
        False -> list_contains(rest, target)
      }
  }
}
