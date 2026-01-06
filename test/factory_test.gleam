import gleeunit
import gleeunit/should
import domain
import persistence
import audit
import validation

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// DOMAIN MODEL TESTS
// ============================================================================

pub fn validate_slug_valid_test() {
  domain.validate_slug("bd-52.1")
  |> should.be_ok
}

pub fn validate_slug_empty_test() {
  domain.validate_slug("")
  |> should.be_error
}

pub fn parse_language_go_test() {
  domain.parse_language("go")
  |> should.equal(Ok(domain.Go))
}

pub fn parse_language_gleam_test() {
  domain.parse_language("gleam")
  |> should.equal(Ok(domain.Gleam))
}

pub fn parse_language_rust_test() {
  domain.parse_language("rust")
  |> should.equal(Ok(domain.Rust))
}

pub fn parse_language_python_test() {
  domain.parse_language("python")
  |> should.equal(Ok(domain.Python))
}

pub fn parse_language_invalid_test() {
  domain.parse_language("java")
  |> should.be_error
}

pub fn standard_pipeline_not_empty_test() {
  let pipeline = domain.standard_pipeline()
  case list_length(pipeline) > 0 {
    True -> Nil
    False -> should.fail()
  }
}

pub fn get_stage_valid_test() {
  domain.get_stage("tdd-setup")
  |> should.be_ok
}

pub fn get_stage_invalid_test() {
  domain.get_stage("invalid-stage")
  |> should.be_error
}

// ============================================================================
// PERSISTENCE TESTS
// ============================================================================

pub fn task_to_record_preserves_slug_test() {
  let task = domain.Task(
    slug: "test-123",
    language: domain.Go,
    status: domain.Created,
    worktree_path: "/tmp/test",
    branch: "feat/test-123",
  )

  let record = persistence.task_to_record(task)
  record.slug
  |> should.equal("test-123")
}

pub fn task_to_record_preserves_language_test() {
  let task = domain.Task(
    slug: "test",
    language: domain.Rust,
    status: domain.Created,
    worktree_path: "/tmp/test",
    branch: "feat/test",
  )

  let record = persistence.task_to_record(task)
  record.language
  |> should.equal("rust")
}

pub fn record_to_task_preserves_language_test() {
  let record = persistence.TaskRecord(
    slug: "test",
    language: "python",
    status: "created",
    created_at: "2025-01-04T00:00:00Z",
    updated_at: "2025-01-04T00:00:00Z",
    stages: [],
  )

  case persistence.record_to_task(record) {
    Ok(task) -> task.language |> should.equal(domain.Python)
    Error(_) -> should.fail()
  }
}

pub fn record_to_task_invalid_language_test() {
  let record = persistence.TaskRecord(
    slug: "test",
    language: "java",
    status: "created",
    created_at: "2025-01-04T00:00:00Z",
    updated_at: "2025-01-04T00:00:00Z",
    stages: [],
  )

  persistence.record_to_task(record)
  |> should.be_error
}

pub fn status_file_path_format_test() {
  let path = persistence.status_file_path("/home/user/repo")
  path
  |> should.equal("/home/user/repo/.factory/tasks.json")
}

// ============================================================================
// AUDIT MODULE TESTS
// ============================================================================

pub fn event_type_to_string_task_created_test() {
  audit.event_type_to_string(audit.TaskCreated)
  |> should.equal("task_created")
}

pub fn event_type_to_string_stage_passed_test() {
  audit.event_type_to_string(audit.StagePassed)
  |> should.equal("stage_passed")
}

pub fn event_type_to_string_deployment_completed_test() {
  audit.event_type_to_string(audit.DeploymentCompleted)
  |> should.equal("deployment_completed")
}

pub fn string_to_event_type_task_created_test() {
  audit.string_to_event_type("task_created")
  |> should.equal(Ok(audit.TaskCreated))
}

pub fn string_to_event_type_stage_failed_test() {
  audit.string_to_event_type("stage_failed")
  |> should.equal(Ok(audit.StageFailed))
}

pub fn string_to_event_type_invalid_test() {
  audit.string_to_event_type("invalid_event")
  |> should.be_error
}

pub fn get_timestamp_returns_iso_format_test() {
  let ts = audit.get_timestamp()
  // Should contain 'T' separator and 'Z' suffix (ISO 8601)
  case contains_substring(ts, "T") && contains_substring(ts, "Z") {
    True -> Nil
    False -> should.fail()
  }
}

pub fn get_actor_returns_string_test() {
  let actor = audit.get_actor()
  // Should return non-empty string (at least "system" or username)
  case actor {
    "" -> should.fail()
    _ -> Nil
  }
}

pub fn create_entry_sets_fields_correctly_test() {
  let entry = audit.create_entry(
    audit.TaskCreated,
    "test-slug",
    "Test task created",
    [#("language", "gleam")],
  )

  entry.task_slug
  |> should.equal("test-slug")

  entry.event_type
  |> should.equal(audit.TaskCreated)

  entry.details
  |> should.equal("Test task created")
}

pub fn filter_by_type_returns_matching_entries_test() {
  let entries = [
    audit.AuditEntry(
      timestamp: "2026-01-05T00:00:00.000Z",
      event_type: audit.TaskCreated,
      task_slug: "test",
      actor: "user",
      details: "created",
      metadata: [],
    ),
    audit.AuditEntry(
      timestamp: "2026-01-05T00:01:00.000Z",
      event_type: audit.StagePassed,
      task_slug: "test",
      actor: "user",
      details: "passed",
      metadata: [],
    ),
    audit.AuditEntry(
      timestamp: "2026-01-05T00:02:00.000Z",
      event_type: audit.TaskCreated,
      task_slug: "test2",
      actor: "user",
      details: "created another",
      metadata: [],
    ),
  ]

  let log = audit.AuditLog(task_slug: "test", entries: entries)
  let filtered = audit.filter_by_type(log, audit.TaskCreated)

  list_length(filtered)
  |> should.equal(2)
}

pub fn get_stage_history_filters_stage_events_test() {
  let entries = [
    audit.AuditEntry(
      timestamp: "2026-01-05T00:00:00.000Z",
      event_type: audit.TaskCreated,
      task_slug: "test",
      actor: "user",
      details: "created",
      metadata: [],
    ),
    audit.AuditEntry(
      timestamp: "2026-01-05T00:01:00.000Z",
      event_type: audit.StageStarted,
      task_slug: "test",
      actor: "user",
      details: "started",
      metadata: [],
    ),
    audit.AuditEntry(
      timestamp: "2026-01-05T00:02:00.000Z",
      event_type: audit.StagePassed,
      task_slug: "test",
      actor: "user",
      details: "passed",
      metadata: [],
    ),
  ]

  let log = audit.AuditLog(task_slug: "test", entries: entries)
  let stage_history = audit.get_stage_history(log)

  list_length(stage_history)
  |> should.equal(2)
}

// ============================================================================
// VALIDATION MODULE TESTS
// ============================================================================

pub fn validate_email_valid_with_at_symbol_test() {
  validation.validate_email("user@example.com")
  |> should.equal(Ok("user@example.com"))
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

fn list_length(list: List(a)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}

fn contains_substring(haystack: String, needle: String) -> Bool {
  contains_substring_ffi(haystack, needle)
}

@external(erlang, "audit_test_ffi", "contains_substring")
fn contains_substring_ffi(haystack: String, needle: String) -> Bool
