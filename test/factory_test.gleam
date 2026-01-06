import gleeunit
import gleeunit/should
import domain
import persistence
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
// VALIDATION TESTS
// ============================================================================

pub fn validate_slug_format_valid_starts_with_letter_test() {
  validation.validate_slug_format("feature_123")
  |> should.be_ok
}

pub fn validate_slug_format_invalid_starts_with_number_test() {
  validation.validate_slug_format("123_feature")
  |> should.be_error
}

pub fn validate_slug_format_invalid_starts_with_underscore_test() {
  validation.validate_slug_format("_feature")
  |> should.be_error
}

pub fn validate_slug_format_invalid_contains_hyphen_test() {
  validation.validate_slug_format("feature-name")
  |> should.be_error
}

pub fn validate_slug_format_invalid_contains_special_chars_test() {
  validation.validate_slug_format("feature@name")
  |> should.be_error
}

pub fn validate_slug_format_valid_underscores_and_numbers_test() {
  validation.validate_slug_format("task_1_a_b_c")
  |> should.be_ok
}

pub fn validate_slug_format_invalid_exceeds_max_length_test() {
  let long_slug = "a" <> string_repeat("b", 50)
  validation.validate_slug_format(long_slug)
  |> should.be_error
}

pub fn validate_slug_format_valid_at_max_length_test() {
  let max_slug = "a" <> string_repeat("b", 49)
  validation.validate_slug_format(max_slug)
  |> should.be_ok
}

pub fn validate_slug_format_invalid_empty_test() {
  validation.validate_slug_format("")
  |> should.be_error
}

pub fn validate_priority_accepts_uppercase_p1_test() {
  validation.validate_priority("P1")
  |> should.be_ok
}

pub fn validate_priority_accepts_lowercase_p1_test() {
  validation.validate_priority("p1")
  |> should.be_ok
}

pub fn validate_priority_accepts_numeric_1_test() {
  validation.validate_priority("1")
  |> should.be_ok
}

pub fn validate_priority_rejects_invalid_p4_test() {
  validation.validate_priority("P4")
  |> should.be_error
}

pub fn validate_priority_rejects_empty_string_test() {
  validation.validate_priority("")
  |> should.be_error
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

fn string_repeat(s: String, n: Int) -> String {
  case n {
    0 -> ""
    _ -> s <> string_repeat(s, n - 1)
  }
}
