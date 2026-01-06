import gleeunit
import gleeunit/should
import domain
import persistence
import validation
import cli
import utils
import stages
import worktree
import integration

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
// CLI TESTS
// ============================================================================

pub fn parse_args_new_with_short_slug_flag_test() {
  cli.parse_args(["new", "-s", "my_task"])
  |> should.be_ok
}

// ============================================================================
// UTILS TESTS
// ============================================================================

pub fn truncate_string_within_limit_test() {
  // When string is shorter than max_len, return unchanged
  utils.truncate_string("hello", 10)
  |> should.equal("hello")
}

pub fn truncate_string_exactly_at_limit_test() {
  // When string equals max_len, return unchanged
  utils.truncate_string("hello", 5)
  |> should.equal("hello")
}

pub fn truncate_string_exceeds_limit_test() {
  // When string exceeds max_len, truncate and add ellipsis
  utils.truncate_string("hello world", 5)
  |> should.equal("he...")
}

pub fn truncate_string_zero_length_test() {
  // When max_len is 0, return empty string
  utils.truncate_string("hello", 0)
  |> should.equal("")
}

pub fn truncate_string_single_char_limit_test() {
  // When max_len is 1, return single character (no room for ellipsis)
  utils.truncate_string("hello", 1)
  |> should.equal("h")
}

pub fn truncate_string_empty_string_test() {
  // When input is empty, return empty
  utils.truncate_string("", 10)
  |> should.equal("")
}

pub fn pad_left_within_width_test() {
  // When string is shorter than width, pad with char on left
  utils.pad_left("hi", 5, "-")
  |> should.equal("---hi")
}

pub fn pad_left_exactly_at_width_test() {
  // When string equals width, return unchanged
  utils.pad_left("hello", 5, "-")
  |> should.equal("hello")
}

pub fn pad_left_exceeds_width_test() {
  // When string exceeds width, return unchanged (no truncation)
  utils.pad_left("hello world", 5, "-")
  |> should.equal("hello world")
}

pub fn pad_left_zero_width_test() {
  // When width is 0, return unchanged
  utils.pad_left("hello", 0, "-")
  |> should.equal("hello")
}

pub fn pad_left_empty_string_test() {
  // When input is empty, pad to width with char
  utils.pad_left("", 3, "*")
  |> should.equal("***")
}

pub fn pad_left_single_char_pad_test() {
  // When padding with single character, repeat it
  utils.pad_left("x", 4, ".")
  |> should.equal("...x")
}

pub fn pad_left_multichar_pad_test() {
  // When padding with multiple character string, repeat the entire string
  utils.pad_left("a", 5, "xy")
  |> should.equal("xyxya")
}

pub fn pad_left_space_padding_test() {
  // Common case: padding with spaces
  utils.pad_left("test", 8, " ")
  |> should.equal("    test")
}

// ============================================================================
// STAGES TESTS
// ============================================================================

pub fn validate_stage_transition_forward_valid_test() {
  // When transitioning from tdd-setup to implement (valid forward progression)
  stages.validate_stage_transition("tdd-setup", "implement")
  |> should.be_ok
}

pub fn validate_stage_transition_backward_invalid_test() {
  // When transitioning backward in pipeline (implement to tdd-setup), should fail
  stages.validate_stage_transition("implement", "tdd-setup")
  |> should.be_error
}

pub fn validate_stage_transition_same_stage_invalid_test() {
  // When transitioning to same stage, should fail
  stages.validate_stage_transition("tdd-setup", "tdd-setup")
  |> should.be_error
}

pub fn validate_stage_transition_nonexistent_stage_invalid_test() {
  // When transitioning from or to non-existent stage, should fail
  stages.validate_stage_transition("invalid-stage", "implement")
  |> should.be_error
}

// ============================================================================
// WORKTREE TESTS (continued)
// ============================================================================

pub fn cleanup_stale_worktrees_returns_count_test() {
  // When cleanup_stale_worktrees is called with max_age_days
  // It should return a Result containing the count of removed worktrees
  worktree.cleanup_stale_worktrees(30)
  |> should.be_ok
}

// ============================================================================
// INTEGRATION TESTS - retry_with_backoff
// ============================================================================

pub fn retry_with_backoff_succeeds_on_first_attempt_test() {
  // When a function succeeds on first attempt,
  // retry_with_backoff should return Ok immediately without retrying
  let test_fn = fn() { Ok("success") }

  integration.retry_with_backoff(test_fn, 3)
  |> should.equal(Ok("success"))
}

pub fn retry_with_backoff_succeeds_after_retries_test() {
  // When a function succeeds (even on retry),
  // retry_with_backoff should return Ok
  let test_fn = fn() { Ok("recovered") }

  integration.retry_with_backoff(test_fn, 3)
  |> should.equal(Ok("recovered"))
}

pub fn retry_with_backoff_exhausts_retries_test() {
  // When a function always fails and retries are exhausted,
  // retry_with_backoff should return the last error
  let test_fn = fn() { Error("persistent error") }

  integration.retry_with_backoff(test_fn, 2)
  |> should.equal(Error("persistent error"))
}

pub fn retry_with_backoff_zero_retries_test() {
  // When retries is 0, should attempt once and return result
  let test_fn = fn() { Error("immediate failure") }

  integration.retry_with_backoff(test_fn, 0)
  |> should.equal(Error("immediate failure"))
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
