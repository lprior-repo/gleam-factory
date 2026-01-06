import gleeunit
import gleeunit/should
import domain
import persistence
import audit
import validation
import errors
import cli
import gleam/option.{None, Some}
import gleam/string

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

pub fn validate_email_format_valid_email_test() {
  // Test that validate_email_format exists and accepts a valid email
  // A valid email: has exactly one @, text before and after @, and a dot after @
  validation.validate_email_format("user@example.com")
  |> should.equal(Ok("user@example.com"))
}

pub fn validate_email_format_rejects_no_at_symbol_test() {
  // Email without @ symbol should be rejected
  validation.validate_email_format("userexample.com")
  |> should.be_error
}

pub fn validate_email_format_rejects_multiple_at_symbols_test() {
  // Email with multiple @ symbols should be rejected
  validation.validate_email_format("user@name@example.com")
  |> should.be_error
}

pub fn validate_email_format_rejects_empty_local_part_test() {
  // Email with no text before @ should be rejected
  validation.validate_email_format("@example.com")
  |> should.be_error
}

pub fn validate_email_format_rejects_empty_domain_part_test() {
  // Email with no text after @ should be rejected
  validation.validate_email_format("user@")
  |> should.be_error
}

pub fn validate_email_format_rejects_domain_without_dot_test() {
  // Email with no dot in domain part should be rejected
  validation.validate_email_format("user@localhost")
  |> should.be_error
}

// ============================================================================
// ERRORS MODULE TESTS
// ============================================================================

pub fn extract_root_cause_finds_error_prefix_pattern_test() {
  // Given output containing an "Error:" pattern
  let output = "Starting build...
Compiling module...
Error: undefined variable 'foo'
  at line 42
  in module.gleam
Done."

  // When we extract the root cause
  let result = errors.extract_root_cause(output)

  // Then we should get Some with the error line and context
  case result {
    Some(cause) -> {
      // Should contain the error line
      should.be_true(contains_substring(cause, "Error: undefined variable 'foo'"))
    }
    None -> should.fail()
  }
}

pub fn extract_root_cause_finds_lowercase_error_pattern_test() {
  // Given output containing a lowercase "error:" pattern (common in many tools)
  let output = "Running tests...
test_main.go:15: error: assertion failed
expected 42, got 0
Build failed."

  // When we extract the root cause
  let result = errors.extract_root_cause(output)

  // Then we should find the lowercase error pattern
  case result {
    Some(cause) -> {
      should.be_true(contains_substring(cause, "error: assertion failed"))
    }
    None -> should.fail()
  }
}

pub fn extract_root_cause_finds_panic_pattern_test() {
  // Given output containing a "panic:" pattern (common in Go, Rust runtime errors)
  let output = "Starting application...
Initializing database...
panic: runtime error: index out of range [5] with length 3
goroutine 1 [running]:
main.processItems()
	/app/main.go:42 +0x123"

  // When we extract the root cause
  let result = errors.extract_root_cause(output)

  // Then we should find the panic pattern
  case result {
    Some(cause) -> {
      should.be_true(contains_substring(cause, "panic: runtime error"))
    }
    None -> should.fail()
  }
}

pub fn extract_root_cause_includes_context_lines_test() {
  // Given output with an error followed by context lines
  let output = "Starting build...
Error: cannot find module 'foo'
  at /src/main.gleam:15
  imported from /src/app.gleam:3
  dependency tree: main -> app -> foo
More unrelated output here..."

  // When we extract the root cause
  let result = errors.extract_root_cause(output)

  // Then we should get the error line PLUS up to 3 lines of context
  case result {
    Some(cause) -> {
      // Should contain the error line
      should.be_true(contains_substring(cause, "Error: cannot find module"))
      // Should contain at least some context (first context line)
      should.be_true(contains_substring(cause, "at /src/main.gleam:15"))
      // Should contain second context line
      should.be_true(contains_substring(cause, "imported from"))
      // Should contain third context line
      should.be_true(contains_substring(cause, "dependency tree"))
    }
    None -> should.fail()
  }
}

pub fn summarize_error_truncates_to_max_lines_test() {
  // Given output with multiple lines of error information
  let output = "Running tests...
Error: test failed
  expected: 42
  actual: 0
  at test_math.gleam:15
  in function: test_addition
  stack trace follows
Build complete with errors."

  // When we summarize with max 3 lines
  let result = errors.summarize_error(output, 3)

  // Then the result should have at most 3 lines
  let line_count = count_lines(result)
  should.be_true(line_count <= 3)

  // And it should still contain relevant error info
  should.be_true(contains_substring(result, "Error"))
}

fn count_lines(text: String) -> Int {
  text
  |> string.split("\n")
  |> list_length
}

pub fn classify_error_detects_compile_error_test() {
  // Given output from a compiler with typical compilation error patterns
  let output = "Compiling project...
error: Syntax error on line 42
  --> src/main.gleam:42:5
  |
42 |     let x =
  |             ^ expected expression
"

  // When we classify the error
  let result = errors.classify_error(output)

  // Then it should be classified as a CompileError
  result
  |> should.equal(errors.CompileError)
}

pub fn classify_error_detects_test_failure_test() {
  // Given output from a test runner showing test failures (typical patterns: FAILED, failed)
  let output = "Running test suite...
test_math_test.gleam: FAILED
  assertion failed: expected 42, got 0
1 test FAILED, 5 passed"

  // When we classify the error
  let result = errors.classify_error(output)

  // Then it should be classified as TestFailure (not CompileError)
  result
  |> should.equal(errors.TestFailure)
}

pub fn classify_error_detects_runtime_panic_test() {
  // Given output containing a panic pattern (Go, Rust, or other runtime panics)
  let output = "Starting application...
panic: runtime error: index out of range [5] with length 3
goroutine 1 [running]:
main.processItems()"

  // When we classify the error
  let result = errors.classify_error(output)

  // Then it should be classified as RuntimePanic (distinct from CompileError or TestFailure)
  result
  |> should.equal(errors.RuntimePanic)
}

pub fn classify_error_detects_timeout_test() {
  // Given output containing a timeout pattern (common in test runners and CI systems)
  let output = "Running tests...
test_long_operation_test: Timeout after 30s
  Operation did not complete in time
Build terminated."

  // When we classify the error
  let result = errors.classify_error(output)

  // Then it should be classified as Timeout
  // Requirements specify ErrorType should include: CompileError, TestFailure, RuntimePanic, Timeout, Unknown
  result
  |> should.equal(errors.Timeout)
}

pub fn classify_error_returns_unknown_for_unrecognized_output_test() {
  // Given output that doesn't match any known error pattern
  // (no "Error:", "error:", "panic:", "FAILED", "failed", or timeout patterns)
  let output = "Build completed successfully.
All operations finished.
No issues detected.
Exiting with status 0."

  // When we classify the error
  let result = errors.classify_error(output)

  // Then it should be classified as Unknown (per requirements)
  // Requirements state: ErrorType should be: CompileError, TestFailure, RuntimePanic, Timeout, Unknown
  result
  |> should.equal(errors.Unknown)
}

// ============================================================================
// CLI ARGUMENT PARSING TESTS
// ============================================================================

/// Test that parse_args exists as a pure function and correctly parses
/// a basic 'new' command with the --slug flag
pub fn parse_args_new_command_with_slug_test() {
  // Arrange: a simple 'new' command with just --slug
  let args = ["new", "--slug", "my-task"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return a NewTask command with the correct slug
  result
  |> should.equal(Ok(cli.NewTask("my-task", None, False)))
}

/// Test flexible flag ordering: --contract before --slug should work
/// This tests requirement #2: flags can appear in any order
pub fn parse_args_new_command_with_flexible_flag_order_test() {
  // Arrange: 'new' command with --contract BEFORE --slug (non-canonical order)
  let args = ["new", "--contract", "path/to/contract.md", "--slug", "my-task"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return a NewTask command with both slug and contract
  // The order of flags should NOT matter
  result
  |> should.equal(Ok(cli.NewTask("my-task", Some("path/to/contract.md"), False)))
}

/// Test that 'new' command without required --slug flag returns an error
/// This tests requirement #3: validation for required arguments
pub fn parse_args_new_command_missing_slug_returns_error_test() {
  // Arrange: 'new' command with NO --slug flag at all
  let args = ["new"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error with a message mentioning --slug is required
  case result {
    Error(msg) -> {
      // The error message should clearly indicate that --slug is required
      should.be_true(contains_substring(msg, "slug") || contains_substring(msg, "required"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that short flag -s is equivalent to --slug
/// This tests requirement #4: support short flags
pub fn parse_args_new_command_with_short_slug_flag_test() {
  // Arrange: 'new' command with -s (short for --slug)
  let args = ["new", "-s", "my-task"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return same result as using --slug
  result
  |> should.equal(Ok(cli.NewTask("my-task", None, False)))
}

/// Test that short flag -c is equivalent to --contract
/// This tests requirement #4: support short flags (-c for --contract)
pub fn parse_args_new_command_with_short_contract_flag_test() {
  // Arrange: 'new' command with -s for --slug and -c for --contract
  let args = ["new", "-s", "my-task", "-c", "path/to/contract.md"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return a NewTask command with both slug and contract
  // -c should be equivalent to --contract
  result
  |> should.equal(Ok(cli.NewTask("my-task", Some("path/to/contract.md"), False)))
}

/// Test that 'stage' command requires both --slug and --stage flags
/// This tests requirement #3: 'stage' requires both --slug and --stage
pub fn parse_args_stage_command_missing_stage_returns_error_test() {
  // Arrange: 'stage' command with only --slug (missing --stage)
  let args = ["stage", "--slug", "my-task"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error with a message indicating --stage is required
  // The error message should specifically mention that --stage is required,
  // not just be a generic "Unknown command" error
  case result {
    Error(msg) -> {
      // The error message should clearly say --stage is required
      should.be_true(contains_substring(msg, "--stage") && contains_substring(msg, "required"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that --priority flag value must be P1, P2, or P3
/// This tests requirement #5: flag value validation for --priority
pub fn parse_args_list_command_rejects_invalid_priority_test() {
  // Arrange: 'list' command with an invalid priority value (not P1, P2, or P3)
  let args = ["list", "--priority", "HIGH"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error with a message about invalid priority value
  // Valid values are: P1, P2, P3
  case result {
    Error(msg) -> {
      // The error message should indicate that the priority value is invalid
      // and ideally mention the valid values (P1, P2, P3)
      should.be_true(contains_substring(msg, "priority") || contains_substring(msg, "P1"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that --status flag value must be open, in_progress, or done
/// This tests requirement #5: flag value validation for --status
pub fn parse_args_list_command_rejects_invalid_status_test() {
  // Arrange: 'list' command with an invalid status value (not open, in_progress, or done)
  let args = ["list", "--status", "pending"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error with a message about invalid status value
  // Valid values are: open, in_progress, done
  case result {
    Error(msg) -> {
      // The error message should indicate that the status value is invalid
      // and ideally mention valid values (open, in_progress, done)
      should.be_true(contains_substring(msg, "status") || contains_substring(msg, "open"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that --strategy flag value must be immediate, gradual, or canary
/// This tests requirement #5: flag value validation for --strategy
pub fn parse_args_approve_command_rejects_invalid_strategy_test() {
  // Arrange: 'approve' command with an invalid strategy value
  // (not immediate, gradual, or canary)
  let args = ["approve", "--slug", "my-task", "--strategy", "fast"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error with a message about invalid strategy value
  // Valid values are: immediate, gradual, canary
  case result {
    Error(msg) -> {
      // The error message should indicate that the strategy value is invalid
      // and ideally mention valid values (immediate, gradual, canary)
      should.be_true(contains_substring(msg, "strategy") || contains_substring(msg, "immediate"))
    }
    Ok(_) -> should.fail()
  }
}

/// Test that short flag -d is equivalent to --dry-run for stage command
/// This tests requirement #4: short flags (-d = --dry-run)
pub fn parse_args_stage_command_with_short_dry_run_flag_test() {
  // Arrange: 'stage' command with -d (short for --dry-run)
  let args = ["stage", "--slug", "my-task", "--stage", "implement", "-d"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return RunStage with dry_run=True
  // -d should be equivalent to --dry-run
  result
  |> should.equal(Ok(cli.RunStage("my-task", "implement", True, None, None)))
}

/// Test that short flag -f is equivalent to --force for approve command
/// This tests requirement #4: short flags (-f = --force)
pub fn parse_args_approve_command_with_short_force_flag_test() {
  // Arrange: 'approve' command with -f (short for --force)
  let args = ["approve", "--slug", "my-task", "-f"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return ApproveTask with force=True
  // -f should be equivalent to --force
  result
  |> should.equal(Ok(cli.ApproveTask("my-task", None, True)))
}

/// Test flexible flag ordering for 'stage' command: --stage before --slug should work
/// This tests requirement #2: flags can appear in any order for all commands
pub fn parse_args_stage_command_with_flexible_flag_order_test() {
  // Arrange: 'stage' command with --stage BEFORE --slug (non-canonical order)
  let args = ["stage", "--stage", "implement", "--slug", "my-task"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return a RunStage command with correct slug and stage
  // The order of flags should NOT matter
  result
  |> should.equal(Ok(cli.RunStage("my-task", "implement", False, None, None)))
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
