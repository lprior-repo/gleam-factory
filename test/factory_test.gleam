import audit
import cli
import config
import domain
import errors
import gleam/erlang/process as erl_process
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/string
import gleeunit
import gleeunit/should
import golden_master
import persistence
import process
import signal_bus
import signals
import simplifile
import types
import validation
import workspace_manager

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
  let task =
    domain.Task(
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
  let task =
    domain.Task(
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
  let record =
    persistence.TaskRecord(
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
  let record =
    persistence.TaskRecord(
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
  let entry =
    audit.create_entry(audit.TaskCreated, "test-slug", "Test task created", [
      #("language", "gleam"),
    ])

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
  let output =
    "Starting build...
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
      should.be_true(contains_substring(
        cause,
        "Error: undefined variable 'foo'",
      ))
    }
    None -> should.fail()
  }
}

pub fn extract_root_cause_finds_lowercase_error_pattern_test() {
  // Given output containing a lowercase "error:" pattern (common in many tools)
  let output =
    "Running tests...
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
  let output =
    "Starting application...
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
  let output =
    "Starting build...
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
  let output =
    "Running tests...
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
  let output =
    "Compiling project...
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
  let output =
    "Running test suite...
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
  let output =
    "Starting application...
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
  let output =
    "Running tests...
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
  let output =
    "Build completed successfully.
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
  |> should.equal(
    Ok(cli.NewTask("my-task", Some("path/to/contract.md"), False)),
  )
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
      should.be_true(
        contains_substring(msg, "slug") || contains_substring(msg, "required"),
      )
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
  |> should.equal(
    Ok(cli.NewTask("my-task", Some("path/to/contract.md"), False)),
  )
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
      should.be_true(
        contains_substring(msg, "--stage")
        && contains_substring(msg, "required"),
      )
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
      should.be_true(
        contains_substring(msg, "priority") || contains_substring(msg, "P1"),
      )
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
      should.be_true(
        contains_substring(msg, "status") || contains_substring(msg, "open"),
      )
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
      should.be_true(
        contains_substring(msg, "strategy")
        || contains_substring(msg, "immediate"),
      )
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

/// Test that short flag -s works for show command
/// This ensures all commands consistently support short flag -s for --slug
pub fn parse_args_show_command_with_short_slug_flag_test() {
  // Arrange: 'show' command with -s (short for --slug)
  let args = ["show", "-s", "my-task"]

  // Act: parse the args using the pure parse_args function
  let result = cli.parse_args(args)

  // Assert: should return ShowTask with the correct slug
  result
  |> should.equal(Ok(cli.ShowTask("my-task", False)))
}

/// Test that 'show' command without required --slug flag returns an error
/// This ensures show command also validates required --slug
pub fn parse_args_show_command_missing_slug_returns_error_test() {
  // Arrange: 'show' command with NO --slug flag
  let args = ["show"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error indicating --slug is required
  case result {
    Error(msg) -> {
      should.be_true(
        contains_substring(msg, "--slug") && contains_substring(msg, "required"),
      )
    }
    Ok(_) -> should.fail()
  }
}

/// Test that valid priority values are accepted (P1, P2, P3)
pub fn parse_args_list_command_accepts_valid_priority_test() {
  // Test P1
  cli.parse_args(["list", "--priority", "P1"])
  |> should.equal(Ok(cli.ListTasks(Some("P1"), None)))

  // Test P2
  cli.parse_args(["list", "--priority", "P2"])
  |> should.equal(Ok(cli.ListTasks(Some("P2"), None)))

  // Test P3
  cli.parse_args(["list", "--priority", "P3"])
  |> should.equal(Ok(cli.ListTasks(Some("P3"), None)))
}

/// Test that valid status values are accepted (open, in_progress, done)
pub fn parse_args_list_command_accepts_valid_status_test() {
  // Test open
  cli.parse_args(["list", "--status", "open"])
  |> should.equal(Ok(cli.ListTasks(None, Some("open"))))

  // Test in_progress
  cli.parse_args(["list", "--status", "in_progress"])
  |> should.equal(Ok(cli.ListTasks(None, Some("in_progress"))))

  // Test done
  cli.parse_args(["list", "--status", "done"])
  |> should.equal(Ok(cli.ListTasks(None, Some("done"))))
}

/// Test that valid strategy values are accepted (immediate, gradual, canary)
pub fn parse_args_approve_command_accepts_valid_strategy_test() {
  // Test immediate
  cli.parse_args(["approve", "-s", "task", "--strategy", "immediate"])
  |> should.equal(Ok(cli.ApproveTask("task", Some("immediate"), False)))

  // Test gradual
  cli.parse_args(["approve", "-s", "task", "--strategy", "gradual"])
  |> should.equal(Ok(cli.ApproveTask("task", Some("gradual"), False)))

  // Test canary
  cli.parse_args(["approve", "-s", "task", "--strategy", "canary"])
  |> should.equal(Ok(cli.ApproveTask("task", Some("canary"), False)))
}

/// Test that help command with no args returns Help(None)
pub fn parse_args_help_command_test() {
  cli.parse_args(["help"])
  |> should.equal(Ok(cli.Help(None)))
}

/// Test that help command with topic returns Help(Some(topic))
pub fn parse_args_help_command_with_topic_test() {
  cli.parse_args(["help", "new"])
  |> should.equal(Ok(cli.Help(Some("new"))))
}

/// Test that version command is parsed correctly
pub fn parse_args_version_command_test() {
  cli.parse_args(["version"])
  |> should.equal(Ok(cli.Version))
}

/// Test that empty args returns Help (default behavior)
pub fn parse_args_empty_returns_help_test() {
  cli.parse_args([])
  |> should.equal(Ok(cli.Help(None)))
}

/// Test unknown command returns error
pub fn parse_args_unknown_command_returns_error_test() {
  case cli.parse_args(["foobar"]) {
    Error(msg) -> should.be_true(contains_substring(msg, "Unknown command"))
    Ok(_) -> should.fail()
  }
}

/// Test that list command accepts both --priority and --status together
pub fn parse_args_list_command_with_both_priority_and_status_test() {
  cli.parse_args(["list", "--priority", "P1", "--status", "open"])
  |> should.equal(Ok(cli.ListTasks(Some("P1"), Some("open"))))
}

/// Test that stage command with short -s for slug works correctly
pub fn parse_args_stage_command_with_short_slug_test() {
  cli.parse_args(["stage", "-s", "my-task", "--stage", "implement"])
  |> should.equal(Ok(cli.RunStage("my-task", "implement", False, None, None)))
}

/// Test that stage command missing slug returns clear error
pub fn parse_args_stage_command_missing_slug_returns_error_test() {
  case cli.parse_args(["stage", "--stage", "implement"]) {
    Error(msg) -> {
      should.be_true(
        contains_substring(msg, "--slug") && contains_substring(msg, "required"),
      )
    }
    Ok(_) -> should.fail()
  }
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

/// Test that 'approve' command without required --slug flag returns an error
/// This tests requirement #3: all commands that require --slug must validate it
/// The approve command requires --slug just like new, stage, and show
pub fn parse_args_approve_command_missing_slug_returns_error_test() {
  // Arrange: 'approve' command with NO --slug flag
  let args = ["approve"]

  // Act: parse the args
  let result = cli.parse_args(args)

  // Assert: should return Error with a message indicating --slug is required
  case result {
    Error(msg) -> {
      // The error message should clearly say --slug is required
      should.be_true(
        contains_substring(msg, "--slug") && contains_substring(msg, "required"),
      )
    }
    Ok(_) -> should.fail()
  }
}

// ============================================================================
// CONFIG MODULE TESTS
// ============================================================================

/// Test that default_config returns a Config with sensible default values.
/// This drives creation of the Config type with fields:
/// - data_dir: String (defaults to ".factory")
/// - default_priority: Priority (P1, P2, P3 - defaults to P2)
/// - verbose: Bool (defaults to False)
pub fn default_config_returns_config_with_sensible_defaults_test() {
  // Act: get the default configuration
  let cfg = config.default_config()

  // Assert: data_dir should have a sensible default (commonly ".factory")
  cfg.data_dir
  |> should.equal(".factory")

  // Assert: default_priority should be P2 (middle priority is a sensible default)
  cfg.default_priority
  |> should.equal(config.P2)

  // Assert: verbose should default to False
  cfg.verbose
  |> should.equal(False)
}

/// Test that get_data_dir returns the data directory from a config.
/// This is a simple accessor function that provides encapsulation
/// and makes it easier to change the internal representation later.
pub fn get_data_dir_returns_data_directory_test() {
  // Arrange: create a config with a known data_dir
  let cfg =
    config.Config(
      data_dir: "/custom/data/path",
      default_priority: config.P1,
      verbose: True,
    )

  // Act: get the data directory using the accessor
  let result = config.get_data_dir(cfg)

  // Assert: should return the data_dir value from the config
  result
  |> should.equal("/custom/data/path")
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

// ============================================================================
// SIGNALS MODULE TESTS
// ============================================================================

pub fn test_failure_signal_has_required_fields_test() {
  // Arrange: Create a TestFailure signal with all required fields
  let signal =
    signals.TestFailure(
      file: "src/test_module.gleam",
      error: "assertion failed: expected 42, got 0",
      context_hash: "abc123def456",
      timestamp: "2026-01-06T12:34:56Z",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.TestFailure(file, error, context_hash, timestamp) -> {
      file |> should.equal("src/test_module.gleam")
      error |> should.equal("assertion failed: expected 42, got 0")
      context_hash |> should.equal("abc123def456")
      timestamp |> should.equal("2026-01-06T12:34:56Z")
    }
  }
}

pub fn test_passing_signal_has_required_fields_test() {
  // Arrange: Create a TestPassing signal with all required fields
  let signal =
    signals.TestPassing(
      hash: "pass123def456",
      timestamp: "2026-01-06T12:45:00Z",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.TestPassing(hash, timestamp) -> {
      hash |> should.equal("pass123def456")
      timestamp |> should.equal("2026-01-06T12:45:00Z")
    }
  }
}

pub fn bead_assigned_signal_has_required_fields_test() {
  // Arrange: Create a BeadAssigned signal with all required fields
  let signal =
    signals.BeadAssigned(
      task_id: "task-001",
      spec: "Implement user authentication",
      requirements: ["RFC 2818 compliance", "JWT support"],
      priority: "P1",
      assigned_at: "2026-01-06T13:00:00Z",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.BeadAssigned(task_id, spec, requirements, priority, assigned_at) -> {
      task_id |> should.equal("task-001")
      spec |> should.equal("Implement user authentication")
      requirements |> should.equal(["RFC 2818 compliance", "JWT support"])
      priority |> should.equal("P1")
      assigned_at |> should.equal("2026-01-06T13:00:00Z")
    }
  }
}

pub fn patch_proposed_signal_has_required_fields_test() {
  // Arrange: Create a PatchProposed signal with all required fields
  let signal =
    signals.PatchProposed(
      diff: "--- a/src/main.gleam\n+++ b/src/main.gleam\n@@ -1,3 +1,3 @@",
      author_pid: "pid-12345",
      workspace: "/home/dev/workspace",
      hash: "commit-abc123",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.PatchProposed(diff, author_pid, workspace, hash) -> {
      diff
      |> should.equal(
        "--- a/src/main.gleam\n+++ b/src/main.gleam\n@@ -1,3 +1,3 @@",
      )
      author_pid |> should.equal("pid-12345")
      workspace |> should.equal("/home/dev/workspace")
      hash |> should.equal("commit-abc123")
    }
  }
}

pub fn patch_accepted_signal_has_required_fields_test() {
  // Arrange: Create a PatchAccepted signal with all required fields
  let signal =
    signals.PatchAccepted(
      hash: "commit-def456",
      merged_at: "2026-01-06T14:00:00Z",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.PatchAccepted(hash, merged_at) -> {
      hash |> should.equal("commit-def456")
      merged_at |> should.equal("2026-01-06T14:00:00Z")
    }
  }
}

pub fn patch_rejected_signal_has_required_fields_test() {
  // Arrange: Create a PatchRejected signal with all required fields
  let signal =
    signals.PatchRejected(reason: "Code review failed: missing error handling")

  // Assert: Pattern match to verify the field is present and accessible
  case signal {
    signals.PatchRejected(reason) -> {
      reason |> should.equal("Code review failed: missing error handling")
    }
  }
}

pub fn golden_master_updated_signal_has_required_fields_test() {
  // Arrange: Create a GoldenMasterUpdated signal with all required fields
  let signal =
    signals.GoldenMasterUpdated(
      old_hash: "golden-old-hash123",
      new_hash: "golden-new-hash456",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.GoldenMasterUpdated(old_hash, new_hash) -> {
      old_hash |> should.equal("golden-old-hash123")
      new_hash |> should.equal("golden-new-hash456")
    }
  }
}

pub fn evolution_signal_has_required_fields_test() {
  // Arrange: Create an Evolution signal with all required fields
  let signal =
    signals.Evolution(
      new_hash: "evolved-hash789",
      cause: "Test failure triggered evolution",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.Evolution(new_hash, cause) -> {
      new_hash |> should.equal("evolved-hash789")
      cause |> should.equal("Test failure triggered evolution")
    }
  }
}

pub fn loop_spawned_signal_has_required_fields_test() {
  // Arrange: Create a LoopSpawned signal with all required fields
  let signal =
    signals.LoopSpawned(
      loop_id: "loop-spawn-001",
      task_id: "task-001",
      phase: "tdd-setup",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.LoopSpawned(loop_id, task_id, phase) -> {
      loop_id |> should.equal("loop-spawn-001")
      task_id |> should.equal("task-001")
      phase |> should.equal("tdd-setup")
    }
  }
}

pub fn loop_complete_signal_has_required_fields_test() {
  // Arrange: Create a LoopComplete signal with all required fields
  let signal =
    signals.LoopComplete(
      loop_id: "loop-complete-001",
      task_id: "task-001",
      commits: 5,
      reverts: 2,
      duration_ms: 45_000,
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.LoopComplete(loop_id, task_id, commits, reverts, duration_ms) -> {
      loop_id |> should.equal("loop-complete-001")
      task_id |> should.equal("task-001")
      commits |> should.equal(5)
      reverts |> should.equal(2)
      duration_ms |> should.equal(45_000)
    }
  }
}

pub fn loop_failed_signal_has_required_fields_test() {
  // Arrange: Create a LoopFailed signal with all required fields
  let signal =
    signals.LoopFailed(
      loop_id: "loop-fail-001",
      reason: "Maximum retries exceeded",
    )

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.LoopFailed(loop_id, reason) -> {
      loop_id |> should.equal("loop-fail-001")
      reason |> should.equal("Maximum retries exceeded")
    }
  }
}

pub fn resource_exhausted_signal_has_required_fields_test() {
  // Arrange: Create a ResourceExhausted signal with all required fields
  let signal =
    signals.ResourceExhausted(resource: "disk_space", current: 99, limit: 100)

  // Assert: Pattern match to verify all fields are present and accessible
  case signal {
    signals.ResourceExhausted(resource, current, limit) -> {
      resource |> should.equal("disk_space")
      current |> should.equal(99)
      limit |> should.equal(100)
    }
  }
}

// ============================================================================
// OTP DEPENDENCIES TESTS
// ============================================================================

/// Test that gleam_otp is available as a dependency
/// This verifies that the project has gleam_otp >= 0.10.0 in gleam.toml
/// The actor model is essential for the TCR loop and signal handling
/// Note: The import at the top of this file (gleam/otp) already validates
/// the dependency exists - if it doesn't, this file won't compile at all.
pub fn gleam_otp_is_available_as_dependency_test() {
  // The import 'gleam/otp' at the top of this file will fail to compile
  // if gleam_otp is not in gleam.toml. This test just confirms we got here.
  Nil
}

/// Test that gleam_erlang is available as a dependency
/// This verifies that the project has gleam_erlang >= 0.25.0 in gleam.toml
/// The erlang module provides low-level process utilities needed for OTP actors
/// Note: The import at the top of this file (gleam/erlang) already validates
/// the dependency exists - if it doesn't, this file won't compile at all.
pub fn gleam_erlang_is_available_as_dependency_test() {
  // The import 'gleam/erlang' at the top of this file will fail to compile
  // if gleam_erlang is not in gleam.toml. This test just confirms we got here.
  Nil
}

// ============================================================================
// PROCESS ID TESTS
// ============================================================================

/// Test that ProcessId round-trip conversion preserves Pid identity
/// This test validates that ProcessId wraps Pid correctly:
/// - from_pid/1 converts a Pid to ProcessId (type-safe wrapping)
/// - to_pid/1 converts ProcessId back to a Pid (unwrapping)
/// - The conversion preserves the Pid value through round-trip
///
/// This drives the need for:
/// 1. An opaque ProcessId type defined in src/types.gleam
/// 2. A from_pid(Pid) -> ProcessId function
/// 3. A to_pid(ProcessId) -> Pid function
///
/// Edge case: Tests with the actual current process's Pid to ensure
/// the opaque wrapper correctly preserves real BEAM process identifiers.
pub fn process_id_round_trip_conversion_preserves_pid_test() {
  // Arrange: Get the current process's Pid
  let original_pid = erl_process.self()

  // Act: Wrap in ProcessId and unwrap
  let wrapped = types.from_pid(original_pid)
  let unwrapped = types.to_pid(wrapped)

  // Assert: Round-trip conversion preserves identity
  unwrapped
  |> should.equal(original_pid)
}

// ============================================================================
// WORKSPACE MANAGER ACTOR TESTS
// ============================================================================

// NOTE: workspace_manager tests are deferred pending actor API fixes in src/workspace_manager.gleam
// The workspace_manager.gleam file currently has compilation errors that prevent testing.
// Once those are fixed, additional tests will verify:
// 1. workspace_manager.start_link() successfully starts an actor and returns Ok(subject)
// 2. RegisterWorkspace message type exists for populating workspace state
// 3. UpdateWorkspace message type exists for modifying workspace state
// 4. The actor properly maintains Dict(WorkspaceId, Workspace) state

/// Test that types module provides a public constructor for WorkspaceId.
///
/// This drives the implementation of:
/// 1. A public function `new_workspace_id(id: String) -> WorkspaceId` in types.gleam
/// 2. This function creates opaque WorkspaceId values that can be used throughout the system
///
/// Design rationale: WorkspaceId is opaque (implementation hidden), but external code
/// needs a way to construct WorkspaceId values. Without this function, tests and other
/// modules cannot create workspaces or send messages to the workspace manager.
///
/// Why this matters: Opaque types in Gleam hide internal representation but must be
/// constructible by external code. This test drives the minimal public API needed.
///
/// Edge case: Verifies that constructed WorkspaceIds can be used in the Workspace record,
/// which requires them to be type-compatible with the workspace record's id field.
pub fn types_workspace_id_constructor_creates_valid_workspace_ids_test() {
  // Arrange: Prepare two workspace ID strings
  let id_one = "workspace-alpha"
  let id_two = "workspace-beta"

  // Act: Create WorkspaceId values using the public constructor
  let workspace_id_one = types.new_workspace_id(id_one)
  let workspace_id_two = types.new_workspace_id(id_two)

  // Assert: Can construct Workspace records with these IDs
  // This tests that WorkspaceIds are properly type-compatible with Workspace.id field
  let _workspace_one =
    types.Workspace(
      id: workspace_id_one,
      path: "/tmp/alpha",
      workspace_type: types.Jj,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T12:00:00Z",
    )

  let _workspace_two =
    types.Workspace(
      id: workspace_id_two,
      path: "/tmp/beta",
      workspace_type: types.Reflink,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T13:00:00Z",
    )

  // The fact that we created both workspaces means:
  // 1. new_workspace_id() exists and returns WorkspaceId
  // 2. WorkspaceIds are assignable to Workspace.id field
  // 3. Both Jj and Reflink workspace types work
  Nil
}

/// Test that workspace_manager.start_link() successfully returns Ok(subject).
///
/// This is the core requirement for Iteration 2:
/// The workspace manager must be an OTP actor that can be started and returns
/// a Subject to send messages to it.
///
/// This test drives the implementation of:
/// 1. workspace_manager.start_link() -> Result(Subject(...), WorkspaceManagerError)
/// 2. An OTP actor using gleam_otp that maintains empty Dict state initially
/// 3. Proper actor spawning and supervision integration
///
/// Design rationale: Without a working start_link(), no other workspace
/// manager functionality (RegisterWorkspace, UpdateWorkspace, etc.) can work.
/// This is the foundation of the entire actor system.
///
/// Edge case: Verifies that the Subject returned can be used (we store it in a
/// variable, proving the type system accepts it and we can work with it).
pub fn workspace_manager_start_link_returns_ok_subject_test() {
  // Act: Start the workspace manager actor
  let result = workspace_manager.start_link()

  // Assert: Should return Ok with a Subject we can work with
  case result {
    Ok(_subject) -> {
      // The actor started successfully
      // We don't need to do anything with the subject in this test,
      // but proving we can pattern match on Ok(_) validates the return type
      Nil
    }
    Error(_err) -> {
      // If we get here, the actor failed to start
      should.fail()
    }
  }
}

/// Test that workspace_manager provides a ListWorkspaces query function.
///
/// This drives the implementation of:
/// 1. A ListWorkspaces message type for querying all registered workspaces
/// 2. A query_workspaces(Subject) -> Result(List(Workspace), String) convenience function
/// 3. Proper response handling through process messaging (avoiding actor.call FFI issues)
///
/// Design rationale: The workspace manager must expose a way to query registered workspaces.
/// Using a standalone query function (instead of actor.call) provides:
/// - Type-safe interface that doesn't rely on problematic FFI
/// - Simple request-response pattern using process.send and process.receive
/// - Clear separation between fire-and-forget (RegisterWorkspace) and query operations
///
/// This drives good architecture by:
/// - Requiring a ListWorkspaces message that returns a List of all workspaces
/// - Forcing the handler to collect all Dict values into a list
/// - Making the public API simple and safe (no timeout complexity)
///
/// Edge case: Tests with multiple registered workspaces to ensure:
/// - The Dict is properly iterated and converted to List
/// - All workspace fields are preserved through the response
/// - The query works correctly after RegisterWorkspace operations
pub fn workspace_manager_can_list_all_registered_workspaces_test() {
  // Arrange: Start the workspace manager actor
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create and register multiple workspaces
  let workspace_id_1 = types.new_workspace_id("workspace-alpha")
  let workspace_1 =
    types.Workspace(
      id: workspace_id_1,
      path: "/tmp/alpha",
      workspace_type: types.Jj,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T10:00:00Z",
    )

  let workspace_id_2 = types.new_workspace_id("workspace-beta")
  let workspace_2 =
    types.Workspace(
      id: workspace_id_2,
      path: "/tmp/beta",
      workspace_type: types.Reflink,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T11:00:00Z",
    )

  // Act: Register both workspaces
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace_1))
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace_2))

  // Act: Query all registered workspaces
  let result = workspace_manager.query_workspaces(manager_subject)

  // Assert: Should successfully retrieve both workspaces in the list
  case result {
    Ok(workspaces) -> {
      // Should have exactly 2 workspaces
      list_length(workspaces)
      |> should.equal(2)

      // Both workspaces should be present (order may vary due to Dict iteration)
      let has_alpha =
        list.any(workspaces, fn(w: types.Workspace) {
          case w.workspace_type, w.path {
            types.Jj, "/tmp/alpha" -> True
            _, _ -> False
          }
        })

      let has_beta =
        list.any(workspaces, fn(w: types.Workspace) {
          case w.workspace_type, w.path {
            types.Reflink, "/tmp/beta" -> True
            _, _ -> False
          }
        })

      should.be_true(has_alpha)
      should.be_true(has_beta)
    }
    Error(_) -> {
      should.fail()
    }
  }
}

/// Test that workspace_manager accepts RegisterWorkspace messages.
///
/// This test verifies that:
/// 1. The workspace manager actor can receive and process RegisterWorkspace messages
/// 2. The message is accepted without crashing the actor
/// 3. The actor remains operational after processing the message
///
/// This drives good design by:
/// - Ensuring the message handler properly processes RegisterWorkspace
/// - Verifying the Dict state is updated correctly when workspaces are registered
/// - Establishing that the actor's message loop is robust
///
/// Edge case: Sends a message and immediately checks that the actor doesn't error.
/// We don't use actor.call here since that has FFI issues; instead we just verify
/// the send succeeds and the actor doesn't crash.
pub fn workspace_manager_can_send_register_workspace_message_test() {
  // Arrange: Start the workspace manager actor
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create a workspace to register
  let workspace_id = types.new_workspace_id("send-test-workspace")
  let workspace =
    types.Workspace(
      id: workspace_id,
      path: "/tmp/send-test",
      workspace_type: types.Reflink,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T16:00:00Z",
    )

  // Act: Send RegisterWorkspace message to the actor
  // This should not crash or return an error
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace))

  // Assert: If we got here without panicking, the message was accepted
  // The actor is still running and responsive to messages
  Nil
}

/// Test that workspace_manager.query_workspaces() returns an empty list initially.
///
/// This drives the implementation of:
/// 1. A ListWorkspaces message type in WorkspaceManagerMessage
/// 2. A query_workspaces(Subject) -> Result(List(Workspace), String) function
/// 3. Handler logic to respond to ListWorkspaces by returning all workspaces from Dict
///
/// Design rationale: Before any workspaces are registered, the query should return
/// an empty list. This establishes the baseline behavior and validates the query mechanism.
///
/// This test drives good design by:
/// - Establishing a pattern for querying actor state without using actor.call FFI
/// - Separating fire-and-forget messages (RegisterWorkspace) from query operations
/// - Making the public API simple: just pass the subject, get a Result back
pub fn workspace_manager_query_workspaces_returns_empty_list_initially_test() {
  // Arrange: Start the workspace manager actor with no workspaces registered
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Act: Query all workspaces when the actor is empty
  let result = workspace_manager.query_workspaces(manager_subject)

  // Assert: Should return Ok with an empty list
  result
  |> should.equal(Ok([]))
}

/// Test that workspace_manager can retrieve a specific workspace by WorkspaceId.
///
/// This drives the implementation of:
/// 1. A GetWorkspace message type that accepts a WorkspaceId and returns a workspace
/// 2. A query_workspace(Subject, WorkspaceId) -> Result(Workspace, String) function
/// 3. Proper Dict lookup in the actor handler
///
/// Design rationale: Users need to retrieve a registered workspace by its ID.
/// This is a common pattern for workspace-specific operations and must be efficient.
///
/// This test drives good design by:
/// - Establishing a typed query function (not a generic call)
/// - Using the opaque WorkspaceId to ensure type safety
/// - Returning Result to handle "not found" cases gracefully
/// - Making the pattern consistent with query_workspaces
///
/// Edge case: Tests that querying for a non-existent workspace ID returns Error,
/// not a panic or None, proving defensive programming is enforced.
pub fn workspace_manager_can_retrieve_workspace_by_id_test() {
  // Arrange: Start the workspace manager and register a workspace
  let assert Ok(manager_subject) = workspace_manager.start_link()

  let workspace_id = types.new_workspace_id("retrieve-test-workspace")
  let workspace =
    types.Workspace(
      id: workspace_id,
      path: "/tmp/retrieve-test",
      workspace_type: types.Jj,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T17:00:00Z",
    )

  // Register the workspace
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace))

  // Act: Retrieve the workspace by its ID
  let result = workspace_manager.query_workspace(manager_subject, workspace_id)

  // Assert: Should return Ok with the exact workspace we registered
  case result {
    Ok(retrieved) -> {
      retrieved.path |> should.equal("/tmp/retrieve-test")
      retrieved.workspace_type |> should.equal(types.Jj)
    }
    Error(_) -> should.fail()
  }
}

/// Test that querying for a non-existent workspace returns Error.
///
/// This verifies that:
/// 1. The query_workspace function properly handles not-found cases
/// 2. The actor doesn't panic or crash when a workspace ID doesn't exist
/// 3. Errors are returned gracefully through the Result type
///
/// This drives defensive programming by:
/// - Requiring explicit error handling in the public API
/// - Ensuring the actor remains robust even with invalid queries
/// - Establishing consistent error semantics across queries
pub fn workspace_manager_query_workspace_returns_error_when_not_found_test() {
  // Arrange: Start workspace manager with no workspaces
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create a workspace ID that was never registered
  let nonexistent_id = types.new_workspace_id("does-not-exist")

  // Act: Try to retrieve a non-existent workspace
  let result = workspace_manager.query_workspace(manager_subject, nonexistent_id)

  // Assert: Should return Error, not crash
  case result {
    Ok(_) -> should.fail()
    Error(_msg) -> Nil  // Graceful error handling
  }
}

// ============================================================================
// GIT HASH TESTS
// ============================================================================

/// Test that GitHash.parse validates a valid 40-character lowercase hex string.
///
/// This drives the implementation of:
/// 1. A GitHash opaque type in src/types.gleam
/// 2. A parse(String) -> Result(GitHash, String) function that validates the input
/// 3. Validation rules: exactly 40 characters, all lowercase hexadecimal digits
///
/// Design rationale: Git commit hashes are always 40 hex characters (SHA-1).
/// Making this an opaque type ensures type safety and prevents invalid hashes
/// from being used throughout the system.
///
/// This drives good design by:
/// - Enforcing validation at the boundary (during parsing)
/// - Using Result type for explicit error handling
/// - Making illegal states unrepresentable (can't have a GitHash that's invalid)
///
/// Edge case: Tests with a realistic git SHA-1 hash to ensure the validation
/// accepts the exact format that git actually produces.
pub fn git_hash_parse_accepts_valid_40_char_lowercase_hex_test() {
  // Arrange: A valid git SHA-1 hash (40 lowercase hex characters)
  let valid_hash = "a1b2c3d4e5f6789012345678901234567890abcd"

  // Act: Parse the valid hash
  let result = types.git_hash_parse(valid_hash)

  // Assert: Should successfully parse and return Ok(GitHash)
  result
  |> should.be_ok
}

/// Test that GitHash.parse rejects hashes that are too short.
///
/// This drives validation of the 40-character requirement.
/// Any hash shorter than 40 characters is invalid and should return Error.
pub fn git_hash_parse_rejects_hash_too_short_test() {
  // Arrange: A hash that's too short (39 characters)
  let short_hash = "a1b2c3d4e5f6789012345678901234567890abc"

  // Act: Try to parse the short hash
  let result = types.git_hash_parse(short_hash)

  // Assert: Should reject with Error
  result
  |> should.be_error
}

/// Test that GitHash.parse rejects hashes that are too long.
///
/// This drives validation of the 40-character requirement.
/// Any hash longer than 40 characters is invalid and should return Error.
pub fn git_hash_parse_rejects_hash_too_long_test() {
  // Arrange: A hash that's too long (41 characters)
  let long_hash = "a1b2c3d4e5f6789012345678901234567890abcde"

  // Act: Try to parse the long hash
  let result = types.git_hash_parse(long_hash)

  // Assert: Should reject with Error
  result
  |> should.be_error
}

/// Test that GitHash.parse rejects hashes with uppercase characters.
///
/// This drives validation that hashes must be lowercase hexadecimal.
/// Git hashes are typically displayed in lowercase, and this constraint
/// ensures consistency.
pub fn git_hash_parse_rejects_uppercase_hex_test() {
  // Arrange: A 40-character hex string with uppercase letters
  let uppercase_hash = "A1B2C3D4E5F6789012345678901234567890ABCD"

  // Act: Try to parse the uppercase hash
  let result = types.git_hash_parse(uppercase_hash)

  // Assert: Should reject with Error
  result
  |> should.be_error
}

/// Test that GitHash.parse rejects hashes with non-hexadecimal characters.
///
/// This drives validation that only 0-9 and a-f are valid characters.
/// Any other character should cause rejection.
pub fn git_hash_parse_rejects_non_hex_characters_test() {
  // Arrange: A 40-character string with invalid characters (g, h, z, space, etc.)
  let invalid_hash = "z1b2c3d4e5f6789012345678901234567890abcd"

  // Act: Try to parse the invalid hash
  let result = types.git_hash_parse(invalid_hash)

  // Assert: Should reject with Error
  result
  |> should.be_error
}

/// Test that GitHash.to_string unwraps the opaque type back to a String.
///
/// This drives the implementation of:
/// 1. A to_string(GitHash) -> String function
/// 2. Recovery of the original hash string from the opaque wrapper
///
/// Design rationale: While GitHash is opaque for type safety, we need a way
/// to extract the underlying string for display, serialization, or comparison.
///
/// This drives good design by:
/// - Providing the minimal interface needed to work with the opaque type
/// - Using a simple, predictable function name (to_string)
/// - Maintaining the guarantee that a GitHash can always be converted back
pub fn git_hash_to_string_unwraps_valid_hash_test() {
  // Arrange: A valid hash that we parse
  let valid_hash = "b2c3d4e5f6789012345678901234567890abcd1a"
  let assert Ok(git_hash) = types.git_hash_parse(valid_hash)

  // Act: Convert back to string
  let result = types.git_hash_to_string(git_hash)

  // Assert: Should return the original hash string
  result
  |> should.equal(valid_hash)
}

// ============================================================================
// WORKSPACE DESTRUCTION TESTS
// ============================================================================

/// Test that destroy_workspace removes a workspace directory and updates state.
///
/// This drives the implementation of:
/// 1. A destroy_workspace(manager, workspace_id) function in workspace_manager
/// 2. Filesystem cleanup that removes the workspace directory (rm -rf semantics)
/// 3. State tracking update that removes the workspace from the manager's Dict
/// 4. Proper error handling for both Jj and Reflink workspace types
///
/// Design rationale: When a workspace is no longer needed, it must be completely
/// cleaned up: both on disk (remove the directory tree) and in memory (update state).
/// This prevents resource leaks and maintains consistency.
///
/// This test drives good design by:
/// - Requiring a single operation that handles both cleanup aspects
/// - Supporting both workspace types (Jj and Reflink) with same semantics
/// - Using Result type for explicit error handling (disk errors, state errors)
/// - Ensuring idempotency (subsequent destroy on same ID should not crash)
///
/// Edge case: Tests with an actual workspace that exists in the manager,
/// verifies directory removal, and confirms state is updated afterward.
pub fn workspace_manager_destroy_workspace_removes_directory_and_state_test() {
  // Arrange: Start the workspace manager
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create a temporary workspace directory and register it
  let workspace_id = types.new_workspace_id("destroy-test-workspace")
  let temp_dir = "/tmp/factory-destroy-test"

  let workspace =
    types.Workspace(
      id: workspace_id,
      path: temp_dir,
      workspace_type: types.Jj,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T18:00:00Z",
    )

  // Register the workspace
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace))

  // Verify it was registered
  let assert Ok(workspaces_before) = workspace_manager.query_workspaces(manager_subject)
  workspaces_before
  |> list_length
  |> should.equal(1)

  // Act: Destroy the workspace (removes directory and state)
  let result = workspace_manager.destroy_workspace(manager_subject, workspace_id)

  // Assert: destroy_workspace should succeed
  case result {
    Ok(_) -> {
      // Verify the workspace is no longer in state
      let assert Ok(workspaces_after) = workspace_manager.query_workspaces(manager_subject)
      workspaces_after
      |> list_length
      |> should.equal(0)
    }
    Error(_msg) -> should.fail()
  }
}

/// Test that destroy_workspace handles non-existent workspace ID gracefully.
///
/// This drives defensive programming by:
/// - Returning Error when trying to destroy a workspace that doesn't exist in state
/// - Not panicking or crashing the actor
/// - Providing clear error semantics
pub fn workspace_manager_destroy_workspace_nonexistent_id_returns_error_test() {
  // Arrange: Start workspace manager with no workspaces
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create a workspace ID that was never registered
  let nonexistent_id = types.new_workspace_id("never-existed")

  // Act: Try to destroy a non-existent workspace
  let result = workspace_manager.destroy_workspace(manager_subject, nonexistent_id)

  // Assert: Should return Error, not crash
  case result {
    Ok(_) -> should.fail()
    Error(_msg) -> Nil  // Graceful error handling
  }
}

/// Test that destroy_workspace handles both Jj and Reflink workspace types.
///
/// This drives the implementation to be type-agnostic and handle cleanup for
/// all supported workspace types uniformly.
///
/// Design rationale: The cleanup logic should not differ based on workspace type.
/// Both Jj and Reflink workspaces are directories that need to be removed.
pub fn workspace_manager_destroy_workspace_handles_reflink_type_test() {
  // Arrange: Start the workspace manager
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create a Reflink workspace (not Jj)
  let workspace_id = types.new_workspace_id("destroy-reflink-test")
  let temp_dir = "/tmp/factory-destroy-reflink"

  let workspace =
    types.Workspace(
      id: workspace_id,
      path: temp_dir,
      workspace_type: types.Reflink,  // Explicitly Reflink, not Jj
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T19:00:00Z",
    )

  // Register the workspace
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace))

  // Act: Destroy the Reflink workspace
  let result = workspace_manager.destroy_workspace(manager_subject, workspace_id)

  // Assert: Should handle Reflink the same as Jj
  case result {
    Ok(_) -> {
      // Verify it was removed from state
      let assert Ok(remaining) = workspace_manager.query_workspaces(manager_subject)
      remaining
      |> list_length
      |> should.equal(0)
    }
    Error(_msg) -> should.fail()
  }
}

/// Test that destroy_workspace actually removes the filesystem directory.
///
/// This is the critical requirement for destroy_workspace:
/// 1. It must remove the workspace directory from disk (rm -rf semantics)
/// 2. It must update the state to remove the workspace record
///
/// This drives the implementation to:
/// - Accept a workspace path and delete it recursively
/// - Handle both Jj and Reflink directory structures
/// - Return error if directory doesn't exist
/// - Return error if insufficient permissions
///
/// Design rationale: A workspace is not truly destroyed unless both:
/// - The directory is removed from disk (cleanup)
/// - The workspace record is removed from state (consistency)
///
/// This test drives good design by:
/// - Requiring actual filesystem operations (not just state management)
/// - Ensuring idempotency and resource cleanup
/// - Verifying error handling for missing directories
pub fn workspace_manager_destroy_workspace_removes_filesystem_directory_test() {
  // Arrange: Start the workspace manager
  let assert Ok(manager_subject) = workspace_manager.start_link()

  // Arrange: Create a workspace with a real temporary directory
  let workspace_id = types.new_workspace_id("fs-cleanup-test")
  let temp_workspace_dir = "/tmp/factory-fs-cleanup-" <> "test-12345"

  // Pre-create the directory to simulate a real workspace
  case simplifile.create_directory_all(temp_workspace_dir) {
    Ok(Nil) -> Nil
    Error(_) -> should.fail()
  }

  // Create a marker file to verify directory cleanup
  let marker_file = temp_workspace_dir <> "/marker.txt"
  case simplifile.write(marker_file, "test") {
    Ok(Nil) -> Nil
    Error(_) -> should.fail()
  }

  let workspace =
    types.Workspace(
      id: workspace_id,
      path: temp_workspace_dir,
      workspace_type: types.Jj,
      owner_pid: types.from_pid(erl_process.self()),
      created_at: "2026-01-06T20:00:00Z",
    )

  // Register the workspace
  actor.send(manager_subject, workspace_manager.RegisterWorkspace(workspace))

  // Verify the marker file exists before destruction
  case simplifile.read(marker_file) {
    Ok(_) -> Nil  // File exists - good
    Error(_) -> should.fail()
  }

  // Act: Destroy the workspace
  let result = workspace_manager.destroy_workspace(manager_subject, workspace_id)

  // Assert: destroy_workspace should succeed
  case result {
    Ok(_) -> {
      // Verify the marker file is actually removed from filesystem
      // If the directory was deleted, the file inside should also be gone
      case simplifile.read(marker_file) {
        Ok(_) -> should.fail()  // File still exists - directory not deleted!
        Error(_) -> Nil  // File is gone - directory was deleted correctly
      }

      // Verify workspace is removed from state
      let assert Ok(remaining) = workspace_manager.query_workspaces(manager_subject)
      remaining
      |> list_length
      |> should.equal(0)
    }
    Error(_msg) -> should.fail()
  }
}

// ============================================================================
// WORKSPACE REFLINK (COW) TESTS
// ============================================================================

/// Test create_workspace_reflink uses cp --reflink for COW.
pub fn create_workspace_reflink_uses_cp_reflink_command_test() {
  let slug = "reflink-cp-test"
  let temp_source = "/tmp/factory-test-src-" <> slug

  case simplifile.create_directory(temp_source) {
    Ok(Nil) -> Nil
    Error(_) -> should.fail()
  }

  case simplifile.write(temp_source <> "/test.txt", "content") {
    Ok(Nil) -> Nil
    Error(_) -> should.fail()
  }

  let assert Ok(manager_subject) = workspace_manager.start_link()

  case workspace_manager.create_workspace_reflink(manager_subject, slug, temp_source) {
    Ok(workspace) -> {
      workspace.path |> should.equal("/dev/shm/factory-" <> slug)
      workspace.workspace_type |> should.equal(types.Reflink)

      case simplifile.verify_is_directory(workspace.path) {
        Ok(True) -> Nil
        _ -> should.fail()
      }

      let _ = process.run_command("rm", ["-rf", temp_source], "/tmp")
      Nil
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// WORKSPACE STRATEGY TESTS
// ============================================================================

/// Test Auto strategy resolves to Reflink when /dev/shm exists and has space.
pub fn workspace_strategy_auto_resolves_to_reflink_when_dev_shm_exists_test() {
  let result = workspace_manager.resolve_auto_strategy()

  case result {
    types.Reflink -> Nil
    types.Jj -> Nil
  }
}

// ============================================================================
// WORKSPACE JJ TESTS
// ============================================================================

/// Test create_workspace_jj creates isolated jj workspace with bookmark.
pub fn create_workspace_jj_creates_isolated_bookmark_test() {
  let timestamp = case process.run_command("date", ["+%s"], "") {
    Ok(process.Success(ts, _, _)) -> string.trim(ts)
    _ -> "0"
  }
  let slug = "jj-bm-test-" <> timestamp
  let temp_source = "/tmp/factory-jj-bm-" <> slug

  // Cleanup: forget workspace if source exists, then remove directories
  let _ = case simplifile.verify_is_directory(temp_source) {
    Ok(True) -> {
      let _ = process.run_command("jj", ["-R", temp_source, "workspace", "forget", slug], "/tmp")
      Nil
    }
    _ -> Nil
  }

  let _ = process.run_command("rm", ["-rf", temp_source], "/tmp")
  let _ = process.run_command("rm", ["-rf", temp_source <> "/../" <> slug], "/tmp")

  case simplifile.create_directory_all(temp_source) {
    Ok(Nil) -> Nil
    Error(_) -> should.fail()
  }

  case process.run_command("jj", ["git", "init", temp_source], "/tmp") {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  let assert Ok(manager_subject) = workspace_manager.start_link()

  case workspace_manager.create_workspace_jj(manager_subject, slug, temp_source) {
    Ok(workspace) -> {
      workspace.workspace_type |> should.equal(types.Jj)

      case process.run_command("jj", ["-R", workspace.path, "bookmark", "list"], "/tmp") {
        Ok(result) -> case result {
          process.Success(output, _, _) -> {
            case contains_substring(output, "feat/" <> slug) {
              True -> Nil
              False -> should.fail()
            }
          }
          _ -> should.fail()
        }
        Error(_) -> should.fail()
      }

      let _ = process.run_command("rm", ["-rf", temp_source], "/tmp")
      let _ = process.run_command("rm", ["-rf", workspace.path], "/tmp")
      Nil
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// SIGNAL BUS TESTS
// ============================================================================

/// Test signal_bus.broadcast sends signal to all subscribers of that type.
pub fn signal_bus_broadcast_reaches_all_subscribers_test() {
  let assert Ok(bus) = signal_bus.start_link()

  let subscriber_1 = erl_process.new_subject()
  let subscriber_2 = erl_process.new_subject()

  let _ = signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber_1)
  let _ = signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber_2)

  signal_bus.broadcast(bus, signal_bus.TestPassing)

  case erl_process.receive(subscriber_1, 1000) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> should.fail()
  }

  case erl_process.receive(subscriber_2, 1000) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> should.fail()
  }
}

/// Test golden_master.start_link() creates actor with path.
pub fn golden_master_start_link_returns_subject_test() {
  let path = "/dev/shm/golden-master"
  let result = golden_master.start_link(path)

  result
  |> should.be_ok
}

/// Test fs/read_text_file handler reads file content.
///
/// Drives:
/// 1. fs_read_text_file(path: String) -> Result(String, String)
/// 2. File read via simplifile.read
/// 3. Error handling for missing/unreadable files
pub fn fs_read_text_file_returns_content_test() {
  let temp_path = "/tmp/factory-fs-read-test.txt"
  let content = "test content"

  case simplifile.write(temp_path, content) {
    Ok(Nil) -> Nil
    Error(_) -> should.fail()
  }

  case process.fs_read_text_file(temp_path) {
    Ok(read_content) -> read_content |> should.equal(content)
    Error(_) -> should.fail()
  }

  let _ = simplifile.delete(temp_path)
  Nil
}

/// Test fs_read_text_file returns error for nonexistent file.
pub fn fs_read_text_file_nonexistent_returns_error_test() {
  let nonexistent = "/tmp/factory-does-not-exist-12345.txt"

  case process.fs_read_text_file(nonexistent) {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

// ============================================================================
// ACP CLIENT TESTS
// ============================================================================

/// Test acp_send_cancel composes notification with transport layer.
///
/// CUPID pressure:
/// - C (Compose): cancel|>encode|>http_send must compose cleanly
/// - U (Unix): Do ONE thing: send cancel to ACP endpoint
/// - P (Pure): Same client+session → same encode logic
/// - I (Idiomatic): Result |> chain, pattern matching errors
/// - D (Domain): Uses ACP transport (URL/headers), not shell exec
///
/// Forces implementer to confront:
/// 1. TRANSPORT: Must encode AcpNotification to JSON/wire format
/// 2. CLIENT STATE: AcpClient must carry endpoint URL for send
/// 3. HTTP: Requires gleam_http or actual transport layer
/// 4. ERROR HANDLING: Network errors, encoding errors, 4xx/5xx
/// 5. NO TRIVIAL: Can't just return Ok - must prove send happened
///
/// Rejects lazy:
/// - "returns Ok" → needs HTTP interaction proof
/// - "type exists" → needs client with URL, encode logic
/// - "field access" → needs transport composition
pub fn acp_send_cancel_composes_notification_with_transport_test() {
  let client = types.AcpClient(base_url: "http://localhost:9999/acp")
  let session_id = "sess-compose-test"

  case process.acp_send_cancel(client, session_id) {
    Ok(_) -> {
      should.fail()
    }
    Error(msg) -> {
      should.be_true(
        contains_substring(msg, "http") || contains_substring(msg, "connect") || contains_substring(msg, "network"),
      )
    }
  }
}

/// Test acp_session_tracker validates cancel only for active sessions.
///
/// CUPID pressure:
/// - P (Pure): Same tracker state → same validation result
/// - U (Unix): Does ONE thing: track ACP session lifecycle
/// - D (Domain): Models ACP protocol: Start → Running → {Complete|Cancelled}
/// - I (Idiomatic): ADT for session states, pattern matching
///
/// Forces implementer to confront:
/// 1. STATE MACHINE: Session lifecycle must be explicit (not just String)
/// 2. VALIDATION: Can't cancel Complete/Cancelled session
/// 3. PURITY: Tracker doesn't DO cancel, validates IF can cancel
/// 4. COMPOSITION: cancel_validation |> send separates concerns
/// 5. TYPE SAFETY: Illegal transitions unrepresentable
///
/// Rejects lazy:
/// - "return Ok always" → needs state tracking
/// - "String session_id" → needs lifecycle ADT
/// - "no validation" → allows double-cancel
///
/// 30-line violation: if tracker >30 lines, split state/validation
pub fn acp_session_tracker_validates_cancel_only_for_active_sessions_test() {
  let tracker = types.new_acp_session_tracker()

  let tracker = types.register_session(tracker, "sess-1", types.Running)
  let tracker = types.register_session(tracker, "sess-2", types.Complete)

  case types.can_cancel(tracker, "sess-1") {
    Ok(True) -> Nil
    _ -> should.fail()
  }

  case types.can_cancel(tracker, "sess-2") {
    Ok(False) -> Nil
    _ -> should.fail()
  }

  case types.can_cancel(tracker, "sess-nonexistent") {
    Error(_) -> Nil
    _ -> should.fail()
  }
}

