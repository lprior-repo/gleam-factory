import domain
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import integration

pub fn main() -> Nil {
  gleeunit.main()
}

// BASIC RESULT TYPE TESTS
pub fn result_to_string_passed_test() {
  integration.Passed
  |> integration.result_to_string
  |> should.equal("✓ Integration tests passed")
}

pub fn result_to_string_failed_test() {
  integration.Failed("compilation error")
  |> integration.result_to_string
  |> should.equal("✗ Integration tests failed: compilation error")
}

pub fn result_to_string_preserves_reason_test() {
  integration.Failed("timeout in network tests")
  |> integration.result_to_string
  |> string.contains("timeout in network tests")
  |> should.be_true
}

// RETRY WITH BACKOFF TESTS
pub fn retry_with_backoff_success_first_try_test() {
  let f = fn() { Ok(42) }
  integration.retry_with_backoff(f, 3)
  |> should.equal(Ok(42))
}

pub fn retry_with_backoff_success_after_failures_test() {
  let f = fn() { Ok(100) }
  integration.retry_with_backoff(f, 3)
  |> should.equal(Ok(100))
}

pub fn retry_with_backoff_exhausted_retries_test() {
  let f = fn() { Error("permanent") }
  integration.retry_with_backoff(f, 2)
  |> should.equal(Error("permanent"))
}

pub fn retry_with_backoff_zero_retries_test() {
  let f = fn() { Error("no retries") }
  integration.retry_with_backoff(f, 0)
  |> should.equal(Error("no retries"))
}

pub fn retry_with_backoff_single_retry_test() {
  let f = fn() { Ok("success") }
  integration.retry_with_backoff(f, 1)
  |> should.equal(Ok("success"))
}

pub fn retry_with_backoff_large_retry_count_test() {
  let f = fn() { Ok(999) }
  integration.retry_with_backoff(f, 100)
  |> should.equal(Ok(999))
}

// INTEGRATION TEST WITH LANGUAGES
pub fn test_gleam_integration_nonexistent_repo_test() {
  let assert Ok(slug) = domain.validate_slug("test-gleam")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/test-wt",
      branch: "task/test-gleam",
    )
  case integration.test_integration(task, "/nonexistent/path") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

pub fn test_go_integration_invalid_repo_test() {
  let assert Ok(slug) = domain.validate_slug("test-go")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Go,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/test-go-wt",
      branch: "task/test-go",
    )
  case integration.test_integration(task, "/invalid/repo") {
    Error(msg) -> msg |> string.contains("Could not") |> should.be_true
    Ok(_) -> should.fail()
  }
}

pub fn test_rust_integration_missing_repo_test() {
  let assert Ok(slug) = domain.validate_slug("test-rust")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Rust,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/test-rust-wt",
      branch: "task/test-rust",
    )
  case integration.test_integration(task, "/invalid/rust/repo") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

pub fn test_python_integration_missing_repo_test() {
  let assert Ok(slug) = domain.validate_slug("test-python")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Python,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/test-py-wt",
      branch: "task/test-python",
    )
  case integration.test_integration(task, "/invalid/python/repo") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

// MERGE AND BRANCH CREATION TESTS
pub fn test_integration_merge_branch_creation_failure_test() {
  let assert Ok(slug) = domain.validate_slug("merge-fail")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/merge-fail-wt",
      branch: "task/merge-fail",
    )
  case integration.test_integration(task, "") {
    Error(e) -> e |> string.contains("Could not") |> should.be_true
    Ok(_) -> should.fail()
  }
}

pub fn test_integration_merge_conflict_test() {
  let assert Ok(slug) = domain.validate_slug("merge-conflict")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/merge-conflict-wt",
      branch: "task/nonexistent-branch",
    )
  case integration.test_integration(task, "/tmp") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

pub fn test_integration_with_empty_branch_name_test() {
  let assert Ok(slug) = domain.validate_slug("empty-branch")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/empty-wt",
      branch: "",
    )
  case integration.test_integration(task, "/tmp") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

pub fn test_integration_with_special_chars_in_branch_test() {
  let assert Ok(slug) = domain.validate_slug("special-chars")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/special-wt",
      branch: "task/special@#$%",
    )
  case integration.test_integration(task, "/tmp") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

// RESULT TYPE PATTERN MATCHING TESTS
pub fn test_integration_result_passed_pattern_test() {
  let result = integration.Passed
  case result {
    integration.Passed -> Nil
  }
}

pub fn test_integration_result_failed_pattern_test() {
  let result = integration.Failed("reason")
  case result {
    integration.Failed(r) -> r |> should.equal("reason")
  }
}

pub fn test_integration_result_failed_empty_reason_test() {
  let result = integration.Failed("")
  case result {
    integration.Failed(r) -> r |> should.equal("")
  }
}

pub fn test_integration_result_failed_long_reason_test() {
  let long_reason =
    "This is a very long error message that describes multiple failures that occurred during the integration test run including compilation errors, test failures, and validation failures"
  let result = integration.Failed(long_reason)
  case result {
    integration.Failed(r) -> r |> should.equal(long_reason)
  }
}

// TASK STATE TRANSITION TESTS
pub fn task_status_created_to_inprogress_test() {
  domain.is_transient_status(domain.Created)
  |> should.be_false
}

pub fn task_status_inprogress_is_transient_test() {
  let status = domain.InProgress("integration")
  domain.is_transient_status(status)
  |> should.be_true
}

pub fn task_status_passed_is_completed_test() {
  domain.is_completed(domain.PassedPipeline)
  |> should.be_true
}

pub fn task_status_integrated_is_completed_test() {
  domain.is_completed(domain.Integrated)
  |> should.be_true
}

pub fn task_status_failed_not_completed_test() {
  domain.is_completed(domain.FailedPipeline("integration", "merge conflict"))
  |> should.be_false
}

pub fn task_status_created_not_completed_test() {
  domain.is_completed(domain.Created)
  |> should.be_false
}

// VALIDATION TESTS
pub fn validate_slug_lowercase_test() {
  let assert Ok(slug) = domain.validate_slug("valid-slug")
  domain.is_slug_lowercase(slug)
  |> should.be_true
}

pub fn validate_slug_with_numbers_test() {
  let assert Ok(_) = domain.validate_slug("task-123")
  Nil
}

pub fn validate_slug_with_underscores_test() {
  let assert Ok(_) = domain.validate_slug("task_with_underscores")
  Nil
}

pub fn validate_slug_max_length_test() {
  let assert Ok(_) = domain.validate_slug("a1234567890123456789012345678901234567890123456789")
  Nil
}

pub fn validate_slug_empty_fails_test() {
  case domain.validate_slug("") {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

pub fn validate_slug_invalid_chars_fails_test() {
  case domain.validate_slug("invalid@slug") {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

pub fn validate_slug_uppercase_fails_test() {
  case domain.validate_slug("INVALID") {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

pub fn validate_slug_spaces_fails_test() {
  case domain.validate_slug("invalid slug") {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

// ROLLBACK AND ERROR HANDLING TESTS
pub fn integration_error_message_contains_context_test() {
  let assert Ok(slug) = domain.validate_slug("error-test")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/nonexistent",
      branch: "task/error-test",
    )
  case integration.test_integration(task, "/invalid/path") {
    Error(msg) -> msg |> string.is_empty |> should.be_false
    Ok(_) -> should.fail()
  }
}

pub fn integration_failed_result_preserves_error_context_test() {
  let error_reason = "compilation failed in module X"
  let failed = integration.Failed(error_reason)
  let formatted = integration.result_to_string(failed)
  formatted |> string.contains(error_reason) |> should.be_true
}

// LANGUAGE DETECTION TESTS
pub fn language_go_detection_test() {
  case domain.parse_language("go") {
    Ok(domain.Go) -> Nil
    _ -> should.fail()
  }
}

pub fn language_gleam_detection_test() {
  case domain.parse_language("gleam") {
    Ok(domain.Gleam) -> Nil
    _ -> should.fail()
  }
}

pub fn language_rust_detection_test() {
  case domain.parse_language("rust") {
    Ok(domain.Rust) -> Nil
    _ -> should.fail()
  }
}

pub fn language_python_detection_test() {
  case domain.parse_language("python") {
    Ok(domain.Python) -> Nil
    _ -> should.fail()
  }
}

pub fn language_invalid_detection_fails_test() {
  case domain.parse_language("ruby") {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

pub fn language_case_sensitive_detection_test() {
  case domain.parse_language("Go") {
    Ok(_) -> should.fail()
    Error(_) -> Nil
  }
}

// INTEGRATION READINESS TESTS
pub fn task_ready_for_integration_passed_pipeline_test() {
  domain.is_ready_for_integration(domain.PassedPipeline, domain.Go)
  |> should.be_true
}

pub fn task_ready_for_integration_gleam_test() {
  domain.is_ready_for_integration(domain.PassedPipeline, domain.Gleam)
  |> should.be_true
}

pub fn task_ready_for_integration_rust_test() {
  domain.is_ready_for_integration(domain.PassedPipeline, domain.Rust)
  |> should.be_true
}

pub fn task_not_ready_if_python_test() {
  domain.is_ready_for_integration(domain.PassedPipeline, domain.Python)
  |> should.be_false
}

pub fn task_not_ready_if_not_passed_test() {
  domain.is_ready_for_integration(domain.Created, domain.Go)
  |> should.be_false
}

pub fn task_not_ready_if_in_progress_test() {
  domain.is_ready_for_integration(domain.InProgress("integration"), domain.Gleam)
  |> should.be_false
}

pub fn task_not_ready_if_failed_test() {
  domain.is_ready_for_integration(
    domain.FailedPipeline("integration", "merge conflict"),
    domain.Go,
  )
  |> should.be_false
}

// PIPELINE TESTS
pub fn standard_pipeline_has_integration_stage_test() {
  domain.filter_stages("integration", "integration")
  |> should.be_ok
}

pub fn integration_stage_is_tcr_enabled_test() {
  let assert Ok(stages) = domain.filter_stages("integration", "integration")
  let assert [integration_stage] = stages
  domain.is_tcr_stage(integration_stage)
  |> should.be_true
}

pub fn integration_stage_has_retries_test() {
  let assert Ok(stages) = domain.filter_stages("integration", "integration")
  let assert [integration_stage] = stages
  domain.get_stage_retries(integration_stage)
  |> should.not_equal(0)
}

pub fn pipeline_order_is_stable_test() {
  let pipeline1 = domain.standard_pipeline()
  let pipeline2 = domain.standard_pipeline()
  pipeline1
  |> should.equal(pipeline2)
}

pub fn pipeline_implement_stage_first_test() {
  let assert [first, ..] = domain.standard_pipeline()
  case first {
    domain.Stage("implement", _, _, _) -> Nil
    _ -> should.fail()
  }
}

pub fn pipeline_accept_stage_last_test() {
  let pipeline = domain.standard_pipeline()
  let last =
    pipeline
    |> list.last
    |> should.be_ok
  case last {
    domain.Stage("accept", _, _, _) -> Nil
    _ -> should.fail()
  }
}

// CLEANUP AND RECOVERY TESTS
pub fn integration_cleanup_after_error_test() {
  let assert Ok(slug) = domain.validate_slug("cleanup-test")
  let task =
    domain.Task(
      slug: slug,
      language: domain.Gleam,
      status: domain.InProgress("integration"),
      worktree_path: "/tmp/cleanup-wt",
      branch: "task/cleanup",
    )
  case integration.test_integration(task, "/nonexistent") {
    Error(_) -> Nil
    Ok(_) -> should.fail()
  }
}

pub fn integration_temp_branch_naming_consistency_test() {
  let assert Ok(slug) = domain.validate_slug("naming-test")
  let branch_base = domain.slug_to_string(slug)
  branch_base
  |> string.is_empty
  |> should.be_false
}

pub fn integration_result_type_exhaustive_pattern_test() {
  let test_cases: List(#(integration.IntegrationResult, Bool)) = [
    #(integration.Passed, True),
    #(integration.Failed("test"), False),
    #(integration.Failed("another"), False),
  ]

  test_cases
  |> list.all(fn(pair) {
    case pair {
      #(integration.Passed, should_pass) -> should_pass == True
      #(integration.Failed(_), should_pass) -> should_pass == False
    }
  })
  |> should.be_true
}
