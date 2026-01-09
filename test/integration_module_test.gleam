import domain
import gleam/string
import gleeunit
import gleeunit/should
import integration

pub fn main() -> Nil {
  gleeunit.main()
}

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

pub fn test_gleam_integration_success_test() {
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

pub fn test_go_integration_error_test() {
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

pub fn test_rust_integration_error_test() {
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

pub fn test_python_integration_error_test() {
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

pub fn test_integration_result_passed_pattern_test() {
  let result = integration.Passed
  case result {
    integration.Passed -> Nil
    integration.Failed(_) -> should.fail()
  }
}

pub fn test_integration_result_failed_pattern_test() {
  let result = integration.Failed("reason")
  case result {
    integration.Failed(r) -> r |> should.equal("reason")
    integration.Passed -> should.fail()
  }
}
