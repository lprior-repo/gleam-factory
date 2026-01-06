// Integration module - Test code after merge
// Verifies that changes work when merged back to main branch

import gleam/string
import gleam/result
import domain
import process

/// Result of integration test run
pub type IntegrationResult {
  Passed
  Failed(reason: String)
}

/// Run integration tests - merge worktree branch to main and test
pub fn test_integration(
  task: domain.Task,
  repo_root: String,
) -> Result(IntegrationResult, String) {
  use base_branch <- result.try(get_base_branch(repo_root))

  // STEP 1: Create temporary merge branch
  let temp_branch = "temp-integration-test-" <> task.slug

  use cmd_result <- result.try(
    process.run_command("jj", [
      "-R",
      repo_root,
      "branch",
      "create",
      temp_branch,
      "-r",
      base_branch,
    ], repo_root)
  )
  use _ <- result.try(
    process.check_success(cmd_result)
    |> result.map_error(fn(_) { "Could not create temp branch" })
  )

  // STEP 2: Merge task branch into temp branch
  use cmd_result <- result.try(
    process.run_command("jj", [
      "-R",
      repo_root,
      "merge",
      task.branch,
      "-r",
      temp_branch,
    ], repo_root)
  )
  use _ <- result.try(
    process.check_success(cmd_result)
    |> result.map_error(fn(_) {
      "Could not merge " <> task.branch <> " into temp branch"
    })
  )

  // STEP 3: Run full test suite in merged state
  let test_result = case task.language {
    domain.Go -> test_go_integration(repo_root)
    domain.Gleam -> test_gleam_integration(repo_root)
    domain.Rust -> test_rust_integration(repo_root)
    domain.Python -> test_python_integration(repo_root)
  }

  // STEP 4: Clean up temp branch (regardless of test result)
  let _ = process.run_command("jj", [
    "-R",
    repo_root,
    "branch",
    "delete",
    temp_branch,
  ], repo_root)

  // STEP 5: Return test result
  test_result
}

/// Test Go integration
fn test_go_integration(repo_root: String) -> Result(IntegrationResult, String) {
  process.run_command("go", ["test", "-v", "./..."], repo_root)
  |> result.map(fn(result) {
    case process.is_success(result) {
      True -> Passed
      False ->
        Failed(
          "Go integration tests failed: " <> case result {
          process.Success(_, _, _) -> "unknown error"
          process.Failure(err, code) ->
            err <> " (exit " <> string.inspect(code) <> ")"
        },
        )
    }
  })
  |> result.map_error(fn(err) { "Go integration test error: " <> err })
}

/// Test Gleam integration
fn test_gleam_integration(repo_root: String) -> Result(IntegrationResult, String) {
  process.run_command("gleam", ["test"], repo_root)
  |> result.map(fn(result) {
    case process.is_success(result) {
      True -> Passed
      False ->
        Failed(
          "Gleam integration tests failed: " <> case result {
          process.Success(_, _, _) -> "unknown error"
          process.Failure(err, code) ->
            err <> " (exit " <> string.inspect(code) <> ")"
        },
        )
    }
  })
  |> result.map_error(fn(err) { "Gleam integration test error: " <> err })
}

/// Test Rust integration
fn test_rust_integration(repo_root: String) -> Result(IntegrationResult, String) {
  process.run_command("cargo", ["test", "--all"], repo_root)
  |> result.map(fn(result) {
    case process.is_success(result) {
      True -> Passed
      False ->
        Failed(
          "Rust integration tests failed: " <> case result {
          process.Success(_, _, _) -> "unknown error"
          process.Failure(err, code) ->
            err <> " (exit " <> string.inspect(code) <> ")"
        },
        )
    }
  })
  |> result.map_error(fn(err) { "Rust integration test error: " <> err })
}

/// Test Python integration
fn test_python_integration(repo_root: String) -> Result(IntegrationResult, String) {
  process.run_command("python", ["-m", "pytest", "-v"], repo_root)
  |> result.map(fn(result) {
    case process.is_success(result) {
      True -> Passed
      False ->
        Failed(
          "Python integration tests failed: " <> case result {
          process.Success(_, _, _) -> "unknown error"
          process.Failure(err, code) ->
            err <> " (exit " <> string.inspect(code) <> ")"
        },
        )
    }
  })
  |> result.map_error(fn(err) { "Python integration test error: " <> err })
}

/// Get the base branch (main or master)
fn get_base_branch(repo_root: String) -> Result(String, String) {
  // Try main first, then master
  process.run_command("jj", [
    "-R",
    repo_root,
    "log",
    "-r",
    "main",
    "--no-graph",
  ], repo_root)
  |> result.map(fn(_) { "main" })
  |> result.try(fn(res) {
    case res {
      "main" -> Ok(res)
      _ ->
        process.run_command("jj", [
          "-R",
          repo_root,
          "log",
          "-r",
          "master",
          "--no-graph",
        ], repo_root)
        |> result.map(fn(_) { "master" })
    }
  })
  |> result.map_error(fn(_) { "Could not determine base branch" })
}

/// Format integration result for display
pub fn result_to_string(result: IntegrationResult) -> String {
  case result {
    Passed -> "✓ Integration tests passed"
    Failed(reason) -> "✗ Integration tests failed: " <> reason
  }
}
