// Integration module - Test code after merge
// Verifies that changes work when merged back to main branch

import domain
import gleam/result
import gleam/string
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

  use cmd_result <- result.try(process.run_command(
    "jj",
    [
      "-R",
      repo_root,
      "branch",
      "create",
      temp_branch,
      "-r",
      base_branch,
    ],
    repo_root,
  ))
  use _ <- result.try(
    process.check_success(cmd_result)
    |> result.map_error(fn(_) { "Could not create temp branch" }),
  )

  // STEP 2: Merge task branch into temp branch
  use cmd_result <- result.try(process.run_command(
    "jj",
    [
      "-R",
      repo_root,
      "merge",
      task.branch,
      "-r",
      temp_branch,
    ],
    repo_root,
  ))
  use _ <- result.try(
    process.check_success(cmd_result)
    |> result.map_error(fn(_) {
      "Could not merge " <> task.branch <> " into temp branch"
    }),
  )

  // STEP 3: Run full test suite in merged state
  let test_result = case task.language {
    domain.Go -> test_go_integration(repo_root)
    domain.Gleam -> test_gleam_integration(repo_root)
    domain.Rust -> test_rust_integration(repo_root)
    domain.Python -> test_python_integration(repo_root)
  }

  // STEP 4: Clean up temp branch (regardless of test result)
  let _ =
    process.run_command(
      "jj",
      [
        "-R",
        repo_root,
        "branch",
        "delete",
        temp_branch,
      ],
      repo_root,
    )

  // STEP 5: Return test result
  test_result
}

fn test_go_integration(repo_root: String) -> Result(IntegrationResult, String) {
  run_test_cmd("go", ["test", "-v", "./..."], "Go", repo_root)
}

fn test_gleam_integration(
  repo_root: String,
) -> Result(IntegrationResult, String) {
  run_test_cmd("gleam", ["test"], "Gleam", repo_root)
}

fn test_rust_integration(repo_root: String) -> Result(IntegrationResult, String) {
  run_test_cmd("cargo", ["test", "--all"], "Rust", repo_root)
}

fn test_python_integration(
  repo_root: String,
) -> Result(IntegrationResult, String) {
  run_test_cmd("python", ["-m", "pytest", "-v"], "Python", repo_root)
}

fn run_test_cmd(
  cmd: String,
  args: List(String),
  lang: String,
  repo_root: String,
) -> Result(IntegrationResult, String) {
  process.run_command(cmd, args, repo_root)
  |> result.map(fn(r) {
    case process.is_success(r) {
      True -> Passed
      False ->
        Failed(
          lang
          <> " integration tests failed: "
          <> case r {
            process.Success(_, _, _) -> "unknown error"
            process.Failure(err, code) ->
              err <> " (exit " <> string.inspect(code) <> ")"
          },
        )
    }
  })
  |> result.map_error(fn(err) { lang <> " integration test error: " <> err })
}

/// Get the base branch (main or master)
fn get_base_branch(repo_root: String) -> Result(String, String) {
  // Try main first, then master
  process.run_command(
    "jj",
    [
      "-R",
      repo_root,
      "log",
      "-r",
      "main",
      "--no-graph",
    ],
    repo_root,
  )
  |> result.map(fn(_) { "main" })
  |> result.try(fn(res) {
    case res {
      "main" -> Ok(res)
      _ ->
        process.run_command(
          "jj",
          [
            "-R",
            repo_root,
            "log",
            "-r",
            "master",
            "--no-graph",
          ],
          repo_root,
        )
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

/// Retry a function with exponential backoff
/// Returns Ok if function succeeds, Error if all retries exhausted
pub fn retry_with_backoff(f: fn() -> Result(a, e), retries: Int) -> Result(a, e) {
  do_retry(f, retries)
}

fn do_retry(f: fn() -> Result(a, e), retries_remaining: Int) -> Result(a, e) {
  case f() {
    Ok(value) -> Ok(value)
    Error(err) ->
      case retries_remaining {
        0 -> Error(err)
        _ -> do_retry(f, retries_remaining - 1)
      }
  }
}
