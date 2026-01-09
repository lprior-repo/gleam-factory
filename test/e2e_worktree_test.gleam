//// End-to-end worktree integration test with real jj repository
//// Tests full pipeline: create worktree, run stages with TCR, verify commits, cleanup
////
//// This test verifies the integration between:
//// - worktree.create_worktree (real jj workspace add)
//// - stages.execute_stage (real gleam build/test)
//// - tcr.run_with_tcr (real jj describe/new/restore)
////
//// Test contract:
//// - Creates real jj worktree in /tmp
//// - Executes real gleam commands
//// - Verifies commits in jj log
//// - Cleans up all resources

import domain
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import process
import stages
import tcr
import worktree

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// E2E WORKTREE TESTS
// ============================================================================

pub fn e2e_worktree_creation_and_cleanup_test() {
  // GIVEN: A test repo directory
  let test_repo = setup_test_repo("e2e_worktree_creation")
  case test_repo {
    Error(_) -> {
      should.fail()
    }
    Ok(repo_path) -> {
      // WHEN: We create a worktree
      case worktree.create_worktree("test-feature", domain.Gleam, repo_path) {
        Error(_) -> {
          let _ = cleanup_test_repo(repo_path)
          should.fail()
        }
        Ok(wt) -> {
          // THEN: Worktree exists and has correct structure
          wt.slug
          |> should.equal("test-feature")

          wt.branch
          |> should.equal("feat/test-feature")

          // Verify directory exists
          case
            process.run_command("test", ["-d", wt.path], "")
            |> result.try(process.check_success)
          {
            Ok(_) -> Nil
            Error(_) -> should.fail()
          }

          // Cleanup
          case worktree.remove_worktree("test-feature", repo_path) {
            Ok(_) -> Nil
            Error(_) -> should.fail()
          }
          let _ = cleanup_test_repo(repo_path)
          Nil
        }
      }
    }
  }
}

pub fn e2e_tcr_commit_on_passing_test() {
  // GIVEN: A test repo with worktree
  let test_repo = setup_test_repo("e2e_tcr_commit")
  case test_repo {
    Error(_) -> should.fail()
    Ok(repo_path) -> {
      case worktree.create_worktree("tcr-test", domain.Gleam, repo_path) {
        Error(_) -> {
          let _ = cleanup_test_repo(repo_path)
          should.fail()
        }
        Ok(wt) -> {
          // WHEN: We run a passing stage with TCR
          let stage =
            domain.Stage(
              name: "unit-test",
              gate: "All tests pass",
              tcr: True,
              retries: 0,
            )

          let execute_fn = fn() {
            stages.execute_stage("unit-test", domain.Gleam, wt.path)
          }

          case tcr.run_with_tcr(stage, wt.path, execute_fn) {
            Error(_) -> {
              let _ = worktree.remove_worktree("tcr-test", repo_path)
              let _ = cleanup_test_repo(repo_path)
              should.fail()
            }
            Ok(outcome) -> {
              // THEN: TCR outcome shows success and commit
              case outcome {
                tcr.Passed(stage_name, commits, persisted) -> {
                  stage_name
                  |> should.equal("unit-test")
                  commits
                  |> should.equal(1)
                  persisted
                  |> should.equal(True)
                }
                tcr.Failed(_, _, _, _) -> should.fail()
              }

              // Verify commit exists in jj log
              case verify_commit_exists(wt.path, "factory: unit-test passed") {
                Ok(_) -> Nil
                Error(_) -> should.fail()
              }

              // Cleanup
              let _ = worktree.remove_worktree("tcr-test", repo_path)
              let _ = cleanup_test_repo(repo_path)
              Nil
            }
          }
        }
      }
    }
  }
}

pub fn e2e_full_pipeline_with_multiple_stages_test() {
  // GIVEN: A test repo with worktree
  let test_repo = setup_test_repo("e2e_full_pipeline")
  case test_repo {
    Error(_) -> should.fail()
    Ok(repo_path) -> {
      case worktree.create_worktree("pipeline", domain.Gleam, repo_path) {
        Error(_) -> {
          let _ = cleanup_test_repo(repo_path)
          should.fail()
        }
        Ok(wt) -> {
          // WHEN: We run multiple stages in sequence
          let stages_to_run = [
            #("implement", "gleam build"),
            #("unit-test", "gleam test"),
          ]

          case run_pipeline_stages(wt.path, stages_to_run) {
            Error(_) -> {
              let _ = worktree.remove_worktree("pipeline", repo_path)
              let _ = cleanup_test_repo(repo_path)
              should.fail()
            }
            Ok(commit_count) -> {
              // THEN: All stages committed successfully
              commit_count
              |> should.equal(2)

              // Verify both commits exist
              case
                verify_commit_exists(wt.path, "factory: implement passed"),
                verify_commit_exists(wt.path, "factory: unit-test passed")
              {
                Ok(_), Ok(_) -> Nil
                _, _ -> should.fail()
              }

              // Cleanup
              let _ = worktree.remove_worktree("pipeline", repo_path)
              let _ = cleanup_test_repo(repo_path)
              Nil
            }
          }
        }
      }
    }
  }
}

// ============================================================================
// TEST HELPERS
// ============================================================================

fn setup_test_repo(test_name: String) -> Result(String, String) {
  let timestamp = get_timestamp()
  let repo_path = "/tmp/factory-e2e-" <> test_name <> "-" <> timestamp

  // Create directory
  use _ <- result.try(
    process.run_command("mkdir", ["-p", repo_path], "")
    |> result.try(process.check_success)
    |> result.map_error(fn(_) { "Failed to create test repo dir" }),
  )

  // Initialize jj repo
  use _ <- result.try(
    process.run_command("jj", ["git", "init", "--colocate"], repo_path)
    |> result.try(process.check_success)
    |> result.map_error(fn(_) { "Failed to init jj repo" }),
  )

  // Create basic gleam project structure
  use _ <- result.try(
    process.run_command("mkdir", ["-p", repo_path <> "/src"], "")
    |> result.try(process.check_success)
    |> result.map_error(fn(_) { "Failed to create src dir" }),
  )

  use _ <- result.try(
    process.run_command("mkdir", ["-p", repo_path <> "/test"], "")
    |> result.try(process.check_success)
    |> result.map_error(fn(_) { "Failed to create test dir" }),
  )

  // Write gleam.toml
  let gleam_toml = "name = \"test_project\"
target = \"erlang\"

[dependencies]
gleam_stdlib = \">= 0.17.0 and < 2.0.0\"

[dev-dependencies]
gleeunit = \">= 0.10.0 and < 2.0.0\"
"
  use _ <- result.try(write_file(repo_path <> "/gleam.toml", gleam_toml))

  // Write minimal src file
  let src_content = "pub fn main() {
  Nil
}
"
  use _ <- result.try(write_file(
    repo_path <> "/src/test_project.gleam",
    src_content,
  ))

  // Write minimal test file
  let test_content = "import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn example_test() {
  1
  |> should.equal(1)
}
"
  use _ <- result.try(write_file(
    repo_path <> "/test/test_project_test.gleam",
    test_content,
  ))

  // Download dependencies
  let _ = process.run_command("gleam", ["deps", "download"], repo_path)

  // Initial commit
  use _ <- result.try(
    process.run_command(
      "jj",
      ["describe", "-m", "Initial test project"],
      repo_path,
    )
    |> result.try(process.check_success)
    |> result.map_error(fn(_) { "Failed to create initial commit" }),
  )

  Ok(repo_path)
}

fn cleanup_test_repo(repo_path: String) {
  let _ = process.run_command("rm", ["-rf", repo_path], "")
  Nil
}

fn write_file(path: String, content: String) -> Result(Nil, String) {
  process.run_command("sh", ["-c", "cat > " <> path], "")
  |> result.try(fn(cmd_result) {
    case cmd_result {
      process.Success(_, _, _) ->
        process.run_command(
          "sh",
          ["-c", "echo '" <> escape_single_quotes(content) <> "' > " <> path],
          "",
        )
        |> result.try(process.check_success)
        |> result.map_error(fn(_) { "Failed to write file" })
      process.Failure(_, _) -> Error("Failed to write file")
    }
  })
}

fn escape_single_quotes(s: String) -> String {
  string.replace(s, "'", "'\"'\"'")
}

fn get_timestamp() -> String {
  case process.run_command("date", ["+%s"], "") {
    Ok(process.Success(ts, _, _)) -> string.trim(ts)
    _ -> "0"
  }
}

fn verify_commit_exists(
  worktree_path: String,
  commit_message: String,
) -> Result(Nil, String) {
  use cmd_result <- result.try(
    process.run_command("jj", ["-R", worktree_path, "log"], ""),
  )

  case cmd_result {
    process.Success(output, _, _) ->
      case string.contains(output, commit_message) {
        True -> Ok(Nil)
        False -> Error("Commit message not found in jj log")
      }
    process.Failure(_, _) -> Error("Failed to read jj log")
  }
}

fn run_pipeline_stages(
  worktree_path: String,
  stages: List(#(String, String)),
) -> Result(Int, String) {
  run_stages_recursive(worktree_path, stages, 0)
}

fn run_stages_recursive(
  worktree_path: String,
  stages: List(#(String, String)),
  commit_count: Int,
) -> Result(Int, String) {
  case stages {
    [] -> Ok(commit_count)
    [#(stage_name, gate_desc), ..rest] -> {
      let stage =
        domain.Stage(name: stage_name, gate: gate_desc, tcr: True, retries: 0)

      let execute_fn = fn() {
        stages.execute_stage(stage_name, domain.Gleam, worktree_path)
      }

      case tcr.run_with_tcr(stage, worktree_path, execute_fn) {
        Ok(tcr.Passed(_, commits, _)) ->
          run_stages_recursive(worktree_path, rest, commit_count + commits)
        Ok(tcr.Failed(_, _, reason, _)) -> Error("Stage failed: " <> reason)
        Error(reason) -> Error("TCR error: " <> reason)
      }
    }
  }
}
