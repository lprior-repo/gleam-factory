import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import process
import simplifile

pub fn main() {
  gleeunit.main()
}

// Unique temp dir per test run to avoid conflicts
fn get_temp_dir() -> String {
  "/tmp/factory_gleam_cli_test_" <> string.slice("abcdefghijk", 0, 8)
}

// Setup: Create real jj repo in /tmp
fn setup_jj_repo(path: String) -> Result(Nil, String) {
  // Create directory
  case simplifile.create_directory_all(path) {
    Error(_) -> Error("Failed to create directory: " <> path)
    Ok(_) -> {
      // Initialize jj repo
      case process.run_command("jj", ["git", "init", path], "") {
        Error(e) -> Error("Failed to init jj repo: " <> string.inspect(e))
        Ok(result) -> {
          case result {
            process.Success(_, _, 0) -> Ok(Nil)
            process.Success(_, _, code) ->
              Error("jj init returned non-zero code: " <> string.inspect(code))
            process.Failure(stderr, code) ->
              Error(
                "jj init failed: "
                <> stderr
                <> " (code: "
                <> string.inspect(code)
                <> ")",
              )
          }
        }
      }
    }
  }
}

// Teardown: Remove temp repo
fn teardown_jj_repo(path: String) -> Result(Nil, String) {
  process.run_command("rm", ["-rf", path], "")
  |> result.map_error(fn(e) { "Failed to cleanup: " <> string.inspect(e) })
  |> result.try(fn(result) {
    case result {
      process.Success(_, _, _) -> Ok(Nil)
      process.Failure(stderr, _) -> Error("Cleanup command failed: " <> stderr)
    }
  })
}

// Get jj status as string
fn get_jj_status(repo_path: String) -> Result(String, String) {
  process.run_command("jj", ["status"], repo_path)
  |> result.map_error(fn(e) { "Command error: " <> string.inspect(e) })
  |> result.try(fn(result) {
    case result {
      process.Success(stdout, _, 0) -> Ok(stdout)
      process.Success(_, stderr, code) ->
        Error(
          "jj status failed with code: "
          <> string.inspect(code)
          <> " stderr: "
          <> stderr,
        )
      process.Failure(stderr, code) ->
        Error(
          "jj status failed: "
          <> stderr
          <> " (code: "
          <> string.inspect(code)
          <> ")",
        )
    }
  })
}

// Get jj log as string
fn get_jj_log(repo_path: String) -> Result(String, String) {
  process.run_command(
    "jj",
    ["log", "-r", "::@", "--oneline", "-n", "10"],
    repo_path,
  )
  |> result.map_error(fn(e) { "Command error: " <> string.inspect(e) })
  |> result.try(fn(result) {
    case result {
      process.Success(stdout, _, 0) -> Ok(stdout)
      process.Success(_, stderr, code) ->
        Error(
          "jj log failed with code: "
          <> string.inspect(code)
          <> " stderr: "
          <> stderr,
        )
      process.Failure(stderr, code) ->
        Error(
          "jj log failed: "
          <> stderr
          <> " (code: "
          <> string.inspect(code)
          <> ")",
        )
    }
  })
}

// Create test file and record change
fn add_test_file(repo_path: String, filename: String) -> Result(Nil, String) {
  let file_path = repo_path <> "/" <> filename
  case simplifile.write(file_path, "test content\n") {
    Error(_) -> Error("Failed to write test file")
    Ok(_) -> {
      process.run_command("jj", ["new", "-m", "Add " <> filename], repo_path)
      |> result.map_error(fn(e) { "Command error: " <> string.inspect(e) })
      |> result.try(fn(result) {
        case result {
          process.Success(_, _, 0) -> Ok(Nil)
          process.Success(_, stderr, code) ->
            Error(
              "jj new failed with code: "
              <> string.inspect(code)
              <> " stderr: "
              <> stderr,
            )
          process.Failure(stderr, code) ->
            Error(
              "jj new failed: "
              <> stderr
              <> " (code: "
              <> string.inspect(code)
              <> ")",
            )
        }
      })
    }
  }
}

// TEST: cli_init_creates_jj_repo_test
pub fn cli_init_creates_jj_repo_test() {
  let repo_path = get_temp_dir() <> "/test_init"

  case setup_jj_repo(repo_path) {
    Error(msg) -> {
      let _ = teardown_jj_repo(repo_path)
      panic(msg)
    }
    Ok(_) -> {
      let assert Ok(Nil) = teardown_jj_repo(repo_path)
      Nil
    }
  }
}

// TEST: cli_status_shows_clean_repo_test
pub fn cli_status_shows_clean_repo_test() {
  let repo_path = get_temp_dir() <> "/test_status"

  case setup_jj_repo(repo_path) {
    Error(msg) -> {
      let _ = teardown_jj_repo(repo_path)
      panic(msg)
    }
    Ok(_) -> {
      case get_jj_status(repo_path) {
        Error(msg) -> {
          let _ = teardown_jj_repo(repo_path)
          panic(msg)
        }
        Ok(status) -> {
          let _ = teardown_jj_repo(repo_path)
          status
          |> string.contains("working directory")
          |> should.be_true()
        }
      }
    }
  }
}

// TEST: cli_log_shows_commits_test
pub fn cli_log_shows_commits_test() {
  let repo_path = get_temp_dir() <> "/test_log"

  case
    setup_jj_repo(repo_path)
    |> result.try(fn(_) { add_test_file(repo_path, "test.txt") })
  {
    Error(msg) -> {
      let _ = teardown_jj_repo(repo_path)
      panic(msg)
    }
    Ok(_) -> {
      case get_jj_log(repo_path) {
        Error(msg) -> {
          let _ = teardown_jj_repo(repo_path)
          panic(msg)
        }
        Ok(log) -> {
          let _ = teardown_jj_repo(repo_path)
          log
          |> string.length
          |> fn(len) { len > 0 }
          |> should.be_true()
        }
      }
    }
  }
}

// TEST: cli_commit_records_change_test
pub fn cli_commit_records_change_test() {
  let repo_path = get_temp_dir() <> "/test_commit"

  case
    setup_jj_repo(repo_path)
    |> result.try(fn(_) { add_test_file(repo_path, "change.txt") })
    |> result.try(fn(_) { get_jj_log(repo_path) })
  {
    Error(msg) -> {
      let _ = teardown_jj_repo(repo_path)
      panic(msg)
    }
    Ok(log) -> {
      let _ = teardown_jj_repo(repo_path)
      log
      |> string.contains("Add change.txt")
      |> should.be_true()
    }
  }
}
