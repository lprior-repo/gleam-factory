// Repo module - Repository detection and analysis
// Detects repo root, language, base branch

import gleam/string
import gleam/list
import gleam/result
import domain
import process
import simplifile

/// Detect repository root directory
pub fn detect_repo_root() -> Result(String, String) {
  // Try jj first (preferred)
  process.run_command("jj", ["workspace", "root"], ".")
  |> result.map(fn(result) {
    case result {
      process.Success(path, _, _) -> string.trim(path)
      _ -> ""
    }
  })
  |> result.try(fn(jj_root) {
    case jj_root {
      "" ->
        // Fall back to git
        process.run_command("git", ["rev-parse", "--show-toplevel"], ".")
        |> result.map(fn(result) {
          case result {
            process.Success(path, _, _) -> string.trim(path)
            _ -> ""
          }
        })
        |> result.try(fn(git_root) {
          case git_root {
            "" -> Error("Not in a jj or git repository")
            root -> Ok(root)
          }
        })
      root -> Ok(root)
    }
  })
}

/// Auto-detect language from repository contents
pub fn detect_language(repo_root: String) -> Result(domain.Language, String) {
  // Check for language files in order of priority
  let has_gleam_toml = file_exists(repo_root <> "/gleam.toml")
  let has_go_mod = file_exists(repo_root <> "/go.mod")
  let has_cargo_toml = file_exists(repo_root <> "/Cargo.toml")
  let has_pyproject = file_exists(repo_root <> "/pyproject.toml")

  domain.detect_language_from_files(has_gleam_toml, has_go_mod, has_cargo_toml, has_pyproject)
}

/// Check if file exists
fn file_exists(path: String) -> Bool {
  simplifile.verify_is_file(path)
  |> result.is_ok()
}

/// Get the main/master branch of the repository
pub fn get_base_branch(repo_root: String) -> Result(String, String) {
  // Try to detect which branch is default (main or master)
  // Check symbolic-ref first
  let symbolic_ref = process.run_command("git", [
    "-C",
    repo_root,
    "symbolic-ref",
    "refs/remotes/origin/HEAD",
  ], repo_root)
  |> result.try(fn(cmd_result) {
    case cmd_result {
      process.Success(output, _, _) -> {
        let trimmed = string.trim(output)
        case string.split(trimmed, "/") {
          [_, _, branch] -> {
            case string.length(branch) > 0 {
              True -> Ok(branch)
              False -> Error("empty branch name")
            }
          }
          _ -> Error("invalid symbolic-ref format")
        }
      }
      _ -> Error("symbolic-ref failed")
    }
  })

  // Fall back to checking if main or master exists
  let check_main = process.run_command("git", [
    "-C",
    repo_root,
    "show-ref",
    "--verify",
    "--quiet",
    "refs/heads/main",
  ], repo_root)
  |> result.try(fn(result) {
    case result {
      process.Success(_, _, 0) -> Ok("main")
      _ -> Error("main not found")
    }
  })

  let check_master = process.run_command("git", [
    "-C",
    repo_root,
    "show-ref",
    "--verify",
    "--quiet",
    "refs/heads/master",
  ], repo_root)
  |> result.try(fn(result) {
    case result {
      process.Success(_, _, 0) -> Ok("master")
      _ -> Error("master not found")
    }
  })

  symbolic_ref
  |> result.lazy_or(fn() { check_main })
  |> result.lazy_or(fn() { check_master })
  |> result.map_error(fn(_) { "Could not determine base branch" })
}

/// Check if repository is clean (no uncommitted changes)
pub fn is_clean(repo_root: String) -> Result(Bool, String) {
  process.run_command("git", ["-C", repo_root, "status", "--porcelain"], repo_root)
  |> result.map(fn(result) {
    case result {
      process.Success(output, _, _) -> string.length(string.trim(output)) == 0
      _ -> False
    }
  })
  |> result.map_error(fn(_) { "Could not check repository status" })
}

/// Get current branch name
pub fn current_branch(repo_root: String) -> Result(String, String) {
  process.run_command("git", [
    "-C",
    repo_root,
    "rev-parse",
    "--abbrev-ref",
    "HEAD",
  ], repo_root)
  |> result.map(fn(result) {
    case result {
      process.Success(output, _, _) -> string.trim(output)
      _ -> ""
    }
  })
  |> result.try(fn(branch) {
    case branch {
      "" -> Error("Could not determine current branch")
      b -> Ok(b)
    }
  })
}

/// Get list of modified files
pub fn modified_files(repo_root: String) -> Result(List(String), String) {
  process.run_command("git", ["-C", repo_root, "status", "--porcelain"], repo_root)
  |> result.map(fn(result) {
    case result {
      process.Success(output, _, _) ->
        output
        |> string.split("\n")
        |> list.filter_map(fn(line) {
          case string.trim(line) {
            "" -> Error(Nil)
            l -> {
              // Skip first 3 chars (status codes and space)
              case string.length(l) {
                len if len > 3 -> Ok(string.drop_start(l, 3))
                _ -> Error(Nil)
              }
            }
          }
        })
      _ -> []
    }
  })
  |> result.map_error(fn(_) { "Could not list modified files" })
}
