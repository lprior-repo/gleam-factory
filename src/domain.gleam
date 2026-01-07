// Domain model for Factory pipeline
// Pure data types that make illegal states unrepresentable

import gleam/list
import gleam/string
import gleam/result

/// Language type - only valid languages can be represented
pub type Language {
  Go
  Gleam
  Rust
  Python
}

/// Language from string, fails if unsupported
pub fn parse_language(lang: String) -> Result(Language, String) {
  case lang {
    "go" -> Ok(Go)
    "gleam" -> Ok(Gleam)
    "rust" -> Ok(Rust)
    "python" -> Ok(Python)
    other ->
      Error(
        "unsupported language: "
        <> other
        <> " (supported: go, gleam, rust, python)",
      )
  }
}

/// Detect language from repository contents
pub fn detect_language_from_files(
  has_gleam_toml: Bool,
  has_go_mod: Bool,
  has_cargo_toml: Bool,
  has_pyproject: Bool,
) -> Result(Language, String) {
  case has_gleam_toml, has_go_mod, has_cargo_toml, has_pyproject {
    True, _, _, _ -> Ok(Gleam)
    _, True, _, _ -> Ok(Go)
    _, _, True, _ -> Ok(Rust)
    _, _, _, True -> Ok(Python)
    _, _, _, _ ->
      Error(
        "could not detect language from repository files (looked for gleam.toml, go.mod, Cargo.toml, pyproject.toml)",
      )
  }
}

/// Slug type - validated task identifier
pub type Slug =
  String

/// Validate slug: non-empty, 1-50 chars, only [a-zA-Z0-9_-]
pub fn validate_slug(slug: String) -> Result(Slug, String) {
  let len = string.length(slug)
  case len {
    0 -> Error("slug cannot be empty")
    n if n > 50 -> Error("slug must be 1-50 characters")
    _ ->
      case is_valid_slug_chars(slug) {
        True -> Ok(slug)
        False ->
          Error(
            "slug contains invalid characters (use a-z, 0-9, -, _)",
          )
      }
  }
}

/// Check if string contains only valid slug characters
/// Valid: lowercase a-z, digits 0-9, hyphen, underscore
fn is_valid_slug_chars(s: String) -> Bool {
  case string.length(s) {
    0 -> False
    _ ->
      s
      |> string.to_graphemes
      |> list.all(fn(char) {
        case char {
          "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j"
          | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t"
          | "u" | "v" | "w" | "x" | "y" | "z"
          | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
          | "-" | "_" -> True
          _ -> False
        }
      })
  }
}

/// Stage type - defines a single gate in the pipeline
pub type Stage {
  Stage(name: String, gate: String, retries: Int, tcr: Bool)
}

/// Task status - overall pipeline status
pub type TaskStatus {
  Created
  InProgress(stage: String)
  PassedPipeline
  FailedPipeline(stage: String, reason: String)
  Integrated
}

/// Task represents a single unit of work
pub type Task {
  Task(
    slug: Slug,
    language: Language,
    status: TaskStatus,
    worktree_path: String,
    branch: String,
  )
}

/// Pipeline configuration - 10 stages for all languages
pub fn standard_pipeline() -> List(Stage) {
  [
    Stage("tdd-setup", "Tests exist and fail", 3, False),
    Stage("implement", "Code compiles", 5, True),
    Stage("unit-test", "All tests pass", 3, True),
    Stage("coverage", "80% coverage", 5, True),
    Stage("lint", "Code formatted", 3, True),
    Stage("static", "Static analysis passes", 3, True),
    Stage("integration", "Integration tests pass", 3, True),
    Stage("security", "No vulnerabilities", 2, True),
    Stage("review", "Code review passes", 3, True),
    Stage("accept", "Ready for merge", 1, False),
  ]
}

/// Get stage by name
pub fn get_stage(name: String) -> Result(Stage, String) {
  standard_pipeline()
  |> list.find(fn(s) { s.name == name })
  |> result.map_error(fn(_) { "unknown stage: " <> name })
}

/// Find position of stage in pipeline (0-indexed)
pub fn find_stage_index(pipeline: List(Stage), name: String) -> Result(Int, Nil) {
  find_stage_index_helper(pipeline, name, 0)
}

fn find_stage_index_helper(
  pipeline: List(Stage),
  name: String,
  index: Int,
) -> Result(Int, Nil) {
  case pipeline {
    [] -> Error(Nil)
    [Stage(n, _, _, _), ..rest] -> {
      case n == name {
        True -> Ok(index)
        False -> find_stage_index_helper(rest, name, index + 1)
      }
    }
  }
}

/// All stages from start_name to end_name (inclusive)
pub fn filter_stages(
  start_name: String,
  end_name: String,
) -> Result(List(Stage), String) {
  let pipeline = standard_pipeline()

  let start_idx = find_stage_index(pipeline, start_name)
  let end_idx = find_stage_index(pipeline, end_name)

  case start_idx, end_idx {
    Ok(si), Ok(ei) ->
      case si <= ei {
        True -> {
          let count = ei - si + 1
          Ok(list.drop(pipeline, si) |> list.take(count))
        }
        False ->
          Error(
            "start stage '"
            <> start_name
            <> "' must come before end stage '"
            <> end_name
            <> "'",
          )
      }
    _, _ ->
      Error("one or both stages not found in pipeline: " <> start_name <> " to " <> end_name)
  }
}
