// Domain model for Factory pipeline
// Pure data types that make illegal states unrepresentable

import gleam/list
import gleam/result
import gleam/string

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
pub opaque type Slug {
  Slug(String)
}

const max_slug_length: Int = 50

/// Validate slug: non-empty, 1-50 chars, only [a-zA-Z0-9_-]
pub fn validate_slug(slug: String) -> Result(Slug, String) {
  let len = string.length(slug)
  case len {
    0 -> Error("slug cannot be empty")
    n if n > max_slug_length ->
      Error(
        "slug must be 1-" <> string.inspect(max_slug_length) <> " characters",
      )
    _ ->
      case is_valid_slug_chars(slug) {
        True -> Ok(Slug(slug))
        False -> Error("slug contains invalid characters (use a-z, 0-9, -, _)")
      }
  }
}

/// Check if string contains only valid slug characters
/// Valid: lowercase a-z, digits 0-9, hyphen, underscore
fn is_valid_slug_char(char: String) -> Bool {
  case string.to_utf_codepoints(char) {
    [cp] -> {
      let code = string.utf_codepoint_to_int(cp)
      { code >= 97 && code <= 122 }
      || { code >= 48 && code <= 57 }
      || code == 45
      || code == 95
    }
    _ -> False
  }
}

fn is_valid_slug_chars(s: String) -> Bool {
  case string.length(s) {
    0 -> False
    _ ->
      s
      |> string.to_graphemes
      |> list.all(is_valid_slug_char)
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

/// Priority levels for tasks
pub type Priority {
  P1
  P2
  P3
}

/// Task represents a single unit of work
pub type Task {
  Task(
    slug: Slug,
    language: Language,
    status: TaskStatus,
    priority: Priority,
    worktree_path: String,
    branch: String,
  )
}

/// Pipeline configuration - TCR stages (all have commit/revert)
pub fn standard_pipeline() -> List(Stage) {
  [
    Stage("implement", "Code compiles", 5, True),
    Stage("unit-test", "All tests pass", 3, True),
    Stage("coverage", "80% coverage", 5, True),
    Stage("lint", "Code formatted", 3, True),
    Stage("static", "Static analysis passes", 3, True),
    Stage("integration", "Integration tests pass", 3, True),
    Stage("security", "No vulnerabilities", 2, True),
    Stage("review", "Code review passes", 3, True),
    Stage("accept", "Ready for merge", 1, True),
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
      Error(
        "one or both stages not found in pipeline: "
        <> start_name
        <> " to "
        <> end_name,
      )
  }
}

// BEAD 1: Check if stage is tcr-enabled
pub fn is_tcr_stage(stage: Stage) -> Bool {
  case stage {
    Stage(_, _, _, tcr) -> tcr
  }
}

// BEAD 2: Get stage retry count
pub fn get_stage_retries(stage: Stage) -> Int {
  case stage {
    Stage(_, _, retries, _) -> retries
  }
}

// BEAD 3: Check if task is in a transient state
pub fn is_transient_status(status: TaskStatus) -> Bool {
  case status {
    InProgress(_) -> True
    _ -> False
  }
}

// BEAD 4: Check if task has failed
pub fn is_failed(status: TaskStatus) -> Bool {
  case status {
    FailedPipeline(_, _) -> True
    _ -> False
  }
}

// BEAD 5: Get failure reason from status
pub fn get_failure_reason(status: TaskStatus) -> Result(String, String) {
  case status {
    FailedPipeline(_, reason) -> Ok(reason)
    _ -> Error("task did not fail")
  }
}

// BEAD 6: Check if task completed successfully
pub fn is_completed(status: TaskStatus) -> Bool {
  case status {
    PassedPipeline | Integrated -> True
    _ -> False
  }
}

// BEAD 7: Get current stage from status
pub fn get_current_stage(status: TaskStatus) -> Result(String, String) {
  case status {
    InProgress(stage) -> Ok(stage)
    _ -> Error("task not in progress")
  }
}

// BEAD 8: Count tcr stages in pipeline
pub fn count_tcr_stages(pipeline: List(Stage)) -> Int {
  pipeline
  |> list.filter(is_tcr_stage)
  |> list.length
}

// BEAD 9: Get non-tcr stages
pub fn non_tcr_stages(pipeline: List(Stage)) -> List(Stage) {
  list.filter(pipeline, fn(s) { !is_tcr_stage(s) })
}

// BEAD 10: Find first tcr stage
pub fn first_tcr_stage(pipeline: List(Stage)) -> Result(Stage, String) {
  list.find(pipeline, is_tcr_stage)
  |> result.map_error(fn(_) { "no tcr stages found" })
}

// BEAD 11: Check if language is compiled
pub fn is_compiled_language(lang: Language) -> Bool {
  case lang {
    Go | Rust | Gleam -> True
    Python -> False
  }
}

// BEAD 12: Check if language is dynamically typed
pub fn is_dynamic_language(lang: Language) -> Bool {
  case lang {
    Python -> True
    _ -> False
  }
}

// BEAD 13: Get language display name
pub fn language_display_name(lang: Language) -> String {
  case lang {
    Go -> "Go"
    Gleam -> "Gleam"
    Rust -> "Rust"
    Python -> "Python"
  }
}

// BEAD 14: Validate slug is lowercase
pub fn is_slug_lowercase(slug: Slug) -> Bool {
  let Slug(s) = slug
  s == string.lowercase(s)
}

// BEAD 15: Get max stage retries in pipeline
pub fn max_pipeline_retries(pipeline: List(Stage)) -> Int {
  pipeline
  |> list.map(get_stage_retries)
  |> list.fold(0, fn(max, retries) {
    case retries > max {
      True -> retries
      False -> max
    }
  })
}

// BEAD 16: Check if slug contains only hyphens and underscores
pub fn has_separators(slug: Slug) -> Bool {
  let Slug(s) = slug
  string.contains(s, "-") || string.contains(s, "_")
}

// BEAD 17: Count stages with specific gate name
pub fn count_stages_by_gate(pipeline: List(Stage), gate: String) -> Int {
  pipeline
  |> list.filter(fn(s) {
    case s {
      Stage(_, g, _, _) -> g == gate
    }
  })
  |> list.length
}

// BEAD 18: Get all gate names from pipeline
pub fn gate_names(pipeline: List(Stage)) -> List(String) {
  pipeline
  |> list.map(fn(s) {
    case s {
      Stage(_, gate, _, _) -> gate
    }
  })
}

// BEAD 19: Check if task is ready for integration
pub fn is_ready_for_integration(status: TaskStatus, language: Language) -> Bool {
  case status {
    PassedPipeline -> is_compiled_language(language)
    _ -> False
  }
}

/// Get slug as string
pub fn slug_to_string(slug: Slug) -> String {
  let Slug(s) = slug
  s
}

/// Convert priority to string
pub fn priority_to_string(priority: Priority) -> String {
  case priority {
    P1 -> "P1"
    P2 -> "P2"
    P3 -> "P3"
  }
}

/// Parse priority from string
pub fn parse_priority(s: String) -> Result(Priority, String) {
  case s {
    "P1" -> Ok(P1)
    "P2" -> Ok(P2)
    "P3" -> Ok(P3)
    _ -> Error("Invalid priority: " <> s)
  }
}
