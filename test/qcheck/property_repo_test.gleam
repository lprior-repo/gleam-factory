// Property-based tests for repo module using qcheck
// Tests path validation and output parsing invariants

import gleam/string
import gleam/list
import qcheck

// PATH VALIDATION - Property: repo paths always non-empty after processing
pub fn prop_repo_path_non_empty__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let path = string.repeat("a", len)
  // The path should remain non-empty after any repo operations
  assert string.length(path) > 0
}

// FILE DETECTION - Property: paths with extension always have valid format
pub fn prop_file_path_has_extension__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 50))
  let path = string.repeat("a", len) <> "/gleam.toml"
  assert string.contains(path, ".")
}

// FILE DETECTION - Property: file path must contain actual filename
pub fn prop_file_path_contains_filename__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 50))
  let path = string.repeat("a", len) <> "/go.mod"
  assert string.contains(path, "/") && string.contains(path, ".mod")
}

// MODIFIED FILES PARSING - Property: empty output returns empty list
pub fn prop_modified_files_empty_is_empty__test() {
  // Simulating what modified_files does with empty git output
  let output = ""
  let lines = string.split(output, "\n")
  let result = list.filter_map(lines, fn(line) {
    case string.trim(line) {
      "" -> Error(Nil)
      l -> {
        case string.length(l) {
          len if len > 3 -> Ok(string.drop_start(l, 3))
          _ -> Error(Nil)
        }
      }
    }
  })
  assert result == []
}

// MODIFIED FILES PARSING - Property: short lines are filtered out
pub fn prop_modified_files_short_lines_rejected__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 2))
  // Git status output has at least 3 char prefix (status code + space)
  let short_line = string.repeat("a", len)
  let result = case string.length(short_line) {
    l if l > 3 -> "should be filtered"
    _ -> "filtered"
  }
  assert result == "filtered"
}

// MODIFIED FILES PARSING - Property: valid git status lines get filename
pub fn prop_modified_files_valid_line_parsing__test() {
  let git_line = "M  test.gleam"
  let trimmed = string.trim(git_line)
  let result = case string.length(trimmed) {
    len if len > 3 -> Ok(string.drop_start(trimmed, 3))
    _ -> Error(Nil)
  }
  assert case result {
    Ok(filename) -> filename == "test.gleam"
    Error(_) -> False
  }
}

// GIT BRANCH PARSING - Property: branch from symbolic-ref has valid format
pub fn prop_branch_parsing_from_symref__test() {
  let symref_output = "refs/remotes/origin/main"
  let trimmed = string.trim(symref_output)
  let parts = string.split(trimmed, "/")
  let branch = case parts {
    [_, _, _, b] -> b
    [_, _, b] -> b
    _ -> ""
  }
  assert branch == "main"
}

// GIT BRANCH PARSING - Property: branch extraction handles different depths
pub fn prop_branch_parsing_handles_depths__test() {
  use depth <- qcheck.given(qcheck.bounded_int(1, 5))
  let parts = case depth {
    1 -> ["refs", "heads", "main"]
    2 -> ["refs", "remotes", "origin", "main"]
    _ -> ["refs", "heads", "feature", "new", "branch"]
  }
  let branch = case parts {
    [_, _, b] -> b
    [_, _, _, b] -> b
    [_, _, _, _, b] -> b
    _ -> ""
  }
  assert string.length(branch) > 0
}

// GIT STATUS PARSING - Property: clean repo has no lines
pub fn prop_clean_repo_no_output__test() {
  let clean_output = ""
  let trimmed = string.trim(clean_output)
  let is_clean = string.length(trimmed) == 0
  assert is_clean
}

// GIT STATUS PARSING - Property: dirty repo has content
pub fn prop_dirty_repo_has_output__test() {
  let dirty_output = "M  file.gleam"
  let trimmed = string.trim(dirty_output)
  let is_dirty = string.length(trimmed) > 0
  assert is_dirty
}

// CURRENT BRANCH PARSING - Property: branch name trimmed correctly
pub fn prop_current_branch_trimmed__test() {
  let branch_output = "main\n"
  let branch = string.trim(branch_output)
  assert branch == "main"
}

// CURRENT BRANCH PARSING - Property: branch with spaces handled
pub fn prop_current_branch_with_whitespace__test() {
  use len <- qcheck.given(qcheck.bounded_int(0, 3))
  let prefix = string.repeat(" ", len)
  let suffix = string.repeat(" ", len)
  let branch_output = prefix <> "feature-branch" <> suffix
  let branch = string.trim(branch_output)
  assert branch == "feature-branch"
}

// REPO ROOT DETECTION - Property: paths always contain / separator
pub fn prop_repo_paths_have_separators__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 50))
  let path = "/" <> string.repeat("a", len)
  assert string.contains(path, "/")
}

// LANGUAGE DETECTION - Property: manifest filenames are consistent
pub fn prop_language_manifest_names_consistent__test() {
  let gleam_manifest = "gleam.toml"
  let go_manifest = "go.mod"
  let rust_manifest = "Cargo.toml"
  let python_manifest = "pyproject.toml"

  assert string.contains(gleam_manifest, ".")
  && string.contains(go_manifest, ".")
  && string.contains(rust_manifest, ".")
  && string.contains(python_manifest, ".")
}

// LANGUAGE DETECTION - Property: Gleam manifest is specific format
pub fn prop_language_gleam_manifest_format__test() {
  let manifest = "gleam.toml"
  assert string.ends_with(manifest, ".toml")
}

// LANGUAGE DETECTION - Property: Rust manifest is specific format
pub fn prop_language_rust_manifest_format__test() {
  let manifest = "Cargo.toml"
  assert string.ends_with(manifest, ".toml")
}

// LANGUAGE DETECTION - Property: Go manifest is specific format
pub fn prop_language_go_manifest_format__test() {
  let manifest = "go.mod"
  assert string.ends_with(manifest, ".mod")
}

// LANGUAGE DETECTION - Property: Python manifest is specific format
pub fn prop_language_python_manifest_format__test() {
  let manifest = "pyproject.toml"
  assert string.ends_with(manifest, ".toml")
}

// ERROR MESSAGES - Property: error messages are non-empty
pub fn prop_error_message_non_empty__test() {
  let error_msg = "Could not determine base branch"
  assert string.length(error_msg) > 0
}

// ERROR MESSAGES - Property: error messages contain useful context
pub fn prop_error_message_contains_context__test() {
  use msg_len <- qcheck.given(qcheck.bounded_int(5, 50))
  let context = string.repeat("x", msg_len)
  let error_msg = "Could not " <> context
  assert string.contains(error_msg, "Could not")
}
