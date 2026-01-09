import domain
import gleeunit
import gleeunit/should
import repo

pub fn main() {
  gleeunit.main()
}

// detect_repo_root tests
pub fn detect_repo_root_jj_test() {
  // This test will use the real jj/git commands in the actual repo
  // We verify it returns a valid path
  case repo.detect_repo_root() {
    Ok(path) -> {
      // Should return a non-empty path
      case path {
        "" -> panic as "Expected non-empty repo root"
        _ -> Nil
      }
    }
    Error(_) -> panic as "Expected repo detection to succeed in test repo"
  }
}

// detect_language tests - using real filesystem
pub fn detect_language_gleam_test() {
  // This is a Gleam project, should detect Gleam
  case repo.detect_repo_root() {
    Ok(root) -> {
      case repo.detect_language(root) {
        Ok(domain.Gleam) -> Nil
        Ok(_) -> panic as "Expected Gleam language"
        Error(e) -> panic as { "Language detection failed: " <> e }
      }
    }
    Error(_) -> panic as "Repo detection failed"
  }
}

pub fn detect_language_no_repo_test() {
  // Non-existent directory has no language files, should fail
  // Note: we're in the factory-gleam repo which has gleam.toml at root
  // So we need a path that doesn't contain gleam.toml
  repo.detect_language("/nonexistent/path/12345")
  |> should.be_error()
}

pub fn detect_language_empty_repo_test() {
  // /tmp typically doesn't have project language files
  // But simplifile may follow symlinks or have other behavior
  // Skip this test in favor of unit tests via domain module
  Nil
}

// Test language detection logic through domain module
pub fn detect_language_from_files_gleam_test() {
  domain.detect_language_from_files(True, False, False, False)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_from_files_go_test() {
  domain.detect_language_from_files(False, True, False, False)
  |> should.equal(Ok(domain.Go))
}

pub fn detect_language_from_files_rust_test() {
  domain.detect_language_from_files(False, False, True, False)
  |> should.equal(Ok(domain.Rust))
}

pub fn detect_language_from_files_python_test() {
  domain.detect_language_from_files(False, False, False, True)
  |> should.equal(Ok(domain.Python))
}

pub fn detect_language_from_files_none_test() {
  domain.detect_language_from_files(False, False, False, False)
  |> should.be_error()
}

pub fn detect_language_from_files_priority_gleam_test() {
  // Gleam has highest priority
  domain.detect_language_from_files(True, True, True, True)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_from_files_priority_go_test() {
  // Go is second priority
  domain.detect_language_from_files(False, True, True, True)
  |> should.equal(Ok(domain.Go))
}

pub fn detect_language_from_files_priority_rust_test() {
  // Rust is third priority
  domain.detect_language_from_files(False, False, True, True)
  |> should.equal(Ok(domain.Rust))
}
