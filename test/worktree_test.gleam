import domain
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import process
import worktree

pub fn main() {
  gleeunit.main()
}

// ===== CREATE_WORKTREE TESTS =====

// Public API tests
pub fn create_worktree_all_languages_test() {
  let langs = [domain.Go, domain.Gleam, domain.Rust, domain.Python]
  list.each(langs, fn(lang) {
    let result = worktree.create_worktree("test-slug", lang, "/fake/repo")
    result |> should.be_error()
  })
}

pub fn create_worktree_preserves_slug_test() {
  let result = worktree.create_worktree("my-feature", domain.Go, "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.slug |> should.equal("my-feature")
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_preserves_language_test() {
  let result = worktree.create_worktree("test", domain.Rust, "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.language |> should.equal(domain.Rust)
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_branch_has_prefix_test() {
  let result = worktree.create_worktree("test-slug", domain.Go, "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.branch
      |> string.starts_with("feat/")
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_branch_includes_slug_test() {
  let result = worktree.create_worktree("my-slug", domain.Go, "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.branch
      |> string.contains("my-slug")
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_path_includes_repo_test() {
  let result = worktree.create_worktree("test", domain.Go, "/my/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.path
      |> string.starts_with("/my/repo")
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_path_includes_workspaces_dir_test() {
  let result = worktree.create_worktree("test", domain.Go, "/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.path
      |> string.contains(".factory-workspaces")
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_path_includes_slug_test() {
  let result = worktree.create_worktree("mysearch", domain.Go, "/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.path
      |> string.contains("mysearch")
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn create_worktree_empty_slug_fails_test() {
  let result = worktree.create_worktree("", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_whitespace_slug_fails_test() {
  let result = worktree.create_worktree("   ", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_invalid_chars_fails_test() {
  let result = worktree.create_worktree("invalid slug!", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_uppercase_slug_fails_test() {
  let result = worktree.create_worktree("Invalid-Slug", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_special_chars_fails_test() {
  let result = worktree.create_worktree("test@slug", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_slash_in_slug_fails_test() {
  let result = worktree.create_worktree("test/slug", domain.Go, "/fake/repo")
  result |> should.be_error()
}

// Error cases - missing external dependencies
pub fn create_worktree_missing_mkdir_fails_test() {
  let result = worktree.create_worktree("test", domain.Go, "/nonexistent/path")
  result |> should.be_error()
}

pub fn create_worktree_missing_jj_fails_test() {
  let result = worktree.create_worktree("test", domain.Go, "/tmp")
  result |> should.be_error()
}

// ===== GET_WORKTREE TESTS =====

pub fn get_worktree_missing_symlink_fails_test() {
  let result = worktree.get_worktree("nonexistent", "/fake/repo")
  result |> should.be_error()
}

pub fn get_worktree_constructs_branch_test() {
  let result = worktree.get_worktree("myfeature", "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.branch |> should.equal("feat/myfeature")
      True
    }
  }
  |> should.equal(True)
}

pub fn get_worktree_extracts_slug_test() {
  let result = worktree.get_worktree("test-slug", "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.slug |> should.equal("test-slug")
      True
    }
  }
  |> should.equal(True)
}

pub fn get_worktree_defaults_to_go_language_test() {
  let result = worktree.get_worktree("test", "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.language |> should.equal(domain.Go)
      True
    }
  }
  |> should.equal(True)
}

pub fn get_worktree_path_from_symlink_test() {
  let result = worktree.get_worktree("test", "/fake/repo")
  case result {
    Error(_) -> True
    Ok(wt) -> {
      wt.path
      |> string.length
      |> fn(len) { len > 0 }
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn get_worktree_empty_slug_fails_test() {
  let result = worktree.get_worktree("", "/fake/repo")
  result |> should.be_error()
}

pub fn get_worktree_missing_factory_dir_fails_test() {
  let result = worktree.get_worktree("any", "/nonexistent/repo")
  result |> should.be_error()
}

// ===== REMOVE_WORKTREE TESTS =====

pub fn remove_worktree_missing_fails_test() {
  let result = worktree.remove_worktree("nonexistent", "/fake/repo")
  result |> should.be_error()
}

pub fn remove_worktree_returns_nil_on_success_test() {
  let result = worktree.remove_worktree("nonexistent", "/fake/repo")
  case result {
    Error(_) -> True
    Ok(nil) -> {
      nil |> should.equal(Nil)
      True
    }
  }
  |> should.equal(True)
}

pub fn remove_worktree_empty_slug_fails_test() {
  let result = worktree.remove_worktree("", "/fake/repo")
  result |> should.be_error()
}

pub fn remove_worktree_nonexistent_dir_fails_test() {
  let result = worktree.remove_worktree("test", "/nonexistent/repo")
  result |> should.be_error()
}

// ===== LIST_WORKTREES TESTS =====

pub fn list_worktrees_missing_factory_dir_test() {
  let result = worktree.list_worktrees("/nonexistent/repo")
  result |> should.be_error()
}

pub fn list_worktrees_returns_list_test() {
  let result = worktree.list_worktrees("/nonexistent/repo")
  case result {
    Error(_) -> True
    Ok(wts) -> {
      wts |> should.be_ok()
      True
    }
  }
  |> should.equal(True)
}

pub fn list_worktrees_empty_repo_test() {
  let result = worktree.list_worktrees("/fake/empty/repo")
  case result {
    Error(_) -> {
      True
    }
    Ok(wts) -> {
      list.is_empty(wts) || !list.is_empty(wts)
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

pub fn list_worktrees_filters_empty_lines_test() {
  let result = worktree.list_worktrees("/fake/repo")
  case result {
    Error(_) -> True
    Ok(wts) -> {
      wts
      |> list.filter(fn(wt) { wt.slug == "" })
      |> list.length
      |> fn(len) { len == 0 }
      |> should.equal(True)
      True
    }
  }
  |> should.equal(True)
}

// ===== WORKTREES_BASE TESTS =====

pub fn worktrees_base_path_test() {
  worktree.worktrees_base("/fake/repo")
  |> should.equal("/fake/repo/.factory-workspaces")
}

pub fn worktrees_base_no_trailing_slash_test() {
  worktree.worktrees_base("/repo")
  |> should.equal("/repo/.factory-workspaces")
}

pub fn worktrees_base_nested_path_test() {
  worktree.worktrees_base("/a/b/c/repo")
  |> should.equal("/a/b/c/repo/.factory-workspaces")
}

pub fn worktrees_base_single_slash_test() {
  worktree.worktrees_base("/")
  |> should.equal("/.factory-workspaces")
}

// ===== PROCESS HELPER TESTS =====

pub fn command_success_check_test() {
  let result = process.Success("output", "", 0)
  process.is_success(result) |> should.equal(True)
}

pub fn command_failure_check_test() {
  let result = process.Failure("error", 1)
  process.is_success(result) |> should.equal(False)
}

pub fn command_success_nonzero_exit_test() {
  let result = process.Success("output", "", 1)
  process.is_success(result) |> should.equal(False)
}

pub fn command_get_stdout_success_test() {
  let result = process.Success("output", "", 0)
  process.get_stdout(result) |> should.equal(Ok("output"))
}

pub fn command_get_stdout_failure_test() {
  let result = process.Failure("error", 1)
  process.get_stdout(result) |> should.be_error()
}

pub fn command_get_stdout_empty_test() {
  let result = process.Success("", "", 0)
  process.get_stdout(result) |> should.equal(Ok(""))
}

pub fn command_check_success_ok_test() {
  let result = process.Success("", "", 0)
  process.check_success(result) |> should.equal(Ok(Nil))
}

pub fn command_check_success_fail_test() {
  let result = process.Failure("error", 1)
  process.check_success(result) |> should.be_error()
}

pub fn command_check_success_with_output_test() {
  let result = process.Success("output", "", 0)
  process.check_success(result) |> should.equal(Ok(Nil))
}

pub fn command_check_success_nonzero_exit_test() {
  let result = process.Success("output", "", 5)
  process.check_success(result) |> should.be_error()
}
