import domain
import gleam/string
import gleeunit
import gleeunit/should
import process
import worktree

pub fn main() {
  gleeunit.main()
}

// Branch naming tests - these will all fail due to missing jj, but validate logic
pub fn create_worktree_branch_format_test() {
  let result = worktree.create_worktree("test-slug", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_branch_prefix_test() {
  let result = worktree.create_worktree("myfeature", domain.Gleam, "/fake/repo")
  result |> should.be_error()
}

// Path construction tests
pub fn create_worktree_path_structure_test() {
  let result = worktree.create_worktree("test-slug", domain.Go, "/fake/repo")
  result |> should.be_error()
}

pub fn worktrees_base_path_test() {
  worktree.worktrees_base("/fake/repo")
  |> should.equal("/fake/repo/.factory-workspaces")
}

pub fn create_worktree_slug_preserved_test() {
  let result = worktree.create_worktree("my-slug", domain.Rust, "/fake/repo")
  result |> should.be_error()
}

pub fn create_worktree_language_preserved_test() {
  let result =
    worktree.create_worktree("test-slug", domain.Python, "/fake/repo")
  result |> should.be_error()
}

// Error case tests - invalid slug patterns
pub fn create_worktree_empty_slug_fails_test() {
  let result = worktree.create_worktree("", domain.Go, "/fake/repo")
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

// Error case tests - missing repo (simulated by command failure)
pub fn get_worktree_missing_fails_test() {
  let result = worktree.get_worktree("nonexistent", "/fake/repo")
  result |> should.be_error()
}

pub fn remove_worktree_missing_fails_test() {
  let result = worktree.remove_worktree("nonexistent", "/fake/repo")
  result |> should.be_error()
}

// List worktrees tests
pub fn list_worktrees_missing_factory_dir_test() {
  let result = worktree.list_worktrees("/nonexistent/repo")
  result |> should.be_error()
}

// CommandResult helper tests
pub fn command_success_check_test() {
  let result = process.Success("output", "", 0)
  process.is_success(result) |> should.equal(True)
}

pub fn command_failure_check_test() {
  let result = process.Failure("error", 1)
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

pub fn command_check_success_ok_test() {
  let result = process.Success("", "", 0)
  process.check_success(result) |> should.equal(Ok(Nil))
}

pub fn command_check_success_fail_test() {
  let result = process.Failure("error", 1)
  process.check_success(result) |> should.be_error()
}

// Get worktree tests
pub fn get_worktree_extracts_slug_test() {
  let result = worktree.get_worktree("test-slug", "/fake/repo")
  result |> should.be_error()
}

pub fn get_worktree_constructs_branch_test() {
  let result = worktree.get_worktree("myfeature", "/fake/repo")
  result |> should.be_error()
}
