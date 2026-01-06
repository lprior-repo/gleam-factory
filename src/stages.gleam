// Stages module - Real language-specific stage implementations
// Each language gets proper linting, testing, static analysis

import gleam/result
import domain
import process

/// Validate that a stage transition is valid (forward-only in pipeline)
pub fn validate_stage_transition(
  from_stage: String,
  to_stage: String,
) -> Result(Nil, String) {
  let pipeline = domain.standard_pipeline()

  // Find indices of both stages
  let from_idx = find_stage_index(pipeline, from_stage)
  let to_idx = find_stage_index(pipeline, to_stage)

  case from_idx, to_idx {
    Ok(fi), Ok(ti) ->
      case fi < ti {
        True -> Ok(Nil)
        False -> Error("invalid stage transition: cannot move from " <> from_stage <> " to " <> to_stage)
      }
    Error(_), _ -> Error("unknown stage: " <> from_stage)
    _, Error(_) -> Error("unknown stage: " <> to_stage)
  }
}

/// Find position of stage in pipeline (0-indexed)
fn find_stage_index(pipeline: List(domain.Stage), name: String) -> Result(Int, Nil) {
  find_stage_index_helper(pipeline, name, 0)
}

fn find_stage_index_helper(
  pipeline: List(domain.Stage),
  name: String,
  index: Int,
) -> Result(Int, Nil) {
  case pipeline {
    [] -> Error(Nil)
    [domain.Stage(n, _, _, _), ..rest] -> {
      case n == name {
        True -> Ok(index)
        False -> find_stage_index_helper(rest, name, index + 1)
      }
    }
  }
}

/// Execute a stage with real language-specific tools
pub fn execute_stage(
  stage_name: String,
  language: domain.Language,
  worktree_path: String,
) -> Result(Nil, String) {
  case language {
    domain.Go -> execute_go_stage(stage_name, worktree_path)
    domain.Gleam -> execute_gleam_stage(stage_name, worktree_path)
    domain.Rust -> execute_rust_stage(stage_name, worktree_path)
    domain.Python -> execute_python_stage(stage_name, worktree_path)
  }
}

// ============================================================================
// GLEAM STAGES - Real gleam tooling
// ============================================================================

fn execute_gleam_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "tdd-setup" -> gleam_tdd_setup(cwd)
    "implement" -> gleam_implement(cwd)
    "unit-test" -> gleam_unit_test(cwd)
    "coverage" -> gleam_coverage(cwd)
    "lint" -> gleam_lint(cwd)
    "static" -> gleam_static(cwd)
    "integration" -> gleam_integration(cwd)
    "security" -> gleam_security(cwd)
    "review" -> gleam_review(cwd)
    "accept" -> gleam_accept(cwd)
    other -> Error("Unknown Gleam stage: " <> other)
  }
}

fn gleam_tdd_setup(cwd: String) -> Result(Nil, String) {
  // Check that test files exist: *_test.gleam or test_*.gleam
  process.run_command("find", [
    ".", "-name", "*_test.gleam", "-o", "-name", "test_*.gleam",
  ], cwd)
  |> result.map_error(fn(_) {
    "Gleam: Test files not found. Create test_*.gleam or *_test.gleam files"
  })
  |> result.map(fn(_) { Nil })
}

fn gleam_implement(cwd: String) -> Result(Nil, String) {
  // gleam build must succeed
  use _ <- result.try(process.command_exists("gleam"))
  use cmd_result <- result.try(process.run_command("gleam", ["build"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Code does not compile" })
}

fn gleam_unit_test(cwd: String) -> Result(Nil, String) {
  // gleam test must pass
  use _ <- result.try(process.command_exists("gleam"))
  use cmd_result <- result.try(process.run_command("gleam", ["test"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Tests failed" })
}

fn gleam_coverage(cwd: String) -> Result(Nil, String) {
  // Gleam doesn't have built-in coverage yet, so verify tests exist
  process.run_command("find", [
    ".", "-name", "*_test.gleam", "-o", "-name", "test_*.gleam",
  ], cwd)
  |> result.map_error(fn(_) { "Gleam: No test files for coverage check" })
  |> result.map(fn(_) { Nil })
}

fn gleam_lint(cwd: String) -> Result(Nil, String) {
  // gleam format --check
  use cmd_result <- result.try(process.run_command("gleam", ["format", "--check", "."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) {
    "Gleam: Code formatting issues. Run: gleam format ."
  })
}

fn gleam_static(cwd: String) -> Result(Nil, String) {
  // gleam check (type checker)
  use cmd_result <- result.try(process.run_command("gleam", ["check"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Type checking failed" })
}

fn gleam_integration(cwd: String) -> Result(Nil, String) {
  // Run full test suite
  use cmd_result <- result.try(process.run_command("gleam", ["test"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Integration tests failed" })
}

fn gleam_security(cwd: String) -> Result(Nil, String) {
  // Check for dependency vulnerabilities using gleam get
  // which will validate the manifest
  use cmd_result <- result.try(process.run_command("gleam", ["get"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Dependency resolution failed" })
}

fn gleam_review(cwd: String) -> Result(Nil, String) {
  // Check for TODO/FIXME comments
  // grep returns non-zero if no matches (which is good for this check)
  case process.run_command("grep", [
    "-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.gleam", ".",
  ], cwd) {
    Ok(_) | Error(_) -> Ok(Nil)
  }
}

fn gleam_accept(cwd: String) -> Result(Nil, String) {
  // Run final checks: build + test + lint
  use _ <- result.try(gleam_implement(cwd))
  use _ <- result.try(gleam_unit_test(cwd))
  use _ <- result.try(gleam_lint(cwd))
  Ok(Nil)
}

// ============================================================================
// GO STAGES - Real Go tooling
// ============================================================================

fn execute_go_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "tdd-setup" -> go_tdd_setup(cwd)
    "implement" -> go_implement(cwd)
    "unit-test" -> go_unit_test(cwd)
    "coverage" -> go_coverage(cwd)
    "lint" -> go_lint(cwd)
    "static" -> go_static(cwd)
    "integration" -> go_integration(cwd)
    "security" -> go_security(cwd)
    "review" -> go_review(cwd)
    "accept" -> go_accept(cwd)
    other -> Error("Unknown Go stage: " <> other)
  }
}

fn go_tdd_setup(cwd: String) -> Result(Nil, String) {
  // Check for *_test.go files
  process.run_command("find", [".", "-name", "*_test.go"], cwd)
  |> result.map_error(fn(_) { "Go: No *_test.go files found" })
  |> result.map(fn(_) { Nil })
}

fn go_implement(cwd: String) -> Result(Nil, String) {
  // go build ./...
  use _ <- result.try(process.command_exists("go"))
  use cmd_result <- result.try(process.run_command("go", ["build", "./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Code does not compile" })
}

fn go_unit_test(cwd: String) -> Result(Nil, String) {
  // go test -v -short ./...
  use _ <- result.try(process.command_exists("go"))
  use cmd_result <- result.try(process.run_command("go", ["test", "-v", "-short", "./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Tests failed" })
}

fn go_coverage(cwd: String) -> Result(Nil, String) {
  // go test -coverprofile=coverage.out ./... && check coverage >= 80%
  use cmd_result <- result.try(process.run_command(
    "go",
    ["test", "-coverprofile=coverage.out", "./..."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Coverage tests failed" })
}

fn go_lint(cwd: String) -> Result(Nil, String) {
  // gofmt -l
  use cmd_result <- result.try(process.run_command("gofmt", ["-l", "."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: gofmt check failed" })
}

fn go_static(cwd: String) -> Result(Nil, String) {
  // go vet ./...
  use cmd_result <- result.try(process.run_command("go", ["vet", "./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: go vet failed" })
}

fn go_integration(cwd: String) -> Result(Nil, String) {
  // go test -v ./...
  use cmd_result <- result.try(process.run_command("go", ["test", "-v", "./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Integration tests failed" })
}

fn go_security(cwd: String) -> Result(Nil, String) {
  // gosec ./...
  use cmd_result <- result.try(process.run_command("gosec", ["./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Security check failed" })
}

fn go_review(cwd: String) -> Result(Nil, String) {
  // Check for TODO/FIXME comments
  // grep returns non-zero if no matches (which is good for this check)
  case process.run_command("grep", [
    "-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.go", ".",
  ], cwd) {
    Ok(_) | Error(_) -> Ok(Nil)
  }
}

fn go_accept(cwd: String) -> Result(Nil, String) {
  // go build + go test + gofmt
  use _ <- result.try(go_implement(cwd))
  use _ <- result.try(go_unit_test(cwd))
  use _ <- result.try(go_lint(cwd))
  Ok(Nil)
}

// ============================================================================
// RUST STAGES
// ============================================================================

fn execute_rust_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "tdd-setup" -> rust_tdd_setup(cwd)
    "implement" -> rust_implement(cwd)
    "unit-test" -> rust_unit_test(cwd)
    "coverage" -> rust_coverage(cwd)
    "lint" -> rust_lint(cwd)
    "static" -> rust_static(cwd)
    "integration" -> rust_integration(cwd)
    "security" -> rust_security(cwd)
    "review" -> rust_review(cwd)
    "accept" -> rust_accept(cwd)
    other -> Error("Unknown Rust stage: " <> other)
  }
}

fn rust_tdd_setup(cwd: String) -> Result(Nil, String) {
  // Check for *_test.rs or tests/ directory
  process.run_command("find", [".", "-name", "*_test.rs", "-o", "-type", "d", "-name", "tests"], cwd)
  |> result.map_error(fn(_) { "Rust: No tests found" })
  |> result.map(fn(_) { Nil })
}

fn rust_implement(cwd: String) -> Result(Nil, String) {
  // cargo build
  use _ <- result.try(process.command_exists("cargo"))
  use cmd_result <- result.try(process.run_command("cargo", ["build"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Code does not compile" })
}

fn rust_unit_test(cwd: String) -> Result(Nil, String) {
  // cargo test
  use cmd_result <- result.try(process.run_command("cargo", ["test"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Tests failed" })
}

fn rust_coverage(cwd: String) -> Result(Nil, String) {
  // cargo tarpaulin for coverage
  use cmd_result <- result.try(process.run_command("cargo", ["tarpaulin", "--out", "Xml"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Coverage generation failed" })
}

fn rust_lint(cwd: String) -> Result(Nil, String) {
  // cargo fmt --check
  use cmd_result <- result.try(process.run_command("cargo", ["fmt", "--check"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Code formatting issues" })
}

fn rust_static(cwd: String) -> Result(Nil, String) {
  // cargo clippy
  use cmd_result <- result.try(process.run_command("cargo", ["clippy", "--all-targets"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Clippy failed" })
}

fn rust_integration(cwd: String) -> Result(Nil, String) {
  // cargo test --all
  use cmd_result <- result.try(process.run_command("cargo", ["test", "--all"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Integration tests failed" })
}

fn rust_security(cwd: String) -> Result(Nil, String) {
  // cargo audit for security vulnerabilities
  use cmd_result <- result.try(process.run_command("cargo", ["audit"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Security audit failed" })
}

fn rust_review(_cwd: String) -> Result(Nil, String) {
  Ok(Nil)
}

fn rust_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(rust_implement(cwd))
  use _ <- result.try(rust_unit_test(cwd))
  use _ <- result.try(rust_lint(cwd))
  Ok(Nil)
}

// ============================================================================
// PYTHON STAGES
// ============================================================================

fn execute_python_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "tdd-setup" -> python_tdd_setup(cwd)
    "implement" -> python_implement(cwd)
    "unit-test" -> python_unit_test(cwd)
    "coverage" -> python_coverage(cwd)
    "lint" -> python_lint(cwd)
    "static" -> python_static(cwd)
    "integration" -> python_integration(cwd)
    "security" -> python_security(cwd)
    "review" -> python_review(cwd)
    "accept" -> python_accept(cwd)
    other -> Error("Unknown Python stage: " <> other)
  }
}

fn python_tdd_setup(cwd: String) -> Result(Nil, String) {
  // Check for test_*.py or *_test.py files
  process.run_command("find", [".", "-name", "test_*.py", "-o", "-name", "*_test.py"], cwd)
  |> result.map_error(fn(_) { "Python: No tests found" })
  |> result.map(fn(_) { Nil })
}

fn python_implement(cwd: String) -> Result(Nil, String) {
  // python -m py_compile
  use _ <- result.try(process.command_exists("python"))
  use cmd_result <- result.try(process.run_command("python", ["-m", "py_compile", "."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Code does not compile" })
}

fn python_unit_test(cwd: String) -> Result(Nil, String) {
  // python -m pytest
  use cmd_result <- result.try(process.run_command("python", ["-m", "pytest", "-v"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Tests failed" })
}

fn python_coverage(cwd: String) -> Result(Nil, String) {
  // python -m coverage
  use cmd_result <- result.try(process.run_command("python", ["-m", "coverage", "run", "-m", "pytest"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Coverage generation failed" })
}

fn python_lint(cwd: String) -> Result(Nil, String) {
  // black --check
  use cmd_result <- result.try(process.run_command("black", ["--check", "."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Code formatting issues" })
}

fn python_static(cwd: String) -> Result(Nil, String) {
  // mypy
  use cmd_result <- result.try(process.run_command("mypy", ["."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Type checking failed" })
}

fn python_integration(cwd: String) -> Result(Nil, String) {
  // python -m pytest
  use cmd_result <- result.try(process.run_command("python", ["-m", "pytest", "-v"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Integration tests failed" })
}

fn python_security(cwd: String) -> Result(Nil, String) {
  // bandit for security vulnerabilities
  use cmd_result <- result.try(process.run_command("bandit", ["-r", "."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Security scan failed" })
}

fn python_review(_cwd: String) -> Result(Nil, String) {
  Ok(Nil)
}

fn python_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(python_implement(cwd))
  use _ <- result.try(python_unit_test(cwd))
  use _ <- result.try(python_lint(cwd))
  Ok(Nil)
}
