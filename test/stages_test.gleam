import domain
import gleeunit
import gleeunit/should
import simplifile
import stages
import stages_types
import stages_test_mode

pub fn main() {
  gleeunit.main()
}

// Stage transition validation tests
pub fn validate_stage_transition_forward_test() {
  stages_types.validate_stage_transition("implement", "unit-test")
  |> should.be_ok()
}

pub fn validate_stage_transition_backward_test() {
  stages_types.validate_stage_transition("unit-test", "implement")
  |> should.be_error()
}

pub fn validate_stage_transition_same_test() {
  stages_types.validate_stage_transition("implement", "implement")
  |> should.be_error()
}

pub fn validate_stage_transition_unknown_from_test() {
  stages_types.validate_stage_transition("unknown", "implement")
  |> should.be_error()
}

pub fn validate_stage_transition_unknown_to_test() {
  stages_types.validate_stage_transition("implement", "unknown")
  |> should.be_error()
}

pub fn validate_stage_transition_distant_test() {
  stages_types.validate_stage_transition("implement", "accept")
  |> should.be_ok()
}

// Gleam stage tests
pub fn execute_gleam_implement_test() {
  stages.execute_stage("implement", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_unit_test_test() {
  stages_test_mode.set_test_mode()
  let result = stages.execute_stage("unit-test", domain.Gleam, ".")
  stages_test_mode.clear_test_mode()
  result
  |> should.be_ok()
}

pub fn execute_gleam_coverage_test() {
  stages.execute_stage("coverage", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_lint_test() {
  stages.execute_stage("lint", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_static_test() {
  stages.execute_stage("static", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_integration_test() {
  stages_test_mode.set_test_mode()
  let result = stages.execute_stage("integration", domain.Gleam, ".")
  stages_test_mode.clear_test_mode()
  result
  |> should.be_ok()
}

pub fn execute_gleam_security_test() {
  stages.execute_stage("security", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_review_test() {
  stages.execute_stage("review", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_accept_test() {
  stages_test_mode.set_test_mode()
  let result = stages.execute_stage("accept", domain.Gleam, ".")
  stages_test_mode.clear_test_mode()
  result
  |> should.be_ok()
}

pub fn execute_gleam_unknown_stage_test() {
  stages.execute_stage("unknown", domain.Gleam, ".")
  |> should.be_error()
}

// Go stage tests
pub fn execute_go_unknown_stage_test() {
  stages.execute_stage("unknown", domain.Go, ".")
  |> should.be_error()
}

// Rust stage tests
pub fn execute_rust_unknown_stage_test() {
  stages.execute_stage("unknown", domain.Rust, ".")
  |> should.be_error()
}

// Python stage tests
pub fn execute_python_unknown_stage_test() {
  stages.execute_stage("unknown", domain.Python, ".")
  |> should.be_error()
}

// Go lint test
pub fn go_lint_detects_unformatted_code_test() {
  stages.execute_stage("lint", domain.Go, "/tmp/go-lint-test")
  |> should.be_error()
}

// Dry-run mode tests
pub fn execute_stages_dry_run_gleam_single_stage_test() {
  let stage = domain.Stage("implement", "Code compiles", 5)
  let previews = stages_types.execute_stages_dry_run([stage], domain.Gleam)

  case previews {
    [preview] -> {
      preview.name |> should.equal("implement")
      preview.command |> should.equal("gleam build")
      preview.estimated_duration |> should.equal(5000)
    }
    _ -> should.fail()
  }
}

pub fn execute_stages_dry_run_go_multiple_stages_test() {
  let stage1 = domain.Stage("implement", "Code compiles", 5)
  let stage2 = domain.Stage("unit-test", "All tests pass", 3)
  let previews = stages_types.execute_stages_dry_run([stage1, stage2], domain.Go)

  case previews {
    [p1, p2] -> {
      p1.name |> should.equal("implement")
      p1.command |> should.equal("go build ./...")
      p1.estimated_duration |> should.equal(5000)
      p2.name |> should.equal("unit-test")
      p2.command |> should.equal("go test -v -short ./...")
      p2.estimated_duration |> should.equal(3000)
    }
    _ -> should.fail()
  }
}

pub fn execute_stages_dry_run_rust_accept_stage_test() {
  let stage = domain.Stage("accept", "Ready for merge", 1)
  let previews = stages_types.execute_stages_dry_run([stage], domain.Rust)

  case previews {
    [preview] -> {
      preview.name |> should.equal("accept")
      preview.command
      |> should.equal("cargo build && cargo test && cargo fmt --check")
      preview.estimated_duration |> should.equal(7000)
    }
    _ -> should.fail()
  }
}

pub fn execute_stages_dry_run_python_security_stage_test() {
  let stage = domain.Stage("security", "No vulnerabilities", 2)
  let previews = stages_types.execute_stages_dry_run([stage], domain.Python)

  case previews {
    [preview] -> {
      preview.name |> should.equal("security")
      preview.command |> should.equal("bandit -r .")
      preview.estimated_duration |> should.equal(6000)
    }
    _ -> should.fail()
  }
}

pub fn execute_stages_dry_run_empty_list_test() {
  let previews = stages_types.execute_stages_dry_run([], domain.Gleam)
  previews |> should.equal([])
}

pub fn execute_stages_dry_run_all_gleam_stages_test() {
  let pipeline = domain.standard_pipeline()
  let previews = stages_types.execute_stages_dry_run(pipeline, domain.Gleam)
  previews |> should.not_equal([])
}

// Review stage tests - grep exit semantics
// grep exit 0 = matches found (TODO/FIXME present) -> should Error
// grep exit 1 = no matches (clean code) -> should Ok
// grep exit 2+ = actual error -> should Error

pub fn gleam_review_with_markers_should_error_test() {
  // Create temp dir with TODO marker
  let test_dir = "/tmp/review-test-markers-gleam"
  let _ = simplifile.delete_all([test_dir])
  let assert Ok(_) = simplifile.create_directory(test_dir)
  let assert Ok(_) =
    simplifile.write(test_dir <> "/app.gleam", "// TODO: fix this later\n")

  let result = stages.execute_stage("review", domain.Gleam, test_dir)

  // Cleanup
  let _ = simplifile.delete_all([test_dir])

  // Should error because markers found
  result
  |> should.be_error()
}

pub fn gleam_review_without_markers_should_pass_test() {
  // Create temp dir with clean code
  let test_dir = "/tmp/review-test-clean-gleam"
  let _ = simplifile.delete_all([test_dir])
  let assert Ok(_) = simplifile.create_directory(test_dir)
  let assert Ok(_) =
    simplifile.write(test_dir <> "/app.gleam", "// Clean code\n")

  let result = stages.execute_stage("review", domain.Gleam, test_dir)

  // Cleanup
  let _ = simplifile.delete_all([test_dir])

  // Should pass because no markers
  result
  |> should.be_ok()
}

pub fn go_review_with_markers_should_error_test() {
  let test_dir = "/tmp/review-test-markers-go"
  let _ = simplifile.delete_all([test_dir])
  let assert Ok(_) = simplifile.create_directory(test_dir)
  let assert Ok(_) =
    simplifile.write(test_dir <> "/main.go", "// FIXME: bad code\n")

  let result = stages.execute_stage("review", domain.Go, test_dir)

  let _ = simplifile.delete_all([test_dir])

  result
  |> should.be_error()
}

pub fn rust_review_with_markers_should_error_test() {
  let test_dir = "/tmp/review-test-markers-rust"
  let _ = simplifile.delete_all([test_dir])
  let assert Ok(_) = simplifile.create_directory(test_dir)
  let assert Ok(_) = simplifile.write(test_dir <> "/main.rs", "// XXX: hack\n")

  let result = stages.execute_stage("review", domain.Rust, test_dir)

  let _ = simplifile.delete_all([test_dir])

  result
  |> should.be_error()
}

pub fn python_review_with_markers_should_error_test() {
  let test_dir = "/tmp/review-test-markers-python"
  let _ = simplifile.delete_all([test_dir])
  let assert Ok(_) = simplifile.create_directory(test_dir)
  let assert Ok(_) =
    simplifile.write(test_dir <> "/main.py", "# HACK: workaround\n")

  let result = stages.execute_stage("review", domain.Python, test_dir)

  let _ = simplifile.delete_all([test_dir])

  result
  |> should.be_error()
}
