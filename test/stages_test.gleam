import domain
import gleeunit
import gleeunit/should
import stages

pub fn main() {
  gleeunit.main()
}

// Stage transition validation tests
pub fn validate_stage_transition_forward_test() {
  stages.validate_stage_transition("implement", "unit-test")
  |> should.be_ok()
}

pub fn validate_stage_transition_backward_test() {
  stages.validate_stage_transition("unit-test", "implement")
  |> should.be_error()
}

pub fn validate_stage_transition_same_test() {
  stages.validate_stage_transition("implement", "implement")
  |> should.be_error()
}

pub fn validate_stage_transition_unknown_from_test() {
  stages.validate_stage_transition("unknown", "implement")
  |> should.be_error()
}

pub fn validate_stage_transition_unknown_to_test() {
  stages.validate_stage_transition("implement", "unknown")
  |> should.be_error()
}

pub fn validate_stage_transition_distant_test() {
  stages.validate_stage_transition("implement", "accept")
  |> should.be_ok()
}

// Gleam stage tests
pub fn execute_gleam_implement_test() {
  stages.execute_stage("implement", domain.Gleam, ".")
  |> should.be_ok()
}

pub fn execute_gleam_unit_test_test() {
  stages.execute_stage("unit-test", domain.Gleam, ".")
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
  stages.execute_stage("integration", domain.Gleam, ".")
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
  stages.execute_stage("accept", domain.Gleam, ".")
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

// Dry-run mode tests
pub fn execute_stages_dry_run_gleam_single_stage_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  let previews = stages.execute_stages_dry_run([stage], domain.Gleam)

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
  let stage1 = domain.Stage("implement", "Code compiles", 5, True)
  let stage2 = domain.Stage("unit-test", "All tests pass", 3, True)
  let previews = stages.execute_stages_dry_run([stage1, stage2], domain.Go)

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
  let stage = domain.Stage("accept", "Ready for merge", 1, True)
  let previews = stages.execute_stages_dry_run([stage], domain.Rust)

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
  let stage = domain.Stage("security", "No vulnerabilities", 2, True)
  let previews = stages.execute_stages_dry_run([stage], domain.Python)

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
  let previews = stages.execute_stages_dry_run([], domain.Gleam)
  previews |> should.equal([])
}

pub fn execute_stages_dry_run_all_gleam_stages_test() {
  let pipeline = domain.standard_pipeline()
  let previews = stages.execute_stages_dry_run(pipeline, domain.Gleam)
  previews |> should.not_equal([])
}
