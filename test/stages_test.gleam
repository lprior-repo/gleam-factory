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
