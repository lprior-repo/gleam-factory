import domain
import gleeunit
import gleeunit/should
import tcr

pub fn main() {
  gleeunit.main()
}

// TCR Outcome tests
pub fn outcome_to_string_passed_test() {
  let outcome = tcr.Passed("implement", 1, True)
  tcr.outcome_to_string(outcome)
  |> should.equal(
    "✓ implement passed (commits: 1, persisted: True)",
  )
}

pub fn outcome_to_string_failed_test() {
  let outcome = tcr.Failed("implement", 1, "compilation error", True)
  tcr.outcome_to_string(outcome)
  |> should.equal(
    "✗ implement failed (attempt: 1, reverted: True, reason: compilation error)",
  )
}

// Retry logic tests
pub fn should_retry_within_limit_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, True)
  tcr.should_retry(stage, 1)
  |> should.equal(True)
}

pub fn should_retry_at_limit_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, True)
  tcr.should_retry(stage, 3)
  |> should.equal(False)
}

pub fn should_retry_over_limit_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, True)
  tcr.should_retry(stage, 4)
  |> should.equal(False)
}

pub fn remaining_retries_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  tcr.remaining_retries(stage, 2)
  |> should.equal(3)
}

pub fn remaining_retries_zero_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  tcr.remaining_retries(stage, 5)
  |> should.equal(0)
}

// run_with_tcr tests - TCR disabled
pub fn run_with_tcr_disabled_success_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, False)
  let execute = fn() { Ok(Nil) }

  let result = tcr.run_with_tcr(stage, "/tmp/worktree", execute)

  case result {
    Ok(tcr.Passed("implement", 0, True)) -> Nil
    _ -> panic as "Expected Passed outcome with 0 commits"
  }
}

pub fn run_with_tcr_disabled_failure_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, False)
  let execute = fn() { Error("stage failed") }

  let result = tcr.run_with_tcr(stage, "/tmp/worktree", execute)

  result
  |> should.be_error()
}

// Pattern matching on TCROutcome
pub fn outcome_pattern_match_passed_test() {
  let outcome = tcr.Passed("test", 1, True)
  let tcr.Passed(name, commits, persisted) = outcome
  name
  |> should.equal("test")
  commits
  |> should.equal(1)
  persisted
  |> should.equal(True)
}

pub fn outcome_pattern_match_failed_test() {
  let outcome = tcr.Failed("test", 2, "error", True)
  let tcr.Failed(name, attempt, reason, reverted) = outcome
  name
  |> should.equal("test")
  attempt
  |> should.equal(2)
  reason
  |> should.equal("error")
  reverted
  |> should.equal(True)
}

// Edge cases
pub fn remaining_retries_negative_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, True)
  tcr.remaining_retries(stage, 5)
  |> should.equal(-2)
}

pub fn should_retry_zero_retries_test() {
  let stage = domain.Stage("implement", "Code compiles", 0, True)
  tcr.should_retry(stage, 0)
  |> should.equal(False)
}
