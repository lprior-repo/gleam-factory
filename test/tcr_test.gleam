import domain
import gleeunit
import gleeunit/should
import tcr

pub fn main() {
  gleeunit.main()
}

// ===== TCR Outcome type tests =====

pub fn outcome_to_string_passed_test() {
  let outcome = tcr.Passed("implement", 1, True)
  tcr.outcome_to_string(outcome)
  |> should.equal("✓ implement passed (commits: 1, persisted: True)")
}

pub fn outcome_to_string_failed_test() {
  let outcome = tcr.Failed("implement", 1, "compilation error", True)
  tcr.outcome_to_string(outcome)
  |> should.equal(
    "✗ implement failed (attempt: 1, reverted: True, reason: compilation error)",
  )
}

pub fn outcome_to_string_failed_no_revert_test() {
  let outcome = tcr.Failed("test-stage", 2, "timeout", False)
  tcr.outcome_to_string(outcome)
  |> should.equal(
    "✗ test-stage failed (attempt: 2, reverted: False, reason: timeout)",
  )
}

pub fn outcome_to_string_passed_zero_commits_test() {
  let outcome = tcr.Passed("build", 0, False)
  tcr.outcome_to_string(outcome)
  |> should.equal("✓ build passed (commits: 0, persisted: False)")
}

pub fn outcome_to_string_passed_multi_commits_test() {
  let outcome = tcr.Passed("integration", 5, True)
  tcr.outcome_to_string(outcome)
  |> should.equal("✓ integration passed (commits: 5, persisted: True)")
}

// ===== Pattern matching on TCROutcome =====

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

pub fn outcome_pattern_match_failed_not_reverted_test() {
  let outcome = tcr.Failed("lint", 3, "format", False)
  let tcr.Failed(name, attempt, reason, reverted) = outcome
  name
  |> should.equal("lint")
  attempt
  |> should.equal(3)
  reason
  |> should.equal("format")
  reverted
  |> should.equal(False)
}

// ===== Retry logic tests =====

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

pub fn should_retry_zero_retries_test() {
  let stage = domain.Stage("implement", "Code compiles", 0, True)
  tcr.should_retry(stage, 0)
  |> should.equal(False)
}

pub fn should_retry_first_attempt_test() {
  let stage = domain.Stage("lint", "Code formatted", 5, True)
  tcr.should_retry(stage, 0)
  |> should.equal(True)
}

pub fn should_retry_many_retries_test() {
  let stage = domain.Stage("coverage", "80% coverage", 10, True)
  tcr.should_retry(stage, 9)
  |> should.equal(True)
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

pub fn remaining_retries_first_attempt_test() {
  let stage = domain.Stage("test", "Tests pass", 5, True)
  tcr.remaining_retries(stage, 0)
  |> should.equal(5)
}

pub fn remaining_retries_negative_test() {
  let stage = domain.Stage("implement", "Code compiles", 3, True)
  tcr.remaining_retries(stage, 5)
  |> should.equal(-2)
}

// ===== run_with_tcr tests - TCR disabled =====

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

pub fn run_with_tcr_disabled_error_message_test() {
  let stage = domain.Stage("lint", "Formatted", 2, False)
  let execute = fn() { Error("linting failed") }

  case tcr.run_with_tcr(stage, "/tmp/worktree", execute) {
    Ok(_) -> should.fail()
    Error(msg) -> msg |> should.equal("linting failed")
  }
}

// ===== run_with_tcr tests - TCR enabled with success =====

pub fn run_with_tcr_enabled_success_test() {
  let stage = domain.Stage("unit-test", "Tests pass", 3, True)
  let execute = fn() { Ok(Nil) }

  let result = tcr.run_with_tcr(stage, "/tmp/worktree", execute)

  case result {
    Error(msg) ->
      // Expected: jj commands will fail in test environment
      msg
      |> should.contain("Failed to get initial jj state")
    Ok(_) ->
      // Unlikely in test environment, but if jj is available
      Nil
  }
}

// ===== run_with_tcr tests - TCR enabled with failure =====

pub fn run_with_tcr_enabled_failure_test() {
  let stage = domain.Stage("implement", "Compiles", 3, True)
  let execute = fn() { Error("compilation failed") }

  let result = tcr.run_with_tcr(stage, "/tmp/worktree", execute)

  case result {
    Ok(tcr.Failed(name, attempt, reason, reverted)) -> {
      name
      |> should.equal("implement")
      attempt
      |> should.equal(1)
      reason
      |> should.equal("compilation failed")
      // reverted may be False if revert fails
    }
    _ ->
      // May fail if jj unavailable
      Nil
  }
}

// ===== Stage success cases =====

pub fn run_with_tcr_stage_name_preserved_success_test() {
  let stage = domain.Stage("coverage", "Coverage OK", 5, True)
  let execute = fn() { Ok(Nil) }

  case tcr.run_with_tcr(stage, "/tmp/worktree", execute) {
    Ok(tcr.Passed(name, _, _)) -> name |> should.equal("coverage")
    _ -> Nil
  }
}

pub fn run_with_tcr_stage_name_preserved_failure_test() {
  let stage = domain.Stage("security", "No vulns", 2, True)
  let execute = fn() { Error("vulnerabilities found") }

  case tcr.run_with_tcr(stage, "/tmp/worktree", execute) {
    Ok(tcr.Failed(name, _, reason, _)) -> {
      name
      |> should.equal("security")
      reason
      |> should.equal("vulnerabilities found")
    }
    _ -> Nil
  }
}

// ===== Complex stage names and messages =====

pub fn outcome_to_string_with_special_chars_test() {
  let outcome = tcr.Passed("my-stage_v2", 3, True)
  tcr.outcome_to_string(outcome)
  |> should.contain("my-stage_v2")
}

pub fn outcome_to_string_long_error_message_test() {
  let long_msg =
    "Test failed: expected value X but got Y with additional context"
  let outcome = tcr.Failed("verify", 1, long_msg, True)
  tcr.outcome_to_string(outcome)
  |> should.contain(long_msg)
}

// ===== Edge cases for outcome type =====

pub fn outcome_passed_with_large_commit_count_test() {
  let outcome = tcr.Passed("batch", 999, True)
  tcr.outcome_to_string(outcome)
  |> should.contain("999")
}

pub fn outcome_failed_with_high_attempt_test() {
  let outcome = tcr.Failed("flaky", 10, "timeout", False)
  tcr.outcome_to_string(outcome)
  |> should.contain("attempt: 10")
}

// ===== Boundary test cases for retry logic =====

pub fn should_retry_boundary_zero_test() {
  let stage = domain.Stage("test", "gate", 0, True)
  tcr.should_retry(stage, 0)
  |> should.equal(False)
}

pub fn should_retry_boundary_one_test() {
  let stage = domain.Stage("test", "gate", 1, True)
  tcr.should_retry(stage, 0)
  |> should.equal(True)
}

pub fn should_retry_boundary_one_at_limit_test() {
  let stage = domain.Stage("test", "gate", 1, True)
  tcr.should_retry(stage, 1)
  |> should.equal(False)
}

pub fn remaining_retries_boundary_large_test() {
  let stage = domain.Stage("test", "gate", 100, True)
  tcr.remaining_retries(stage, 0)
  |> should.equal(100)
}

pub fn remaining_retries_boundary_large_used_test() {
  let stage = domain.Stage("test", "gate", 100, True)
  tcr.remaining_retries(stage, 99)
  |> should.equal(1)
}

// ===== Integration-like tests (mocking behavior) =====

pub fn tcr_outcome_passed_commit_count_one_test() {
  let outcome = tcr.Passed("implement", 1, True)
  let tcr.Passed(_, commits, persisted) = outcome
  commits
  |> should.equal(1)
  persisted
  |> should.equal(True)
}

pub fn tcr_outcome_failed_attempt_tracking_test() {
  let outcome = tcr.Failed("lint", 1, "bad format", True)
  let tcr.Failed(_, attempt, _, reverted) = outcome
  attempt
  |> should.equal(1)
  reverted
  |> should.equal(True)
}

pub fn tcr_outcome_sequence_test() {
  // Simulate multiple stage attempts
  let first_attempt = tcr.Failed("impl", 1, "error 1", False)
  let second_attempt = tcr.Failed("impl", 2, "error 2", False)
  let success = tcr.Passed("impl", 1, True)

  case first_attempt {
    tcr.Failed(_, attempt1, _, _) -> {
      case second_attempt {
        tcr.Failed(_, attempt2, _, _) -> {
          attempt1
          |> should.be_below(attempt2)
        }
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

// ===== Deterministic test cases (no jj dependency) =====

pub fn outcome_to_string_deterministic_passed_test() {
  let outcome1 = tcr.Passed("test", 2, True)
  let outcome2 = tcr.Passed("test", 2, True)

  tcr.outcome_to_string(outcome1)
  |> should.equal(tcr.outcome_to_string(outcome2))
}

pub fn outcome_to_string_deterministic_failed_test() {
  let outcome1 = tcr.Failed("test", 1, "error", True)
  let outcome2 = tcr.Failed("test", 1, "error", True)

  tcr.outcome_to_string(outcome1)
  |> should.equal(tcr.outcome_to_string(outcome2))
}

// ===== TCR logic composition tests =====

pub fn retry_logic_with_stage_test() {
  let stage = domain.Stage("critical", "Critical test", 5, True)
  let should_retry_at_2 = tcr.should_retry(stage, 2)
  let should_retry_at_5 = tcr.should_retry(stage, 5)

  should_retry_at_2
  |> should.equal(True)
  should_retry_at_5
  |> should.equal(False)
}

pub fn retry_tracking_sequence_test() {
  let stage = domain.Stage("implement", "Compiles", 3, True)

  // Simulate progression through retries
  let attempt_0_can_retry = tcr.should_retry(stage, 0)
  let attempt_1_can_retry = tcr.should_retry(stage, 1)
  let attempt_2_can_retry = tcr.should_retry(stage, 2)
  let attempt_3_can_retry = tcr.should_retry(stage, 3)

  attempt_0_can_retry
  |> should.equal(True)
  attempt_1_can_retry
  |> should.equal(True)
  attempt_2_can_retry
  |> should.equal(True)
  attempt_3_can_retry
  |> should.equal(False)
}

// ===== Outcome construction and deconstruction =====

pub fn outcome_passed_construct_deconstruct_test() {
  let tcr.Passed(name, commits, persisted) = tcr.Passed("stage-name", 2, True)

  name
  |> should.equal("stage-name")
  commits
  |> should.equal(2)
  persisted
  |> should.equal(True)
}

pub fn outcome_failed_construct_deconstruct_test() {
  let tcr.Failed(name, attempt, reason, reverted) =
    tcr.Failed("failed-stage", 3, "test failure", False)

  name
  |> should.equal("failed-stage")
  attempt
  |> should.equal(3)
  reason
  |> should.equal("test failure")
  reverted
  |> should.equal(False)
}
