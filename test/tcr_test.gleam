// TCR module unit tests
// Tests attempt parameter propagation in Failed outcomes

import domain
import gleeunit/should
import tcr

pub fn failed_outcome_has_correct_attempt_test() {
  // GIVEN: A Failed outcome with attempt=1
  let outcome =
    tcr.Failed(stage_name: "test-stage", attempt: 1, reason: "error", reverted: True)

  // THEN: The attempt value is accessible and correct
  case outcome {
    tcr.Failed(_, attempt, _, _) -> attempt |> should.equal(1)
    _ -> should.fail()
  }
}

pub fn failed_outcome_attempt_2_test() {
  // GIVEN: A Failed outcome with attempt=2
  let outcome =
    tcr.Failed(stage_name: "test-stage", attempt: 2, reason: "error", reverted: True)

  // THEN: The attempt value is 2
  case outcome {
    tcr.Failed(_, attempt, _, _) -> attempt |> should.equal(2)
    _ -> should.fail()
  }
}

pub fn failed_outcome_attempt_3_test() {
  // GIVEN: A Failed outcome with attempt=3
  let outcome =
    tcr.Failed(stage_name: "test-stage", attempt: 3, reason: "error", reverted: True)

  // THEN: The attempt value is 3
  case outcome {
    tcr.Failed(_, attempt, _, _) -> attempt |> should.equal(3)
    _ -> should.fail()
  }
}

pub fn should_retry_returns_true_when_attempts_remaining_test() {
  // GIVEN: A stage with 3 retries
  let stage =
    domain.Stage(name: "test", gate: "Test gate", tcr: True, retries: 3)

  // THEN: should_retry returns true for attempts 1 and 2
  tcr.should_retry(stage, 1) |> should.equal(True)
  tcr.should_retry(stage, 2) |> should.equal(True)
}

pub fn should_retry_returns_false_when_no_attempts_remaining_test() {
  // GIVEN: A stage with 3 retries
  let stage =
    domain.Stage(name: "test", gate: "Test gate", tcr: True, retries: 3)

  // THEN: should_retry returns false for attempt 3 and beyond
  tcr.should_retry(stage, 3) |> should.equal(False)
  tcr.should_retry(stage, 4) |> should.equal(False)
}

pub fn outcome_to_string_includes_attempt_test() {
  // GIVEN: A Failed outcome with attempt=2
  let outcome =
    tcr.Failed(stage_name: "build", attempt: 2, reason: "compile error", reverted: True)

  // THEN: The string representation includes the attempt number
  let result = tcr.outcome_to_string(outcome)
  result |> should.equal("✗ build failed (attempt: 2, reverted: True, reason: compile error)")
}
