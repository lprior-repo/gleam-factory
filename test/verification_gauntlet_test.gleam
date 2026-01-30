import gleeunit/should
import verification_gauntlet

pub fn gauntlet_stage_build_exists_test() {
  let stage = verification_gauntlet.Build
  case stage {
    verification_gauntlet.Build -> should.be_true(True)
  }
}

pub fn gauntlet_stage_test_exists_test() {
  let stage = verification_gauntlet.Test
  case stage {
    verification_gauntlet.Test -> should.be_true(True)
  }
}

pub fn gauntlet_stage_lint_exists_test() {
  let stage = verification_gauntlet.Lint
  case stage {
    verification_gauntlet.Lint -> should.be_true(True)
  }
}

pub fn gauntlet_stage_format_exists_test() {
  let stage = verification_gauntlet.Format
  case stage {
    verification_gauntlet.Format -> should.be_true(True)
  }
}

pub fn gauntlet_result_passed_captures_stages_test() {
  let result = verification_gauntlet.Passed(stages_run: 3)
  case result {
    verification_gauntlet.Passed(stages_run: 3) -> should.be_true(True)
  }
}

pub fn gauntlet_result_failed_captures_stage_test() {
  let result =
    verification_gauntlet.Failed(
      stage: verification_gauntlet.Build,
      error: "compilation failed",
    )
  case result {
    verification_gauntlet.Failed(stage: verification_gauntlet.Build, ..) ->
      should.be_true(True)
  }
}

pub fn gauntlet_result_failed_captures_error_test() {
  let result =
    verification_gauntlet.Failed(
      stage: verification_gauntlet.Test,
      error: "test error",
    )
  case result {
    verification_gauntlet.Failed(error: "test error", ..) ->
      should.be_true(True)
  }
}

pub fn stage_name_build_returns_build_test() {
  verification_gauntlet.stage_name(verification_gauntlet.Build)
  |> should.equal("build")
}

pub fn stage_name_test_returns_test_test() {
  verification_gauntlet.stage_name(verification_gauntlet.Test)
  |> should.equal("test")
}

pub fn stage_name_lint_returns_lint_test() {
  verification_gauntlet.stage_name(verification_gauntlet.Lint)
  |> should.equal("lint")
}

pub fn stage_name_format_returns_format_test() {
  verification_gauntlet.stage_name(verification_gauntlet.Format)
  |> should.equal("format")
}

pub fn is_passed_returns_true_for_passed_test() {
  let result = verification_gauntlet.Passed(stages_run: 4)
  verification_gauntlet.is_passed(result)
  |> should.be_true
}

pub fn is_passed_returns_false_for_failed_test() {
  let result =
    verification_gauntlet.Failed(
      stage: verification_gauntlet.Build,
      error: "failed",
    )
  verification_gauntlet.is_passed(result)
  |> should.be_false
}

pub fn passed_with_zero_stages_test() {
  let result = verification_gauntlet.Passed(stages_run: 0)
  case result {
    verification_gauntlet.Passed(stages_run: 0) -> should.be_true(True)
  }
}

pub fn passed_with_multiple_stages_test() {
  let result = verification_gauntlet.Passed(stages_run: 10)
  case result {
    verification_gauntlet.Passed(stages_run: 10) -> should.be_true(True)
  }
}

pub fn can_discriminate_passed_from_failed_test() {
  let passed = verification_gauntlet.Passed(stages_run: 3)
  let failed =
    verification_gauntlet.Failed(
      stage: verification_gauntlet.Build,
      error: "oops",
    )

  case passed {
    verification_gauntlet.Passed(..) -> should.be_true(True)
    verification_gauntlet.Failed(..) -> should.fail()
  }

  case failed {
    verification_gauntlet.Failed(..) -> should.be_true(True)
  }
}
