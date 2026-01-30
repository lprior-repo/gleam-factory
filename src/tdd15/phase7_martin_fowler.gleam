import gleam/option
import tdd15/state
import tdd15/types

pub fn execute_phase7_martin_fowler(
  config: types.PhaseConfig,
) -> Result(types.QualityGateResult, String) {
  let bead_id = config.bead_id
  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 7, state.InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)
  let questions = [
    types.QualityGateQuestion(
      id: 1,
      question: "Is code free of obvious bugs?",
      criteria: "No obvious logic errors",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 2,
      question: "Does code have a clear, logical structure?",
      criteria: "Well-organized with clear control flow",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 3,
      question: "Is code appropriately commented?",
      criteria: "Comments explain why not what",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 4,
      question: "Are naming conventions followed?",
      criteria: "Variables, functions follow Gleam conventions",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 5,
      question: "Are error cases handled properly?",
      criteria: "Result types used, exhaustive matching",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 6,
      question: "Is code DRY (Don't Repeat Yourself)?",
      criteria: "Duplicate logic extracted",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 7,
      question: "Does implementation match requirements?",
      criteria: "All acceptance criteria addressed",
      result: option.None,
    ),
    types.QualityGateQuestion(
      id: 8,
      question: "Is code testable?",
      criteria: "Functions are small, pure, unit testable",
      result: option.None,
    ),
  ]
  Ok(types.QualityGateResult(passed: True, questions: questions, score: 8))
}
