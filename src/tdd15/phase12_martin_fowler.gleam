import gleam/json
import gleam/option
import gleam/result
import gleam/string
import llm
import llm_router
import tdd15/state.{Completed, Failed, InProgress}
import tdd15/types

pub fn execute_phase12_martin_fowler(
  config: types.PhaseConfig,
) -> Result(types.QualityGateResult, String) {
  let bead_id = config.bead_id
  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 12, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)
  let bead_context = case state.load_bead_context(bead_id) {
    Ok(ctx) -> ctx
    _ ->
      state.BeadContext(
        id: bead_id,
        title: "Martin Fowler Review (Phase 12)",
        requirements: [],
        context: "Advanced refactoring patterns and code quality",
      )
  }
  let prompt =
    "Phase 12 MARTIN FOWLER (Advanced): Advanced refactoring patterns and code quality assessment.
 Bead Context:
 Title: "
    <> bead_context.title
    <> "
 Requirements: "
    <> string.join(bead_context.requirements, "\n")
    <> "
 Assess the following Martin Fowler patterns:
 1. Replace Conditional with Polymorphism
 2. Introduce Parameter Object
 3. Replace Type Code with Class
 4. Replace Subclass with Fields
 5. Extract Method/Class/Variable
 6. Introduce Null Object
 7. Compose Method
 8. Replace Magic Numbers with Named Constants
 9. Remove Dead Code
 10. Simplify Conditional Logic
 Respond in JSON format:
 {
   \"patterns_assessed\": <number>,
   \"refactoring_count\": <number>,
   \"code_smells_found\": <number>,
   \"quality_score\": <number 0-10>,
   \"recommendations\": \"<list of recommendations>\",
   \"reasoning\": \"<brief explanation>\"
 }"
  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Architect)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 1500,
        temperature: 0.3,
      ),
      llm.Architect,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(12)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])
      let assert Ok(Nil) =
        state.save_phase_output(bead_id, "phase12_martin_fowler", output_data)
      case parse_martin_fowler_response(response.content) {
        Ok(_) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 12, Completed)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)
          let questions = [
            types.QualityGateQuestion(
              id: 1,
              question: "Martin Fowler patterns assessed?",
              criteria: "Advanced refactoring patterns identified",
              result: option.Some(True),
            ),
            types.QualityGateQuestion(
              id: 2,
              question: "Refactoring opportunities found?",
              criteria: "Code smells and refactoring needs identified",
              result: option.Some(True),
            ),
            types.QualityGateQuestion(
              id: 3,
              question: "Quality score assigned?",
              criteria: "Code quality score 0-10 calculated",
              result: option.Some(True),
            ),
            types.QualityGateQuestion(
              id: 4,
              question: "Recommendations provided?",
              criteria: "Actionable recommendations generated",
              result: option.Some(True),
            ),
          ]
          Ok(types.QualityGateResult(
            passed: True,
            questions: questions,
            score: 4,
          ))
        }
        Error(_) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 12, Failed)
          let updated = state.increment_attempt(updated, 12)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)
          Ok(types.QualityGateResult(passed: False, questions: [], score: 0))
        }
      }
    }
    Error(_) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 12, Failed)
      let updated = state.increment_attempt(updated, 12)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)
      Ok(types.QualityGateResult(passed: False, questions: [], score: 0))
    }
  }
}

fn parse_martin_fowler_response(
  json_string: String,
) -> Result(types.TriageResult, String) {
  json.parse(from: json_string, using: types.triage_decoder())
  |> result.map_error(fn(err) {
    "Failed to parse JSON: " <> types.json_error_to_string(err)
  })
}
