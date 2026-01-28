import gleam/int
import gleam/json
import gleam/option
import gleam/result
import gleam/string
import llm
import llm_router
import tdd15/state.{Completed, Failed, InProgress}
import tdd15/types

pub fn execute_phase0_triage(config: types.PhaseConfig) -> Result(types.QualityGateResult, String) {
  let bead_id = config.bead_id
  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 0, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)
  let bead_context = case state.load_bead_context(bead_id) {
    Ok(ctx) -> ctx
    _ -> {
      let assert Ok(_) = state.init_cache(bead_id)
      state.BeadContext(id: bead_id, title: "Triage Assessment", requirements: [], context: "Assess complexity for routing")
    }
  }
  let prompt = "Phase 0 TRIAGE: Assess complexity for routing.
 Bead Context:
Title: " <> bead_context.title <> "
Requirements: " <> string.join(bead_context.requirements, "\n") <> "
Assess the following criteria:
1. Requirements count: How many distinct requirements?
2. File estimate: How many source files will be modified/created?
3. Dependency depth: How deep are the dependencies?
4. Domain complexity: Is this a simple CRUD, medium business logic, or complex algorithm?
Respond in JSON format:
{
  \"criteria_count\": <number>,
  \"file_estimate\": <number>,
  \"dependency_depth\": <number>,
  \"complexity\": \"SIMPLE\" | \"MEDIUM\" | \"COMPLEX\",
  \"reasoning\": \"<brief explanation>\"
}"
  case llm_router.call(config.llm_config, llm.LLMRequest(prompt: prompt, system_prompt: option.Some(llm.system_prompt(llm.Architect)), model: "claude-3-5-sonnet-20241022", max_tokens: 1000, temperature: 0.3), llm.Architect) {
    Ok(response) -> {
      let output_data = json.object([#("phase", json.int(0)), #("prompt_tokens", json.int(response.usage.prompt_tokens)), #("completion_tokens", json.int(response.usage.completion_tokens)), #("content", json.string(response.content))])
      let assert Ok(Nil) = state.save_phase_output(bead_id, "phase0_triage", output_data)
      case parse_triage_response(response.content) {
        Ok(_) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 0, Completed)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)
          let questions = [types.QualityGateQuestion(id: 1, question: "Criteria count assessed?", criteria: "Number of requirements identified", result: option.Some(True)), types.QualityGateQuestion(id: 2, question: "File estimate determined?", criteria: "Source file count estimated", result: option.Some(True)), types.QualityGateQuestion(id: 3, question: "Dependency depth analyzed?", criteria: "Dependency depth calculated", result: option.Some(True)), types.QualityGateQuestion(id: 4, question: "Complexity routed?", criteria: "SIMPLE/MEDIUM/COMPLEX assigned", result: option.Some(True))]
          Ok(types.QualityGateResult(passed: True, questions: questions, score: 4))
        }
        Error(_) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 0, Failed)
          let updated = state.increment_attempt(updated, 0)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)
          Ok(types.QualityGateResult(passed: False, questions: [], score: 0))
        }
      }
    }
    Error(_) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 0, Failed)
      let updated = state.increment_attempt(updated, 0)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)
      Ok(types.QualityGateResult(passed: False, questions: [], score: 0))
    }
  }
}

fn parse_triage_response(json_string: String) -> Result(types.TriageResult, String) {
  json.parse(from: json_string, using: types.triage_decoder()) |> result.map_error(fn(err) { "Failed to parse JSON: " <> types.json_error_to_string(err) })
}
