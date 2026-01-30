import gleam/json
import gleam/list
import gleam/option
import gleam/result
import llm_router
import tdd15/state
import tdd15/types

fn is_passed_check(check: types.FPGateCheck) -> Bool {
  case check.result {
    option.Some(True) -> True
    _ -> False
  }
}

pub fn execute_phase10_fp_gates(
  config: types.PhaseConfig,
) -> Result(types.FPGateResult, String) {
  let bead_id = config.bead_id
  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 10, state.InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)
  let checks = [
    types.FPGateCheck(
      id: 1,
      name: "Immutability",
      description: "No mutable state",
      result: option.None,
    ),
    types.FPGateCheck(
      id: 2,
      name: "Purity",
      description: "Functions are pure",
      result: option.None,
    ),
    types.FPGateCheck(
      id: 3,
      name: "No Panic",
      description: "No panics",
      result: option.None,
    ),
    types.FPGateCheck(
      id: 4,
      name: "Exhaustive Match",
      description: "Exhaustive matching",
      result: option.None,
    ),
    types.FPGateCheck(
      id: 5,
      name: "Result Types",
      description: "Result types",
      result: option.None,
    ),
  ]
  let prompt = "Phase 10 FP Gates"
  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Reviewer)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 1500,
        temperature: 0.2,
      ),
      llm.Reviewer,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(10)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])
      let assert Ok(Nil) =
        state.save_phase_output(bead_id, "phase10_fp_gates", output_data)
      case parse_fp_gate_response(response.content) {
        Ok(fp_result) -> {
          let passed = calculate_fp_gate_pass(fp_result)
          case passed {
            True -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated =
                state.update_phase_status(progress, 10, state.Completed)
              let updated = state.mark_gate_result(updated, 10, True)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)
              Ok(fp_result)
            }
            False -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated =
                state.update_phase_status(progress, 10, state.Failed)
              let updated = state.increment_attempt(updated, 10)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)
              Ok(fp_result)
            }
          }
        }
        Error(_) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 10, state.Failed)
          let updated = state.increment_attempt(updated, 10)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)
          Ok(types.FPGateResult(passed: False, checks: checks))
        }
      }
    }
    Error(_) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 10, state.Failed)
      let updated = state.increment_attempt(updated, 10)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)
      Ok(types.FPGateResult(passed: False, checks: checks))
    }
  }
}

fn parse_fp_gate_response(
  json_string: String,
) -> Result(types.FPGateResult, String) {
  json.parse(from: json_string, using: types.fp_gate_decoder())
  |> result.map_error(fn(err) {
    "Failed to parse JSON: " <> types.json_error_to_string(err)
  })
}

fn calculate_fp_gate_pass(fp_result: types.FPGateResult) -> Bool {
  let pass_count =
    fp_result.checks |> list.filter(is_passed_check) |> list.length
  pass_count >= 4
}
