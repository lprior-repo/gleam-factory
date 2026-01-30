import gleam/json
import gleam/option
import gleam/result
import gleam/string
import llm
import llm_router
import tdd15/state.{Completed, Failed, InProgress}
import tdd15/types

pub fn execute_phase15_landing(
  config: types.PhaseConfig,
) -> Result(types.LandingResult, String) {
  let bead_id = config.bead_id
  use progress <- result.try(
    state.load_progress(bead_id)
    |> result.map_error(fn(_) { "Failed to load progress for phase15" }),
  )
  let updated = state.update_phase_status(progress, 15, InProgress)
  use _ <- result.try(
    state.save_progress(bead_id, updated)
    |> result.map_error(fn(_) { "Failed to save progress for phase15" }),
  )
  let bead_context = case state.load_bead_context(bead_id) {
    Ok(ctx) -> ctx
    _ ->
      state.BeadContext(
        id: bead_id,
        title: "Landing",
        requirements: [],
        context: "Final phase: commit, push, and close",
      )
  }
  let prompt = "Phase 15 LANDING: Final verification and cleanup.
 Bead Context:
 Title: " <> bead_context.title <> "
 Requirements: " <> string.join(bead_context.requirements, "\n") <> "
 Verify the following:
 1. All changes committed to VCS
 2. All changes pushed to remote
 3. All tests passing
 4. All documentation updated
 5. No dead code or unused imports
 6. All phases completed successfully
 Respond in JSON format:
 {
   \"committed\": <bool>,
   \"pushed\": <bool>,
   \"tests_passing\": <bool>,
   \"documentation_updated\": <bool>,
   \"cleanup_complete\": <bool>,
   \"ready_to_close\": <bool>,
   \"message\": \"<final status message>\"
 }"
  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Architect)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 1000,
        temperature: 0.3,
      ),
      llm.Architect,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(15)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])
      use _ <- result.try(
        state.save_phase_output(bead_id, "phase15_landing", output_data)
        |> result.map_error(fn(_) { "Failed to save phase15 output" }),
      )
      case parse_landing_response(response.content) {
        Ok(landing) -> {
          use progress <- result.try(
            state.load_progress(bead_id)
            |> result.map_error(fn(_) { "Failed to load progress" }),
          )
          let updated = state.update_phase_status(progress, 15, Completed)
          use _ <- result.try(
            state.save_progress(bead_id, updated)
            |> result.map_error(fn(_) { "Failed to save progress" }),
          )
          Ok(types.LandingResult(
            committed: landing.committed,
            pushed: landing.pushed,
            bead_closed: landing.committed && landing.pushed,
            message: landing.message,
          ))
        }
        Error(_) -> {
          use progress <- result.try(
            state.load_progress(bead_id)
            |> result.map_error(fn(_) { "Failed to load progress" }),
          )
          let updated = state.update_phase_status(progress, 15, Failed)
          let updated = state.increment_attempt(updated, 15)
          use _ <- result.try(
            state.save_progress(bead_id, updated)
            |> result.map_error(fn(_) { "Failed to save progress" }),
          )
          Ok(types.LandingResult(
            committed: False,
            pushed: False,
            bead_closed: False,
            message: "Failed to parse landing response",
          ))
        }
      }
    }
    Error(_) -> {
      use progress <- result.try(
        state.load_progress(bead_id)
        |> result.map_error(fn(_) { "Failed to load progress" }),
      )
      let updated = state.update_phase_status(progress, 15, Failed)
      let updated = state.increment_attempt(updated, 15)
      use _ <- result.try(
        state.save_progress(bead_id, updated)
        |> result.map_error(fn(_) { "Failed to save progress" }),
      )
      Ok(types.LandingResult(
        committed: False,
        pushed: False,
        bead_closed: False,
        message: "LLM call failed",
      ))
    }
  }
}

fn parse_landing_response(
  json_string: String,
) -> Result(types.LandingResult, String) {
  json.parse(from: json_string, using: types.landing_decoder())
  |> result.map_error(fn(err) {
    "Failed to parse JSON: " <> types.json_error_to_string(err)
  })
}
