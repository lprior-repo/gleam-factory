//// Phase handlers for factory loop phases.
////
//// Handles transitions through Refactoring, Reviewing, and Implementing phases.

import gleam/option
import factory_loop
import llm
import llm_router
import verification_gauntlet

pub fn handle_refactoring_phase(
  state: factory_loop.FactoryLoopState,
  config: llm_router.RouterConfig,
) -> factory_loop.Event {
  case llm_router.call(
    config,
    llm.LLMRequest(
      prompt: "Refactor: " <> state.task_spec,
      system_prompt: option.Some(llm.system_prompt(llm.Architect)),
      model: "claude-3-5-sonnet-20241022",
      max_tokens: 2000,
      temperature: 0.3,
    ),
    llm.Architect,
  ) {
    Ok(_response) -> {
      case verification_gauntlet.run_gauntlet(state.workspace_path, "gleam") {
        Ok(verification_gauntlet.Passed(_)) -> factory_loop.TestPassed
        Ok(verification_gauntlet.Failed(_, _)) -> factory_loop.TestFailed
        Error(_) -> factory_loop.TestFailed
      }
    }
    Error(_) -> factory_loop.TestFailed
  }
}

pub fn handle_reviewing_phase(
  state: factory_loop.FactoryLoopState,
  config: llm_router.RouterConfig,
) -> factory_loop.Event {
  case llm_router.call(
    config,
    llm.LLMRequest(
      prompt: "Review implementation of: " <> state.task_spec,
      system_prompt: option.Some(llm.system_prompt(llm.Reviewer)),
      model: "claude-3-5-sonnet-20241022",
      max_tokens: 1500,
      temperature: 0.2,
    ),
    llm.Reviewer,
  ) {
    Ok(_response) -> {
      case verification_gauntlet.run_gauntlet(state.workspace_path, "gleam") {
        Ok(verification_gauntlet.Passed(_)) -> factory_loop.TestPassed
        Ok(verification_gauntlet.Failed(_, _)) -> factory_loop.TestFailed
        Error(_) -> factory_loop.TestFailed
      }
    }
    Error(_) -> factory_loop.TestFailed
  }
}

pub fn handle_implementing_phase(
  state: factory_loop.FactoryLoopState,
  config: llm_router.RouterConfig,
  max_attempts: Int,
) -> factory_loop.Event {
  case state.iteration > max_attempts {
    True -> factory_loop.MaxIterationsReached
    False -> {
      case llm_router.call(
        config,
        llm.LLMRequest(
          prompt: "Implement: " <> state.task_spec,
          system_prompt: option.Some(llm.system_prompt(llm.Implementer)),
          model: "claude-3-5-sonnet-20241022",
          max_tokens: 3000,
          temperature: 0.4,
        ),
        llm.Implementer,
      ) {
        Ok(_response) -> {
          case verification_gauntlet.run_gauntlet(state.workspace_path, "gleam") {
            Ok(verification_gauntlet.Passed(_)) -> factory_loop.TestPassed
            Ok(verification_gauntlet.Failed(_, _)) -> factory_loop.TestFailed
            Error(_) -> factory_loop.TestFailed
          }
        }
        Error(_) -> factory_loop.TestFailed
      }
    }
  }
}
