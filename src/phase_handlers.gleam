//// Phase handlers for factory loop phases.
////
//// Handles transitions through all phases: Auditing, VerifyingRed, Implementing, TcrChecking, Refactoring, Reviewing, Pushing, Rebasing.

import gleam/option
import gleam/result
import factory_loop
import llm
import llm_router
import verification_gauntlet
import process

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

pub fn handle_tcr_checking_phase(
  state: factory_loop.FactoryLoopState,
) -> factory_loop.Event {
  case verification_gauntlet.run_gauntlet(state.workspace_path, "gleam") {
    Ok(verification_gauntlet.Passed(_)) -> {
      // Tests pass - commit changes
      case tcr_commit(state.workspace_path) {
        Ok(_) -> factory_loop.TestPassed
        Error(_) -> factory_loop.TestFailed
      }
    }
    Ok(verification_gauntlet.Failed(_, _)) -> {
      // Tests fail - revert changes
      case tcr_revert(state.workspace_path) {
        Ok(_) -> factory_loop.TestFailed
        Error(_) -> factory_loop.TestFailed
      }
    }
    Error(_) -> factory_loop.TestFailed
  }
}

fn tcr_commit(workspace_path: String) -> Result(Nil, String) {
  process.run_command("jj", ["commit", "-m", "Auto-commit from TCR"], workspace_path)
  |> result.try(fn(result) {
    case result {
      process.Success(_, _, _) -> Ok(Nil)
      process.Failure(err, _) -> Error(err)
    }
  })
}

fn tcr_revert(workspace_path: String) -> Result(Nil, String) {
  process.run_command("jj", ["restore"], workspace_path)
  |> result.try(fn(result) {
    case result {
      process.Success(_, _, _) -> Ok(Nil)
      process.Failure(err, _) -> Error(err)
    }
  })
}
