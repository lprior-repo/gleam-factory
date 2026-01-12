//// Phase handlers for factory loop phases.

import factory_loop
import gleam/option
import gleam/result
import gleam/string
import llm
import llm_router
import process
import simplifile
import verification_gauntlet

pub fn handle_reviewing_phase(
  state: factory_loop.FactoryLoopState,
  config: llm_router.RouterConfig,
) -> factory_loop.Event {
  case
    llm_router.call(
      config,
      llm.LLMRequest(
        prompt: "Review implementation of: " <> state.task_spec,
        system_prompt: option.Some(llm.system_prompt(llm.Reviewer)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 1500,
        temperature: 0.2,
      ),
      llm.Reviewer,
    )
  {
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
      case
        llm_router.call(
          config,
          llm.LLMRequest(
            prompt: "Implement: " <> state.task_spec,
            system_prompt: option.Some(llm.system_prompt(llm.Implementer)),
            model: "claude-3-5-sonnet-20241022",
            max_tokens: 3000,
            temperature: 0.4,
          ),
          llm.Implementer,
        )
      {
        Ok(_response) -> {
          case
            verification_gauntlet.run_gauntlet(state.workspace_path, "gleam")
          {
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


pub fn handle_write_file(
  path: String,
  content: String,
  role: llm.Role,
) -> Result(Nil, String) {
  case is_path_safe(path) {
    False -> Error("Path escapes workspace: " <> path)
    True -> {
      case role, is_src_path(path), is_test_path(path) {
        llm.Implementer, True, False ->
          simplifile.write(path, content)
          |> result.map_error(fn(_) { "write failed" })
        llm.Implementer, _, _ -> Error("Implementer: src/ only")
        llm.Architect, _, _ -> Error("Architect: read-only")
        llm.Reviewer, False, True ->
          simplifile.write(path, content)
          |> result.map_error(fn(_) { "write failed" })
        llm.Reviewer, _, _ -> Error("Reviewer: test/ only")
        llm.Auditor, _, _ -> Error("Auditor: read-only")
      }
    }
  }
}

fn is_path_safe(path: String) -> Bool {
  !string.contains(path, "..") && !string.starts_with(path, "/")
}

fn is_src_path(path: String) -> Bool {
  string.starts_with(path, "src/")
}

fn is_test_path(path: String) -> Bool {
  string.starts_with(path, "test/")
}
