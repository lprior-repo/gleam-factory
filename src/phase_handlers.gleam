//// Phase handlers for factory loop phases.
////
//// Handles TCR phases: Implementing, TcrChecking, Reviewing, Pushing, Rebasing.

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
  process.run_command(
    "jj",
    ["commit", "-m", "Auto-commit from TCR"],
    workspace_path,
  )
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
