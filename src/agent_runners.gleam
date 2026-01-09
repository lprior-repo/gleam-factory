//// Agent runners - Execute real Claude agents with full execution in worktrees.

import acp_client
import agent_executor
import factory_loop
import gleam/io
import gleam/string
import types

/// Runs implementer agent with real Claude API in worktree.
pub fn run_implementer(
  state: factory_loop.FactoryLoopState,
  api_key: String,
) -> factory_loop.Event {
  run_implementer_with_mode(state, api_key, agent_executor.ApiMode)
}

/// Runs implementer agent with CLI mode.
pub fn run_implementer_cli(
  state: factory_loop.FactoryLoopState,
  api_key: String,
) -> factory_loop.Event {
  run_implementer_with_mode(state, api_key, agent_executor.CliMode)
}

fn run_implementer_with_mode(
  state: factory_loop.FactoryLoopState,
  _api_key: String,
  mode: agent_executor.ExecutionMode,
) -> factory_loop.Event {
  let ctx =
    agent_executor.ExecutionContext(
      worktree_path: state.workspace_path,
      task_id: state.task_id,
      task_spec: state.task_spec,
      iteration: state.iteration,
      mode:,
    )

  case agent_executor.execute_agent_task(ctx) {
    Ok(agent_executor.AgentSuccess(output:, artifacts:)) -> {
      io.println("Agent output: " <> string.slice(output, 0, 200) <> "...")
      io.println("Artifacts: " <> string.inspect(artifacts))

      case agent_executor.run_tests_in_worktree(state.workspace_path) {
        Ok(True) -> {
          case
            agent_executor.commit_changes(
              state.workspace_path,
              "TCR: " <> state.task_spec,
            )
          {
            Ok(_) -> factory_loop.TestPassed
            Error(e) -> {
              io.println("Commit failed: " <> e)
              factory_loop.TestFailed
            }
          }
        }
        Ok(False) -> {
          let _ = agent_executor.revert_changes(state.workspace_path)
          factory_loop.TestFailed
        }
        Error(e) -> {
          io.println("Test run error: " <> e)
          factory_loop.TestFailed
        }
      }
    }
    Ok(agent_executor.AgentFailure(reason:)) -> {
      io.println("Agent failure: " <> reason)
      factory_loop.TestFailed
    }
    Error(e) -> {
      io.println("Executor error: " <> e)
      factory_loop.TestFailed
    }
  }
}

/// Runs auditor agent with real Claude API to review code.
pub fn run_auditor(
  state: factory_loop.FactoryLoopState,
  api_key: String,
) -> factory_loop.Event {
  run_auditor_with_mode(state, api_key, agent_executor.ApiMode)
}

/// Runs auditor agent with CLI mode.
pub fn run_auditor_cli(
  state: factory_loop.FactoryLoopState,
  api_key: String,
) -> factory_loop.Event {
  run_auditor_with_mode(state, api_key, agent_executor.CliMode)
}

fn run_auditor_with_mode(
  state: factory_loop.FactoryLoopState,
  _api_key: String,
  mode: agent_executor.ExecutionMode,
) -> factory_loop.Event {
  case mode {
    agent_executor.ApiMode -> run_auditor_acp(state)
    agent_executor.CliMode -> run_auditor_cli_mode(state)
  }
}

fn run_auditor_acp(state: factory_loop.FactoryLoopState) -> factory_loop.Event {
  let client = types.new_acp_client("http://localhost:3001")
  let store = types.new_update_store()

  case
    acp_client.handle_create_session(acp_client.new("http://localhost:3001"))
  {
    Error(_) -> factory_loop.TestFailed
    Ok(#(session_id, _)) -> {
      let prompt = build_auditor_prompt(state)
      case acp_client.handle_send_prompt(client, store, session_id, prompt) {
        Ok(#(response, _)) -> parse_auditor_response(response)
        Error(_) -> factory_loop.TestFailed
      }
    }
  }
}

fn run_auditor_cli_mode(
  state: factory_loop.FactoryLoopState,
) -> factory_loop.Event {
  let ctx =
    agent_executor.ExecutionContext(
      worktree_path: state.workspace_path,
      task_id: state.task_id,
      task_spec: "Review: " <> state.task_spec,
      iteration: state.iteration,
      mode: agent_executor.CliMode,
    )

  case agent_executor.execute_agent_task(ctx) {
    Ok(agent_executor.AgentSuccess(output:, ..)) ->
      parse_auditor_response(output)
    _ -> factory_loop.TestFailed
  }
}

fn build_auditor_prompt(state: factory_loop.FactoryLoopState) -> String {
  "Review code for task: "
  <> state.task_spec
  <> "\nWorkspace: "
  <> state.workspace_path
  <> "\nIteration: "
  <> string.inspect(state.iteration)
  <> "\n\nCheck for test coverage, type safety, and correctness. Respond with APPROVE if good, or suggest improvements."
}

fn parse_auditor_response(response: String) -> factory_loop.Event {
  let lower = string.lowercase(response)
  case
    string.contains(lower, "approve")
    || string.contains(lower, "looks good")
    || string.contains(lower, "requirements_complete")
  {
    True -> factory_loop.TestPassed
    False -> factory_loop.TestFailed
  }
}

/// Runs implementer with streaming progress updates.
pub fn run_implementer_streaming(
  state: factory_loop.FactoryLoopState,
  api_key: String,
  on_progress: fn(String) -> Nil,
) -> factory_loop.Event {
  run_implementer_streaming_with_mode(
    state,
    api_key,
    on_progress,
    agent_executor.ApiMode,
  )
}

/// Runs implementer with CLI streaming.
pub fn run_implementer_streaming_cli(
  state: factory_loop.FactoryLoopState,
  api_key: String,
  on_progress: fn(String) -> Nil,
) -> factory_loop.Event {
  run_implementer_streaming_with_mode(
    state,
    api_key,
    on_progress,
    agent_executor.CliMode,
  )
}

fn run_implementer_streaming_with_mode(
  state: factory_loop.FactoryLoopState,
  _api_key: String,
  on_progress: fn(String) -> Nil,
  mode: agent_executor.ExecutionMode,
) -> factory_loop.Event {
  let ctx =
    agent_executor.ExecutionContext(
      worktree_path: state.workspace_path,
      task_id: state.task_id,
      task_spec: state.task_spec,
      iteration: state.iteration,
      mode:,
    )

  case agent_executor.execute_with_streaming(ctx, on_progress) {
    Ok(agent_executor.AgentSuccess(..)) -> {
      case agent_executor.run_tests_in_worktree(state.workspace_path) {
        Ok(True) -> {
          case
            agent_executor.commit_changes(
              state.workspace_path,
              "TCR: " <> state.task_spec,
            )
          {
            Ok(_) -> factory_loop.TestPassed
            Error(_) -> factory_loop.TestFailed
          }
        }
        _ -> {
          let _ = agent_executor.revert_changes(state.workspace_path)
          factory_loop.TestFailed
        }
      }
    }
    _ -> factory_loop.TestFailed
  }
}
