//// Agent executor runs Claude prompts in worktrees with real subprocess execution.

import audit
import claude_code_client
import gleam/result
import gleam/string
import shellout
import simplifile

pub type AgentResult {
  AgentSuccess(output: String, artifacts: List(String))
  AgentFailure(reason: String)
}

pub type ExecutionMode {
  ApiMode
  CliMode
}

pub type ExecutionContext {
  ExecutionContext(
    worktree_path: String,
    task_id: String,
    task_spec: String,
    iteration: Int,
    mode: ExecutionMode,
  )
}

pub fn execute_agent_task(ctx: ExecutionContext) -> Result(AgentResult, String) {
  case ctx.mode {
    ApiMode -> execute_via_api(ctx)
    CliMode -> execute_via_cli(ctx)
  }
}

fn execute_via_api(_ctx: ExecutionContext) -> Result(AgentResult, String) {
  Error("API mode disabled - use CLI mode only")
}

fn execute_via_cli(ctx: ExecutionContext) -> Result(AgentResult, String) {
  let config = claude_code_client.new_claude_code_config(ctx.worktree_path, 20)

  let prompt = build_cli_prompt(ctx.task_spec, ctx.iteration)

  use cli_result <- result.try(claude_code_client.execute_task(config, prompt))

  case cli_result {
    claude_code_client.CliSuccess(output:, turns_used:, ..) -> {
      let _ =
        audit.log_cli_run(
          ctx.task_id,
          ctx.iteration,
          "implement",
          output,
          turns_used,
        )
      let artifacts = collect_artifacts(ctx.worktree_path)
      Ok(AgentSuccess(output:, artifacts:))
    }
    claude_code_client.CliFailure(reason:, ..) -> {
      Error("CLI execution failed: " <> reason)
    }
  }
}

pub fn execute_with_streaming(
  ctx: ExecutionContext,
  on_progress: fn(String) -> Nil,
) -> Result(AgentResult, String) {
  case ctx.mode {
    ApiMode -> execute_streaming_api(ctx, on_progress)
    CliMode -> execute_streaming_cli(ctx, on_progress)
  }
}

fn execute_streaming_api(
  _ctx: ExecutionContext,
  _on_progress: fn(String) -> Nil,
) -> Result(AgentResult, String) {
  Error("API mode disabled - use CLI mode only")
}

fn execute_streaming_cli(
  ctx: ExecutionContext,
  on_progress: fn(String) -> Nil,
) -> Result(AgentResult, String) {
  let config = claude_code_client.new_claude_code_config(ctx.worktree_path, 20)

  let prompt = build_cli_prompt(ctx.task_spec, ctx.iteration)

  use cli_result <- result.try(claude_code_client.execute_streaming(
    config,
    prompt,
    on_progress,
  ))

  case cli_result {
    claude_code_client.CliSuccess(output:, turns_used:, ..) -> {
      let _ =
        audit.log_cli_run(
          ctx.task_id,
          ctx.iteration,
          "implement",
          output,
          turns_used,
        )
      let artifacts = collect_artifacts(ctx.worktree_path)
      Ok(AgentSuccess(output:, artifacts:))
    }
    claude_code_client.CliFailure(reason:, ..) -> {
      Error("CLI streaming failed: " <> reason)
    }
  }
}

fn build_cli_prompt(task_spec: String, iteration: Int) -> String {
  let iter_context = case iteration {
    1 -> "First attempt."
    _ ->
      "Iteration " <> string.inspect(iteration) <> ". Previous attempt failed."
  }

  iter_context <> " Implement: " <> task_spec <> "

Run tests with 'gleam test'. If tests pass, commit with 'git add . && git commit -m \"TCR: implemented\"'.
If tests fail, revert with 'git reset --hard HEAD'.
Commit only when all tests pass."
}

fn collect_artifacts(worktree_path: String) -> List(String) {
  case simplifile.read_directory(worktree_path <> "/src") {
    Ok(files) -> files
    Error(_) -> []
  }
}

pub fn run_tests_in_worktree(worktree_path: String) -> Result(Bool, String) {
  case shellout.command("gleam", ["test"], worktree_path, []) {
    Ok(output) ->
      case
        string.contains(output, "0 failed")
        || string.contains(output, "All tests passed")
      {
        True -> Ok(True)
        False -> Ok(False)
      }
    Error(_) -> Error("Failed to run tests")
  }
}

pub fn commit_changes(
  worktree_path: String,
  message: String,
) -> Result(Nil, String) {
  case shellout.command("git", ["add", "."], worktree_path, []) {
    Ok(_) ->
      case
        shellout.command("git", ["commit", "-m", message], worktree_path, [])
      {
        Ok(_) -> Ok(Nil)
        Error(_) -> Error("Git commit failed")
      }
    Error(_) -> Error("Git add failed")
  }
}

pub fn revert_changes(worktree_path: String) -> Result(Nil, String) {
  case shellout.command("git", ["reset", "--hard", "HEAD"], worktree_path, []) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Git reset failed")
  }
}
