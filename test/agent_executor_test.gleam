import agent_executor
import gleeunit/should

// === AgentResult Type Behavior ===

pub fn agent_success_captures_output_test() {
  let result = agent_executor.AgentSuccess(output: "Implementation complete", artifacts: [])
  case result {
    agent_executor.AgentSuccess(output: output, ..) -> output |> should.equal("Implementation complete")
  }
}

pub fn agent_success_captures_artifacts_test() {
  let result = agent_executor.AgentSuccess(output: "done", artifacts: ["main.gleam", "test.gleam"])
  case result {
    agent_executor.AgentSuccess(artifacts: artifacts, ..) -> artifacts |> should.equal(["main.gleam", "test.gleam"])
  }
}

pub fn agent_success_can_have_empty_artifacts_test() {
  let result = agent_executor.AgentSuccess(output: "done", artifacts: [])
  case result {
    agent_executor.AgentSuccess(artifacts: artifacts, ..) -> artifacts |> should.equal([])
  }
}

pub fn agent_failure_captures_reason_test() {
  let result = agent_executor.AgentFailure(reason: "tests failed")
  case result {
    agent_executor.AgentFailure(reason: reason) -> reason |> should.equal("tests failed")
  }
}

// === ExecutionMode Type Behavior ===

pub fn execution_mode_api_mode_exists_test() {
  let mode = agent_executor.ApiMode
  case mode {
    agent_executor.ApiMode -> should.be_true(True)
  }
}

pub fn execution_mode_cli_mode_exists_test() {
  let mode = agent_executor.CliMode
  case mode {
    agent_executor.CliMode -> should.be_true(True)
  }
}

// === ExecutionContext Type Behavior ===

pub fn execution_context_captures_worktree_path_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/workspace/my-task",
    task_id: "task-1",
    task_spec: "Implement feature X",
    iteration: 1,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(worktree_path: value, ..) -> value |> should.equal("/workspace/my-task")
  }
}

pub fn execution_context_captures_task_id_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "task-abc-123",
    task_spec: "spec",
    iteration: 1,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(task_id: task_id, ..) ->
      task_id |> should.equal("task-abc-123")
  }
}

pub fn execution_context_captures_task_spec_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "Implement the login feature with OAuth",
    iteration: 1,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(task_spec: value, ..) -> value |> should.equal("Implement the login feature with OAuth")
  }
}

pub fn execution_context_captures_iteration_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "spec",
    iteration: 5,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(iteration: iteration, ..) ->
      iteration |> should.equal(5)
  }
}

pub fn execution_context_captures_mode_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "spec",
    iteration: 1,
    mode: agent_executor.ApiMode,
  )
  case ctx {
    agent_executor.ExecutionContext(mode: mode, ..) ->
      mode |> should.equal(agent_executor.ApiMode)
  }
}

// === API Mode Behavior ===

pub fn execute_agent_task_rejects_api_mode_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "spec",
    iteration: 1,
    mode: agent_executor.ApiMode,
  )
  agent_executor.execute_agent_task(ctx)
  |> should.be_error
}

pub fn execute_with_streaming_rejects_api_mode_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "spec",
    iteration: 1,
    mode: agent_executor.ApiMode,
  )
  let on_progress = fn(_msg: String) { Nil }
  agent_executor.execute_with_streaming(ctx, on_progress)
  |> should.be_error
}

// === AgentResult Discrimination ===

pub fn can_discriminate_success_from_failure_test() {
  let success = agent_executor.AgentSuccess(output: "ok", artifacts: [])
  let failure = agent_executor.AgentFailure(reason: "failed")

  case success {
    agent_executor.AgentSuccess(..) -> should.be_true(True)
  }

  case failure {
    agent_executor.AgentFailure(..) -> should.be_true(True)
  }
}

// === ExecutionContext with Different Iterations ===

pub fn first_iteration_context_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "spec",
    iteration: 1,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(iteration: iteration, ..) ->
      iteration |> should.equal(1)
  }
}

pub fn retry_iteration_context_test() {
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: "spec",
    iteration: 3,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(iteration: iteration, ..) ->
      iteration |> should.equal(3)
  }
}

// === Multiline Task Spec ===

pub fn execution_context_handles_multiline_task_spec_test() {
  let spec = "1. Implement the feature\n2. Add tests\n3. Update docs"
  let ctx = agent_executor.ExecutionContext(
    worktree_path: "/path",
    task_id: "id",
    task_spec: spec,
    iteration: 1,
    mode: agent_executor.CliMode,
  )
  case ctx {
    agent_executor.ExecutionContext(task_spec: s, ..) -> {
      s |> should.equal(spec)
    }
  }
}

// === AgentSuccess with Multiple Artifacts ===

pub fn agent_success_preserves_artifact_order_test() {
  let result = agent_executor.AgentSuccess(
    output: "done",
    artifacts: ["a.gleam", "b.gleam", "c.gleam"],
  )
  case result {
    agent_executor.AgentSuccess(artifacts: artifacts, ..) ->
      artifacts |> should.equal(["a.gleam", "b.gleam", "c.gleam"])
  }
}
