import agent_executor
import gleeunit/should

// === AgentResult Type Behavior ===

pub fn agent_success_captures_output_test() {
  let result = agent_executor.AgentSuccess(output: "Implementation complete", artifacts: [])
  case result {
    agent_executor.AgentSuccess(output: "Implementation complete", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn agent_success_captures_artifacts_test() {
  let result = agent_executor.AgentSuccess(output: "done", artifacts: ["main.gleam", "test.gleam"])
  case result {
    agent_executor.AgentSuccess(artifacts: ["main.gleam", "test.gleam"], ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn agent_success_can_have_empty_artifacts_test() {
  let result = agent_executor.AgentSuccess(output: "done", artifacts: [])
  case result {
    agent_executor.AgentSuccess(artifacts: [], ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn agent_failure_captures_reason_test() {
  let result = agent_executor.AgentFailure(reason: "tests failed")
  case result {
    agent_executor.AgentFailure(reason: "tests failed") -> should.be_true(True)
    _ -> should.fail()
  }
}

// === ExecutionMode Type Behavior ===

pub fn execution_mode_api_mode_exists_test() {
  let mode = agent_executor.ApiMode
  case mode {
    agent_executor.ApiMode -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn execution_mode_cli_mode_exists_test() {
  let mode = agent_executor.CliMode
  case mode {
    agent_executor.CliMode -> should.be_true(True)
    _ -> should.fail()
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
    agent_executor.ExecutionContext(worktree_path: "/workspace/my-task", ..) ->
      should.be_true(True)
    _ -> should.fail()
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
    agent_executor.ExecutionContext(task_id: "task-abc-123", ..) -> should.be_true(True)
    _ -> should.fail()
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
    agent_executor.ExecutionContext(task_spec: "Implement the login feature with OAuth", ..) ->
      should.be_true(True)
    _ -> should.fail()
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
    agent_executor.ExecutionContext(iteration: 5, ..) -> should.be_true(True)
    _ -> should.fail()
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
    agent_executor.ExecutionContext(mode: agent_executor.ApiMode, ..) -> should.be_true(True)
    _ -> should.fail()
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
    agent_executor.AgentFailure(..) -> should.fail()
  }

  case failure {
    agent_executor.AgentFailure(..) -> should.be_true(True)
    agent_executor.AgentSuccess(..) -> should.fail()
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
    agent_executor.ExecutionContext(iteration: 1, ..) -> should.be_true(True)
    _ -> should.fail()
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
    agent_executor.ExecutionContext(iteration: 3, ..) -> should.be_true(True)
    _ -> should.fail()
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
    _ -> should.fail()
  }
}

// === AgentSuccess with Multiple Artifacts ===

pub fn agent_success_preserves_artifact_order_test() {
  let result = agent_executor.AgentSuccess(
    output: "done",
    artifacts: ["a.gleam", "b.gleam", "c.gleam"],
  )
  case result {
    agent_executor.AgentSuccess(artifacts: ["a.gleam", "b.gleam", "c.gleam"], ..) ->
      should.be_true(True)
    _ -> should.fail()
  }
}
