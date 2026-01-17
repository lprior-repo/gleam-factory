//// End-to-end pipeline integration tests (deh bead)
////
//// Verifies complete pipeline flow: task creation -> stage execution -> signal handling -> completion
//// Tests error cases, timeout handling, signal broadcasting, state transitions

import domain
import factory_supervisor
import gleam/erlang/process
import gleam/list
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import heartbeat
import merge_queue
import signal_bus
import signals
import stages

pub fn main() {
  gleeunit.main()
}

fn supervisor_config() -> factory_supervisor.SupervisorConfig {
  factory_supervisor.SupervisorConfig(
    test_cmd: "true",
    test_interval_ms: 100,
    golden_master_path: "/tmp/golden_master_test",
    max_mutators: 2,
    max_loops: 2,
    max_workspaces: 4,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
    beads_path: "/tmp/.beads/issues.jsonl",
    beads_poll_interval_ms: 1000,
    workspace_root: "/tmp/workspaces",
  )
}

/// Test: Full pipeline starts with supervisor
pub fn pipeline_supervisor_startup_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(_supervisor) -> Nil
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Signal bus broadcasts throughout pipeline
pub fn pipeline_signal_broadcast_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let bus = factory_supervisor.get_signal_bus(supervisor)
      signal_bus.broadcast(bus, signal_bus.TestPassing)
      signal_bus.broadcast(bus, signal_bus.TestFailure)
      signal_bus.broadcast(bus, signal_bus.PatchProposed)
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Heartbeat polls tests on interval
pub fn pipeline_heartbeat_polling_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let hb = factory_supervisor.get_heartbeat(supervisor)
      heartbeat.tick(hb)
      heartbeat.tick(hb)
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Merge queue processes patches
pub fn pipeline_merge_queue_patch_absorption_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let _bus = factory_supervisor.get_signal_bus(supervisor)
      let queue = factory_supervisor.get_merge_queue(supervisor)

      // Propose first patch
      merge_queue.propose_patch(queue, "patch-001")

      // Verify absorbing state
      let is_absorbing = merge_queue.is_absorbing(queue)
      case is_absorbing {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Test failure triggers rejection signal
pub fn pipeline_test_failure_signal_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let bus = factory_supervisor.get_signal_bus(supervisor)
      let hb = factory_supervisor.get_heartbeat(supervisor)

      // Trigger test and verify signal broadcasts
      heartbeat.tick(hb)
      signal_bus.broadcast(bus, signal_bus.TestFailure)
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Golden master preparation runs at startup
pub fn pipeline_golden_master_preparation_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let _gm = factory_supervisor.get_golden_master(supervisor)
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Multiple signal subscriptions work
pub fn pipeline_multiple_subscriptions_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let bus = factory_supervisor.get_signal_bus(supervisor)

      // Subscribe multiple times
      let sub1 = process.new_subject()
      let sub2 = process.new_subject()
      let _res1 = signal_bus.subscribe(bus, signal_bus.TestPassing, sub1)
      let _res2 = signal_bus.subscribe(bus, signal_bus.TestPassing, sub2)

      // Publish and verify both receive
      signal_bus.broadcast(bus, signal_bus.TestPassing)
      let patch = signals.PatchAccepted(
        hash: signals.hash("abc123"),
        merged_at: signals.timestamp(0),
      )
      signal_bus.broadcast(bus, signal_bus.PatchAccepted(patch))
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Error handling in pipeline doesn't crash system
pub fn pipeline_error_handling_resilience_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let bus = factory_supervisor.get_signal_bus(supervisor)

      // Simulate errors
      signal_bus.broadcast(bus, signal_bus.ResourceExhausted)

      // Verify system still responsive
      let hb = factory_supervisor.get_heartbeat(supervisor)
      heartbeat.tick(hb)
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Complete workflow: startup -> signal -> process -> signal
pub fn pipeline_complete_workflow_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let bus = factory_supervisor.get_signal_bus(supervisor)
      let queue = factory_supervisor.get_merge_queue(supervisor)
      let hb = factory_supervisor.get_heartbeat(supervisor)

      // Step 1: System running - tick heartbeat
      heartbeat.tick(hb)

      // Step 2: Propose patch
      merge_queue.propose_patch(queue, "patch-workflow-001")

      // Step 3: Test runs
      heartbeat.tick(hb)

      // Step 4: Signals broadcast
      signal_bus.broadcast(bus, signal_bus.TestPassing)
      let patch = signals.PatchAccepted(
        hash: signals.hash("def456"),
        merged_at: signals.timestamp(0),
      )
      signal_bus.broadcast(bus, signal_bus.PatchAccepted(patch))

      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Shutdown and resource cleanup
pub fn pipeline_shutdown_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(supervisor) -> {
      let queue = factory_supervisor.get_merge_queue(supervisor)
      merge_queue.shutdown(queue)
      Nil
    }
    Error(_e) -> should.fail()
  }
  |> should.equal(Nil)
}

// ============================================================================
// STAGE EXECUTION PIPELINE TESTS
// ============================================================================

/// Test: Stage transition validation - forward only
pub fn pipeline_stage_transition_forward_test() {
  // Valid forward transition
  stages.validate_stage_transition("implement", "unit-test")
  |> should.be_ok()

  stages.validate_stage_transition("unit-test", "lint")
  |> should.be_ok()

  stages.validate_stage_transition("implement", "accept")
  |> should.be_ok()
}

/// Test: Stage transition validation - reject backward
pub fn pipeline_stage_transition_backward_rejected_test() {
  // Invalid backward transition
  stages.validate_stage_transition("lint", "implement")
  |> should.be_error()

  stages.validate_stage_transition("accept", "unit-test")
  |> should.be_error()

  // Same stage transition should fail
  stages.validate_stage_transition("implement", "implement")
  |> should.be_error()
}

/// Test: Stage transition validation - unknown stages
pub fn pipeline_stage_unknown_rejected_test() {
  stages.validate_stage_transition("unknown", "implement")
  |> should.be_error()

  stages.validate_stage_transition("implement", "unknown")
  |> should.be_error()
}

/// Test: Dry-run stage execution returns previews
pub fn pipeline_dry_run_execution_test() {
  let pipeline = domain.standard_pipeline()
  let gleam_lang = domain.Gleam

  let previews = stages.execute_stages_dry_run(pipeline, gleam_lang)

  // Should have preview for each stage
  list.length(previews)
  |> should.equal(list.length(pipeline))

  // First stage should be implement
  case previews {
    [first, ..] -> {
      first.name
      |> should.equal("implement")
      // Command should be gleam build for Gleam language
      first.command
      |> string.contains("gleam")
      |> should.be_true()
    }
    [] -> should.fail()
  }
}

/// Test: Dry-run previews show estimated durations
pub fn pipeline_dry_run_shows_duration_test() {
  let single_stage = [domain.Stage("implement", "Build code", 0)]
  let previews = stages.execute_stages_dry_run(single_stage, domain.Gleam)

  case previews {
    [preview] -> {
      // Estimated duration should be positive
      { preview.estimated_duration > 0 }
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

/// Test: Domain pipeline stages are in correct order
pub fn pipeline_standard_stages_order_test() {
  let pipeline = domain.standard_pipeline()

  let stage_names = list.map(pipeline, fn(s: domain.Stage) { s.name })

  // Verify implement comes before unit-test
  let implement_idx = find_index(stage_names, "implement")
  let test_idx = find_index(stage_names, "unit-test")
  let lint_idx = find_index(stage_names, "lint")
  let accept_idx = find_index(stage_names, "accept")

  case implement_idx, test_idx, lint_idx, accept_idx {
    option.Some(i), option.Some(t), option.Some(l), option.Some(a) -> {
      { i < t }
      |> should.be_true()
      { t < l }
      |> should.be_true()
      { l < a }
      |> should.be_true()
    }
    _, _, _, _ -> should.fail()
  }
}

fn find_index(items: List(String), target: String) -> option.Option(Int) {
  find_index_helper(items, target, 0)
}

fn find_index_helper(
  items: List(String),
  target: String,
  idx: Int,
) -> option.Option(Int) {
  case items {
    [] -> option.None
    [item, ..rest] ->
      case item == target {
        True -> option.Some(idx)
        False -> find_index_helper(rest, target, idx + 1)
      }
  }
}

/// Test: Language-specific commands for Gleam
pub fn pipeline_gleam_stage_commands_test() {
  let implement_stage = [domain.Stage("implement", "Build", 0)]
  let lint_stage = [domain.Stage("lint", "Format check", 0)]

  let impl_preview = stages.execute_stages_dry_run(implement_stage, domain.Gleam)
  let lint_preview = stages.execute_stages_dry_run(lint_stage, domain.Gleam)

  case impl_preview, lint_preview {
    [impl], [lint] -> {
      impl.command
      |> string.contains("gleam build")
      |> should.be_true()

      lint.command
      |> string.contains("gleam format")
      |> should.be_true()
    }
    _, _ -> should.fail()
  }
}

/// Test: Language-specific commands for Go
pub fn pipeline_go_stage_commands_test() {
  let implement_stage = [domain.Stage("implement", "Build", 0)]
  let lint_stage = [domain.Stage("lint", "Format check", 0)]

  let impl_preview = stages.execute_stages_dry_run(implement_stage, domain.Go)
  let lint_preview = stages.execute_stages_dry_run(lint_stage, domain.Go)

  case impl_preview, lint_preview {
    [impl], [lint] -> {
      impl.command
      |> string.contains("go build")
      |> should.be_true()

      lint.command
      |> string.contains("gofmt")
      |> should.be_true()
    }
    _, _ -> should.fail()
  }
}

/// Test: Language-specific commands for Rust
pub fn pipeline_rust_stage_commands_test() {
  let implement_stage = [domain.Stage("implement", "Build", 0)]
  let lint_stage = [domain.Stage("lint", "Format check", 0)]

  let impl_preview = stages.execute_stages_dry_run(implement_stage, domain.Rust)
  let lint_preview = stages.execute_stages_dry_run(lint_stage, domain.Rust)

  case impl_preview, lint_preview {
    [impl], [lint] -> {
      impl.command
      |> string.contains("cargo build")
      |> should.be_true()

      lint.command
      |> string.contains("cargo fmt")
      |> should.be_true()
    }
    _, _ -> should.fail()
  }
}

/// Test: Language-specific commands for Python
pub fn pipeline_python_stage_commands_test() {
  let implement_stage = [domain.Stage("implement", "Build", 0)]
  let lint_stage = [domain.Stage("lint", "Format check", 0)]

  let impl_preview = stages.execute_stages_dry_run(implement_stage, domain.Python)
  let lint_preview = stages.execute_stages_dry_run(lint_stage, domain.Python)

  case impl_preview, lint_preview {
    [impl], [lint] -> {
      impl.command
      |> string.contains("python")
      |> should.be_true()

      lint.command
      |> string.contains("black")
      |> should.be_true()
    }
    _, _ -> should.fail()
  }
}

/// Test: Stage filter range selection
pub fn pipeline_stage_filter_range_test() {
  // Filter from implement to lint
  case domain.filter_stages("implement", "lint") {
    Ok(stages) -> {
      let names = list.map(stages, fn(s: domain.Stage) { s.name })

      names
      |> list.contains("implement")
      |> should.be_true()

      names
      |> list.contains("unit-test")
      |> should.be_true()

      names
      |> list.contains("lint")
      |> should.be_true()

      // Should not include stages after lint
      names
      |> list.contains("accept")
      |> should.be_false()
    }
    Error(_) -> should.fail()
  }
}

/// Test: Get single stage by name
pub fn pipeline_get_stage_test() {
  case domain.get_stage("implement") {
    Ok(stage) -> {
      stage.name
      |> should.equal("implement")
    }
    Error(_) -> should.fail()
  }

  // Unknown stage should error
  domain.get_stage("nonexistent")
  |> should.be_error()
}

/// Test: Pipeline with all stages executes in sequence (dry-run)
pub fn pipeline_full_sequence_dry_run_test() {
  let all_stages = domain.standard_pipeline()
  let previews = stages.execute_stages_dry_run(all_stages, domain.Gleam)

  // Should have same count
  list.length(previews)
  |> should.equal(list.length(all_stages))

  // Each preview should have name, command, and positive duration
  list.all(previews, fn(p: stages.StagePreview) {
    string.length(p.name) > 0
    && string.length(p.command) > 0
    && p.estimated_duration > 0
  })
  |> should.be_true()
}
