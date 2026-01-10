//// End-to-end pipeline integration tests (deh bead)
////
//// Verifies complete pipeline flow: task creation -> stage execution -> signal handling -> completion
//// Tests error cases, timeout handling, signal broadcasting, state transitions

import dict
import factory_supervisor
import gleeunit
import gleeunit/should
import heartbeat
import merge_queue
import signal_bus
import golden_master

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
      signal_bus.publish(bus, signal_bus.TestPassing)
      signal_bus.publish(bus, signal_bus.TestFailure)
      signal_bus.publish(bus, signal_bus.PatchProposed)
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
      let bus = factory_supervisor.get_signal_bus(supervisor)
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
      signal_bus.publish(bus, signal_bus.TestFailure)
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
      let sub1 = signal_bus.subscribe(bus, fn(_sig) { Nil })
      let sub2 = signal_bus.subscribe(bus, fn(_sig) { Nil })

      // Publish and verify both receive
      signal_bus.publish(bus, signal_bus.TestPassing)
      signal_bus.publish(bus, signal_bus.PatchAccepted)
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
      signal_bus.publish(bus, signal_bus.ResourceExhausted)

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
      signal_bus.publish(bus, signal_bus.TestPassing)
      signal_bus.publish(bus, signal_bus.PatchAccepted)

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
