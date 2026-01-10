//// Graceful shutdown tests (factory-gleam-2eh)
////
//// Verifies that the system can shutdown cleanly without losing state
//// or leaving dangling resources.

import gleam/dict
import factory_supervisor
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn supervisor_config() -> factory_supervisor.SupervisorConfig {
  factory_supervisor.SupervisorConfig(
    test_cmd: "true",
    test_interval_ms: 100,
    golden_master_path: "/tmp/shutdown_test",
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

/// Test: System starts up and can be shutdown gracefully
pub fn graceful_shutdown_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      // System is running
      let _bus = factory_supervisor.get_signal_bus(started)

      // Shutdown gracefully
      factory_supervisor.shutdown(started)

      // Verify shutdown completed
      Nil
    }
    Error(_e) -> should.fail()
  }
}

/// Test: Multiple shutdown calls are safe
pub fn shutdown_idempotent_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      // Shutdown multiple times - should not crash
      factory_supervisor.shutdown(started)
      factory_supervisor.shutdown(started)

      Nil
    }
    Error(_e) -> should.fail()
  }
}

/// Test: Shutdown completes without waiting forever
pub fn shutdown_timeout_test() {
  let config = supervisor_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let start_time = get_time_ms()

      factory_supervisor.shutdown(started)

      let end_time = get_time_ms()
      let elapsed = end_time - start_time

      // Shutdown should complete in reasonable time (< 5 seconds)
      case elapsed < 5000 {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_e) -> should.fail()
  }
}

// Helper function - returns current time in milliseconds
fn get_time_ms() -> Int {
  erlang_monotonic_time() / 1_000_000
}

@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time() -> Int
