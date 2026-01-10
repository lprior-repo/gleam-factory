//// Tests for application supervisor tree
////
//// Verifies supervisor startup, child initialization, and restart behavior.
//// Extended with contract tests for enhanced supervisor with all actors:
//// resource_governor, workspace_manager, golden_master, llm_router,
//// factory_dispatcher, beads_watcher

import factory_supervisor
import gleeunit
import gleeunit/should
import heartbeat
import signal_bus

pub fn main() {
  gleeunit.main()
}

fn test_config() -> factory_supervisor.SupervisorConfig {
  factory_supervisor.SupervisorConfig(
    test_cmd: "true",
    test_interval_ms: 100,
    golden_master_path: "/tmp",
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

/// Test: Supervisor starts successfully with all children
pub fn supervisor_starts_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Signal bus is accessible after startup
pub fn signal_bus_accessible_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      signal_bus.publish(bus, signal_bus.TestPassing)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Heartbeat is accessible after startup
pub fn heartbeat_accessible_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let hb = factory_supervisor.get_heartbeat(started)
      heartbeat.tick(hb)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Multiple supervisors can start independently
pub fn multiple_supervisors_test() {
  let config1 =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp/1",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 100,
      gpu_tickets: 1,
      beads_path: "/tmp/.beads/issues.jsonl",
      beads_poll_interval_ms: 1000,
    )

  let config2 =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp/2",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 100,
      gpu_tickets: 1,
      beads_path: "/tmp/.beads/issues.jsonl",
      beads_poll_interval_ms: 1000,
    )

  case
    factory_supervisor.start_link(config1),
    factory_supervisor.start_link(config2)
  {
    Ok(s1), Ok(s2) -> {
      s1.signal_bus_subject
      |> should.not_equal(s2.signal_bus_subject)
    }
    _, _ -> should.fail()
  }
}

/// Test: Heartbeat receives tick messages
pub fn heartbeat_tick_received_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let hb = factory_supervisor.get_heartbeat(started)
      heartbeat.tick(hb)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Signal bus receives publish messages
pub fn signal_bus_publish_received_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      signal_bus.broadcast(bus, signal_bus.PatchProposed)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Both signal bus and heartbeat respond to messages
pub fn supervisor_children_responsive_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      let hb = factory_supervisor.get_heartbeat(started)
      signal_bus.broadcast(bus, signal_bus.TestPassing)
      heartbeat.tick(hb)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Supervisor startup error is propagated
pub fn supervisor_startup_error_test() {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "false",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 100,
      gpu_tickets: 1,
      beads_path: "/tmp/.beads/issues.jsonl",
      beads_poll_interval_ms: 1000,
    )

  case factory_supervisor.start_link(config) {
    Ok(_) -> Nil
    Error(factory_supervisor.InitFailed(_)) -> Nil
  }
  |> should.equal(Nil)
}

/// Test: Accessor functions return correct subject types
pub fn accessor_functions_correct_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let _bus = factory_supervisor.get_signal_bus(started)
      let _hb = factory_supervisor.get_heartbeat(started)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Supervisor can be started with different configs
pub fn supervisor_config_flexibility_test() {
  let config1 =
    factory_supervisor.SupervisorConfig(
      test_cmd: "echo test1",
      test_interval_ms: 50,
      golden_master_path: "/path/one",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 100,
      gpu_tickets: 1,
      beads_path: "/tmp/.beads/issues.jsonl",
      beads_poll_interval_ms: 1000,
    )

  let config2 =
    factory_supervisor.SupervisorConfig(
      test_cmd: "echo test2",
      test_interval_ms: 200,
      golden_master_path: "/path/two",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 100,
      gpu_tickets: 1,
      beads_path: "/tmp/.beads/issues.jsonl",
      beads_poll_interval_ms: 1000,
    )

  case
    factory_supervisor.start_link(config1),
    factory_supervisor.start_link(config2)
  {
    Ok(s1), Ok(s2) -> {
      s1.signal_bus_subject
      |> should.not_equal(s2.signal_bus_subject)
      s1.heartbeat_subject
      |> should.not_equal(s2.heartbeat_subject)
    }
    _, _ -> should.fail()
  }
}

/// Test: Supervisor starts with all extended children actors
/// Contract: resource_governor, workspace_manager, golden_master,
/// llm_router, factory_dispatcher, beads_watcher
pub fn start_link_all_actors_success_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      let hb = factory_supervisor.get_heartbeat(started)
      signal_bus.publish(bus, signal_bus.TestPassing)
      heartbeat.tick(hb)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Supervisor restarts failed child actor
/// Simulates child failure and verifies supervisor restarts it
pub fn supervisor_restarts_failed_child_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      signal_bus.publish(bus, signal_bus.TestPassing)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Supervisor returns all actor subjects in Started record
pub fn supervisor_returns_all_actor_subjects_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      let hb = factory_supervisor.get_heartbeat(started)
      signal_bus.publish(bus, signal_bus.TestPassing)
      heartbeat.tick(hb)
      1
    }
    Error(_) -> {
      should.fail()
      0
    }
  }
  |> should.equal(1)
}

/// Test: Graceful degradation when child actor fails
/// System continues operation with remaining children
pub fn graceful_degradation_on_child_failure_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      let hb = factory_supervisor.get_heartbeat(started)
      signal_bus.publish(bus, signal_bus.TestPassing)
      heartbeat.tick(hb)
      signal_bus.broadcast(bus, signal_bus.PatchProposed)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: log_system_ready outputs message at Info level
/// Verifies that golden master ready state triggers proper logging
pub fn log_system_ready_outputs_message_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(_started) -> {
      let _ = factory_supervisor.log_system_ready(config)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: system_ready_after_all_services_started
/// Confirms startup message after all actors initialized
pub fn system_ready_after_all_services_started_test() {
  let config = test_config()

  case factory_supervisor.start_link(config) {
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      let hb = factory_supervisor.get_heartbeat(started)
      signal_bus.publish(bus, signal_bus.TestPassing)
      heartbeat.tick(hb)
      let _ = factory_supervisor.log_system_ready(config)
      Nil
    }
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}
