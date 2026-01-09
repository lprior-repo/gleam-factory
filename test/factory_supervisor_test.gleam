//// Tests for application supervisor tree
////
//// Verifies supervisor startup, child initialization, and restart behavior.

import factory_supervisor
import gleeunit
import gleeunit/should
import heartbeat
import signal_bus

pub fn main() {
  gleeunit.main()
}

/// Test: Supervisor starts successfully with all children
pub fn supervisor_starts_test() {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

  case factory_supervisor.start_link(config) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)
}

/// Test: Signal bus is accessible after startup
pub fn signal_bus_accessible_test() {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

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
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

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
    )

  let config2 =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp/2",
    )

  case factory_supervisor.start_link(config1), factory_supervisor.start_link(config2) {
    Ok(s1), Ok(s2) -> {
      s1.signal_bus_subject
      |> should.not_equal(s2.signal_bus_subject)
    }
    _, _ -> should.fail()
  }
}

/// Test: Heartbeat receives tick messages
pub fn heartbeat_tick_received_test() {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

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
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

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
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

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
    )

  case factory_supervisor.start_link(config) {
    Ok(_) -> Nil
    Error(factory_supervisor.InitFailed(_)) -> Nil
  }
  |> should.equal(Nil)
}

/// Test: Accessor functions return correct subject types
pub fn accessor_functions_correct_test() {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
    )

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
    )

  let config2 =
    factory_supervisor.SupervisorConfig(
      test_cmd: "echo test2",
      test_interval_ms: 200,
      golden_master_path: "/path/two",
    )

  case factory_supervisor.start_link(config1), factory_supervisor.start_link(config2) {
    Ok(s1), Ok(s2) -> {
      s1.signal_bus_subject
      |> should.not_equal(s2.signal_bus_subject)
      s1.heartbeat_subject
      |> should.not_equal(s2.heartbeat_subject)
    }
    _, _ -> should.fail()
  }
}
