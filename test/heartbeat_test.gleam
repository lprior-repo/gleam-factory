import gleam/erlang/process
import gleam/int
import gleam/list
import gleeunit
import gleeunit/should
import heartbeat
import signal_bus

pub fn main() {
  gleeunit.main()
}

pub fn tick_updates_status_to_green_when_tests_pass_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "true",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(1000)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Green)

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}

pub fn tick_updates_status_to_red_when_tests_fail_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "false",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Red)

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}

pub fn transition_from_green_to_red_broadcasts_test_failure_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)

  process.sleep(100)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "true",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  process.sleep(100)
  case process.receive(subscriber, 100) {
    Error(Nil) -> Nil
    Ok(_) -> panic as "Expected no TestFailure from Red->Green"
  }

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Green)

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}

pub fn transition_from_red_to_green_broadcasts_test_passing_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let passing_sub = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, passing_sub)

  process.sleep(100)

  let config_pass =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "true",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb_pass) = heartbeat.start_link(config_pass, bus)

  heartbeat.tick(hb_pass)
  process.sleep(100)

  case process.receive(passing_sub, 1000) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> panic as "Expected TestPassing signal on transition"
  }

  let status_final = heartbeat.get_status(hb_pass)
  status_final |> should.equal(heartbeat.Green)

  heartbeat.shutdown(hb_pass)
  signal_bus.shutdown(bus)
}

pub fn no_signal_broadcast_when_status_unchanged_green_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "true",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  process.sleep(100)
  case process.receive(subscriber, 100) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> panic as "Expected initial TestPassing from Red->Green"
  }

  heartbeat.tick(hb)
  process.sleep(100)

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(subscriber, 200) {
    Ok(_) -> panic as "Expected no signal when status unchanged"
    Error(Nil) -> Nil
  }

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}

pub fn no_signal_broadcast_when_status_unchanged_red_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "false",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(subscriber, 200) {
    Ok(_) -> panic as "Expected no signal when status unchanged"
    Error(Nil) -> Nil
  }

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}

pub fn multiple_transitions_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let failure_sub = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestFailure, failure_sub)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "false",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  process.sleep(100)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Red)

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}

pub fn get_status_returns_current_state_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "true",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  process.sleep(100)
  let initial_status = heartbeat.get_status(hb)
  initial_status |> should.equal(heartbeat.Green)

  heartbeat.shutdown(hb)

  let config_fail =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "false",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb_fail) = heartbeat.start_link(config_fail, bus)
  process.sleep(100)

  let status = heartbeat.get_status(hb_fail)
  status |> should.equal(heartbeat.Red)

  heartbeat.shutdown(hb_fail)
  signal_bus.shutdown(bus)
}

pub fn progress_buffer_bounded_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "true",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  process.sleep(100)

  list.range(0, 2000)
  |> list.fold(Nil, fn(_acc, i) {
    heartbeat.stream_progress(hb, "task_" <> int.to_string(i), "chunk_data")
    Nil
  })
  |> fn(_) { Nil }

  process.sleep(100)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Green)

  heartbeat.shutdown(hb)
  signal_bus.shutdown(bus)
}
