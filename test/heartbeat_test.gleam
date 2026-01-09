import gleam/erlang/process
import gleam/result
import gleeunit
import gleeunit/should
import heartbeat
import signal_bus
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn tick_updates_status_to_green_when_tests_pass_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 0",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Green)
}

pub fn tick_updates_status_to_red_when_tests_fail_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 1",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Red)
}

pub fn transition_from_green_to_red_broadcasts_test_failure_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 1",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(subscriber, 500) {
    Ok(signal_bus.TestFailure) -> Nil
    _ -> panic as "Expected TestFailure signal on Green->Red transition"
  }
}

pub fn transition_from_red_to_green_broadcasts_test_passing_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let passing_sub = process.new_subject()
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestPassing, passing_sub)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "sh -c 'test -f /tmp/heartbeat_toggle_test && exit 0 || exit 1'",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Red)

  let _ = process.spawn(fn() {
    use _ <- result.try(simplifile.write("/tmp/heartbeat_toggle_test", ""))
    Ok(Nil)
  })
  process.sleep(100)

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(passing_sub, 500) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> panic as "Expected TestPassing signal on Red->Green transition"
  }

  let status_final = heartbeat.get_status(hb)
  status_final |> should.equal(heartbeat.Green)
}

pub fn no_signal_broadcast_when_status_unchanged_green_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 0",
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
}

pub fn no_signal_broadcast_when_status_unchanged_red_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 1",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(subscriber, 200) {
    Ok(signal_bus.TestFailure) -> Nil
    _ -> panic as "Expected TestFailure on first tick"
  }

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(subscriber, 200) {
    Ok(_) -> panic as "Expected no signal when status unchanged"
    Error(Nil) -> Nil
  }
}

pub fn multiple_transitions_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let failure_sub = process.new_subject()
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, failure_sub)

  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 1",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  heartbeat.tick(hb)
  process.sleep(100)

  case process.receive(failure_sub, 500) {
    Ok(signal_bus.TestFailure) -> Nil
    _ -> panic as "Expected TestFailure on Green->Red"
  }

  let status = heartbeat.get_status(hb)
  status |> should.equal(heartbeat.Red)
}

pub fn get_status_returns_current_state_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let config =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 0",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb) = heartbeat.start_link(config, bus)

  let initial_status = heartbeat.get_status(hb)
  initial_status |> should.equal(heartbeat.Green)

  let config_fail =
    heartbeat.HeartbeatConfig(
      interval_ms: 1000,
      test_cmd: "exit 1",
      golden_master_path: "/tmp",
    )
  let assert Ok(hb_fail) = heartbeat.start_link(config_fail, bus)
  heartbeat.tick(hb_fail)
  process.sleep(100)

  let status = heartbeat.get_status(hb_fail)
  status |> should.equal(heartbeat.Red)
}
