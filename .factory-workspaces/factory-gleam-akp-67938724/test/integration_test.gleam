import factory_loop
import gleam/erlang/process
import gleeunit
import gleeunit/should
import signal_bus
import signals

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// SIGNAL BUS INTEGRATION TESTS
// ============================================================================

pub fn signal_bus_subscribe_and_broadcast_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()

  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber)

  signal_bus.broadcast(bus, signal_bus.TestPassing)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> should.fail()
  }
}

pub fn signal_bus_multiple_subscribers_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let sub1 = process.new_subject()
  let sub2 = process.new_subject()
  let sub3 = process.new_subject()

  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestPassing, sub1)
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestPassing, sub2)
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestPassing, sub3)

  signal_bus.broadcast(bus, signal_bus.TestPassing)

  case
    process.receive(sub1, 1000),
    process.receive(sub2, 1000),
    process.receive(sub3, 1000)
  {
    Ok(signal_bus.TestPassing),
      Ok(signal_bus.TestPassing),
      Ok(signal_bus.TestPassing)
    -> Nil
    _, _, _ -> should.fail()
  }
}

pub fn signal_bus_different_signal_types_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let test_sub = process.new_subject()
  let loop_sub = process.new_subject()

  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, test_sub)
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopSpawned, loop_sub)

  signal_bus.broadcast(bus, signal_bus.TestPassing)
  signal_bus.broadcast(bus, signal_bus.LoopSpawned)

  case process.receive(test_sub, 1000), process.receive(loop_sub, 1000) {
    Ok(signal_bus.TestPassing), Ok(signal_bus.LoopSpawned) -> Nil
    _, _ -> should.fail()
  }
}

// ============================================================================
// FACTORY LOOP INTEGRATION TESTS
// ============================================================================

pub fn factory_loop_starts_in_implementing_phase_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-1"),
      spec: "implement feature",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) = factory_loop.start_link("loop-1", bead, "/tmp/ws", bus)

  let state = factory_loop.get_state(loop)
  state.phase
  |> should.equal(factory_loop.Implementing)
  state.task_id
  |> should.equal("test-1")
  state.iteration
  |> should.equal(1)
}

pub fn factory_loop_phase_transitions_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-2"),
      spec: "test transitions",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) = factory_loop.start_link("loop-2", bead, "/tmp/ws2", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  process.sleep(100)
  let state1 = factory_loop.get_state(loop)
  state1.phase
  |> should.equal(factory_loop.TcrChecking)

  factory_loop.advance(loop, factory_loop.TestPassed)
  process.sleep(100)
  let state2 = factory_loop.get_state(loop)
  state2.phase
  |> should.equal(factory_loop.Reviewing)

  factory_loop.advance(loop, factory_loop.TestPassed)
  process.sleep(100)
  let state3 = factory_loop.get_state(loop)
  state3.phase
  |> should.equal(factory_loop.Pushing)

  factory_loop.advance(loop, factory_loop.PushSuccess)
  process.sleep(100)
  let state4 = factory_loop.get_state(loop)
  state4.phase
  |> should.equal(factory_loop.Completed)
}

pub fn factory_loop_failure_transitions_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-3"),
      spec: "test failures",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) = factory_loop.start_link("loop-3", bead, "/tmp/ws3", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestFailed)
  process.sleep(100)
  let state = factory_loop.get_state(loop)
  state.phase
  |> should.equal(factory_loop.Failed)
}

pub fn factory_loop_rebase_flow_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-4"),
      spec: "test rebase",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) = factory_loop.start_link("loop-4", bead, "/tmp/ws4", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.PushConflict)
  process.sleep(100)
  let state1 = factory_loop.get_state(loop)
  state1.phase
  |> should.equal(factory_loop.Rebasing)

  factory_loop.advance(loop, factory_loop.RebaseSuccess)
  process.sleep(100)
  let state2 = factory_loop.get_state(loop)
  state2.phase
  |> should.equal(factory_loop.Pushing)
}

pub fn factory_loop_broadcasts_spawned_signal_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopSpawned, subscriber)

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-5"),
      spec: "test signals",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(_loop) =
    factory_loop.start_link("loop-5", bead, "/tmp/ws5", bus)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.LoopSpawned) -> Nil
    _ -> should.fail()
  }
}

pub fn factory_loop_broadcasts_completion_signals_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopComplete, subscriber)

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-6"),
      spec: "complete successfully",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) = factory_loop.start_link("loop-6", bead, "/tmp/ws6", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.PushSuccess)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.LoopComplete) -> Nil
    _ -> should.fail()
  }
}

pub fn factory_loop_broadcasts_failure_signals_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopFailed, subscriber)

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("test-7"),
      spec: "fail on review",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) = factory_loop.start_link("loop-7", bead, "/tmp/ws7", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestFailed)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.LoopFailed) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// FACTORY DISPATCHER INTEGRATION TESTS
// ============================================================================

pub fn dispatcher_spawns_loop_on_bead_assigned_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let loop_subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopSpawned, loop_subscriber)

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("disp-1"),
      spec: "dispatcher test",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) =
    factory_loop.start_link(
      "dispatcher-test-1",
      bead,
      "/tmp/dispatcher-ws",
      bus,
    )

  case process.receive(loop_subscriber, 2000) {
    Ok(signal_bus.LoopSpawned) -> Nil
    _ -> should.fail()
  }

  let state = factory_loop.get_state(loop)
  state.task_id
  |> should.equal("disp-1")
}

pub fn dispatcher_handles_multiple_beads_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let loop_subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopSpawned, loop_subscriber)

  let bead1 =
    signals.BeadAssigned(
      task_id: signals.task_id("disp-2a"),
      spec: "first bead",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )
  let bead2 =
    signals.BeadAssigned(
      task_id: signals.task_id("disp-2b"),
      spec: "second bead",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )
  let bead3 =
    signals.BeadAssigned(
      task_id: signals.task_id("disp-2c"),
      spec: "third bead",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(_loop1) =
    factory_loop.start_link("disp-2a", bead1, "/tmp/disp-ws2a", bus)
  let assert Ok(_loop2) =
    factory_loop.start_link("disp-2b", bead2, "/tmp/disp-ws2b", bus)
  let assert Ok(_loop3) =
    factory_loop.start_link("disp-2c", bead3, "/tmp/disp-ws2c", bus)

  let received1 = process.receive(loop_subscriber, 2000)
  let received2 = process.receive(loop_subscriber, 2000)
  let received3 = process.receive(loop_subscriber, 2000)

  case received1, received2, received3 {
    Ok(signal_bus.LoopSpawned),
      Ok(signal_bus.LoopSpawned),
      Ok(signal_bus.LoopSpawned)
    -> Nil
    _, _, _ -> should.fail()
  }
}

// ============================================================================
// FULL PIPELINE E2E TESTS
// ============================================================================

pub fn e2e_single_bead_full_lifecycle_test() {
  let assert Ok(bus) = signal_bus.start_link()

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("e2e-1"),
      spec: "end to end test",
      requirements: ["gleam", "tests"],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) =
    factory_loop.start_link("e2e-loop-1", bead, "/tmp/e2e-ws1", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.PushSuccess)

  process.sleep(200)

  let final_state = factory_loop.get_state(loop)
  final_state.phase
  |> should.equal(factory_loop.Completed)
  final_state.task_id
  |> should.equal("e2e-1")
}

pub fn e2e_concurrent_loops_test() {
  let assert Ok(bus) = signal_bus.start_link()

  let bead1 =
    signals.BeadAssigned(
      task_id: signals.task_id("e2e-2a"),
      spec: "concurrent task 1",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )
  let bead2 =
    signals.BeadAssigned(
      task_id: signals.task_id("e2e-2b"),
      spec: "concurrent task 2",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop1) =
    factory_loop.start_link("e2e-loop-2a", bead1, "/tmp/e2e-ws2a", bus)
  let assert Ok(loop2) =
    factory_loop.start_link("e2e-loop-2b", bead2, "/tmp/e2e-ws2b", bus)

  factory_loop.advance(loop1, factory_loop.TestPassed)
  factory_loop.advance(loop2, factory_loop.TestPassed)
  process.sleep(200)

  let state1 = factory_loop.get_state(loop1)
  let state2 = factory_loop.get_state(loop2)

  state1.phase
  |> should.equal(factory_loop.TcrChecking)
  state2.phase
  |> should.equal(factory_loop.TcrChecking)
  state1.task_id
  |> should.equal("e2e-2a")
  state2.task_id
  |> should.equal("e2e-2b")
}

pub fn e2e_error_recovery_test() {
  let assert Ok(bus) = signal_bus.start_link()

  let loop_fail_sub = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopFailed, loop_fail_sub)

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("e2e-3"),
      spec: "error recovery test",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) =
    factory_loop.start_link("e2e-loop-3", bead, "/tmp/e2e-ws3", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestFailed)

  let assert Ok(signal_bus.LoopFailed) = process.receive(loop_fail_sub, 2000)

  let final_state = factory_loop.get_state(loop)
  final_state.phase
  |> should.equal(factory_loop.Failed)
}

pub fn e2e_rebase_recovery_test() {
  let assert Ok(bus) = signal_bus.start_link()

  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("e2e-4"),
      spec: "rebase recovery",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) =
    factory_loop.start_link("e2e-loop-4", bead, "/tmp/e2e-ws4", bus)

  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.TestPassed)
  factory_loop.advance(loop, factory_loop.PushConflict)
  process.sleep(200)

  let state1 = factory_loop.get_state(loop)
  state1.phase
  |> should.equal(factory_loop.Rebasing)

  factory_loop.advance(loop, factory_loop.RebaseSuccess)
  process.sleep(200)

  let state2 = factory_loop.get_state(loop)
  state2.phase
  |> should.equal(factory_loop.Pushing)

  factory_loop.advance(loop, factory_loop.PushSuccess)
  process.sleep(200)

  let state3 = factory_loop.get_state(loop)
  state3.phase
  |> should.equal(factory_loop.Completed)
}
