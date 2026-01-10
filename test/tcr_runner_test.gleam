//// Tests for tcr_runner phase handling.

import factory_loop
import gleam/erlang/process
import gleeunit/should
import signal_bus
import signals

fn setup_test_loop(phase: factory_loop.Phase) -> process.Subject(factory_loop.LoopMessage) {
  let bus = signal_bus.start()
  let bead = signals.BeadAssigned(
    task_id: signals.task_id("test-task"),
    spec: "test spec",
    requirements: [],
    priority: signals.P1,
    assigned_at: signals.timestamp(0),
  )
  let assert Ok(loop) = factory_loop.start_link("test-loop", bead, "/tmp/test", bus)

  // Advance to desired phase
  case phase {
    factory_loop.Implementing -> Nil
    factory_loop.TcrChecking -> {
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
    }
    factory_loop.Reviewing -> {
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
    }
    factory_loop.Pushing -> {
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
    }
    factory_loop.Rebasing -> {
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
      factory_loop.advance(loop, factory_loop.TestPassed)
      process.sleep(10)
      factory_loop.advance(loop, factory_loop.PushConflict)
      process.sleep(10)
    }
    factory_loop.Completed -> Nil
    factory_loop.Failed -> Nil
  }
  loop
}

pub fn tcr_checking_phase_transitions_on_test_passed_test() {
  let loop = setup_test_loop(factory_loop.TcrChecking)
  let state = factory_loop.get_state(loop)
  state.phase |> should.equal(factory_loop.TcrChecking)

  factory_loop.advance(loop, factory_loop.TestPassed)
  process.sleep(10)
  let new_state = factory_loop.get_state(loop)
  new_state.phase |> should.equal(factory_loop.Reviewing)
}

pub fn tcr_checking_phase_returns_to_implementing_on_failure_test() {
  let loop = setup_test_loop(factory_loop.TcrChecking)

  factory_loop.advance(loop, factory_loop.TestFailed)
  process.sleep(10)
  let state = factory_loop.get_state(loop)
  state.phase |> should.equal(factory_loop.Implementing)
}

pub fn pushing_phase_completes_on_success_test() {
  let loop = setup_test_loop(factory_loop.Pushing)
  let state = factory_loop.get_state(loop)
  state.phase |> should.equal(factory_loop.Pushing)

  factory_loop.advance(loop, factory_loop.PushSuccess)
  process.sleep(10)
  let new_state = factory_loop.get_state(loop)
  new_state.phase |> should.equal(factory_loop.Completed)
}

pub fn pushing_phase_goes_to_rebasing_on_conflict_test() {
  let loop = setup_test_loop(factory_loop.Pushing)

  factory_loop.advance(loop, factory_loop.PushConflict)
  process.sleep(10)
  let state = factory_loop.get_state(loop)
  state.phase |> should.equal(factory_loop.Rebasing)
}

pub fn rebasing_phase_returns_to_pushing_on_success_test() {
  let loop = setup_test_loop(factory_loop.Rebasing)
  let state = factory_loop.get_state(loop)
  state.phase |> should.equal(factory_loop.Rebasing)

  factory_loop.advance(loop, factory_loop.RebaseSuccess)
  process.sleep(10)
  let new_state = factory_loop.get_state(loop)
  new_state.phase |> should.equal(factory_loop.Pushing)
}

pub fn rebasing_phase_fails_on_conflict_test() {
  let loop = setup_test_loop(factory_loop.Rebasing)

  factory_loop.advance(loop, factory_loop.RebaseConflict)
  process.sleep(10)
  let state = factory_loop.get_state(loop)
  state.phase |> should.equal(factory_loop.Failed)
}
