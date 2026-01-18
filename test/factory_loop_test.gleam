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
// GET_STATE RESULT TYPE TESTS
// ============================================================================

pub fn get_state_returns_ok_on_valid_response_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("state-ok-1"),
      spec: "test get_state returns Ok",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) =
    factory_loop.start_link("state-ok-loop", bead, "/tmp/state-ok-ws", bus)

  let assert factory_loop.GotState(state) = factory_loop.get_state(loop)
  state.task_id
  |> should.equal("state-ok-1")
  state.phase
  |> should.equal(factory_loop.Implementing)
}

pub fn get_state_returns_timeout_on_dead_actor_test() {
  // Create a subject that no actor listens to - simulates dead/unresponsive actor
  let dead_loop: process.Subject(factory_loop.LoopMessage) =
    process.new_subject()

  // Calling get_state on a dead subject should timeout (no orphaned Subject created)
  case factory_loop.get_state(dead_loop) {
    factory_loop.GetStateTimeout -> Nil
    factory_loop.GotState(_) -> should.fail()
  }
}

// ============================================================================
// UNEXPECTED EVENT HANDLING TESTS (RED PHASE - factory-gleam-ezne)
// ============================================================================

pub fn transition_logs_unexpected_phase_event_combinations_test() {
  // Reviewing phase should not receive PushSuccess (should be in Pushing phase)
  let result = factory_loop.transition(factory_loop.Reviewing, factory_loop.PushSuccess)
  // Should return original phase and log warning (not crash)
  result
  |> should.equal(factory_loop.Reviewing)
}

pub fn transition_logs_unexpected_rebasing_testpassed_test() {
  // Rebasing phase should not receive TestPassed (only RebaseSuccess/RebaseConflict)
  let result = factory_loop.transition(factory_loop.Rebasing, factory_loop.TestPassed)
  // Should return original phase and log warning (not crash)
  result
  |> should.equal(factory_loop.Rebasing)
}

pub fn advance_handles_unexpected_event_state_combination_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id("unexpected-1"),
      spec: "test unexpected event handling",
      requirements: [],
      priority: signals.P2,
      assigned_at: signals.timestamp(0),
    )

  let assert Ok(loop) =
    factory_loop.start_link("unexpected-loop", bead, "/tmp/unexpected-ws", bus)

  // Send MaxIterationsReached when in Pushing phase (unexpected combo)
  // Should log warning but not crash
  factory_loop.advance(loop, factory_loop.MaxIterationsReached)

  // Verify loop still responsive
  let assert factory_loop.GotState(state) = factory_loop.get_state(loop)
  state.phase
  |> should.equal(factory_loop.Failed)
}
