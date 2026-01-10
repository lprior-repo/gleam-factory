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

  case factory_loop.get_state(loop) {
    Ok(state) -> {
      state.task_id
      |> should.equal("state-ok-1")
      state.phase
      |> should.equal(factory_loop.Implementing)
    }
    Error(_) -> should.fail()
  }
}

pub fn get_state_returns_error_on_timeout_test() {
  // Create a subject that no actor listens to - simulates dead/unresponsive actor
  let dead_loop: process.Subject(factory_loop.LoopMessage) = process.new_subject()

  // Calling get_state on a dead subject should timeout and return Error
  case factory_loop.get_state(dead_loop) {
    Ok(_) -> should.fail()
    Error(err) -> {
      err
      |> should.equal("timeout")
    }
  }
}

pub fn get_state_result_type_prevents_orphaned_subject_test() {
  // This test ensures that the new Result return type means no orphaned Subject
  // is created on timeout - callers must handle the Error case explicitly
  let dead_loop: process.Subject(factory_loop.LoopMessage) = process.new_subject()

  // The old implementation would return a fake FactoryLoopState with an orphaned
  // signal_bus Subject. The new implementation returns Error("timeout") instead.
  let result = factory_loop.get_state(dead_loop)

  // Result should be Error, not a fake state with orphaned Subject
  result
  |> should.be_error
}
