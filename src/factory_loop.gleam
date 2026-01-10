//// Factory loop actor for TCR cycles.
////
//// Manages implementing a bead through pure TCR: test && commit || revert.

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import signal_bus
import signals

pub type Phase {
  Implementing
  TcrChecking
  Reviewing
  Pushing
  Rebasing
  Completed
  Failed
}

pub type Event {
  TestPassed
  TestFailed
  PushSuccess
  PushConflict
  RebaseSuccess
  RebaseConflict
  MaxIterationsReached
}

pub type HistoryEntry {
  HistoryEntry(iteration: Int, role: String, content: String, timestamp: String)
}

pub type FactoryLoopState {
  FactoryLoopState(
    loop_id: String,
    task_id: String,
    task_spec: String,
    workspace_path: String,
    phase: Phase,
    iteration: Int,
    green_count: Int,
    commit_count: Int,
    revert_count: Int,
    history: List(HistoryEntry),
    last_feedback: String,
    signal_bus: Subject(signal_bus.SignalBusMessage),
    tests_were_green: Bool,
  )
}

pub type LoopMessage {
  Advance(event: Event)
  GetState(reply_with: Subject(FactoryLoopState))
}

pub type LoopError {
  InitFailed
}

pub fn start_link(
  loop_id: String,
  bead: signals.BeadAssigned,
  workspace_path: String,
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(LoopMessage), LoopError) {
  let initial =
    FactoryLoopState(
      loop_id:,
      task_id: signals.unwrap_task_id(bead.task_id),
      task_spec: bead.spec,
      workspace_path:,
      phase: Implementing,
      iteration: 1,
      green_count: 0,
      commit_count: 0,
      revert_count: 0,
      history: [],
      last_feedback: "",
      signal_bus: bus,
      tests_were_green: False,
    )

  signal_bus.broadcast(bus, signal_bus.LoopSpawned)

  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(InitFailed)
  }
}

fn handle_message(
  state: FactoryLoopState,
  msg: LoopMessage,
) -> actor.Next(FactoryLoopState, LoopMessage) {
  case msg {
    GetState(reply) -> {
      process.send(reply, state)
      actor.continue(state)
    }
    Advance(event) -> {
      let new_phase = transition(state.phase, event)
      let new_state = case event, state.tests_were_green {
        TestPassed, _ -> {
          FactoryLoopState(
            ..state,
            phase: new_phase,
            tests_were_green: True,
          )
        }
        TestFailed, True -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.TestFailure)
          FactoryLoopState(
            ..state,
            phase: new_phase,
            tests_were_green: False,
          )
        }
        TestFailed, False -> {
          FactoryLoopState(
            ..state,
            phase: new_phase,
            tests_were_green: False,
          )
        }
        _, _ -> FactoryLoopState(..state, phase: new_phase)
      }
      let final_state = case new_phase {
        Completed -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.LoopComplete)
          FactoryLoopState(..new_state, phase: Completed)
        }
        Failed -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.LoopFailed)
          FactoryLoopState(..new_state, phase: Failed)
        }
        _ -> new_state
      }
      actor.continue(final_state)
    }
  }
}

pub fn transition(from: Phase, event: Event) -> Phase {
  case from, event {
    Implementing, TestPassed -> TcrChecking
    Implementing, TestFailed -> Implementing
    Implementing, MaxIterationsReached -> Failed
    TcrChecking, TestPassed -> Reviewing
    TcrChecking, TestFailed -> Implementing
    Reviewing, TestPassed -> Pushing
    Reviewing, TestFailed -> Failed
    Pushing, PushSuccess -> Completed
    Pushing, PushConflict -> Rebasing
    Rebasing, RebaseSuccess -> Pushing
    Rebasing, RebaseConflict -> Failed
    _, _ -> from
  }
}

pub fn get_state(loop: Subject(LoopMessage)) -> Result(FactoryLoopState, String) {
  let reply = process.new_subject()
  process.send(loop, GetState(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(state) -> Ok(state)
    Error(Nil) -> Error("timeout")
  }
}

pub fn advance(loop: Subject(LoopMessage), event: Event) -> Nil {
  process.send(loop, Advance(event:))
}
