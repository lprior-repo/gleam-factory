//// Factory loop actor for managing implementation cycles.
////
//// Integrates with feedback_loop for auto-heal on test failures.
//// Tracks token usage for budget management.

import feedback_loop
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/otp/actor
import logging
import signal_bus
import signals

pub type Phase {
  Implementing
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
  BudgetExhausted
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
    // Token tracking for budget management
    total_tokens_used: Int,
    token_budget: Int,
  )
}

pub type LoopMessage {
  Advance(event: Event)
  GetState(reply_with: Subject(FactoryLoopState))
  RecordTokens(tokens: Int)
  SetFeedback(feedback: String)
}

pub type LoopError {
  InitFailed
}

/// Default token budget per task (can be overridden)
pub const default_token_budget = 50_000

pub fn start_link(
  loop_id: String,
  bead: signals.BeadAssigned,
  workspace_path: String,
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(LoopMessage), LoopError) {
  start_link_with_budget(
    loop_id,
    bead,
    workspace_path,
    bus,
    default_token_budget,
  )
}

pub fn start_link_with_budget(
  loop_id: String,
  bead: signals.BeadAssigned,
  workspace_path: String,
  bus: Subject(signal_bus.SignalBusMessage),
  token_budget: Int,
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
      total_tokens_used: 0,
      token_budget:,
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
    RecordTokens(tokens) -> {
      let new_total = state.total_tokens_used + tokens
      let new_state = FactoryLoopState(..state, total_tokens_used: new_total)
      // Check if budget exhausted
      case new_total >= state.token_budget {
        True -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.LoopFailed)
          actor.continue(FactoryLoopState(..new_state, phase: Failed))
        }
        False -> actor.continue(new_state)
      }
    }
    SetFeedback(feedback) -> {
      actor.continue(FactoryLoopState(..state, last_feedback: feedback))
    }
    Advance(event) -> {
      let new_phase = transition(state.phase, event)
      let new_iteration = case event {
        TestFailed -> state.iteration + 1
        _ -> state.iteration
      }
      let new_state = case event, state.tests_were_green {
        TestPassed, _ -> {
          FactoryLoopState(
            ..state,
            phase: new_phase,
            iteration: new_iteration,
            green_count: state.green_count + 1,
            tests_were_green: True,
          )
        }
        TestFailed, True -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.TestFailure)
          FactoryLoopState(
            ..state,
            phase: new_phase,
            iteration: new_iteration,
            revert_count: state.revert_count + 1,
            tests_were_green: False,
          )
        }
        TestFailed, False -> {
          FactoryLoopState(
            ..state,
            phase: new_phase,
            iteration: new_iteration,
            tests_were_green: False,
          )
        }
        BudgetExhausted, _ -> {
          FactoryLoopState(..state, phase: Failed)
        }
        PushSuccess, _
        | PushConflict, _
        | RebaseSuccess, _
        | RebaseConflict, _
        | MaxIterationsReached, _
        -> FactoryLoopState(..state, phase: new_phase)
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
    Implementing, TestPassed -> Reviewing
    Implementing, TestFailed -> Implementing
    Implementing, MaxIterationsReached -> Failed
    Implementing, BudgetExhausted -> Failed
    Reviewing, TestPassed -> Pushing
    Reviewing, TestFailed -> Failed
    Pushing, PushSuccess -> Completed
    Pushing, PushConflict -> Rebasing
    Rebasing, RebaseSuccess -> Pushing
    Rebasing, RebaseConflict -> Failed
    _, BudgetExhausted -> Failed
    _, _ -> {
      logging.log(
        logging.Error,
        "Unexpected phase/event in transition",
        dict.from_list([
          #("phase", format_phase(from)),
          #("event", format_event(event)),
        ]),
      )
      from
    }
  }
}

fn format_event(event: Event) -> String {
  case event {
    TestPassed -> "TestPassed"
    TestFailed -> "TestFailed"
    PushSuccess -> "PushSuccess"
    PushConflict -> "PushConflict"
    RebaseSuccess -> "RebaseSuccess"
    RebaseConflict -> "RebaseConflict"
    MaxIterationsReached -> "MaxIterationsReached"
    BudgetExhausted -> "BudgetExhausted"
  }
}

pub type GetStateResult {
  GotState(state: FactoryLoopState)
  GetStateTimeout
}

pub fn get_state(loop: Subject(LoopMessage)) -> GetStateResult {
  let reply = process.new_subject()
  process.send(loop, GetState(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(state) -> GotState(state)
    Error(Nil) -> GetStateTimeout
  }
}

pub fn unwrap_state(result: GetStateResult) -> FactoryLoopState {
  case result {
    GotState(state) -> state
    GetStateTimeout -> panic as "get_state timed out - loop unresponsive"
  }
}

pub fn advance(loop: Subject(LoopMessage), event: Event) -> Nil {
  process.send(loop, Advance(event:))
}

pub fn record_tokens(loop: Subject(LoopMessage), tokens: Int) -> Nil {
  process.send(loop, RecordTokens(tokens:))
}

pub fn set_feedback(loop: Subject(LoopMessage), feedback: String) -> Nil {
  process.send(loop, SetFeedback(feedback:))
}

/// Check if loop has budget remaining
pub fn has_budget(state: FactoryLoopState) -> Bool {
  state.total_tokens_used < state.token_budget
}

/// Get budget utilization as percentage
pub fn budget_utilization(state: FactoryLoopState) -> Int {
  case state.token_budget {
    0 -> 100
    budget -> state.total_tokens_used * 100 / budget
  }
}

/// Format state for logging
pub fn format_state(state: FactoryLoopState) -> String {
  "Loop "
  <> state.loop_id
  <> " | Phase: "
  <> format_phase(state.phase)
  <> " | Iter: "
  <> int.to_string(state.iteration)
  <> " | Tokens: "
  <> int.to_string(state.total_tokens_used)
  <> "/"
  <> int.to_string(state.token_budget)
  <> " ("
  <> int.to_string(budget_utilization(state))
  <> "%)"
}

fn format_phase(phase: Phase) -> String {
  case phase {
    Implementing -> "implementing"
    Reviewing -> "reviewing"
    Pushing -> "pushing"
    Rebasing -> "rebasing"
    Completed -> "completed"
    Failed -> "failed"
  }
}
