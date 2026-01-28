//// Factory loop actor for managing implementation cycles.
////
//// Integrates with TDD15 workflow for structured development.
//// Tracks token usage for budget management.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option
import gleam/otp/actor
import logging
import signal_bus
import signals
import tdd15/phases
import tdd15/state

pub type Phase {
  TDD15Phase(Int)
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
  PhaseComplete(Int, Bool)
  PhaseFailed(Int, String)
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
    total_tokens_used: Int,
    token_budget: Int,
    bead_id: option.Option(String),
    tdd15_route: option.Option(phases.Route),
    current_tdd15_phase: option.Option(phases.Phase),
  )
}

pub type LoopMessage {
  Shutdown
  Advance(event: Event)
  GetState(reply_with: Subject(FactoryLoopState))
  RecordTokens(tokens: Int)
  SetFeedback(feedback: String)
  StartTDD15(bead_id: String, complexity: state.Complexity)
}

pub type LoopError {
  InitFailed
  InvalidTDD15Route
  CacheInitFailed
}

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
      bead_id: option.None,
      tdd15_route: option.None,
      current_tdd15_phase: option.None,
    )

  signal_bus.broadcast(bus, signal_bus.LoopSpawned)

  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(InitFailed)
  }
}

fn init_tdd15(
  state: FactoryLoopState,
  bead_id: String,
  complexity: state.Complexity,
) -> Result(FactoryLoopState, String) {
  case state.init_cache(bead_id) {
    Ok(_) -> {
      let phases_complexity = case complexity {
        state.Simple -> phases.Simple
        state.Medium -> phases.Medium
        state.Complex -> phases.Complex
      }
      let route = phases.route_for_complexity(phases_complexity)
      let phases.Route(numbers) = route
      let assert Ok(start_phase) = phases.route_start(route)
      let phases.PhaseMeta(number: start_num, ..) =
        phases.phase_meta(start_phase)

      let progress =
        state.Progress(
          bead_id: bead_id,
          language: state.Gleam,
          complexity: complexity,
          route: numbers,
          phases: dict.new(),
          current_phase: start_num,
          last_commit: "init",
        )

      case state.save_progress(bead_id, progress) {
        Ok(_) -> {
          Ok(
            FactoryLoopState(
              ..state,
              bead_id: option.Some(bead_id),
              tdd15_route: option.Some(route),
              current_tdd15_phase: option.Some(start_phase),
              phase: TDD15Phase(start_num),
            ),
          )
        }
        Error(_) -> Error("Failed to save progress")
      }
    }
    Error(_) -> Error("Failed to init cache")
  }
}

fn handle_message(
  state: FactoryLoopState,
  msg: LoopMessage,
) -> actor.Next(FactoryLoopState, LoopMessage) {
  case msg {
    Shutdown -> actor.stop()
    GetState(reply) -> {
      process.send(reply, state)
      actor.continue(state)
    }
    RecordTokens(tokens) -> {
      let new_total = state.total_tokens_used + tokens
      let new_state = FactoryLoopState(..state, total_tokens_used: new_total)
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
    StartTDD15(bead_id, complexity) -> {
      case init_tdd15(state, bead_id, complexity) {
        Ok(new_state) -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.TDD15Started)
          actor.continue(new_state)
        }
        Error(err) -> {
          let msg = "Failed to start TDD15: " <> err
          logging.log(logging.Error, msg, dict.new())
          actor.continue(state)
        }
      }
    }
    Advance(event) -> {
      let new_phase = transition(state.phase, event)
      let new_iteration = case event {
        TestFailed -> state.iteration + 1
        _ -> state.iteration
      }
      let new_state =
        handle_event_transition(state, event, new_phase, new_iteration)
      let final_state = case new_state.phase {
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

fn handle_event_transition(
  state: FactoryLoopState,
  event: Event,
  new_phase: Phase,
  new_iteration: Int,
) -> FactoryLoopState {
  case event, state.tests_were_green, state.phase {
    TestPassed, _, Implementing -> {
      FactoryLoopState(
        ..state,
        phase: new_phase,
        iteration: new_iteration,
        green_count: state.green_count + 1,
        tests_were_green: True,
      )
    }
    TestPassed, _, Reviewing -> {
      FactoryLoopState(
        ..state,
        phase: new_phase,
        iteration: new_iteration,
        green_count: state.green_count + 1,
        tests_were_green: True,
      )
    }
    TestPassed, _, TDD15Phase(_) -> {
      case state.current_tdd15_phase {
        option.Some(phase) -> {
          let phases.PhaseMeta(number: phase_num, ..) = phases.phase_meta(phase)
          case advance_tdd15_phase(state, phase_num, True) {
            Ok(updated) -> updated
            Error(err) -> {
              let msg = "TDD15 advance failed: " <> err
              logging.log(logging.Error, msg, dict.new())
              state
            }
          }
        }
        option.None -> state
      }
    }
    PhaseComplete(phase_num, True), _, TDD15Phase(_) -> {
      case advance_tdd15_phase(state, phase_num, True) {
        Ok(updated) -> updated
        Error(err) -> {
          let msg = "TDD15 phase complete failed: " <> err
          logging.log(logging.Error, msg, dict.new())
          state
        }
      }
    }
    PhaseFailed(phase_num, _), _, TDD15Phase(_) -> {
      case advance_tdd15_phase(state, phase_num, False) {
        Ok(updated) -> updated
        Error(err) -> {
          let msg = "TDD15 phase failed: " <> err
          logging.log(logging.Error, msg, dict.new())
          state
        }
      }
    }
    TestFailed, True, Implementing -> {
      signal_bus.broadcast(state.signal_bus, signal_bus.TestFailure)
      FactoryLoopState(
        ..state,
        phase: new_phase,
        iteration: new_iteration,
        revert_count: state.revert_count + 1,
        tests_were_green: False,
      )
    }
    TestFailed, True, Reviewing -> {
      signal_bus.broadcast(state.signal_bus, signal_bus.TestFailure)
      FactoryLoopState(
        ..state,
        phase: new_phase,
        iteration: new_iteration,
        revert_count: state.revert_count + 1,
        tests_were_green: False,
      )
    }
    TestFailed, _, TDD15Phase(_) -> {
      case state.current_tdd15_phase {
        option.Some(phase) -> {
          let phases.PhaseMeta(number: phase_num, ..) = phases.phase_meta(phase)
          case advance_tdd15_phase(state, phase_num, False) {
            Ok(updated) -> updated
            Error(err) -> {
              let msg = "TDD15 advance failed: " <> err
              logging.log(logging.Error, msg, dict.new())
              state
            }
          }
        }
        option.None -> state
      }
    }
    BudgetExhausted, _, _ -> {
      FactoryLoopState(..state, phase: Failed)
    }
    _, _, _ -> {
      FactoryLoopState(..state, phase: new_phase, iteration: new_iteration)
    }
  }
}

fn advance_tdd15_phase(
  state: FactoryLoopState,
  current_phase: Int,
  passed: Bool,
) -> Result(FactoryLoopState, String) {
  case state.bead_id, state.tdd15_route {
    option.Some(bead_id), option.Some(route) -> {
      let assert Ok(progress) = state.load_progress(bead_id)

      let updated_progress = case passed {
        True -> {
          let updated =
            state.update_phase_status(progress, current_phase, state.Completed)
          state.mark_gate_result(updated, current_phase, True)
        }
        False -> {
          let updated =
            state.update_phase_status(progress, current_phase, state.Failed)
          state.increment_attempt(updated, current_phase)
        }
      }

      let assert Ok(_) = state.save_progress(bead_id, updated_progress)

      let assert Ok(current) = phases.phase_by_number(current_phase)
      case phases.next_phase(current, route) {
        Ok(next_phase) -> {
          let phases.PhaseMeta(number: next_num, ..) =
            phases.phase_meta(next_phase)
          let next_progress =
            state.update_phase_status(
              updated_progress,
              next_num,
              state.InProgress,
            )
          let assert Ok(_) = state.save_progress(bead_id, next_progress)

          signal_bus.broadcast(
            state.signal_bus,
            signal_bus.TDD15PhaseComplete(next_num, passed),
          )

          Ok(
            FactoryLoopState(
              ..state,
              current_tdd15_phase: option.Some(next_phase),
              tests_were_green: passed,
            ),
          )
        }
        Error(_) -> {
          Ok(
            FactoryLoopState(
              ..state,
              phase: Completed,
              current_tdd15_phase: option.None,
            ),
          )
        }
      }
    }
    _, _ -> Error("TDD15 not initialized")
  }
}

fn transition(from: Phase, event: Event) -> Phase {
  case from, event {
    Implementing, TestPassed -> Reviewing
    Implementing, TestFailed -> Implementing
    Implementing, MaxIterationsReached -> Failed
    Implementing, BudgetExhausted -> Failed
    Reviewing, TestPassed -> Reviewing
    Reviewing, TestFailed -> Failed
    Pushing, TestFailed -> Failed
    Pushing, PushSuccess -> Completed
    Pushing, PushConflict -> Rebasing
    Rebasing, TestFailed -> Failed
    Rebasing, RebaseSuccess -> Pushing
    Rebasing, RebaseConflict -> Failed
    TDD15Phase(_), PhaseComplete(_, True) -> from
    TDD15Phase(_), PhaseFailed(_, _) -> from
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

fn format_phase(phase: Phase) -> String {
  case phase {
    Implementing -> "implementing"
    Reviewing -> "reviewing"
    Pushing -> "pushing"
    Rebasing -> "rebasing"
    Completed -> "completed"
    Failed -> "failed"
    TDD15Phase(n) -> "tdd15_phase_" <> int.to_string(n)
  }
}

fn format_event(event: Event) -> String {
  case event {
    TestPassed -> "TestPassed"
    TestFailed -> "TestFailed"
    PhaseComplete(n, _) -> "PhaseComplete_" <> int.to_string(n)
    PhaseFailed(n, _) -> "PhaseFailed_" <> int.to_string(n)
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

pub fn start_tdd15(
  loop: Subject(LoopMessage),
  bead_id: String,
  complexity: state.Complexity,
) -> Nil {
  process.send(loop, StartTDD15(bead_id: bead_id, complexity: complexity))
}

pub fn has_budget(state: FactoryLoopState) -> Bool {
  state.total_tokens_used < state.token_budget
}

pub fn budget_utilization(state: FactoryLoopState) -> Int {
  case state.token_budget {
    0 -> 100
    budget -> state.total_tokens_used * 100 / budget
  }
}

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

pub fn shutdown(loop: Subject(LoopMessage)) -> Nil {
  process.send(loop, Shutdown)
}
