//// Heartbeat actor for monitoring test status of golden master.
////
//// Polls tests at configurable intervals, broadcasts TestFailure/TestPassing
//// signals on red-to-green or green-to-red transitions.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import logging
import process as shell_process
import signal_bus

const default_timeout_ms = 5000

const max_buffer_size = 1000

pub type TestStatus {
  Green
  Red
}

pub type HeartbeatConfig {
  HeartbeatConfig(
    interval_ms: Int,
    test_cmd: String,
    golden_master_path: String,
  )
}

pub type HeartbeatMessage {
  SetSelf(subject: Subject(HeartbeatMessage))
  Tick
  GetStatus(reply_with: Subject(TestStatus))
  StreamProgress(task_id: String, chunk: String)
  Shutdown
}

type HeartbeatState {
  HeartbeatState(
    config: HeartbeatConfig,
    last_status: TestStatus,
    last_hash: String,
    signal_bus: Subject(signal_bus.SignalBusMessage),
    progress_buffer: List(#(String, String)),
    self_subject: Subject(HeartbeatMessage),
    shutting_down: Bool,
    timer_ref: Int,
  )
}

pub type HeartbeatError {
  InitFailed
}

pub fn start_link(
  config: HeartbeatConfig,
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(HeartbeatMessage), HeartbeatError) {
  // Create a placeholder subject that will be replaced after actor starts
  let placeholder_subject = process.new_subject()
  let initial =
    HeartbeatState(
      config:,
      last_status: Red,
      last_hash: "",
      signal_bus: bus,
      progress_buffer: [],
      self_subject: placeholder_subject,
      shutting_down: False,
      timer_ref: 0,
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> {
      logging.log(
        logging.Info,
        "Heartbeat started with test_cmd: " <> config.test_cmd,
        dict.from_list([#("interval_ms", int.to_string(config.interval_ms))]),
      )
      process.send(started.data, SetSelf(subject: started.data))
      Ok(started.data)
    }
    Error(_) -> {
      logging.log(logging.Error, "Heartbeat startup failed", dict.new())
      Error(InitFailed)
    }
  }
}

fn schedule_tick(subject: Subject(HeartbeatMessage), interval_ms: Int) -> Int {
  erlang_send_after(interval_ms, subject, Tick)
}

fn handle_message(
  state: HeartbeatState,
  msg: HeartbeatMessage,
) -> actor.Next(HeartbeatState, HeartbeatMessage) {
  case msg {
    SetSelf(subject:) -> {
      process.send(subject, Tick)
      actor.continue(HeartbeatState(..state, self_subject: subject))
    }
    GetStatus(reply_with:) -> {
      process.send(reply_with, state.last_status)
      actor.continue(state)
    }
    Tick -> {
      case state.shutting_down {
        True -> actor.stop()
        False -> {
          let new_status = run_tests(state.config)
          let new_timer_ref =
            schedule_tick(state.self_subject, state.config.interval_ms)
          let new_state = update_status(state, new_status)
          actor.continue(HeartbeatState(..new_state, timer_ref: new_timer_ref))
        }
      }
    }
    StreamProgress(task_id:, chunk:) -> {
      let new_buffer = [#(task_id, chunk), ..state.progress_buffer]
      let trimmed_buffer = list.take(new_buffer, max_buffer_size)
      actor.continue(HeartbeatState(..state, progress_buffer: trimmed_buffer))
    }
    Shutdown -> {
      // Cancel any pending timer
      let _ = erlang_cancel_timer(state.timer_ref)
      logging.log(logging.Info, "Heartbeat shutting down", dict.new())
      actor.stop()
    }
  }
}

fn update_status(
  state: HeartbeatState,
  new_status: TestStatus,
) -> HeartbeatState {
  case state.last_status, new_status {
    Green, Red -> {
      signal_bus.broadcast(state.signal_bus, signal_bus.TestFailure)
      HeartbeatState(..state, last_status: Red)
    }
    Red, Green -> {
      signal_bus.broadcast(state.signal_bus, signal_bus.TestPassing)
      HeartbeatState(..state, last_status: Green)
    }
    _, _ -> HeartbeatState(..state, last_status: new_status)
  }
}

fn run_tests(config: HeartbeatConfig) -> TestStatus {
  case
    shell_process.run_command(config.test_cmd, [], config.golden_master_path)
  {
    Ok(shell_process.Success(_, _, _)) -> Green
    _ -> Red
  }
}

pub fn get_status(hb: Subject(HeartbeatMessage)) -> TestStatus {
  let reply = process.new_subject()
  process.send(hb, GetStatus(reply_with: reply))
  case process.receive(reply, default_timeout_ms) {
    Ok(status) -> status
    Error(Nil) -> Red
  }
}

pub fn tick(hb: Subject(HeartbeatMessage)) -> Nil {
  process.send(hb, Tick)
}

pub fn stream_progress(
  hb: Subject(HeartbeatMessage),
  task_id: String,
  chunk: String,
) -> Nil {
  process.send(hb, StreamProgress(task_id:, chunk:))
}

/// Shutdown the heartbeat actor.
pub fn shutdown(hb: Subject(HeartbeatMessage)) -> Nil {
  process.send(hb, Shutdown)
}

@external(erlang, "erlang", "send_after")
fn erlang_send_after(
  time: Int,
  to: process.Subject(HeartbeatMessage),
  msg: HeartbeatMessage,
) -> Int

@external(erlang, "erlang", "cancel_timer")
fn erlang_cancel_timer(timer: Int) -> Int
