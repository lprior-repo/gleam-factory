//// Heartbeat actor for monitoring test status of golden master.
////
//// Polls tests at configurable intervals, broadcasts TestFailure/TestPassing
//// signals on red-to-green or green-to-red transitions.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/otp/actor
import logging
import process as shell_process
import signal_bus

const default_timeout_ms = 5000

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
  Tick
  GetStatus(reply_with: Subject(TestStatus))
  StreamProgress(task_id: String, chunk: String)
}

type HeartbeatState {
  HeartbeatState(
    config: HeartbeatConfig,
    last_status: TestStatus,
    last_hash: String,
    signal_bus: Subject(signal_bus.SignalBusMessage),
    progress_buffer: List(#(String, String)),
    self_subject: Subject(HeartbeatMessage),
  )
}

pub type HeartbeatError {
  InitFailed
}

pub fn start_link(
  config: HeartbeatConfig,
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(HeartbeatMessage), HeartbeatError) {
  let initial =
    HeartbeatState(
      config:,
      last_status: Green,
      last_hash: "",
      signal_bus: bus,
      progress_buffer: [],
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> {
      logging.log(
        logging.Info,
        "Heartbeat started with test_cmd: " <> config.test_cmd,
        dict.from_list([#("interval_ms", int.to_string(config.interval_ms))]),
      )
      schedule_tick(started.data, config.interval_ms)
      Ok(started.data)
    }
    Error(_) -> {
      logging.log(logging.Error, "Heartbeat startup failed", dict.new())
      Error(InitFailed)
    }
  }
}

fn schedule_tick(subject: Subject(HeartbeatMessage), interval_ms: Int) -> Nil {
  let _ = process.send_after(subject, interval_ms, Tick)
  Nil
}

fn handle_message(
  state: HeartbeatState,
  msg: HeartbeatMessage,
) -> actor.Next(HeartbeatState, HeartbeatMessage) {
  case msg {
    GetStatus(reply) -> {
      process.send(reply, state.last_status)
      actor.continue(state)
    }
    Tick -> {
      let new_status = run_tests(state.config)
      let new_state = update_status(state, new_status)
      schedule_tick(process.self(), state.config.interval_ms)
      actor.continue(new_state)
    }
    StreamProgress(task_id, chunk) -> {
      let new_buffer = [#(task_id, chunk), ..state.progress_buffer]
      actor.continue(HeartbeatState(..state, progress_buffer: new_buffer))
    }
  }
}

fn update_status(
  state: HeartbeatState,
  new_status: TestStatus,
) -> HeartbeatState {
  case state.last_status, new_status {
    Green, Red -> {
      logging.log(
        logging.Error,
        "Tests transitioned from Green to Red",
        dict.new(),
      )
      signal_bus.broadcast(state.signal_bus, signal_bus.TestFailure)
      HeartbeatState(..state, last_status: Red)
    }
    Red, Green -> {
      logging.log(
        logging.Info,
        "Tests transitioned from Red to Green",
        dict.new(),
      )
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
  process.send(hb, StreamProgress(task_id, chunk))
}
