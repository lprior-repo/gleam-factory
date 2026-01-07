//// Heartbeat actor for monitoring test status of golden master.
////
//// Polls tests at configurable intervals, broadcasts TestFailure/TestPassing
//// signals on red-to-green or green-to-red transitions.

import gleam/erlang/process.{type Subject}
import otp_actor as actor
import process as shell_process
import signal_bus

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
}

type HeartbeatState {
  HeartbeatState(
    config: HeartbeatConfig,
    last_status: TestStatus,
    last_hash: String,
    signal_bus: Subject(signal_bus.SignalBusMessage),
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
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(InitFailed)
  }
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
      let new_state = case state.last_status, new_status {
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
      actor.continue(new_state)
    }
  }
}

fn run_tests(config: HeartbeatConfig) -> TestStatus {
  case shell_process.run_command(config.test_cmd, [], config.golden_master_path)
  {
    Ok(shell_process.Success(_, _, _)) -> Green
    _ -> Red
  }
}

pub fn get_status(hb: Subject(HeartbeatMessage)) -> TestStatus {
  let reply = process.new_subject()
  process.send(hb, GetStatus(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(status) -> status
    Error(Nil) -> Red
  }
}

pub fn tick(hb: Subject(HeartbeatMessage)) -> Nil {
  process.send(hb, Tick)
}
