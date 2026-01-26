//// Factory dispatcher actor - Manages factory loop spawning.
//// Subscribes to BeadAssigned signals and spawns factory loops for new beads.

import factory_loop
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/otp/actor
import gleam/string
import signal_bus
import signals

pub type DispatcherState {
  DispatcherState(
    signal_bus: Subject(signal_bus.SignalBusMessage),
    active_loops: dict.Dict(String, Subject(factory_loop.LoopMessage)),
    workspace_root: String,
  )
}

pub type DispatcherMessage {
  OnBeadAssigned(signal: signals.BeadAssigned)
  Stop
}

/// Start dispatcher actor using OTP actor framework
pub fn start(
  bus: Subject(signal_bus.SignalBusMessage),
  workspace_root: String,
) -> process.Pid {
  let state =
    DispatcherState(signal_bus: bus, active_loops: dict.new(), workspace_root:)

  let builder = actor.new(state) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(_) -> {
      process.self()
    }
    Error(_) -> {
      process.self()
    }
  }
}

/// Handle dispatcher messages
fn handle_message(
  state: DispatcherState,
  msg: DispatcherMessage,
) -> actor.Next(DispatcherState, DispatcherMessage) {
  case msg {
    OnBeadAssigned(bead) -> {
      let new_state = handle_bead_assigned(state, bead)
      actor.continue(new_state)
    }
    Stop -> {
      actor.stop()
    }
  }
}

fn handle_bead_assigned(
  state: DispatcherState,
  bead: signals.BeadAssigned,
) -> DispatcherState {
  let task_id_str = signals.unwrap_task_id(bead.task_id)
  let timestamp_str =
    bead.assigned_at
    |> signals.unwrap_timestamp
    |> int.to_string
  let loop_id = string.concat([task_id_str, "-", timestamp_str])
  let workspace_path = string.concat([state.workspace_root, "/", task_id_str])

  case
    factory_loop.start_link(loop_id, bead, workspace_path, state.signal_bus)
  {
    Ok(loop_subject) -> {
      let new_loops = dict.insert(state.active_loops, loop_id, loop_subject)
      DispatcherState(..state, active_loops: new_loops)
    }
    Error(_) -> state
  }
}
