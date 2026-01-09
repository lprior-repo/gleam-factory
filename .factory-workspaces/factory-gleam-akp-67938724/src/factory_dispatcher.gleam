//// Factory dispatcher actor - Manages factory loop spawning.
//// Subscribes to BeadAssigned signals and spawns factory loops for new beads.

import factory_loop
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/string
import signal_bus
import signals

pub type DispatcherState {
  DispatcherState(
    signal_bus: Subject(signal_bus.SignalBusMessage),
    active_loops: dict.Dict(String, Subject(factory_loop.LoopMessage)),
    workspace_root: String,
    dispatcher_subject: Subject(DispatcherMessage),
  )
}

pub type DispatcherMessage {
  OnBeadAssigned(signal: signals.BeadAssigned)
  Stop
}

/// Start dispatcher actor
pub fn start(
  bus: Subject(signal_bus.SignalBusMessage),
  workspace_root: String,
) -> process.Pid {
  let dispatcher_subject = process.new_subject()
  let signal_subject = process.new_subject()
  let state =
    DispatcherState(
      signal_bus: bus,
      active_loops: dict.new(),
      workspace_root:,
      dispatcher_subject:,
    )

  process.spawn(fn() {
    let _ =
      signal_bus.subscribe(
        bus,
        signal_bus.BeadAssigned(signals.BeadAssigned(
          task_id: signals.task_id(""),
          spec: "",
          requirements: [],
          priority: signals.P2,
          assigned_at: signals.timestamp(0),
        )),
        signal_subject,
      )
    signal_adapter_loop(state, signal_subject)
  })
}

/// Adapts signal_bus.Signal messages and spawns factory loops
fn signal_adapter_loop(
  state: DispatcherState,
  signal_subject: Subject(signal_bus.Signal),
) -> Nil {
  case process.receive(signal_subject, 100) {
    Ok(signal_bus.BeadAssigned(bead)) -> {
      let new_state = handle_bead_assigned(state, bead)
      signal_adapter_loop(new_state, signal_subject)
    }
    Ok(_) -> signal_adapter_loop(state, signal_subject)
    Error(Nil) -> signal_adapter_loop(state, signal_subject)
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
