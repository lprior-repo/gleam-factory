//// Factory dispatcher actor - Manages factory loop spawning.
//// Subscribes to BeadAssigned signals and spawns factory loops for new beads.

import gleam/erlang/process.{type Subject}
import gleam/dict
import signal_bus
import signals
import factory_loop

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

/// Start dispatcher actor
pub fn start(
  bus: Subject(signal_bus.SignalBusMessage),
  workspace_root: String,
) -> process.Pid {
  let state = DispatcherState(
    signal_bus: bus,
    active_loops: dict.new(),
    workspace_root:,
  )

  process.spawn(fn() {
    dispatcher_loop(state)
  })
}

/// Main dispatcher loop - handles bead assignment and loop spawning
fn dispatcher_loop(state: DispatcherState) -> Nil {
  // TODO: Subscribe to BeadAssigned signals via signal_bus
  // TODO: Spawn factory loops for new beads
  let _ = state
  process.sleep(5000)
  dispatcher_loop(state)
}
