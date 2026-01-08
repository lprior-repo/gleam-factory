//// Beads broadcaster - Broadcasts BeadAssigned signals for new beads.
//// Monitors bead changes and publishes signals to the signal bus.

import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/string
import bead_detector
import bead_manager
import signal_bus
import signals

pub type BroadcasterState {
  BroadcasterState(
    db_path: String,
    bus: Subject(signal_bus.SignalBusMessage),
    snapshot: bead_detector.BeadSnapshot,
  )
}

/// Start broadcaster actor
pub fn start(
  db_path: String,
  bus: Subject(signal_bus.SignalBusMessage),
) -> process.Pid {
  let initial_snapshot = bead_detector.empty_snapshot()
  let state = BroadcasterState(db_path:, bus:, snapshot: initial_snapshot)

  process.spawn(fn() {
    broadcast_loop(state)
  })
}

/// Main broadcast loop - detects changes and publishes signals
fn broadcast_loop(state: BroadcasterState) -> Nil {
  case bead_detector.detect_changes(state.db_path, state.snapshot) {
    Ok(detection) -> {
      // Broadcast signals for new beads
      broadcast_new_beads(detection.new_beads, state.bus)

      let updated = BroadcasterState(
        ..state,
        snapshot: detection.current_snapshot,
      )

      // Continue polling
      process.sleep(5000)
      broadcast_loop(updated)
    }
    Error(_) -> {
      // Retry on error
      process.sleep(5000)
      broadcast_loop(state)
    }
  }
}

/// Broadcast BeadAssigned signal for each new bead
fn broadcast_new_beads(
  beads: List(bead_manager.Bead),
  bus: Subject(signal_bus.SignalBusMessage),
) -> Nil {
  beads
  |> list.each(fn(bead) {
    let signal = bead_to_assigned_signal(bead)
    signal_bus.broadcast(bus, signal_bus.BeadAssigned(signal))
  })
}

/// Convert Bead to BeadAssigned signal
fn bead_to_assigned_signal(bead: bead_manager.Bead) -> signals.BeadAssigned {
  signals.BeadAssigned(
    task_id: bead.id,
    spec: bead.description,
    requirements: parse_requirements(bead.description),
    priority: bead.priority,
    assigned_at: "",
  )
}

/// Parse requirements from description
fn parse_requirements(description: String) -> List(String) {
  description
  |> string.split("\n")
  |> list.filter(fn(line) {
    let trimmed = string.trim(line)
    string.starts_with(trimmed, "-") || string.starts_with(trimmed, "*")
  })
  |> list.map(fn(line) {
    line
    |> string.trim
    |> string.drop_start(2)
    |> string.trim
  })
}
