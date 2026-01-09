//// Bead detector - Identifies new open beads by comparing snapshots.
//// Maintains state of previously seen beads and detects additions.

import bead_manager
import gleam/list
import gleam/result

pub type BeadSnapshot {
  BeadSnapshot(beads: List(bead_manager.Bead), last_check_ms: Int)
}

pub type DetectionResult {
  DetectionResult(
    new_beads: List(bead_manager.Bead),
    removed_beads: List(bead_manager.Bead),
    current_snapshot: BeadSnapshot,
  )
}

/// Create initial empty snapshot
pub fn empty_snapshot() -> BeadSnapshot {
  BeadSnapshot(beads: [], last_check_ms: 0)
}

/// Detect new beads by comparing current load against snapshot
pub fn detect_changes(
  db_path: String,
  previous: BeadSnapshot,
) -> Result(DetectionResult, String) {
  use current_beads <- result.try(bead_manager.load_open_beads(db_path))

  let new_beads = find_new_beads(current_beads, previous.beads)
  let removed_beads = find_removed_beads(current_beads, previous.beads)
  let snapshot = BeadSnapshot(beads: current_beads, last_check_ms: 0)

  Ok(DetectionResult(new_beads:, removed_beads:, current_snapshot: snapshot))
}

/// Find beads in current that aren't in previous
fn find_new_beads(
  current: List(bead_manager.Bead),
  previous: List(bead_manager.Bead),
) -> List(bead_manager.Bead) {
  let prev_ids = list.map(previous, fn(b) { b.id })

  current
  |> list.filter(fn(bead) { !list.contains(prev_ids, bead.id) })
}

/// Find beads in previous that aren't in current
fn find_removed_beads(
  current: List(bead_manager.Bead),
  previous: List(bead_manager.Bead),
) -> List(bead_manager.Bead) {
  let current_ids = list.map(current, fn(b) { b.id })

  previous
  |> list.filter(fn(bead) { !list.contains(current_ids, bead.id) })
}

/// Filter beads by minimum priority (lower number = higher priority)
pub fn filter_by_priority(
  beads: List(bead_manager.Bead),
  min_priority: String,
) -> List(bead_manager.Bead) {
  beads
  |> list.filter(fn(bead) {
    bead_manager.priority_to_int(bead.priority)
    <= string_to_priority(min_priority)
  })
}

/// Convert priority string to comparable value
fn string_to_priority(priority: String) -> Int {
  case priority {
    "0" | "P0" -> 0
    "1" | "P1" -> 1
    "2" | "P2" -> 2
    "3" | "P3" -> 3
    "4" | "P4" -> 4
    _ -> 2
  }
}
