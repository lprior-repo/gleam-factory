//// Bead manager for loading and broadcasting beads from SQLite database.
////
//// Scans .beads/beads.db for open beads and broadcasts BeadAssigned signals.

import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/string
import signal_bus
import signals
import process as shell_process

pub type Bead {
  Bead(
    id: String,
    title: String,
    description: String,
    priority: String,
  )
}

pub fn load_open_beads(db_path: String) -> Result(List(Bead), String) {
  let query =
    "SELECT id, title, description, priority FROM issues WHERE status = 'open' ORDER BY priority ASC, id ASC"
  case
    shell_process.run_command(
      "sqlite3",
      ["-separator", "|", db_path, query],
      "",
    )
  {
    Ok(shell_process.Success(stdout, _, _)) -> {
      stdout
      |> string.split("\n")
      |> list.filter(fn(line) { string.length(string.trim(line)) > 0 })
      |> list.map(parse_bead_line)
      |> list.filter_map(fn(r) { r })
      |> Ok
    }
    Ok(shell_process.Failure(err, _)) -> Error("sqlite3 failed: " <> err)
    Error(e) -> Error(e)
  }
}

fn parse_bead_line(line: String) -> Result(Bead, Nil) {
  case string.split(line, "|") {
    [id, title, description, priority, ..] ->
      Ok(Bead(id:, title:, description:, priority:))
    [id, title, description] ->
      Ok(Bead(id:, title:, description:, priority: "2"))
    _ -> Error(Nil)
  }
}

pub fn broadcast_beads(
  beads: List(Bead),
  bus: Subject(signal_bus.SignalBusMessage),
) -> Int {
  beads
  |> list.map(fn(bead) {
    let assigned = bead_to_signal(bead)
    signal_bus.broadcast(bus, signal_bus.BeadAssigned(assigned))
    1
  })
  |> list.fold(0, fn(acc, x) { acc + x })
}

fn bead_to_signal(bead: Bead) -> signals.BeadAssigned {
  signals.BeadAssigned(
    task_id: bead.id,
    spec: bead.description,
    requirements: parse_requirements(bead.description),
    priority: bead.priority,
    assigned_at: "",
  )
}

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

pub fn get_bead_count(db_path: String) -> Result(Int, String) {
  case load_open_beads(db_path) {
    Ok(beads) -> Ok(list.length(beads))
    Error(e) -> Error(e)
  }
}

pub type BeadStatus {
  Open
  InProgress
  Completed
  Failed
}

pub fn update_bead_state(
  bead_id: String,
  new_status: BeadStatus,
  close_reason: String,
) -> Result(Nil, String) {
  let status_str = status_to_string(new_status)
  let args = case new_status {
    Completed | Failed -> ["update", bead_id, "--status", status_str, "--close-reason", close_reason]
    _ -> ["update", bead_id, "--status", status_str]
  }
  case shell_process.run_command("bd", args, "") {
    Ok(shell_process.Success(_, _, _)) -> Ok(Nil)
    Ok(shell_process.Failure(err, _)) -> Error("bd update failed: " <> err)
    Error(e) -> Error(e)
  }
}

fn status_to_string(status: BeadStatus) -> String {
  case status {
    Open -> "open"
    InProgress -> "in_progress"
    Completed -> "completed"
    Failed -> "failed"
  }
}

pub fn mark_bead_completed(bead_id: String, reason: String) -> Result(Nil, String) {
  update_bead_state(bead_id, Completed, reason)
}

pub fn mark_bead_failed(bead_id: String, reason: String) -> Result(Nil, String) {
  update_bead_state(bead_id, Failed, reason)
}
