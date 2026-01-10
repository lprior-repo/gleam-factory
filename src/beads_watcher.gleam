//// Beads watcher actor - Monitors .beads/issues.jsonl for changes.
//// Polls file periodically and detects changes.

import gleam/erlang/process
import gleam/int
import gleam/result
import gleam/string
import simplifile

pub type WatcherState {
  WatcherState(path: String, last_hash: String, poll_interval_ms: Int)
}

/// Compute hash of file contents using erlang:phash2 (32-bit hash)
fn compute_hash(content: String) -> String {
  content
  |> erlang_phash2
  |> int.to_string
}

@external(erlang, "erlang", "phash2")
fn erlang_phash2(term: a) -> Int

/// Initialize watcher with polling interval
pub fn new(path: String, poll_interval_ms: Int) -> WatcherState {
  WatcherState(path:, last_hash: "", poll_interval_ms:)
}

/// Start watcher as async task with periodic polling
pub fn start(path: String, poll_interval_ms: Int) -> process.Pid {
  let state = new(path, poll_interval_ms)
  process.spawn(fn() { poll_loop(state) })
}

/// Main polling loop - checks file periodically
fn poll_loop(state: WatcherState) -> Nil {
  case read_file_hash(state.path) {
    Ok(new_hash) -> {
      case new_hash {
        _ if new_hash != state.last_hash -> {
          // File changed detected
          let updated = WatcherState(..state, last_hash: new_hash)
          process.sleep(state.poll_interval_ms)
          poll_loop(updated)
        }
        _ -> {
          process.sleep(state.poll_interval_ms)
          poll_loop(state)
        }
      }
    }
    Error(_) -> {
      process.sleep(state.poll_interval_ms)
      poll_loop(state)
    }
  }
}

/// Read file and compute hash for change detection
fn read_file_hash(path: String) -> Result(String, String) {
  simplifile.read(path)
  |> result.map(compute_hash)
  |> result.map_error(fn(_) { "Failed to read file" })
}
