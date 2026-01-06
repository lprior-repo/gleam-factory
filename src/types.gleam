//// Type definitions for process management.

/// ProcessId wraps a process identifier to provide type-safe process references.
pub opaque type ProcessId {
  ProcessId(pid: String)
}

/// Converts a process identifier string to a ProcessId opaque type.
pub fn from_pid(pid: String) -> ProcessId {
  ProcessId(pid)
}

/// Converts a ProcessId back to its underlying process identifier string.
pub fn to_pid(process_id: ProcessId) -> String {
  let ProcessId(pid) = process_id
  pid
}
