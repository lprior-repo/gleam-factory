//// Type definitions for process management and workspace state.

import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/string

/// ProcessId wraps a process identifier to provide type-safe process references.
pub opaque type ProcessId {
  ProcessId(pid: Pid)
}

/// Converts a process Pid to a ProcessId opaque type.
pub fn from_pid(pid: Pid) -> ProcessId {
  ProcessId(pid)
}

/// Converts a ProcessId back to its underlying Pid.
pub fn to_pid(process_id: ProcessId) -> Pid {
  let ProcessId(pid) = process_id
  pid
}

/// WorkspaceType represents the classification of a workspace.
pub type WorkspaceType {
  Jj
  Reflink
}

/// WorkspaceId provides type-safe workspace identification.
pub opaque type WorkspaceId {
  WorkspaceId(String)
}

/// Converts a string to a WorkspaceId opaque type.
pub fn new_workspace_id(id: String) -> WorkspaceId {
  WorkspaceId(id)
}

/// Workspace represents a managed workspace with metadata.
pub type Workspace {
  Workspace(
    id: WorkspaceId,
    path: String,
    workspace_type: WorkspaceType,
    owner_pid: ProcessId,
    created_at: String,
  )
}

/// GitHash is an opaque type wrapping a validated git SHA-1 hash string.
/// The hash is guaranteed to be exactly 40 lowercase hexadecimal characters.
pub opaque type GitHash {
  GitHash(hash: String)
}

/// Parses a string into a GitHash, validating that it is exactly 40 lowercase
/// hexadecimal characters (valid git SHA-1 hash format).
///
/// Returns Ok(GitHash) if the string is valid, Error(String) with a descriptive
/// message if validation fails.
///
/// Validation rules:
/// - Must be exactly 40 characters long
/// - Must contain only lowercase hexadecimal characters (0-9, a-f)
/// - Uppercase letters (A-F) are rejected to ensure consistency with git output
pub fn git_hash_parse(input: String) -> Result(GitHash, String) {
  let trimmed = string.trim(input)
  let length = string.length(trimmed)

  case length == 40 {
    False ->
      Error(
        "Invalid git hash: expected 40 characters, got "
        <> int.to_string(length),
      )
    True -> {
      // Validate that all characters are lowercase hexadecimal (0-9, a-f)
      case is_valid_hex(trimmed) {
        True -> Ok(GitHash(trimmed))
        False ->
          Error(
            "Invalid git hash: must contain only lowercase hexadecimal characters (0-9, a-f)",
          )
      }
    }
  }
}

/// Converts a GitHash back to its string representation.
/// This unwraps the opaque type to access the underlying hash string.
///
/// This function is needed for serialization, display, and comparison operations
/// where we need to work with the raw hash value.
pub fn git_hash_to_string(git_hash: GitHash) -> String {
  let GitHash(hash) = git_hash
  hash
}

/// Internal helper to validate that a string contains only lowercase hex characters.
/// Returns True if all characters are in [0-9a-f], False otherwise.
fn is_valid_hex(input: String) -> Bool {
  input
  |> string.split("")
  |> list.all(fn(char) {
    case char {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      "a" | "b" | "c" | "d" | "e" | "f" -> True
      _ -> False
    }
  })
}

/// AcpNotification represents an Agent Communication Protocol notification.
pub type AcpNotification {
  AcpNotification(session_id: String, method: String)
}
