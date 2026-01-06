//// Type definitions for process management and workspace state.

import gleam/erlang/process.{type Pid}

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
