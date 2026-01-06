//// Workspace manager OTP actor for managing workspace state.

import gleam/dict.{type Dict}
import gleam/otp/actor
import gleam/result
import types.{type Workspace, type WorkspaceId}

/// Message type for workspace manager actor.
pub type WorkspaceManagerMessage {
  GetWorkspace(id: WorkspaceId)
}

/// Error type for workspace manager initialization.
pub type WorkspaceManagerError {
  InitializationFailed
}

/// Starts the workspace manager actor.
/// Returns Ok(subject) on successful initialization, Error otherwise.
pub fn start_link() -> Result(actor.Subject(WorkspaceManagerMessage), WorkspaceManagerError) {
  let initial_state: Dict(WorkspaceId, Workspace) = dict.new()

  actor.start(initial_state, handle_message)
  |> result.map_error(fn(_) { InitializationFailed })
}

fn handle_message(
  message: WorkspaceManagerMessage,
  state: Dict(WorkspaceId, Workspace),
) -> actor.Next(WorkspaceManagerMessage, Dict(WorkspaceId, Workspace)) {
  case message {
    GetWorkspace(_id) -> actor.continue(state)
  }
}
