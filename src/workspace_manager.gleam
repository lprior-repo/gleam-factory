//// Workspace manager OTP actor for managing workspace state.
////
//// This module provides the core actor skeleton for workspace management,
//// including actor initialization and message routing.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import types.{type WorkspaceId, type Workspace}

/// Message type for workspace manager actor.
pub type WorkspaceManagerMessage {
  GetWorkspace(id: WorkspaceId)
}

/// Error type for workspace manager initialization.
pub type WorkspaceManagerError {
  InitializationFailed
}

/// WorkspaceManagerState holds the actor's internal state.
type WorkspaceManagerState {
  WorkspaceManagerState(workspaces: dict.Dict(WorkspaceId, Workspace))
}

/// Starts the workspace manager OTP actor.
///
/// Returns Ok(subject) on successful actor initialization,
/// or Error(InitializationFailed) if startup fails.
pub fn start_link() -> Result(Subject(WorkspaceManagerMessage), WorkspaceManagerError) {
  let initial_state = WorkspaceManagerState(workspaces: dict.new())

  case
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start
  {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(InitializationFailed)
  }
}

/// Internal message handler for the workspace manager actor.
fn handle_message(
  state: WorkspaceManagerState,
  message: WorkspaceManagerMessage,
) -> actor.Next(WorkspaceManagerState, WorkspaceManagerMessage) {
  case message {
    GetWorkspace(_id) -> {
      // Placeholder: will implement workspace retrieval in future iterations
      actor.continue(state)
    }
  }
}
