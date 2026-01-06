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
  RegisterWorkspace(workspace: Workspace)
  GetWorkspace(id: WorkspaceId, reply_with: Subject(Result(Workspace, String)))
  ListWorkspaces(reply_with: Subject(Result(List(Workspace), String)))
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

  let builder =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> Ok(started.subject)
    Error(_) -> Error(InitializationFailed)
  }
}

/// Internal message handler for the workspace manager actor.
fn handle_message(
  state: WorkspaceManagerState,
  message: WorkspaceManagerMessage,
) -> actor.Next(WorkspaceManagerState, WorkspaceManagerMessage) {
  case message {
    RegisterWorkspace(workspace) -> {
      let new_workspaces = dict.insert(state.workspaces, workspace.id, workspace)
      actor.continue(WorkspaceManagerState(workspaces: new_workspaces))
    }
    GetWorkspace(id, reply_with) -> {
      let result = case dict.get(state.workspaces, id) {
        Ok(workspace) -> Ok(workspace)
        Error(Nil) -> Error("Workspace not found")
      }
      process.send(reply_with, result)
      actor.continue(state)
    }
    ListWorkspaces(reply_with) -> {
      let workspaces = dict.values(state.workspaces)
      let result = Ok(workspaces)
      process.send(reply_with, result)
      actor.continue(state)
    }
  }
}

/// Queries all registered workspaces from the workspace manager.
///
/// Returns Ok(list) with all registered workspaces, or Error(msg) if the query fails.
pub fn query_workspaces(
  manager_subject: Subject(WorkspaceManagerMessage),
) -> Result(List(Workspace), String) {
  let reply_subject = process.new_subject()
  process.send(manager_subject, ListWorkspaces(reply_with: reply_subject))
  case process.receive(reply_subject, 5000) {
    Ok(response) -> response
    Error(Nil) -> Error("Query timeout")
  }
}

/// Queries a specific workspace by ID from the workspace manager.
///
/// Returns Ok(workspace) if found, or Error(msg) if not found or query fails.
pub fn query_workspace(
  manager_subject: Subject(WorkspaceManagerMessage),
  workspace_id: WorkspaceId,
) -> Result(Workspace, String) {
  let reply_subject = process.new_subject()
  process.send(manager_subject, GetWorkspace(id: workspace_id, reply_with: reply_subject))
  case process.receive(reply_subject, 5000) {
    Ok(response) -> response
    Error(Nil) -> Error("Query timeout")
  }
}
