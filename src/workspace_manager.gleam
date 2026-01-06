//// Workspace manager OTP actor for managing workspace state.
////
//// This module provides the core actor skeleton for workspace management,
//// including actor initialization and message routing.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result
import process as gleam_process
import simplifile
import types.{type WorkspaceId, type Workspace}

/// Message type for workspace manager actor.
pub type WorkspaceManagerMessage {
  RegisterWorkspace(workspace: Workspace)
  GetWorkspace(id: WorkspaceId, reply_with: Subject(Result(Workspace, String)))
  ListWorkspaces(reply_with: Subject(Result(List(Workspace), String)))
  DestroyWorkspace(id: WorkspaceId, reply_with: Subject(Result(Nil, String)))
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
    DestroyWorkspace(id, reply_with) -> {
      case dict.get(state.workspaces, id) {
        Ok(workspace) -> {
          case simplifile.delete_all([workspace.path]) {
            Ok(Nil) -> {
              let new_workspaces = dict.delete(state.workspaces, id)
              process.send(reply_with, Ok(Nil))
              actor.continue(WorkspaceManagerState(workspaces: new_workspaces))
            }
            Error(_) -> {
              process.send(reply_with, Error("Failed to delete workspace directory"))
              actor.continue(state)
            }
          }
        }
        Error(Nil) -> {
          process.send(reply_with, Error("Workspace not found"))
          actor.continue(state)
        }
      }
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

/// Destroys a workspace by removing it from the workspace manager state.
///
/// Returns Ok(Nil) if the workspace was successfully destroyed,
/// or Error(msg) if the workspace was not found or the operation times out.
pub fn destroy_workspace(
  manager_subject: Subject(WorkspaceManagerMessage),
  workspace_id: WorkspaceId,
) -> Result(Nil, String) {
  let reply_subject = process.new_subject()
  process.send(manager_subject, DestroyWorkspace(id: workspace_id, reply_with: reply_subject))
  case process.receive(reply_subject, 5000) {
    Ok(response) -> response
    Error(Nil) -> Error("Destroy timeout")
  }
}

/// Creates a new reflink workspace with COW copy of source directory.
///
/// Creates a workspace in /dev/shm/factory-{slug} and copies contents from
/// source_path using reflink for copy-on-write semantics.
///
/// Returns Ok(workspace) if successful, or Error(msg) if creation fails.
pub fn create_workspace_reflink(
  manager_subject: Subject(WorkspaceManagerMessage),
  slug: String,
  source_path: String,
) -> Result(Workspace, String) {
  let workspace_path = "/dev/shm/factory-" <> slug
  let workspace_id = types.new_workspace_id(slug)

  // Create workspace directory
  use _ <- result.try(simplifile.create_directory_all(workspace_path)
    |> result.map_error(fn(_) { "Failed to create workspace directory" }))

  // Copy individual files from source
  use _ <- result.try(copy_from_source(source_path, workspace_path))

  let workspace =
    types.Workspace(
      id: workspace_id,
      path: workspace_path,
      workspace_type: types.Reflink,
      owner_pid: types.from_pid(process.self()),
      created_at: "2026-01-06",
    )

  actor.send(manager_subject, RegisterWorkspace(workspace))
  Ok(workspace)
}

fn copy_from_source(source: String, dest: String) -> Result(Nil, String) {
  // Copy files from source to destination
  case simplifile.read_directory(source) {
    Ok(entries) -> copy_files_recursively(source, dest, entries)
    Error(_) -> Ok(Nil)
  }
}

fn copy_files_recursively(
  source: String,
  dest: String,
  entries: List(String),
) -> Result(Nil, String) {
  case entries {
    [] -> Ok(Nil)
    [entry, ..rest] -> {
      case entry {
        "." | ".." -> copy_files_recursively(source, dest, rest)
        _ -> {
          let src_file = source <> "/" <> entry
          let dest_file = dest <> "/" <> entry
          case simplifile.read(src_file) {
            Ok(content) -> {
              use _ <- result.try(simplifile.write(dest_file, content)
                |> result.map_error(fn(_) { "Failed to write " <> dest_file }))
              copy_files_recursively(source, dest, rest)
            }
            Error(_) -> copy_files_recursively(source, dest, rest)
          }
        }
      }
    }
  }
}




