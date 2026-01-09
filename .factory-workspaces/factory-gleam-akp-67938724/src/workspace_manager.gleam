//// Workspace manager OTP actor for managing workspace state.
////
//// This module provides the core actor skeleton for workspace management,
//// including actor initialization and message routing.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/result
import otp_actor as actor
import process as shell_process
import simplifile
import types.{type Workspace, type WorkspaceId}

const timeout_ms = 5000

const workspace_prefix = "/dev/shm/factory-"

const jj_bookmark_prefix = "feat/"

const jj_parent_dir = "../"

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
pub fn start_link() -> Result(
  Subject(WorkspaceManagerMessage),
  WorkspaceManagerError,
) {
  let initial_state = WorkspaceManagerState(workspaces: dict.new())

  let builder =
    actor.new(initial_state)
    |> actor.on_message(handle_message)

  case actor.start(builder) {
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
    RegisterWorkspace(workspace) -> {
      let new_workspaces =
        dict.insert(state.workspaces, workspace.id, workspace)
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
              process.send(
                reply_with,
                Error("Failed to delete workspace directory"),
              )
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
  case process.receive(reply_subject, timeout_ms) {
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
  process.send(
    manager_subject,
    GetWorkspace(id: workspace_id, reply_with: reply_subject),
  )
  case process.receive(reply_subject, timeout_ms) {
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
  process.send(
    manager_subject,
    DestroyWorkspace(id: workspace_id, reply_with: reply_subject),
  )
  case process.receive(reply_subject, timeout_ms) {
    Ok(response) -> response
    Error(Nil) -> Error("Destroy timeout")
  }
}

fn make_workspace_path(slug: String) -> String {
  workspace_prefix <> slug
}

fn clean_workspace_path(workspace_path: String) -> Nil {
  let _ = simplifile.delete_all([workspace_path])
  Nil
}

fn copy_workspace_directory(
  source_path: String,
  workspace_path: String,
) -> Result(Nil, String) {
  simplifile.copy_directory(source_path, workspace_path)
  |> result.map_error(fn(e) { "copy failed: " <> simplifile.describe_error(e) })
}

fn build_workspace(
  workspace_id: WorkspaceId,
  workspace_path: String,
  workspace_type: types.WorkspaceType,
) -> Workspace {
  types.Workspace(
    id: workspace_id,
    path: workspace_path,
    workspace_type: workspace_type,
    owner_pid: types.from_pid(process.self()),
    created_at: "0",
  )
}

/// Creates a new reflink workspace with COW copy of source directory.
///
/// Creates a workspace in /dev/shm/factory-{slug} and copies contents from
/// source_path using cp --reflink for copy-on-write semantics.
///
/// Returns Ok(workspace) if successful, or Error(msg) if creation fails.
pub fn create_workspace_reflink(
  manager_subject: Subject(WorkspaceManagerMessage),
  slug: String,
  source_path: String,
) -> Result(Workspace, String) {
  let workspace_path = make_workspace_path(slug)
  let workspace_id = types.new_workspace_id(slug)

  clean_workspace_path(workspace_path)

  use _ <- result.try(copy_workspace_directory(source_path, workspace_path))

  let workspace = build_workspace(workspace_id, workspace_path, types.Reflink)

  actor.send(manager_subject, RegisterWorkspace(workspace))
  Ok(workspace)
}

/// Resolves the auto workspace strategy based on system capabilities.
///
/// Returns Reflink if /dev/shm exists, otherwise Jj.
pub fn resolve_auto_strategy() -> types.WorkspaceType {
  case simplifile.verify_is_directory("/dev/shm") {
    Ok(True) -> types.Reflink
    _ -> types.Jj
  }
}

fn check_cmd_success(
  result: shell_process.CommandResult,
  error_msg: String,
) -> Result(Nil, String) {
  case result {
    shell_process.Success(_, _, _) -> Ok(Nil)
    shell_process.Failure(stderr, _) -> Error(error_msg <> ": " <> stderr)
  }
}

fn make_jj_bookmark(slug: String) -> String {
  jj_bookmark_prefix <> slug
}

fn make_jj_workspace_path(source_path: String, slug: String) -> String {
  source_path <> jj_parent_dir <> slug
}

fn run_jj_workspace_add(
  slug: String,
  workspace_path: String,
  source_path: String,
) -> Result(Nil, String) {
  use add_result <- result.try(shell_process.run_command(
    "jj",
    ["workspace", "add", "--name", slug, workspace_path],
    source_path,
  ))
  check_cmd_success(add_result, "jj workspace add failed")
}

fn run_jj_bookmark_create(
  workspace_path: String,
  bookmark: String,
  source_path: String,
) -> Result(Nil, String) {
  use bookmark_result <- result.try(shell_process.run_command(
    "jj",
    ["-R", workspace_path, "bookmark", "create", bookmark],
    source_path,
  ))
  check_cmd_success(bookmark_result, "jj bookmark create failed")
}

/// Creates a new jj workspace with isolated jj bookmark.
///
/// Returns Ok(workspace) if successful, or Error(msg) if creation fails.
pub fn create_workspace_jj(
  manager_subject: Subject(WorkspaceManagerMessage),
  slug: String,
  source_path: String,
) -> Result(types.Workspace, String) {
  let bookmark = make_jj_bookmark(slug)
  let workspace_path = make_jj_workspace_path(source_path, slug)

  use _ <- result.try(run_jj_workspace_add(slug, workspace_path, source_path))
  use _ <- result.try(run_jj_bookmark_create(
    workspace_path,
    bookmark,
    source_path,
  ))

  let workspace_id = types.new_workspace_id(slug)
  let workspace = build_workspace(workspace_id, workspace_path, types.Jj)

  actor.send(manager_subject, RegisterWorkspace(workspace))
  Ok(workspace)
}
