// Worktree module - Git/jj workspace isolation
// Manages creating, retrieving, and removing isolated work directories

import domain
import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import logging
import process
import repo

const workspaces_dir = ".factory-workspaces"

const factory_dir = ".factory"

const branch_prefix = "feat/"

pub type Worktree {
  Worktree(
    slug: String,
    path: String,
    branch: String,
    language: domain.Language,
  )
}

pub fn create_worktree(
  slug: String,
  language: domain.Language,
  repo_root: String,
) -> Result(Worktree, String) {
  use _ <- result.try(check_slug_not_exists(slug, repo_root))

  let workspaces_base = repo_root <> "/" <> workspaces_dir
  let unique_id = generate_unique_id()
  let worktree_name = slug <> "-" <> unique_id
  let worktree_path = workspaces_base <> "/" <> worktree_name
  let branch = branch_prefix <> slug

  logging.log(
    logging.Info,
    "Creating worktree",
    dict.from_list([
      #("slug", slug),
      #("path", worktree_path),
      #("branch", branch),
    ]),
  )

  use _ <- result.try(create_base_dir(workspaces_base, repo_root))
  use _ <- result.try(create_jj_workspace(
    worktree_name,
    worktree_path,
    repo_root,
  ))
  use _ <- result.try(create_bookmark(worktree_path, branch, repo_root))
  use _ <- result.try(create_symlink(worktree_path, slug, repo_root))

  logging.log(
    logging.Info,
    "Worktree created",
    dict.from_list([#("slug", slug), #("path", worktree_path)]),
  )

  Ok(Worktree(slug, worktree_path, branch, language))
}

fn check_slug_not_exists(slug: String, repo_root: String) -> Result(Nil, String) {
  let symlink_path = repo_root <> "/" <> factory_dir <> "/" <> slug

  case
    process.run_command("test", ["-e", symlink_path], repo_root)
    |> result.map_error(fn(_) { "Failed to check for existing slug" })
  {
    Ok(process.Success(_, _, 0)) -> Error("Slug already exists: " <> slug)
    _ -> Ok(Nil)
  }
}

fn create_base_dir(
  workspaces_base: String,
  repo_root: String,
) -> Result(Nil, String) {
  run_command_checked(
    "mkdir",
    ["-p", workspaces_base],
    repo_root,
    "Could not create workspaces directory",
  )
}

fn create_jj_workspace(
  worktree_name: String,
  worktree_path: String,
  repo_root: String,
) -> Result(Nil, String) {
  // Try jj first, fall back to git worktree
  case
    process.run_command(
      "jj",
      ["workspace", "add", "--name", worktree_name, worktree_path],
      repo_root,
    )
  {
    Ok(process.Success(_, _, 0)) -> Ok(Nil)
    _ -> create_git_worktree(worktree_name, worktree_path, repo_root)
  }
}

fn create_git_worktree(
  worktree_name: String,
  worktree_path: String,
  repo_root: String,
) -> Result(Nil, String) {
  let branch = branch_prefix <> worktree_name
  run_command_checked(
    "git",
    ["-C", repo_root, "worktree", "add", worktree_path, "-b", branch],
    repo_root,
    "Could not create git worktree: " <> worktree_name,
  )
}

fn create_bookmark(
  worktree_path: String,
  branch: String,
  repo_root: String,
) -> Result(Nil, String) {
  // Try jj bookmark first, fall back to git (git branch already created by worktree add)
  case
    process.run_command(
      "jj",
      ["-R", worktree_path, "bookmark", "create", branch],
      repo_root,
    )
  {
    Ok(process.Success(_, _, 0)) -> Ok(Nil)
    // For git, the branch was already created by worktree add, so just succeed
    _ -> Ok(Nil)
  }
}

fn create_symlink(
  worktree_path: String,
  slug: String,
  repo_root: String,
) -> Result(Nil, String) {
  let symlink_dir = repo_root <> "/" <> factory_dir
  use _ <- result.try(run_command_checked(
    "mkdir",
    ["-p", symlink_dir],
    repo_root,
    "Could not create .factory directory",
  ))

  run_command_checked(
    "ln",
    ["-sf", worktree_path, symlink_dir <> "/" <> slug],
    repo_root,
    "Could not create symlink",
  )
}

fn run_command_checked(
  cmd: String,
  args: List(String),
  cwd: String,
  error_msg: String,
) -> Result(Nil, String) {
  process.run_command(cmd, args, cwd)
  |> result.try(fn(cmd_result) {
    process.check_success(cmd_result)
    |> result.map_error(fn(_) { error_msg })
  })
}

pub fn get_worktree(slug: String, repo_root: String) -> Result(Worktree, String) {
  let symlink_path = repo_root <> "/" <> factory_dir <> "/" <> slug

  process.run_command("readlink", [symlink_path], repo_root)
  |> result.map_error(fn(_) { "Worktree not found: " <> slug })
  |> result.try(fn(result) {
    case result {
      process.Success(path, _, _) -> {
        let worktree_path = string.trim(path)
        let language =
          repo.detect_language(worktree_path)
          |> result.unwrap(domain.Go)
        Ok(Worktree(slug, worktree_path, branch_prefix <> slug, language))
      }
      _ -> Error("Could not resolve worktree path")
    }
  })
}

pub fn remove_worktree(slug: String, repo_root: String) -> Result(Nil, String) {
  logging.log(
    logging.Info,
    "Removing worktree",
    dict.from_list([#("slug", slug)]),
  )

  use wt <- result.try(get_worktree(slug, repo_root))

  // Try jj workspace forget first
  let _ =
    process.run_command(
      "jj",
      ["-R", repo_root, "workspace", "forget", slug <> "-*"],
      repo_root,
    )

  // Also try git worktree remove
  let _ =
    process.run_command(
      "git",
      ["-C", repo_root, "worktree", "remove", wt.path, "--force"],
      repo_root,
    )

  // If git worktree remove didn't clean up, remove manually
  use _ <- result.try(run_command_checked(
    "rm",
    ["-rf", wt.path],
    repo_root,
    "Could not remove worktree directory",
  ))

  let symlink_path = repo_root <> "/" <> factory_dir <> "/" <> slug
  let _ = process.run_command("rm", ["-f", symlink_path], repo_root)

  logging.log(
    logging.Info,
    "Worktree removed",
    dict.from_list([#("slug", slug), #("path", wt.path)]),
  )

  Ok(Nil)
}

pub fn list_worktrees(repo_root: String) -> Result(List(Worktree), String) {
  let factory_path = repo_root <> "/" <> factory_dir

  process.run_command("ls", ["-1", factory_path], repo_root)
  |> result.map_error(fn(_) { "Could not list worktrees" })
  |> result.try(fn(result) {
    case result {
      process.Success(output, _, _) ->
        output
        |> string.split("\n")
        |> list.filter(fn(line) { string.length(string.trim(line)) > 0 })
        |> list.try_map(fn(slug) { get_worktree(string.trim(slug), repo_root) })
      _ -> Ok([])
    }
  })
}

const unique_id_offset = 2

const unique_id_length = 8

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

fn generate_unique_id() -> String {
  case process.run_command("date", ["+%s%N"], ".") {
    Ok(process.Success(timestamp, _, _)) ->
      timestamp
      |> string.trim
      |> string.slice(unique_id_offset, unique_id_length)
    _ ->
      case process.run_command("sh", ["-c", "echo $RANDOM$RANDOM"], ".") {
        Ok(process.Success(rand, _, _)) -> string.trim(rand)
        _ -> int.to_string(int.absolute_value(erlang_unique_integer()))
      }
  }
}

pub fn worktrees_base(repo_root: String) -> String {
  repo_root <> "/" <> workspaces_dir
}
