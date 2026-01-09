// Worktree module - Git/jj workspace isolation
// Manages creating, retrieving, and removing isolated work directories

import domain
import gleam/list
import gleam/result
import gleam/string
import process

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
  let workspaces_base = repo_root <> "/" <> workspaces_dir
  let unique_id = generate_unique_id()
  let worktree_name = slug <> "-" <> unique_id
  let worktree_path = workspaces_base <> "/" <> worktree_name
  let branch = branch_prefix <> slug

  use _ <- result.try(create_base_dir(workspaces_base, repo_root))
  use _ <- result.try(create_jj_workspace(
    worktree_name,
    worktree_path,
    repo_root,
  ))
  use _ <- result.try(create_bookmark(worktree_path, branch, repo_root))
  use _ <- result.try(create_symlink(worktree_path, slug, repo_root))

  Ok(Worktree(slug, worktree_path, branch, language))
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
  run_command_checked(
    "jj",
    ["workspace", "add", "--name", worktree_name, worktree_path],
    repo_root,
    "Could not create jj workspace: " <> worktree_name,
  )
}

fn create_bookmark(
  worktree_path: String,
  branch: String,
  repo_root: String,
) -> Result(Nil, String) {
  run_command_checked(
    "jj",
    ["-R", worktree_path, "bookmark", "create", branch],
    repo_root,
    "Could not create branch bookmark",
  )
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
      process.Success(path, _, _) ->
        Ok(Worktree(slug, string.trim(path), branch_prefix <> slug, domain.Go))
      _ -> Error("Could not resolve worktree path")
    }
  })
}

pub fn remove_worktree(slug: String, repo_root: String) -> Result(Nil, String) {
  use wt <- result.try(get_worktree(slug, repo_root))

  let _ =
    process.run_command(
      "jj",
      ["-R", repo_root, "workspace", "forget", slug <> "-*"],
      repo_root,
    )

  use _ <- result.try(run_command_checked(
    "rm",
    ["-rf", wt.path],
    repo_root,
    "Could not remove worktree directory",
  ))

  let symlink_path = repo_root <> "/" <> factory_dir <> "/" <> slug
  let _ = process.run_command("rm", ["-f", symlink_path], repo_root)

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

fn generate_unique_id() -> String {
  case process.run_command("date", ["+%s%N"], ".") {
    Ok(process.Success(timestamp, _, _)) ->
      timestamp
      |> string.trim
      |> string.slice(unique_id_offset, unique_id_length)
    _ ->
      case process.run_command("sh", ["-c", "echo $RANDOM$RANDOM"], ".") {
        Ok(process.Success(rand, _, _)) -> string.trim(rand)
        _ -> "default"
      }
  }
}

pub fn worktrees_base(repo_root: String) -> String {
  repo_root <> "/" <> workspaces_dir
}
