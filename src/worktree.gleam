// Worktree module - Git/jj workspace isolation
// Manages creating, retrieving, and removing isolated work directories

import gleam/string
import gleam/list
import gleam/result
import domain
import process

/// Worktree represents an isolated workspace
pub type Worktree {
  Worktree(
    slug: String,
    path: String,
    branch: String,
    language: domain.Language,
  )
}

/// Create a new worktree for a task
pub fn create_worktree(
  slug: String,
  language: domain.Language,
  repo_root: String,
) -> Result(Worktree, String) {
  let workspaces_base = repo_root <> "/.factory-workspaces"
  let unique_id = generate_unique_id()
  let worktree_name = slug <> "-" <> unique_id
  let worktree_path = workspaces_base <> "/" <> worktree_name
  let branch = "feat/" <> slug

  // STEP 1: Create base directory
  use _ <- result.try(
    process.run_command("mkdir", ["-p", workspaces_base], repo_root)
    |> result.map_error(fn(_) { "Could not create workspaces directory" })
    |> result.map(fn(_) { Nil }),
  )

  // STEP 2: Create jj workspace
  use _ <- result.try(
    process.run_command("jj", [
      "workspace",
      "add",
      "--name",
      worktree_name,
      worktree_path,
    ], repo_root)
    |> result.map_error(fn(_) {
      "Could not create jj workspace: " <> worktree_name
    })
    |> result.map(fn(_) { Nil }),
  )

  // STEP 3: Create bookmark for git compatibility
  use _ <- result.try(
    process.run_command("jj", ["-R", worktree_path, "bookmark", "create", branch], repo_root)
    |> result.map_error(fn(_) { "Could not create branch bookmark" })
    |> result.map(fn(_) { Nil }),
  )

  // STEP 4: Create symlink in .factory for easy access
  let symlink_dir = repo_root <> "/.factory"
  use _ <- result.try(
    process.run_command("mkdir", ["-p", symlink_dir], repo_root)
    |> result.map_error(fn(_) { "Could not create .factory directory" })
    |> result.map(fn(_) { Nil }),
  )

  use _ <- result.try(
    process.run_command("ln", ["-sf", worktree_path, symlink_dir <> "/" <> slug], repo_root)
    |> result.map_error(fn(_) { "Could not create symlink" })
    |> result.map(fn(_) { Nil }),
  )

  Ok(Worktree(
    slug: slug,
    path: worktree_path,
    branch: branch,
    language: language,
  ))
}

/// Get existing worktree by slug
pub fn get_worktree(slug: String, repo_root: String) -> Result(Worktree, String) {
  let symlink_path = repo_root <> "/.factory/" <> slug

  // Try to read the symlink
  process.run_command("readlink", [symlink_path], repo_root)
  |> result.map_error(fn(_) { "Worktree not found: " <> slug })
  |> result.map(fn(result) {
    case result {
      process.Success(path, _, _) -> {
        // Extract language from task record
        Worktree(
          slug: slug,
          path: string.trim(path),
          branch: "feat/" <> slug,
          language: domain.Go, // Default - would be loaded from persistence
        )
      }
      _ -> Worktree(slug, "", "feat/" <> slug, domain.Go)
    }
  })
  |> result.try(fn(wt) {
    case wt.path {
      "" -> Error("Could not resolve worktree path")
      _ -> Ok(wt)
    }
  })
}

/// Remove a worktree
pub fn remove_worktree(slug: String, repo_root: String) -> Result(Nil, String) {
  use wt <- result.try(get_worktree(slug, repo_root))

  // STEP 1: Forget the workspace in jj
  let _ =
    process.run_command("jj", [
      "-R",
      repo_root,
      "workspace",
      "forget",
      slug <> "-*",
    ], repo_root)

  // STEP 2: Remove the worktree directory
  use _ <- result.try(
    process.run_command("rm", ["-rf", wt.path], repo_root)
    |> result.map_error(fn(_) { "Could not remove worktree directory" })
    |> result.map(fn(_) { Nil }),
  )

  // STEP 3: Remove the symlink
  let symlink_path = repo_root <> "/.factory/" <> slug
  let _ = process.run_command("rm", ["-f", symlink_path], repo_root)

  Ok(Nil)
}

/// List all active worktrees
pub fn list_worktrees(repo_root: String) -> Result(List(Worktree), String) {
  let factory_dir = repo_root <> "/.factory"

  // List symlinks in .factory directory
  process.run_command("ls", ["-1", factory_dir], repo_root)
  |> result.map_error(fn(_) { "Could not list worktrees" })
  |> result.map(fn(result) {
    case result {
      process.Success(output, _, _) -> {
        // Parse output to get worktree names
        output
        |> string.split("\n")
        |> list.filter(fn(line) { string.length(string.trim(line)) > 0 })
      }
      _ -> []
    }
  })
  |> result.try(fn(slugs) {
    // Load each worktree
    slugs
    |> list.try_map(fn(slug) {
      get_worktree(string.trim(slug), repo_root)
    })
  })
}

/// Generate unique ID for worktree isolation
fn generate_unique_id() -> String {
  // In production, would generate cryptographically random hex string
  // For now, use timestamp-based ID
  "abc123"
}

/// Get worktree directory from repo root
pub fn worktrees_base(repo_root: String) -> String {
  repo_root <> "/.factory-workspaces"
}
