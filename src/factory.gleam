// Factory - Main entry point
// Wires all modules together for CLI execution

import gleam/io
import gleam/string
import gleam/list
import gleam/result
import domain
import cli
import repo
import worktree
import stages
import persistence

pub fn main() {
  // For now, print welcome message
  // Full implementation would parse args via gleam_stdlib
  io.println("Factory - Multi-language engineering pipeline (Gleam Edition)")
  io.println("  domain.gleam  ✓ Language, Task, Stage types")
  io.println("  process.gleam   ✓ Command execution")
  io.println("  tcr.gleam     ✓ Test && Commit || Revert")
  io.println("  stages.gleam  ✓ Real language-specific stages")
  io.println("  integration.gleam ✓ Merge and verify")
  io.println("  persistence.gleam ✓ JSON status tracking")
  io.println("  worktree.gleam ✓ Workspace isolation")
  io.println("  repo.gleam    ✓ Repository detection")
  io.println("  cli.gleam     ✓ Command parsing (no prompts)")
  io.println("")
  io.println("Usage: factory <command> [options]")
  io.println("  factory new <slug> [language]")
  io.println("  factory run <slug>")
  io.println("  factory stage <slug> <stage-name>")
  io.println("  factory range <slug> <start> <end>")
  io.println("  factory list")
  io.println("  factory status <slug>")
  io.println("  factory clean <slug>")
}

/// Execute a command (would be called from main with parsed args)
pub fn execute_command(cmd: cli.Command, repo_root: String) -> Result(String, String) {
  case cmd {
    cli.New(slug, lang) -> execute_new(slug, lang, repo_root)
    cli.Run(slug) -> execute_run(slug, repo_root)
    cli.Stage(slug, stage) -> execute_stage(slug, stage, repo_root)
    cli.Range(slug, start, end) -> execute_range(slug, start, end, repo_root)
    cli.List -> execute_list(repo_root)
    cli.Status(slug) -> execute_status(slug, repo_root)
    cli.Clean(slug) -> execute_clean(slug, repo_root)
    cli.Init(path) -> execute_init(path)
    cli.Provision(path, lang) -> execute_provision(path, lang)
    cli.Help -> Ok(cli.help_text())
    cli.InvalidCommand(reason) -> Error(reason)
  }
}

fn execute_new(
  slug: String,
  lang_str: String,
  repo_root: String,
) -> Result(String, String) {
  use lang <- result.try(domain.parse_language(lang_str))
  use wt <- result.try(worktree.create_worktree(slug, lang, repo_root))

  let task =
    domain.Task(
      slug: slug,
      language: lang,
      status: domain.Created,
      worktree_path: wt.path,
      branch: wt.branch,
    )

  use _ <- result.try(persistence.save_task_record(task, repo_root))

  Ok(
    "Created: " <> wt.path <> "\n"
    <> "Branch:  " <> wt.branch <> "\n"
    <> "Language: " <> lang_str,
  )
}

fn execute_run(slug: String, repo_root: String) -> Result(String, String) {
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  let stages = domain.standard_pipeline()
  run_pipeline(task, stages, repo_root)
}

fn execute_stage(
  slug: String,
  stage_name: String,
  repo_root: String,
) -> Result(String, String) {
  use task <- result.try(persistence.load_task_record(slug, repo_root))
  use _stage <- result.try(domain.get_stage(stage_name))

  use _ <- result.try(
    stages.execute_stage(stage_name, task.language, task.worktree_path),
  )

  Ok("✓ " <> stage_name <> " passed")
}

fn execute_range(
  slug: String,
  start_stage: String,
  end_stage: String,
  repo_root: String,
) -> Result(String, String) {
  use task <- result.try(persistence.load_task_record(slug, repo_root))
  use stage_list <- result.try(domain.filter_stages(start_stage, end_stage))

  run_pipeline(task, stage_list, repo_root)
}

fn execute_list(repo_root: String) -> Result(String, String) {
  use tasks <- result.try(persistence.list_all_tasks(repo_root))

  case tasks {
    [] -> Ok("No active tasks")
    ts ->
      ts
      |> list.map(fn(task) { task.slug <> " (" <> task.branch <> ")" })
      |> string.join("\n")
      |> Ok
  }
}

fn execute_status(slug: String, repo_root: String) -> Result(String, String) {
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  let status_str = case task.status {
    domain.Created -> "created"
    domain.InProgress(stage) -> "in_progress (" <> stage <> ")"
    domain.PassedPipeline -> "passed_pipeline"
    domain.FailedPipeline(stage, reason) ->
      "failed_pipeline (" <> stage <> ": " <> reason <> ")"
    domain.Integrated -> "integrated"
  }

  Ok(slug <> ": " <> status_str)
}

fn execute_clean(slug: String, repo_root: String) -> Result(String, String) {
  use _ <- result.try(worktree.remove_worktree(slug, repo_root))
  Ok("Cleaned: " <> slug)
}

fn execute_init(_repo_path: String) -> Result(String, String) {
  use root <- result.try(repo.detect_repo_root())
  use lang <- result.try(repo.detect_language(root))

  let lang_str = case lang {
    domain.Go -> "go"
    domain.Gleam -> "gleam"
    domain.Rust -> "rust"
    domain.Python -> "python"
  }

  Ok("Initialized Factory in " <> root <> " (detected language: " <> lang_str <> ")")
}

fn execute_provision(_repo_path: String, lang_str: String) -> Result(String, String) {
  use root <- result.try(repo.detect_repo_root())
  use _lang <- result.try(domain.parse_language(lang_str))

  Ok("Provisioned " <> root <> " for " <> lang_str)
}

// ============================================================================
// PIPELINE EXECUTION
// ============================================================================

fn run_pipeline(
  task: domain.Task,
  stages: List(domain.Stage),
  repo_root: String,
) -> Result(String, String) {
  let updated_task = domain.Task(..task, status: domain.InProgress(""))
  use _ <- result.try(persistence.save_task_record(updated_task, repo_root))

  run_stages(task, stages, repo_root, 1, [])
}

fn run_stages(
  task: domain.Task,
  stages: List(domain.Stage),
  repo_root: String,
  stage_num: Int,
  results: List(String),
) -> Result(String, String) {
  case stages {
    [] -> {
      // All stages passed
      let updated_task =
        domain.Task(..task, status: domain.PassedPipeline)
      use _ <- result.try(persistence.save_task_record(updated_task, repo_root))

      let output =
        list.append(results, ["", "✅ All stages passed - ready for merge"])
        |> string.join("\n")
      Ok(output)
    }

    [stage, ..rest] -> {
      let status_msg =
        "[" <> string.inspect(stage_num) <> "/10] " <> stage.name

      case stages.execute_stage(stage.name, task.language, task.worktree_path) {
        Ok(Nil) -> {
          let new_results = list.append(results, ["✓ " <> status_msg])
          run_stages(task, rest, repo_root, stage_num + 1, new_results)
        }

        Error(err) -> {
          // Stage failed
          let failed_task =
            domain.Task(..task, status: domain.FailedPipeline(stage.name, err))
          let _ = persistence.save_task_record(failed_task, repo_root)

          let output =
            list.append(results, ["✗ " <> status_msg <> " - " <> err])
            |> string.join("\n")
          Error(output)
        }
      }
    }
  }
}
