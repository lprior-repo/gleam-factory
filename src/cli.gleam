import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/list
import gleam/string
import argv
import domain
import repo
import persistence
import stages
import worktree

pub type Command {
  NewTask(slug: String, contract_path: Option(String), interactive: Bool)
  RunStage(slug: String, stage_name: String, dry_run: Bool, from: Option(String), to: Option(String))
  ApproveTask(slug: String, strategy: Option(String), force: Bool)
  ShowTask(slug: String, detailed: Bool)
  ListTasks(priority: Option(String), status: Option(String))
  Help(topic: Option(String))
  Version
}

pub fn parse_args(args: List(String)) -> Result(Command, String) {
  normalize_short_flags(args) |> do_parse
}

pub fn parse() -> Result(Command, String) {
  argv.load().arguments |> do_parse
}

fn normalize_short_flags(args: List(String)) -> List(String) {
  case args {
    [] -> []
    ["-s", ..rest] -> ["--slug", ..normalize_short_flags(rest)]
    [arg, ..rest] -> [arg, ..normalize_short_flags(rest)]
  }
}

fn do_parse(args: List(String)) -> Result(Command, String) {
  case args {
    [] -> Ok(Help(None))
    ["help"] -> Ok(Help(None))
    ["help", topic] -> Ok(Help(Some(topic)))
    ["version"] -> Ok(Version)
    ["new", "--slug", slug] -> Ok(NewTask(slug, None, False))
    ["new", "--slug", slug, "--contract", contract] ->
      Ok(NewTask(slug, Some(contract), False))
    ["new", "--slug", slug, "--interactive"] -> Ok(NewTask(slug, None, True))
    ["stage", "--slug", slug, "--stage", stage] ->
      Ok(RunStage(slug, stage, False, None, None))
    ["stage", "--slug", slug, "--stage", stage, "--dry-run"] ->
      Ok(RunStage(slug, stage, True, None, None))
    ["stage", "--slug", slug, "--stage", stage, "--from", from] ->
      Ok(RunStage(slug, stage, False, Some(from), None))
    ["stage", "--slug", slug, "--stage", stage, "--to", to] ->
      Ok(RunStage(slug, stage, False, None, Some(to)))
    ["approve", "--slug", slug] -> Ok(ApproveTask(slug, None, False))
    ["approve", "--slug", slug, "--strategy", strategy] ->
      Ok(ApproveTask(slug, Some(strategy), False))
    ["approve", "--slug", slug, "--force"] -> Ok(ApproveTask(slug, None, True))
    ["show", "--slug", slug] -> Ok(ShowTask(slug, False))
    ["show", "--slug", slug, "--detailed"] -> Ok(ShowTask(slug, True))
    ["list"] -> Ok(ListTasks(None, None))
    ["list", "--priority", p] -> Ok(ListTasks(Some(p), None))
    ["list", "--status", s] -> Ok(ListTasks(None, Some(s)))
    [cmd, ..] -> Error("Unknown command: " <> cmd)
  }
}

pub fn execute(cmd: Command) -> Result(Nil, String) {
  case cmd {
    NewTask(slug, contract, interactive) ->
      execute_new(slug, contract, interactive)

    RunStage(slug, stage, dry_run, from, to) ->
      execute_stage(slug, stage, dry_run, from, to)

    ApproveTask(slug, strategy, force) ->
      execute_approve(slug, strategy, force)

    ShowTask(slug, detailed) -> execute_show(slug, detailed)

    ListTasks(priority, status) -> execute_list(priority, status)

    Help(topic) -> {
      show_help(topic)
      Ok(Nil)
    }

    Version -> {
      show_version()
      Ok(Nil)
    }
  }
}

fn execute_new(
  slug: String,
  _contract: Option(String),
  _interactive: Bool,
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use lang <- result.try(repo.detect_language(repo_root))

  let lang_str = case lang {
    domain.Go -> "go"
    domain.Gleam -> "gleam"
    domain.Rust -> "rust"
    domain.Python -> "python"
  }

  use message <- result.try(execute_new_impl(slug, lang_str, repo_root))
  io.println(message)
  Ok(Nil)
}

fn execute_stage(
  slug: String,
  stage: String,
  _dry_run: Bool,
  _from: Option(String),
  _to: Option(String),
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use task <- result.try(persistence.load_task_record(slug, repo_root))
  use message <- result.try(execute_stage_impl(slug, stage, task, repo_root))
  io.println(message)
  Ok(Nil)
}

fn execute_approve(
  slug: String,
  _strategy: Option(String),
  _force: Bool,
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  let approved_task = domain.Task(..task, status: domain.Integrated)
  use _ <- result.try(persistence.save_task_record(approved_task, repo_root))
  io.println("✓ Approved: " <> slug)
  Ok(Nil)
}

fn execute_show(slug: String, _detailed: Bool) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  io.println(slug <> ": " <> format_status(task.status))
  Ok(Nil)
}

fn execute_list(_priority: Option(String), _status: Option(String)) -> Result(Nil, String) {
  use repo_root <- result.try(repo.detect_repo_root())
  use tasks <- result.try(persistence.list_all_tasks(repo_root))

  case tasks {
    [] -> io.println("No active tasks")
    ts ->
      ts
      |> list.map(fn(task) { task.slug <> " (" <> task.branch <> ")" })
      |> string.join("\n")
      |> io.println
  }
  Ok(Nil)
}

fn format_status(status: domain.TaskStatus) -> String {
  case status {
    domain.Created -> "created"
    domain.InProgress(stage) -> "in_progress (" <> stage <> ")"
    domain.PassedPipeline -> "passed_pipeline"
    domain.FailedPipeline(stage, reason) ->
      "failed_pipeline (" <> stage <> ": " <> reason <> ")"
    domain.Integrated -> "integrated"
  }
}

fn execute_new_impl(
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

fn execute_stage_impl(
  _slug: String,
  stage_name: String,
  task: domain.Task,
  _repo_root: String,
) -> Result(String, String) {
  use _ <- result.try(domain.get_stage(stage_name))

  use _ <- result.try(
    stages.execute_stage(stage_name, task.language, task.worktree_path),
  )

  Ok("✓ " <> stage_name <> " passed")
}

fn show_help(topic: Option(String)) -> Nil {
  case topic {
    None -> io.println(help_text())
    Some(t) -> io.println("Help for: " <> t)
  }
}

fn show_version() -> Nil {
  io.println(
    "Factory Gleam v1.0.0\nBuilt for contract-driven CI/CD\n"
    <> "Docs: ./ARCHITECTURE.md",
  )
}

/// Help text
pub fn help_text() -> String {
  "Factory - Contract-driven CI/CD Pipeline

USAGE:
  factory <COMMAND> [FLAGS]

COMMANDS:
  new      Create new task
           factory new --slug bd-52.1 [--contract path] [--interactive]

  stage    Run pipeline stage
           factory stage --slug bd-52.1 --stage implement [--dry-run] [--from X] [--to Y]

  approve  Approve for deployment
           factory approve --slug bd-52.1 [--strategy gradual] [--force]

  show     Show task details
           factory show --slug bd-52.1 [--detailed]

  list     List all tasks
           factory list [--priority P1|P2|P3] [--status open|in_progress|done]

  help     Show this help [--topic COMMAND]
  version  Show version

EXAMPLES:
  factory new --slug bd-52.1
  factory stage --slug bd-52.1 --stage implement --dry-run
  factory approve --slug bd-52.1 --strategy gradual
  factory list --priority P1

Documentation: ./ARCHITECTURE.md"
}
