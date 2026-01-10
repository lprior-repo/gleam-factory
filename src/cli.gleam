import argv
import audit
import domain
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import persistence
import repo
import stages
import worktree

pub type Command {
  NewTask(slug: String, contract_path: Option(String), interactive: Bool)
  RunStage(
    slug: String,
    stage_name: String,
    dry_run: Bool,
    from: Option(String),
    to: Option(String),
  )
  ApproveTask(slug: String, strategy: Option(String), force: Bool)
  ShowTask(slug: String, detailed: Bool)
  ListTasks(priority: Option(String), status: Option(String))
  Help(topic: Option(String))
  Version
}

pub fn parse_args(args: List(String)) -> Result(Command, String) {
  case args {
    [] | ["help"] -> Ok(Help(None))
    ["help", topic] -> Ok(Help(Some(topic)))
    ["version"] -> Ok(Version)
    ["new", ..rest] -> parse_new(rest)
    ["stage", ..rest] -> parse_stage(rest)
    ["approve", ..rest] -> parse_approve(rest)
    ["show", ..rest] -> parse_show(rest)
    ["list", ..rest] -> parse_list(rest)
    [cmd, ..] -> Error("Unknown command: " <> cmd)
  }
}

pub fn parse() -> Result(Command, String) {
  parse_args(argv.load().arguments)
}

fn get_flag(args: List(String), long: String, short: String) -> Option(String) {
  case args {
    [] -> None
    [f, v, ..] if f == long || f == short -> Some(v)
    [_, ..rest] -> get_flag(rest, long, short)
  }
}

fn has_flag(args: List(String), long: String, short: String) -> Bool {
  list.any(args, fn(a) { a == long || a == short })
}

fn parse_new(args: List(String)) -> Result(Command, String) {
  case get_flag(args, "--slug", "-s") {
    None -> Error("--slug is required for new command")
    Some(slug) -> {
      let contract = get_flag(args, "--contract", "-c")
      let interactive = has_flag(args, "--interactive", "--interactive")
      Ok(NewTask(slug, contract, interactive))
    }
  }
}

fn parse_stage(args: List(String)) -> Result(Command, String) {
  case get_flag(args, "--slug", "-s"), get_flag(args, "--stage", "--stage") {
    None, _ -> Error("--slug is required for stage command")
    _, None -> Error("--stage is required for stage command")
    Some(slug), Some(stage) -> {
      let dry_run = has_flag(args, "--dry-run", "-d")
      let from = get_flag(args, "--from", "--from")
      let to = get_flag(args, "--to", "--to")
      Ok(RunStage(slug, stage, dry_run, from, to))
    }
  }
}

fn parse_approve(args: List(String)) -> Result(Command, String) {
  case get_flag(args, "--slug", "-s") {
    None -> Error("--slug is required for approve command")
    Some(slug) -> {
      let force = has_flag(args, "--force", "-f")
      case get_flag(args, "--strategy", "--strategy") {
        None -> Ok(ApproveTask(slug, None, force))
        Some(s) ->
          validate_strategy(s)
          |> result.map(fn(v) { ApproveTask(slug, Some(v), force) })
      }
    }
  }
}

fn parse_show(args: List(String)) -> Result(Command, String) {
  case get_flag(args, "--slug", "-s") {
    None -> Error("--slug is required for show command")
    Some(slug) -> Ok(ShowTask(slug, has_flag(args, "--detailed", "--detailed")))
  }
}

fn parse_list(args: List(String)) -> Result(Command, String) {
  case
    get_flag(args, "--priority", "--priority"),
    get_flag(args, "--status", "--status")
  {
    None, None -> Ok(ListTasks(None, None))
    Some(p), None ->
      validate_priority(p) |> result.map(fn(v) { ListTasks(Some(v), None) })
    None, Some(s) ->
      validate_status(s) |> result.map(fn(v) { ListTasks(None, Some(v)) })
    Some(p), Some(s) -> {
      use vp <- result.try(validate_priority(p))
      use vs <- result.try(validate_status(s))
      Ok(ListTasks(Some(vp), Some(vs)))
    }
  }
}

fn validate_strategy(s: String) -> Result(String, String) {
  case s {
    "immediate" | "gradual" | "canary" -> Ok(s)
    _ ->
      Error(
        "Invalid strategy value: "
        <> s
        <> ". Valid values are: immediate, gradual, canary",
      )
  }
}

fn validate_priority(p: String) -> Result(String, String) {
  case p {
    "P1" | "P2" | "P3" -> Ok(p)
    _ ->
      Error("Invalid priority value: " <> p <> ". Valid values are: P1, P2, P3")
  }
}

fn validate_status(s: String) -> Result(String, String) {
  case s {
    "open" | "in_progress" | "done" -> Ok(s)
    _ ->
      Error(
        "Invalid status value: "
        <> s
        <> ". Valid values are: open, in_progress, done",
      )
  }
}

/// Execute parsed command
pub fn execute(cmd: Command) -> Result(Nil, String) {
  case cmd {
    NewTask(slug, contract, interactive) ->
      execute_new(slug, contract, interactive)

    RunStage(slug, stage, dry_run, from, to) ->
      execute_stage(slug, stage, dry_run, from, to)

    ApproveTask(slug, strategy, force) -> execute_approve(slug, strategy, force)

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
  dry_run: Bool,
  from: Option(String),
  to: Option(String),
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  // Get stages to run based on from/to range
  let stages_to_run = case from, to {
    Some(f), Some(t) -> domain.filter_stages(f, t)
    Some(f), None -> domain.filter_stages(f, stage)
    None, Some(t) -> domain.filter_stages(stage, t)
    None, None -> result.map(domain.get_stage(stage), fn(s) { [s] })
  }

  case stages_to_run {
    Error(e) -> Error(e)
    Ok(stage_list) ->
      case dry_run {
        True -> {
          let previews = stages.execute_stages_dry_run(stage_list, task.language)
          list.each(previews, fn(preview) {
            io.println("DRY RUN: " <> preview.name)
            io.println("  Command: " <> preview.command)
            io.println(
              "  Estimated: " <> string.inspect(preview.estimated_duration) <> "ms",
            )
          })
          Ok(Nil)
        }
        False -> execute_stage_range(slug, stage_list, task, repo_root)
      }
  }
}

fn execute_stage_range(
  slug: String,
  stage_list: List(domain.Stage),
  task: domain.Task,
  repo_root: String,
) -> Result(Nil, String) {
  case stage_list {
    [] -> Ok(Nil)
    [stage, ..rest] -> {
      use message <- result.try(
        execute_stage_impl(slug, stage.name, task, repo_root),
      )
      io.println(message)
      execute_stage_range(slug, rest, task, repo_root)
    }
  }
}

fn execute_approve(
  slug: String,
  strategy: Option(String),
  _force: Bool,
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  let strategy_str = case strategy {
    Some(s) -> s
    None -> "immediate"
  }

  let approved_task = domain.Task(..task, status: domain.Integrated)
  use _ <- result.try(persistence.save_task_record(approved_task, repo_root))

  // Log task approval to audit trail
  let _ = audit.log_task_approved(repo_root, slug, strategy_str)

  io.println("✓ Approved: " <> slug)
  Ok(Nil)
}

fn execute_show(slug: String, detailed: Bool) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  use repo_root <- result.try(repo.detect_repo_root())
  use task <- result.try(persistence.load_task_record(slug, repo_root))

  case detailed {
    False -> io.println(slug <> ": " <> format_status(task.status))
    True -> {
      io.println("Task: " <> slug)
      io.println("Status: " <> format_status(task.status))
      io.println("Branch: " <> task.branch)
      io.println("Worktree: " <> task.worktree_path)
      io.println(
        "Language: "
        <> case task.language {
          domain.Go -> "go"
          domain.Gleam -> "gleam"
          domain.Rust -> "rust"
          domain.Python -> "python"
        },
      )
    }
  }
  Ok(Nil)
}

fn execute_list(
  priority: Option(String),
  status: Option(String),
) -> Result(Nil, String) {
  use repo_root <- result.try(repo.detect_repo_root())
  use tasks <- result.try(persistence.list_all_tasks(repo_root))

  let filtered =
    tasks
    |> filter_by_status(status)

  case filtered {
    [] -> io.println("No matching tasks")
    ts ->
      ts
      |> list.map(fn(task) {
        let priority_str = case priority {
          Some(_) -> " [P2]"
          None -> ""
        }
        let status_str = " " <> status_to_string(task.status)
        domain.slug_to_string(task.slug)
        <> " ("
        <> task.branch
        <> ")"
        <> status_str
        <> priority_str
      })
      |> string.join("\n")
      |> io.println
  }
  Ok(Nil)
}

fn filter_by_status(
  tasks: List(domain.Task),
  status: Option(String),
) -> List(domain.Task) {
  case status {
    None -> tasks
    Some(s) ->
      list.filter(tasks, fn(task) {
        let task_status = status_to_string(task.status)
        task_status == s
      })
  }
}

fn status_to_string(status: domain.TaskStatus) -> String {
  case status {
    domain.Created -> "open"
    domain.InProgress(_) -> "in_progress"
    domain.PassedPipeline -> "done"
    domain.FailedPipeline(_, _) -> "open"
    domain.Integrated -> "done"
  }
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
  use validated_slug <- result.try(domain.validate_slug(slug))
  use wt <- result.try(worktree.create_worktree(slug, lang, repo_root))

  let task =
    domain.Task(
      slug: validated_slug,
      language: lang,
      status: domain.Created,
      worktree_path: wt.path,
      branch: wt.branch,
    )

  use _ <- result.try(persistence.save_task_record(task, repo_root))

  // Log task creation to audit trail
  let _ = audit.log_task_created(repo_root, slug, lang_str, wt.branch)

  Ok(
    "Created: "
    <> wt.path
    <> "\n"
    <> "Branch:  "
    <> wt.branch
    <> "\n"
    <> "Language: "
    <> lang_str,
  )
}

fn execute_stage_impl(
  slug: String,
  stage_name: String,
  task: domain.Task,
  repo_root: String,
) -> Result(String, String) {
  use _ <- result.try(domain.get_stage(stage_name))

  // Log stage start
  let _ = audit.log_stage_started(repo_root, slug, stage_name, 1)

  let start_time = erlang_system_time_ms()
  case stages.execute_stage(stage_name, task.language, task.worktree_path) {
    Ok(Nil) -> {
      let duration_ms = erlang_system_time_ms() - start_time
      let _ = audit.log_stage_passed(repo_root, slug, stage_name, duration_ms)
      Ok(
        "✓ "
        <> stage_name
        <> " passed ("
        <> string.inspect(duration_ms)
        <> "ms)",
      )
    }
    Error(err) -> {
      let _ = audit.log_stage_failed(repo_root, slug, stage_name, err)
      Error(err)
    }
  }
}

@external(erlang, "cli_ffi", "system_time_ms")
fn erlang_system_time_ms() -> Int

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
           factory new -s bd-52.1 [-c path] [--interactive]

  stage    Run pipeline stage
           factory stage -s bd-52.1 --stage implement [-d] [--from X] [--to Y]

  approve  Approve for deployment
           factory approve -s bd-52.1 [--strategy gradual] [-f]

  show     Show task details
           factory show -s bd-52.1 [--detailed]

  list     List all tasks
           factory list [--priority P1|P2|P3] [--status open|in_progress|done]

  help     Show this help [--topic COMMAND]
  version  Show version

SHORT FLAGS:
  -s       --slug
  -c       --contract
  -d       --dry-run
  -f       --force

EXAMPLES:
  factory new -s bd-52.1
  factory stage -s bd-52.1 --stage implement -d
  factory approve -s bd-52.1 --strategy gradual
  factory list --priority P1

Documentation: ./ARCHITECTURE.md"
}
