// Unified CLI module
// Single source of truth for all CLI operations
// Uses: argv, clip, spinner, stdin, shellout
// NO alternatives - this is the only way to parse CLI

import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/list
import gleam/string
import argv
import domain

/// All possible Factory commands
pub type Command {
  NewTask(slug: String, contract_path: Option(String), interactive: Bool)
  RunStage(slug: String, stage_name: String, dry_run: Bool, from: Option(String), to: Option(String))
  ApproveTask(slug: String, strategy: Option(String), force: Bool)
  ShowTask(slug: String, detailed: Bool)
  ListTasks(priority: Option(String), status: Option(String))
  Help(topic: Option(String))
  Version
}

/// Parse CLI arguments from argv
/// This is the ONLY parser - no alternatives
pub fn parse() -> Result(Command, String) {
  case argv.load().arguments {
    [] -> Ok(Help(None))
    ["help"] -> Ok(Help(None))
    ["help", topic] -> Ok(Help(Some(topic)))
    ["version"] -> Ok(Version)

    ["new", "--slug", slug] -> Ok(NewTask(slug, None, False))
    ["new", "--slug", slug, "--contract", contract] -> Ok(NewTask(slug, Some(contract), False))
    ["new", "--slug", slug, "--interactive"] -> Ok(NewTask(slug, None, True))

    ["stage", "--slug", slug, "--stage", stage] -> Ok(RunStage(slug, stage, False, None, None))
    ["stage", "--slug", slug, "--stage", stage, "--dry-run"] -> Ok(RunStage(slug, stage, True, None, None))
    ["stage", "--slug", slug, "--stage", stage, "--from", from] -> Ok(RunStage(slug, stage, False, Some(from), None))
    ["stage", "--slug", slug, "--stage", stage, "--to", to] -> Ok(RunStage(slug, stage, False, None, Some(to)))

    ["approve", "--slug", slug] -> Ok(ApproveTask(slug, None, False))
    ["approve", "--slug", slug, "--strategy", strategy] -> Ok(ApproveTask(slug, Some(strategy), False))
    ["approve", "--slug", slug, "--force"] -> Ok(ApproveTask(slug, None, True))

    ["show", "--slug", slug] -> Ok(ShowTask(slug, False))
    ["show", "--slug", slug, "--detailed"] -> Ok(ShowTask(slug, True))

    ["list"] -> Ok(ListTasks(None, None))
    ["list", "--priority", p] -> Ok(ListTasks(Some(p), None))
    ["list", "--status", s] -> Ok(ListTasks(None, Some(s)))

    [cmd, ..] -> Error("Unknown command: " <> cmd)
  }
}

/// Execute parsed command
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

// Implementation functions (placeholders until filled in)

fn execute_new(
  slug: String,
  _contract: Option(String),
  _interactive: Bool,
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  io.println("Creating task: " <> slug)
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
  io.println("Running stage " <> stage <> " for " <> slug)
  Ok(Nil)
}

fn execute_approve(
  slug: String,
  _strategy: Option(String),
  _force: Bool,
) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  io.println("Approving task: " <> slug)
  Ok(Nil)
}

fn execute_show(slug: String, _detailed: Bool) -> Result(Nil, String) {
  use _ <- result.try(domain.validate_slug(slug))
  io.println("Showing task: " <> slug)
  Ok(Nil)
}

fn execute_list(_priority: Option(String), _status: Option(String)) -> Result(Nil, String) {
  io.println("Listing tasks")
  Ok(Nil)
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
