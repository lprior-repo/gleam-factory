// Unified CLI module using glint framework
// Single source of truth for all CLI operations
// Uses: glint, argv, spinner, stdin, shellout, glitzer
// NO alternatives - this is the only way to parse CLI

import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import argv
import glint
import glint/flag as glint_flag
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

/// Parse CLI arguments using glint
/// This is the ONLY parser - no alternatives
pub fn parse() -> Result(Command, String) {
  let app =
    glint.new()
    |> glint.add(
      ["new"],
      glint.command(fn(input) {
        use slug <- glint.string_flag(
          "slug",
          glint_flag.Flag()
          |> glint_flag.description("Task ID (e.g., bd-52.1)")
          |> glint_flag.required(),
        )(input)
        use contract <- glint.optional_string_flag(
          "contract",
          glint_flag.Flag()
          |> glint_flag.description("Path to contract.yaml"),
        )(input)
        use interactive <- glint.bool_flag(
          "interactive",
          glint_flag.Flag()
          |> glint_flag.short_help("Enable interactive mode")
          |> glint_flag.default(False),
        )(input)

        Ok(NewTask(slug, contract, interactive))
      }),
    )
    |> glint.add(
      ["stage"],
      glint.command(fn(input) {
        use slug <- glint.string_flag(
          "slug",
          glint_flag.Flag() |> glint_flag.required(),
        )(input)
        use stage <- glint.string_flag(
          "stage",
          glint_flag.Flag() |> glint_flag.required(),
        )(input)
        use dry_run <- glint.bool_flag(
          "dry-run",
          glint_flag.Flag()
          |> glint_flag.short_help("Show what would execute"),
        )(input)
        use from <- glint.optional_string_flag(
          "from",
          glint_flag.Flag()
          |> glint_flag.description("Start from this stage"),
        )(input)
        use to <- glint.optional_string_flag(
          "to",
          glint_flag.Flag()
          |> glint_flag.description("Run until this stage"),
        )(input)

        Ok(RunStage(slug, stage, dry_run, from, to))
      }),
    )
    |> glint.add(
      ["approve"],
      glint.command(fn(input) {
        use slug <- glint.string_flag(
          "slug",
          glint_flag.Flag() |> glint_flag.required(),
        )(input)
        use strategy <- glint.optional_string_flag(
          "strategy",
          glint_flag.Flag()
          |> glint_flag.description("gradual, staged, or immediate"),
        )(input)
        use force <- glint.bool_flag(
          "force",
          glint_flag.Flag()
          |> glint_flag.short_help("Skip safety checks"),
        )(input)

        Ok(ApproveTask(slug, strategy, force))
      }),
    )
    |> glint.add(
      ["show"],
      glint.command(fn(input) {
        use slug <- glint.string_flag(
          "slug",
          glint_flag.Flag() |> glint_flag.required(),
        )(input)
        use detailed <- glint.bool_flag(
          "detailed",
          glint_flag.Flag()
          |> glint_flag.short_help("Show full details"),
        )(input)

        Ok(ShowTask(slug, detailed))
      }),
    )
    |> glint.add(
      ["list"],
      glint.command(fn(input) {
        use priority <- glint.optional_string_flag(
          "priority",
          glint_flag.Flag() |> glint_flag.description("P1, P2, or P3"),
        )(input)
        use status <- glint.optional_string_flag(
          "status",
          glint_flag.Flag()
          |> glint_flag.description("open, in_progress, or done"),
        )(input)

        Ok(ListTasks(priority, status))
      }),
    )
    |> glint.add(
      ["help"],
      glint.command(fn(input) {
        use topic <- glint.optional_string_flag(
          "topic",
          glint_flag.Flag() |> glint_flag.description("Help topic"),
        )(input)
        Ok(Help(topic))
      }),
    )
    |> glint.add(
      ["version"],
      glint.command(fn(_input) { Ok(Version) }),
    )

  case argv.load().arguments {
    [] -> Ok(Help(None))
    args ->
      glint.run(app, args)
      |> result.map_error(fn(_) { "Failed to parse arguments" })
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
