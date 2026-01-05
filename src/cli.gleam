// CLI module - Command parsing and execution
// NO INTERACTIVE PROMPTS - All parameters from command line

import gleam/result
import domain

/// Parsed CLI command
pub type Command {
  New(slug: String, language: String)
  Run(slug: String)
  Stage(slug: String, stage_name: String)
  Range(slug: String, start_stage: String, end_stage: String)
  List
  Status(slug: String)
  Clean(slug: String)
  Init(repo_path: String)
  Provision(repo_path: String, language: String)
  Help
  InvalidCommand(reason: String)
}

/// Parse command-line arguments into a Command
pub fn parse_args(args: List(String)) -> Result(Command, String) {
  case args {
    [] -> Ok(Help)
    ["help"] -> Ok(Help)
    ["--help"] -> Ok(Help)
    ["-h"] -> Ok(Help)

    ["new", slug] -> {
      // Validate slug
      case domain.validate_slug(slug) {
        Ok(_) -> Ok(New(slug, "go"))
        Error(err) -> Error(err)
      }
    }

    ["new", slug, lang] -> {
      // Validate slug and language
      use _ <- result.try(case domain.validate_slug(slug) {
        Ok(_) -> Ok(Nil)
        Error(err) -> Error(err)
      })
      use _ <- result.try(case domain.parse_language(lang) {
        Ok(_) -> Ok(Nil)
        Error(err) -> Error(err)
      })
      Ok(New(slug, lang))
    }

    ["run", slug] -> {
      case domain.validate_slug(slug) {
        Ok(_) -> Ok(Run(slug))
        Error(err) -> Error(err)
      }
    }

    ["stage", slug, stage_name] -> {
      case domain.validate_slug(slug) {
        Ok(_) -> {
          case domain.get_stage(stage_name) {
            Ok(_) -> Ok(Stage(slug, stage_name))
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    }

    ["range", slug, start_stage, end_stage] -> {
      case domain.validate_slug(slug) {
        Ok(_) -> {
          case domain.filter_stages(start_stage, end_stage) {
            Ok(_) -> Ok(Range(slug, start_stage, end_stage))
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    }

    ["list"] -> Ok(List)

    ["status", slug] -> {
      case domain.validate_slug(slug) {
        Ok(_) -> Ok(Status(slug))
        Error(err) -> Error(err)
      }
    }

    ["clean", slug] -> {
      case domain.validate_slug(slug) {
        Ok(_) -> Ok(Clean(slug))
        Error(err) -> Error(err)
      }
    }

    ["init"] -> Ok(Init("."))
    ["init", path] -> Ok(Init(path))

    ["provision", path] -> Ok(Provision(path, "go"))
    ["provision", path, lang] -> {
      case domain.parse_language(lang) {
        Ok(_) -> Ok(Provision(path, lang))
        Error(err) -> Error(err)
      }
    }

    [cmd, ..] -> Error("Unknown command: " <> cmd)
  }
}

/// Format command for display
pub fn command_to_string(cmd: Command) -> String {
  case cmd {
    New(slug, lang) -> "new " <> slug <> " " <> lang
    Run(slug) -> "run " <> slug
    Stage(slug, stage) -> "stage " <> slug <> " " <> stage
    Range(slug, start, end) -> "range " <> slug <> " " <> start <> " " <> end
    List -> "list"
    Status(slug) -> "status " <> slug
    Clean(slug) -> "clean " <> slug
    Init(path) -> "init " <> path
    Provision(path, lang) -> "provision " <> path <> " " <> lang
    Help -> "help"
    InvalidCommand(reason) -> "invalid: " <> reason
  }
}

/// Generate help text
pub fn help_text() -> String {
  "Factory - Multi-language engineering pipeline

USAGE:
  factory <command> [options]

COMMANDS:
  new <slug> [language]      Create new task (language: go|gleam|rust|python)
  run <slug>                 Run full 10-stage pipeline headless
  stage <slug> <stage>       Run single stage
  range <slug> <start> <end> Run stage range
  list                       List all active tasks
  status <slug>              Show task status
  clean <slug>               Remove task worktree
  init [path]                Initialize factory in repo
  provision [path] [lang]    Auto-detect and provision repo

OPTIONS:
  --help, -h                 Show this help message

EXAMPLE:
  factory new my-feature gleam
  factory run my-feature
  factory status my-feature
  factory clean my-feature

All commands are non-interactive - safe for AI automation.
"
}

/// Check if command is a write operation (modifies state)
pub fn is_write_command(cmd: Command) -> Bool {
  case cmd {
    New(_, _) -> True
    Clean(_) -> True
    Init(_) -> True
    Provision(_, _) -> True
    _ -> False
  }
}

/// Check if command is a read-only operation
pub fn is_readonly_command(cmd: Command) -> Bool {
  case cmd {
    List -> True
    Status(_) -> True
    Help -> True
    _ -> False
  }
}
