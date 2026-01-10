//// User-friendly error messages (factory-gleam-ssh)
////
//// Consistent, helpful error messages for common failure cases
//// Helps users understand what went wrong and how to fix it

import gleam/string

pub type ErrorKind {
  InvalidSlug(reason: String)
  RepoNotFound
  TaskNotFound(slug: String)
  StageNotFound(stage: String)
  InvalidStage(stage: String)
  CommandFailed(cmd: String, code: Int, stderr: String)
  PermissionDenied(path: String)
  ResourceExhausted(resource: String)
  ConfigError(msg: String)
  Timeout(operation: String)
}

pub fn format(kind: ErrorKind) -> String {
  case kind {
    InvalidSlug(reason) ->
      "Invalid task slug: "
      <> reason
      <> "\n\nTask slugs must be 1-50 characters containing only letters, numbers, hyphens, and underscores."
      <> "\nExample: my-feature-task, implement_new_api, fix123"

    RepoNotFound ->
      "Could not detect project repository.\n\nMake sure you're running this command from within a project directory with one of:\n  - gleam.toml (Gleam)\n  - go.mod (Go)\n  - Cargo.toml (Rust)\n  - pyproject.toml (Python)"

    TaskNotFound(slug) ->
      "Task not found: " <> slug <> "\n\nRun 'factory list' to see available tasks."

    StageNotFound(stage) ->
      "Unknown stage: "
      <> stage
      <> "\n\nAvailable stages: implement, unit-test, coverage, lint, static, integration, security, review, accept"

    InvalidStage(stage) ->
      "Invalid stage transition: "
      <> stage
      <> "\n\nStages must run in order. Use 'factory show -s <task>' to see current status."

    CommandFailed(cmd, code, stderr) ->
      "Command failed: "
      <> cmd
      <> " (exit code: "
      <> string.inspect(code)
      <> ")\n\n"
      <> case stderr {
        "" -> "Run with verbose output for more details."
        err -> "Error:\n" <> err
      }

    PermissionDenied(path) ->
      "Permission denied: "
      <> path
      <> "\n\nMake sure you have read/write permissions. Try: chmod u+rw "
      <> path

    ResourceExhausted(resource) ->
      "Resource exhausted: "
      <> resource
      <> "\n\nFactor has reached configured limits. Reduce workspaces or increase system resources."

    ConfigError(msg) ->
      "Configuration error: "
      <> msg
      <> "\n\nCheck .factory/config or factory.toml for issues."

    Timeout(operation) ->
      "Operation timed out: "
      <> operation
      <> "\n\nThe operation took longer than expected. Try again or increase timeout."
  }
}

pub fn format_hint(kind: ErrorKind) -> String {
  case kind {
    InvalidSlug(_) ->
      "💡 Tip: Use lowercase letters, numbers, hyphens, and underscores only"
    RepoNotFound ->
      "💡 Tip: Run from the project root directory"
    TaskNotFound(_) ->
      "💡 Tip: Check task name with 'factory list'"
    StageNotFound(_) ->
      "💡 Tip: Use 'factory show -s <task>' to see available stages"
    InvalidStage(_) ->
      "💡 Tip: Stages must run in order"
    CommandFailed(_, _, _) ->
      "💡 Tip: Check that required tools are installed"
    PermissionDenied(_) ->
      "💡 Tip: Check directory permissions with 'ls -la'"
    ResourceExhausted(_) ->
      "💡 Tip: Clean up old workspaces with 'factory cleanup'"
    ConfigError(_) ->
      "💡 Tip: Validate config syntax"
    Timeout(_) ->
      "💡 Tip: System may be overloaded"
  }
}
