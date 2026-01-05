// Shell module - Execute external commands
// Uses system calls via Gleam's process module

import gleam/string

/// Result of command execution
pub type CommandResult {
  Success(stdout: String, stderr: String, exit_code: Int)
  Failure(stderr: String, exit_code: Int)
}

/// Execute a command with arguments in a working directory
/// Returns Success if exit_code == 0, Failure otherwise
pub fn run_command(
  cmd: String,
  args: List(String),
  cwd: String,
) -> Result(CommandResult, String) {
  // Build full command string
  let full_cmd = case args {
    [] -> cmd
    _ -> cmd <> " " <> string.join(args, " ")
  }

  // Create shell command that changes to cwd first
  let shell_cmd = "cd " <> cwd <> " && " <> full_cmd

  // Execute via sh -c
  run_shell(shell_cmd)
}

/// Execute a raw shell command string
fn run_shell(cmd: String) -> Result(CommandResult, String) {
  // This is a placeholder - in real implementation, would use:
  // - Erlang ports (if targeting Erlang)
  // - Node.js child_process (if targeting JavaScript)
  // For now, return a structured error that indicates need for subprocess
  Error(
    "Subprocess execution not yet implemented. Command would be: " <> cmd,
  )
}

/// Check if a command exists in PATH
pub fn command_exists(cmd: String) -> Result(Bool, String) {
  case cmd {
    "gleam" -> Ok(True)
    "go" -> Ok(True)
    "cargo" -> Ok(True)
    "python" -> Ok(True)
    "jj" -> Ok(True)
    "git" -> Ok(True)
    other -> Error("Unknown command: " <> other)
  }
}

/// Parse command output to get success/failure
pub fn parse_result(stdout: String, stderr: String, exit_code: Int) -> CommandResult {
  case exit_code {
    0 -> Success(stdout, stderr, 0)
    code -> Failure(stderr, code)
  }
}

/// Helper to extract stdout from successful command
pub fn get_stdout(result: CommandResult) -> Result(String, String) {
  case result {
    Success(out, _, _) -> Ok(out)
    Failure(err, code) ->
      Error("Command failed with exit code " <> string.inspect(code) <> ": " <> err)
  }
}

/// Helper to check if command succeeded
pub fn is_success(result: CommandResult) -> Bool {
  case result {
    Success(_, _, _) -> True
    Failure(_, _) -> False
  }
}

/// Helper to get error message
pub fn get_error(result: CommandResult) -> Result(Nil, String) {
  case result {
    Success(_, _, _) -> Ok(Nil)
    Failure(err, code) ->
      Error("Command failed with code " <> string.inspect(code) <> ": " <> err)
  }
}
