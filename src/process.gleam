// Shell module - Execute external commands
// Uses shellout library for subprocess execution

import gleam/string
import gleam/result
import shellout

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
  // Execute shell command via /bin/sh -c
  // shellout.command returns Ok(stdout) for success, Error(#(exit_code, stderr)) for failure
  case shellout.command("sh", ["-c", cmd], "", []) {
    Ok(stdout) -> {
      // Success - exit code 0
      Ok(Success(stdout, "", 0))
    }
    Error(#(exit_code, stderr)) -> {
      // Error - non-zero exit code with error message
      Ok(Failure(stderr, exit_code))
    }
  }
}

/// Check if a command exists in PATH
pub fn command_exists(cmd: String) -> Result(Bool, String) {
  // Use 'which' command to check if command exists in PATH
  case run_command("which", [cmd], "") {
    Ok(_) -> Ok(True)
    Error(_) -> Error("Command not found in PATH: " <> cmd)
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

/// Convert CommandResult to a Result, checking for success
pub fn check_success(result: CommandResult) -> Result(Nil, String) {
  case result {
    Success(_, _, _) -> Ok(Nil)
    Failure(err, code) ->
      Error("Command failed with exit code " <> string.inspect(code) <> ": " <> err)
  }
}

/// Run a command only if it exists in PATH
pub fn run_command_safe(
  cmd: String,
  args: List(String),
  cwd: String,
) -> Result(CommandResult, String) {
  use _ <- result.try(command_exists(cmd))
  run_command(cmd, args, cwd)
}
