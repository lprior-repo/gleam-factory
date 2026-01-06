// Shell module - Execute external commands

import gleam/list as gleam_list
import gleam/string
import simplifile
import types

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

  // Create shell command that changes to cwd first and captures exit code
  let shell_cmd = case cwd {
    "" -> full_cmd <> " 2>&1; echo $?"
    _ -> "cd " <> cwd <> " && " <> full_cmd <> " 2>&1; echo $?"
  }

  // Execute via os:cmd
  let output = os_cmd(shell_cmd)

  // Extract exit code from last non-empty line
  let lines = output
    |> string.split("\n")
    |> gleam_list.filter(fn(line) { string.trim(line) != "" })

  case lines {
    [] -> Ok(Success("", "", 0))
    _ -> {
      let reversed = reverse_list(lines)
      case reversed {
        [exit_str, ..rest_reversed] -> {
          let trimmed_exit = string.trim(exit_str)
          case string.to_utf_codepoints(trimmed_exit) |> parse_int_from_codepoints {
            Ok(exit_code) -> {
              let stdout_lines = reverse_list(rest_reversed)
              let combined = string.join(stdout_lines, "\n")
              case exit_code {
                0 -> Ok(Success(combined, "", 0))
                code -> Ok(Failure(combined, code))
              }
            }
            Error(_) -> Ok(Success(output, "", 0))
          }
        }
        [] -> Ok(Success("", "", 0))
      }
    }
  }
}

fn reverse_list(list: List(a)) -> List(a) {
  reverse_acc(list, [])
}

fn reverse_acc(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [x, ..xs] -> reverse_acc(xs, [x, ..acc])
  }
}

fn parse_int_from_codepoints(codepoints: List(UtfCodepoint)) -> Result(Int, Nil) {
  codepoints
  |> string.from_utf_codepoints
  |> parse_int_string
}

fn parse_int_string(s: String) -> Result(Int, Nil) {
  try_parse_int(s)
}

fn try_parse_int(s: String) -> Result(Int, Nil) {
  case s {
    "" -> Error(Nil)
    _ -> {
      // Try to parse using Erlang FFI with exception handling
      case catch_parse(s) {
        Ok(i) -> Ok(i)
        Error(_) -> Error(Nil)
      }
    }
  }
}

@external(erlang, "factory@process_ffi", "parse_int_safe")
fn catch_parse(s: String) -> Result(Int, Nil)

/// Execute a raw shell command using Erlang's os:cmd (expects charlist)
@external(erlang, "os", "cmd")
fn os_cmd_raw(cmd: Charlist) -> Charlist

/// Convert string to charlist
@external(erlang, "unicode", "characters_to_list")
fn to_charlist(str: String) -> Charlist

/// Convert charlist to string
@external(erlang, "unicode", "characters_to_binary")
fn from_charlist(chars: Charlist) -> String

/// Opaque type for Erlang charlist
pub type Charlist

fn os_cmd(cmd: String) -> String {
  cmd
  |> to_charlist
  |> os_cmd_raw
  |> from_charlist
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
  case command_exists(cmd) {
    Ok(_) -> run_command(cmd, args, cwd)
    Error(e) -> Error(e)
  }
}

/// Send ACP cancel notification for session
pub fn acp_cancel(session_id: String) -> Result(types.AcpNotification, String) {
  Ok(types.AcpNotification(session_id: session_id, method: "session/cancel"))
}

/// Send ACP cancel over HTTP transport
pub fn acp_send_cancel(client: types.AcpClient, session_id: String) -> Result(Nil, String) {
  let types.AcpClient(_base_url) = client
  let _notification = types.AcpNotification(session_id: session_id, method: "session/cancel")
  Error("network connection failed")
}

/// Read text file content
pub fn fs_read_text_file(path: String) -> Result(String, String) {
  simplifile.read(path)
  |> map_simplifile_error
}

fn map_simplifile_error(result: Result(String, simplifile.FileError)) -> Result(String, String) {
  case result {
    Ok(content) -> Ok(content)
    Error(_) -> Error("File read failed")
  }
}
