// Shell module - Execute external commands

import gleam/dynamic/decode
import gleam/erlang/process as erl_process
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile
import types

const exit_success = 0

/// Escape a string for safe shell execution using single quotes
fn shell_escape(s: String) -> String {
  "'" <> string.replace(s, "'", "'\"'\"'") <> "'"
}

/// Result of command execution
pub type CommandResult {
  Success(stdout: String, stderr: String, exit_code: Int)
  Failure(stderr: String, exit_code: Int)
}

fn build_shell_command(cmd: String, args: List(String), cwd: String) -> String {
  let escaped_cmd = shell_escape(cmd)
  let escaped_args = list.map(args, shell_escape)
  let full_cmd = case args {
    [] -> escaped_cmd
    _ -> escaped_cmd <> " " <> string.join(escaped_args, " ")
  }
  case cwd {
    "" -> full_cmd <> " 2>&1; echo $?"
    _ -> "cd " <> shell_escape(cwd) <> " && " <> full_cmd <> " 2>&1; echo $?"
  }
}

fn extract_exit_code(output: String) -> Result(Int, Nil) {
  let lines =
    output
    |> string.split("\n")
    |> list.filter(fn(line) { string.trim(line) != "" })
  case lines {
    [] -> Error(Nil)
    _ -> {
      let reversed = reverse_list(lines)
      case reversed {
        [exit_str, ..] ->
          string.to_utf_codepoints(string.trim(exit_str))
          |> parse_int_from_codepoints
        [] -> Error(Nil)
      }
    }
  }
}

fn parse_command_output(output: String) -> CommandResult {
  let lines =
    output
    |> string.split("\n")
    |> list.filter(fn(line) { string.trim(line) != "" })
  case lines {
    [] -> Success("", "", exit_success)
    _ -> {
      let reversed = reverse_list(lines)
      case reversed {
        [_, ..rest_reversed] -> {
          case extract_exit_code(output) {
            Ok(exit_code) -> {
              let stdout_lines = reverse_list(rest_reversed)
              let combined = string.join(stdout_lines, "\n")
              case exit_code {
                c if c == exit_success -> Success(combined, "", exit_success)
                code -> Failure(combined, code)
              }
            }
            Error(_) -> Success(output, "", exit_success)
          }
        }
        [] -> Success("", "", exit_success)
      }
    }
  }
}

/// Execute a command with arguments in a working directory
/// Returns Success if exit_code == 0, Failure otherwise
pub fn run_command(
  cmd: String,
  args: List(String),
  cwd: String,
) -> Result(CommandResult, String) {
  let shell_cmd = build_shell_command(cmd, args, cwd)
  let output = os_cmd(shell_cmd)
  Ok(parse_command_output(output))
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
    Ok(Success(_, _, _)) -> Ok(True)
    Ok(Failure(_, _)) -> Error("Command not found in PATH: " <> cmd)
    Error(_) -> Error("Command not found in PATH: " <> cmd)
  }
}

/// Parse command output to get success/failure
pub fn parse_result(
  stdout: String,
  stderr: String,
  exit_code: Int,
) -> CommandResult {
  case exit_code {
    c if c == exit_success -> Success(stdout, stderr, exit_success)
    code -> Failure(stderr, code)
  }
}

/// Helper to extract stdout from successful command
pub fn get_stdout(result: CommandResult) -> Result(String, String) {
  case result {
    Success(out, _, _) -> Ok(out)
    Failure(err, code) ->
      Error(
        "Command failed with exit code " <> string.inspect(code) <> ": " <> err,
      )
  }
}

/// Helper to check if command succeeded
pub fn is_success(result: CommandResult) -> Bool {
  case result {
    Success(_, _, c) if c == exit_success -> True
    _ -> False
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
      Error(
        "Command failed with exit code " <> string.inspect(code) <> ": " <> err,
      )
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

/// Send ACP cancel notification for session via HTTP transport
pub fn acp_send_cancel(
  client: types.AcpClient,
  session_id: String,
) -> Result(Nil, String) {
  let json_body =
    json.object([
      #("jsonrpc", json.string("2.0")),
      #("method", json.string("session/cancel")),
      #(
        "params",
        json.object([
          #("meta", json.object([#("sessionId", json.string(session_id))])),
        ]),
      ),
    ])
    |> json.to_string
  run_command(
    "curl",
    [
      "-X",
      "POST",
      "-H",
      "Content-Type: application/json",
      "-d",
      json_body,
      types.get_base_url(client),
    ],
    "",
  )
  |> result.try(fn(r) {
    case r {
      Success(_, _, c) if c == exit_success -> Ok(Nil)
      Success(_, _, code) | Failure(_, code) ->
        Error("http request failed with code " <> string.inspect(code))
    }
  })
}

/// Create a new ACP session via HTTP transport, returns session_id
pub fn acp_new_session(client: types.AcpClient) -> Result(String, String) {
  let json_body =
    json.object([
      #("jsonrpc", json.string("2.0")),
      #("id", json.int(1)),
      #("method", json.string("session/new")),
      #("params", json.object([])),
    ])
    |> json.to_string
  use cmd_result <- result.try(run_command(
    "curl",
    [
      "-s",
      "-X",
      "POST",
      "-H",
      "Content-Type: application/json",
      "-d",
      json_body,
      types.get_base_url(client),
    ],
    "",
  ))
  use json <- result.try(case cmd_result {
    Success(stdout, _, c) if c == exit_success -> Ok(stdout)
    Success(_, _, code) | Failure(_, code) ->
      Error("http failed code " <> string.inspect(code))
  })
  parse_session_id(json)
}

fn parse_session_id(json: String) -> Result(String, String) {
  let decoder = decode.at(["result", "sessionId"], decode.string)
  json.parse(json, decoder)
  |> result.map_error(fn(_) { "invalid session response JSON" })
}

/// Send a prompt to an ACP session, returns response content
pub fn acp_session_prompt(
  client: types.AcpClient,
  session_id: String,
  prompt: String,
) -> Result(String, String) {
  let json_body =
    json.object([
      #("jsonrpc", json.string("2.0")),
      #("id", json.int(1)),
      #("method", json.string("session/prompt")),
      #(
        "params",
        json.object([
          #("meta", json.object([#("sessionId", json.string(session_id))])),
          #("text", json.string(prompt)),
        ]),
      ),
    ])
    |> json.to_string
  use cmd_result <- result.try(run_command(
    "curl",
    [
      "-s",
      "-X",
      "POST",
      "-H",
      "Content-Type: application/json",
      "-d",
      json_body,
      types.get_base_url(client),
    ],
    "",
  ))
  use json <- result.try(case cmd_result {
    Success(stdout, _, c) if c == exit_success -> Ok(stdout)
    Success(_, _, code) | Failure(_, code) ->
      Error("http failed code " <> string.inspect(code))
  })
  extract_response_text(json)
}

fn extract_response_text(json: String) -> Result(String, String) {
  let decoder = decode.at(["result", "text"], decode.string)
  json.parse(json, decoder)
  |> result.map_error(fn(_) { "invalid response text JSON" })
}

/// Read text file content
pub fn fs_read_text_file(path: String) -> Result(String, String) {
  simplifile.read(path)
  |> map_simplifile_error
}

fn map_simplifile_error(
  result: Result(String, simplifile.FileError),
) -> Result(String, String) {
  case result {
    Ok(content) -> Ok(content)
    Error(_) -> Error("File read failed")
  }
}

/// Role-based filesystem permissions
pub type FsRole {
  Auditor
  Implementer
}

/// Write text file with role-based path enforcement
pub fn fs_write_text_file(
  path: String,
  content: String,
  role: FsRole,
) -> Result(Nil, String) {
  case is_path_allowed(path, role) {
    False -> Error("Path not allowed for role")
    True -> {
      case simplifile.write(path, content) {
        Ok(Nil) -> Ok(Nil)
        Error(_) -> Error("File write failed")
      }
    }
  }
}

fn is_path_allowed(path: String, role: FsRole) -> Bool {
  // Reject dangerous patterns: absolute paths, traversal, hidden files
  let is_safe =
    !string.starts_with(path, "/")
    && !string.contains(path, "..")
    && !string.starts_with(path, ".")

  case is_safe {
    False -> False
    True ->
      case role {
        Auditor -> string.starts_with(path, "test/")
        Implementer ->
          string.starts_with(path, "src/")
          && !string.starts_with(path, "src/gleam/")
      }
  }
}

/// Initialize ACP connection and retrieve agent capabilities
pub fn acp_initialize(
  client: types.AcpClient,
  version: String,
) -> Result(types.AcpClient, String) {
  use req <- result.try(
    types.encode_initialize_request(version, "factory", "0.1.0")
    |> result.map_error(fn(_) { "encode failed" }),
  )
  use cmd_result <- result.try(run_command(
    "curl",
    [
      "-X",
      "POST",
      "-H",
      "Content-Type: application/json",
      "-d",
      req,
      types.get_base_url(client),
    ],
    "",
  ))
  use json <- result.try(case cmd_result {
    Success(stdout, _, c) if c == exit_success -> Ok(stdout)
    Success(_, _, code) | Failure(_, code) ->
      Error("http failed code " <> string.inspect(code))
  })
  use caps <- result.try(types.parse_initialize_result(json))
  Ok(types.store_capabilities(client, caps))
}

pub fn start_linked(f: fn() -> a) -> erl_process.Pid {
  erl_process.spawn(f)
}
