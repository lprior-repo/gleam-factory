//// Generic coding CLI wrapper - supports Claude Code and future tools.

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import process

pub type CliTool {
  ClaudeCode
  CustomTool(name: String, command: String)
}

pub type CliConfig {
  CliConfig(
    tool: CliTool,
    cwd: String,
    max_turns: Int,
    model: String,
    allowed_tools: List(String),
  )
}

pub type CliResult {
  CliSuccess(output: String, turns_used: Int, exit_code: Int)
  CliFailure(reason: String, exit_code: Int)
}

pub fn new_claude_code_config(cwd: String, max_turns: Int) -> CliConfig {
  CliConfig(
    tool: ClaudeCode,
    cwd:,
    max_turns:,
    model: "claude-sonnet-4-5-20250929",
    allowed_tools: ["Read", "Edit", "Write", "Bash", "Glob", "Grep"],
  )
}

pub fn execute_task(
  config: CliConfig,
  prompt: String,
) -> Result(CliResult, String) {
  case config.tool {
    ClaudeCode -> execute_claude_code(config, prompt)
    CustomTool(_, cmd) -> execute_custom_tool(cmd, config, prompt)
  }
}

pub fn execute_streaming(
  config: CliConfig,
  prompt: String,
  on_chunk: fn(String) -> Nil,
) -> Result(CliResult, String) {
  case config.tool {
    ClaudeCode -> execute_claude_code_stream(config, prompt, on_chunk)
    CustomTool(_, cmd) -> execute_custom_tool(cmd, config, prompt)
  }
}

fn execute_claude_code(
  config: CliConfig,
  prompt: String,
) -> Result(CliResult, String) {
  let args = build_claude_args(config, prompt, False)

  use cmd_result <- result.try(process.run_command("claude", args, config.cwd))

  case cmd_result {
    process.Success(stdout, _, exit_code) -> {
      case exit_code {
        0 -> parse_claude_output(stdout, exit_code)
        _ -> Ok(CliFailure("Claude Code failed: " <> stdout, exit_code))
      }
    }
    process.Failure(stderr, exit_code) ->
      Ok(CliFailure("Claude Code error: " <> stderr, exit_code))
  }
}

fn execute_claude_code_stream(
  config: CliConfig,
  prompt: String,
  on_chunk: fn(String) -> Nil,
) -> Result(CliResult, String) {
  let args = build_claude_args(config, prompt, True)

  use cmd_result <- result.try(process.run_command("claude", args, config.cwd))

  case cmd_result {
    process.Success(stdout, _, exit_code) -> {
      stdout
      |> string.split("\n")
      |> list.each(on_chunk)

      parse_claude_output(stdout, exit_code)
    }
    process.Failure(stderr, exit_code) ->
      Ok(CliFailure("Claude Code stream error: " <> stderr, exit_code))
  }
}

fn build_claude_args(
  config: CliConfig,
  prompt: String,
  stream: Bool,
) -> List(String) {
  let base = [
    "-p",
    prompt,
    "--output-format",
    case stream {
      True -> "stream-json"
      False -> "json"
    },
    "--max-turns",
    int.to_string(config.max_turns),
    "--model",
    config.model,
  ]

  let with_tools = case list.is_empty(config.allowed_tools) {
    True -> base
    False ->
      list.append(base, [
        "--allowed-tools",
        string.join(config.allowed_tools, ","),
      ])
  }

  with_tools
}

fn parse_claude_output(
  output: String,
  exit_code: Int,
) -> Result(CliResult, String) {
  case extract_turns_from_json(output) {
    Ok(turns) -> Ok(CliSuccess(output:, turns_used: turns, exit_code:))
    Error(_) -> Ok(CliSuccess(output:, turns_used: 0, exit_code:))
  }
}

fn extract_turns_from_json(json_str: String) -> Result(Int, String) {
  let decoder = decode.at(["turns_used"], decode.int)
  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "No turns_used in output" })
}

fn execute_custom_tool(
  cmd: String,
  config: CliConfig,
  prompt: String,
) -> Result(CliResult, String) {
  use cmd_result <- result.try(process.run_command(cmd, [prompt], config.cwd))

  case cmd_result {
    process.Success(stdout, _, exit_code) ->
      Ok(CliSuccess(output: stdout, turns_used: 0, exit_code:))
    process.Failure(stderr, exit_code) -> Ok(CliFailure(stderr, exit_code))
  }
}

pub fn with_allowed_tools(config: CliConfig, tools: List(String)) -> CliConfig {
  CliConfig(..config, allowed_tools: tools)
}

pub fn with_model(config: CliConfig, model: String) -> CliConfig {
  CliConfig(..config, model:)
}

pub fn with_max_turns(config: CliConfig, max_turns: Int) -> CliConfig {
  CliConfig(..config, max_turns:)
}
