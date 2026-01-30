import claude_code_client
import gleeunit/should

pub fn cli_tool_claude_code_exists_test() {
  let tool = claude_code_client.ClaudeCode
  case tool {
    claude_code_client.ClaudeCode -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_tool_custom_tool_captures_name_test() {
  let tool = claude_code_client.CustomTool(name: "custom-ai", command: "/usr/bin/custom")
  case tool {
    claude_code_client.CustomTool(name: "custom-ai", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_tool_custom_tool_captures_command_test() {
  let tool = claude_code_client.CustomTool(name: "tool", command: "/path/to/cmd")
  case tool {
    claude_code_client.CustomTool(command: "/path/to/cmd", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_config_captures_tool_test() {
  let config = claude_code_client.CliConfig(
    tool: claude_code_client.ClaudeCode,
    cwd: "/tmp",
    max_turns: 20,
    model: "claude-3",
    allowed_tools: [],
  )
  case config {
    claude_code_client.CliConfig(tool: claude_code_client.ClaudeCode, ..) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_config_captures_cwd_test() {
  let config = claude_code_client.CliConfig(
    tool: claude_code_client.ClaudeCode,
    cwd: "/home/user/project",
    max_turns: 20,
    model: "claude-3",
    allowed_tools: [],
  )
  case config {
    claude_code_client.CliConfig(cwd: "/home/user/project", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_config_captures_max_turns_test() {
  let config = claude_code_client.CliConfig(
    tool: claude_code_client.ClaudeCode,
    cwd: "/tmp",
    max_turns: 50,
    model: "claude-3",
    allowed_tools: [],
  )
  case config {
    claude_code_client.CliConfig(max_turns: 50, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_config_captures_model_test() {
  let config = claude_code_client.CliConfig(
    tool: claude_code_client.ClaudeCode,
    cwd: "/tmp",
    max_turns: 20,
    model: "claude-opus",
    allowed_tools: [],
  )
  case config {
    claude_code_client.CliConfig(model: "claude-opus", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_config_captures_allowed_tools_test() {
  let tools = ["Read", "Bash", "Glob"]
  let config = claude_code_client.CliConfig(
    tool: claude_code_client.ClaudeCode,
    cwd: "/tmp",
    max_turns: 20,
    model: "claude-3",
    allowed_tools: tools,
  )
  case config {
    claude_code_client.CliConfig(allowed_tools: t, ..) -> {
      t |> should.equal(tools)
    }
    _ -> should.fail()
  }
}

pub fn cli_success_captures_output_test() {
  let result = claude_code_client.CliSuccess(output: "Done", turns_used: 5, exit_code: 0)
  case result {
    claude_code_client.CliSuccess(output: "Done", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_success_captures_turns_used_test() {
  let result = claude_code_client.CliSuccess(output: "output", turns_used: 15, exit_code: 0)
  case result {
    claude_code_client.CliSuccess(turns_used: 15, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_success_captures_exit_code_test() {
  let result = claude_code_client.CliSuccess(output: "output", turns_used: 5, exit_code: 42)
  case result {
    claude_code_client.CliSuccess(exit_code: 42, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_failure_captures_reason_test() {
  let result = claude_code_client.CliFailure(reason: "timeout", exit_code: 124)
  case result {
    claude_code_client.CliFailure(reason: "timeout", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn cli_failure_captures_exit_code_test() {
  let result = claude_code_client.CliFailure(reason: "error", exit_code: 1)
  case result {
    claude_code_client.CliFailure(exit_code: 1, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_claude_code_config_uses_claude_code_tool_test() {
  let config = claude_code_client.new_claude_code_config("/tmp", 20)
  case config {
    claude_code_client.CliConfig(tool: claude_code_client.ClaudeCode, ..) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_claude_code_config_sets_cwd_test() {
  let config = claude_code_client.new_claude_code_config("/home/work", 20)
  case config {
    claude_code_client.CliConfig(cwd: "/home/work", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_claude_code_config_sets_max_turns_test() {
  let config = claude_code_client.new_claude_code_config("/tmp", 30)
  case config {
    claude_code_client.CliConfig(max_turns: 30, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_claude_code_config_includes_default_tools_test() {
  let config = claude_code_client.new_claude_code_config("/tmp", 20)
  case config {
    claude_code_client.CliConfig(allowed_tools: tools, ..) -> {
      tools |> should.not_equal([])
    }
    _ -> should.fail()
  }
}

pub fn with_allowed_tools_updates_config_test() {
  let config = claude_code_client.new_claude_code_config("/tmp", 20)
  let updated = claude_code_client.with_allowed_tools(config, ["Read", "Write"])
  case updated {
    claude_code_client.CliConfig(allowed_tools: ["Read", "Write"], ..) ->
      should.be_true(True)
    _ -> should.fail()
  }
}

pub fn with_model_updates_config_test() {
  let config = claude_code_client.new_claude_code_config("/tmp", 20)
  let updated = claude_code_client.with_model(config, "claude-haiku")
  case updated {
    claude_code_client.CliConfig(model: "claude-haiku", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn with_max_turns_updates_config_test() {
  let config = claude_code_client.new_claude_code_config("/tmp", 10)
  let updated = claude_code_client.with_max_turns(config, 50)
  case updated {
    claude_code_client.CliConfig(max_turns: 50, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}
