import cli
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// parse_args: help command tests
pub fn parse_args_help_no_args_test() {
  cli.parse_args([])
  |> should.equal(Ok(cli.Help(None)))
}

pub fn parse_args_help_explicit_test() {
  cli.parse_args(["help"])
  |> should.equal(Ok(cli.Help(None)))
}

pub fn parse_args_help_with_topic_test() {
  cli.parse_args(["help", "new"])
  |> should.equal(Ok(cli.Help(Some("new"))))
}

// parse_args: version command tests
pub fn parse_args_version_test() {
  cli.parse_args(["version"])
  |> should.equal(Ok(cli.Version))
}

// parse_args: new command tests
pub fn parse_args_new_with_slug_test() {
  cli.parse_args(["new", "--slug", "test-slug"])
  |> should.equal(Ok(cli.NewTask("test-slug", None, False)))
}

pub fn parse_args_new_with_short_slug_test() {
  cli.parse_args(["new", "-s", "test-slug"])
  |> should.equal(Ok(cli.NewTask("test-slug", None, False)))
}

pub fn parse_args_new_with_contract_test() {
  cli.parse_args(["new", "--slug", "test-slug", "--contract", "path/to/contract"])
  |> should.equal(Ok(cli.NewTask("test-slug", Some("path/to/contract"), False)))
}

pub fn parse_args_new_with_interactive_test() {
  cli.parse_args(["new", "--slug", "test-slug", "--interactive"])
  |> should.equal(Ok(cli.NewTask("test-slug", None, True)))
}

pub fn parse_args_new_missing_slug_test() {
  cli.parse_args(["new"])
  |> should.equal(Error("--slug is required for new command"))
}

// parse_args: stage command tests
pub fn parse_args_stage_basic_test() {
  cli.parse_args(["stage", "--slug", "test-slug", "--stage", "implement"])
  |> should.equal(Ok(cli.RunStage("test-slug", "implement", False, None, None)))
}

pub fn parse_args_stage_with_dry_run_test() {
  cli.parse_args(["stage", "--slug", "test-slug", "--stage", "implement", "--dry-run"])
  |> should.equal(Ok(cli.RunStage("test-slug", "implement", True, None, None)))
}

pub fn parse_args_stage_with_from_to_test() {
  cli.parse_args([
    "stage",
    "--slug", "test-slug",
    "--stage", "implement",
    "--from", "start",
    "--to", "end",
  ])
  |> should.equal(Ok(cli.RunStage("test-slug", "implement", False, Some("start"), Some("end"))))
}

pub fn parse_args_stage_missing_slug_test() {
  cli.parse_args(["stage", "--stage", "implement"])
  |> should.equal(Error("--slug is required for stage command"))
}

pub fn parse_args_stage_missing_stage_test() {
  cli.parse_args(["stage", "--slug", "test-slug"])
  |> should.equal(Error("--stage is required for stage command"))
}

// parse_args: approve command tests
pub fn parse_args_approve_basic_test() {
  cli.parse_args(["approve", "--slug", "test-slug"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", None, False)))
}

pub fn parse_args_approve_with_short_slug_test() {
  cli.parse_args(["approve", "-s", "test-slug"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", None, False)))
}

pub fn parse_args_approve_with_force_test() {
  cli.parse_args(["approve", "--slug", "test-slug", "--force"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", None, True)))
}

pub fn parse_args_approve_with_short_force_test() {
  cli.parse_args(["approve", "-s", "test-slug", "-f"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", None, True)))
}

pub fn parse_args_approve_with_strategy_immediate_test() {
  cli.parse_args(["approve", "--slug", "test-slug", "--strategy", "immediate"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", Some("immediate"), False)))
}

pub fn parse_args_approve_with_strategy_gradual_test() {
  cli.parse_args(["approve", "--slug", "test-slug", "--strategy", "gradual"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", Some("gradual"), False)))
}

pub fn parse_args_approve_with_strategy_canary_test() {
  cli.parse_args(["approve", "--slug", "test-slug", "--strategy", "canary"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", Some("canary"), False)))
}

pub fn parse_args_approve_with_invalid_strategy_test() {
  cli.parse_args(["approve", "--slug", "test-slug", "--strategy", "invalid"])
  |> should.equal(Error("Invalid strategy value: invalid. Valid values are: immediate, gradual, canary"))
}

pub fn parse_args_approve_missing_slug_test() {
  cli.parse_args(["approve"])
  |> should.equal(Error("--slug is required for approve command"))
}

// parse_args: show command tests
pub fn parse_args_show_basic_test() {
  cli.parse_args(["show", "--slug", "test-slug"])
  |> should.equal(Ok(cli.ShowTask("test-slug", False)))
}

pub fn parse_args_show_with_short_slug_test() {
  cli.parse_args(["show", "-s", "test-slug"])
  |> should.equal(Ok(cli.ShowTask("test-slug", False)))
}

pub fn parse_args_show_with_detailed_test() {
  cli.parse_args(["show", "--slug", "test-slug", "--detailed"])
  |> should.equal(Ok(cli.ShowTask("test-slug", True)))
}

pub fn parse_args_show_missing_slug_test() {
  cli.parse_args(["show"])
  |> should.equal(Error("--slug is required for show command"))
}

// parse_args: list command tests
pub fn parse_args_list_no_filters_test() {
  cli.parse_args(["list"])
  |> should.equal(Ok(cli.ListTasks(None, None)))
}

pub fn parse_args_list_with_priority_p1_test() {
  cli.parse_args(["list", "--priority", "P1"])
  |> should.equal(Ok(cli.ListTasks(Some("P1"), None)))
}

pub fn parse_args_list_with_priority_p2_test() {
  cli.parse_args(["list", "--priority", "P2"])
  |> should.equal(Ok(cli.ListTasks(Some("P2"), None)))
}

pub fn parse_args_list_with_priority_p3_test() {
  cli.parse_args(["list", "--priority", "P3"])
  |> should.equal(Ok(cli.ListTasks(Some("P3"), None)))
}

pub fn parse_args_list_with_invalid_priority_test() {
  cli.parse_args(["list", "--priority", "P4"])
  |> should.equal(Error("Invalid priority value: P4. Valid values are: P1, P2, P3"))
}

pub fn parse_args_list_with_status_open_test() {
  cli.parse_args(["list", "--status", "open"])
  |> should.equal(Ok(cli.ListTasks(None, Some("open"))))
}

pub fn parse_args_list_with_status_in_progress_test() {
  cli.parse_args(["list", "--status", "in_progress"])
  |> should.equal(Ok(cli.ListTasks(None, Some("in_progress"))))
}

pub fn parse_args_list_with_status_done_test() {
  cli.parse_args(["list", "--status", "done"])
  |> should.equal(Ok(cli.ListTasks(None, Some("done"))))
}

pub fn parse_args_list_with_invalid_status_test() {
  cli.parse_args(["list", "--status", "invalid"])
  |> should.equal(Error("Invalid status value: invalid. Valid values are: open, in_progress, done"))
}

pub fn parse_args_list_with_priority_and_status_test() {
  cli.parse_args(["list", "--priority", "P1", "--status", "open"])
  |> should.equal(Ok(cli.ListTasks(Some("P1"), Some("open"))))
}

pub fn parse_args_list_with_invalid_priority_and_valid_status_test() {
  cli.parse_args(["list", "--priority", "invalid", "--status", "open"])
  |> should.equal(Error("Invalid priority value: invalid. Valid values are: P1, P2, P3"))
}

pub fn parse_args_list_with_valid_priority_and_invalid_status_test() {
  cli.parse_args(["list", "--priority", "P1", "--status", "invalid"])
  |> should.equal(Error("Invalid status value: invalid. Valid values are: open, in_progress, done"))
}

// parse_args: unknown command tests
pub fn parse_args_unknown_command_test() {
  cli.parse_args(["unknown"])
  |> should.equal(Error("Unknown command: unknown"))
}

pub fn parse_args_unknown_command_with_args_test() {
  cli.parse_args(["invalid", "--flag", "value"])
  |> should.equal(Error("Unknown command: invalid"))
}

// help_text test
pub fn help_text_not_empty_test() {
  let text = cli.help_text()
  case text {
    "" -> should.fail()
    _ -> {
      string.contains(text, "USAGE:")
      |> should.equal(True)
      string.contains(text, "COMMANDS:")
      |> should.equal(True)
      string.contains(text, "EXAMPLES:")
      |> should.equal(True)
    }
  }
}
