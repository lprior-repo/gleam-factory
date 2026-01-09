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
  cli.parse_args([
    "new",
    "--slug",
    "test-slug",
    "--contract",
    "path/to/contract",
  ])
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
  cli.parse_args([
    "stage",
    "--slug",
    "test-slug",
    "--stage",
    "implement",
    "--dry-run",
  ])
  |> should.equal(Ok(cli.RunStage("test-slug", "implement", True, None, None)))
}

pub fn parse_args_stage_with_from_to_test() {
  cli.parse_args([
    "stage", "--slug", "test-slug", "--stage", "implement", "--from", "start",
    "--to", "end",
  ])
  |> should.equal(
    Ok(cli.RunStage("test-slug", "implement", False, Some("start"), Some("end"))),
  )
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
  |> should.equal(Error(
    "Invalid strategy value: invalid. Valid values are: immediate, gradual, canary",
  ))
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
  |> should.equal(Error(
    "Invalid priority value: P4. Valid values are: P1, P2, P3",
  ))
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
  |> should.equal(Error(
    "Invalid status value: invalid. Valid values are: open, in_progress, done",
  ))
}

pub fn parse_args_list_with_priority_and_status_test() {
  cli.parse_args(["list", "--priority", "P1", "--status", "open"])
  |> should.equal(Ok(cli.ListTasks(Some("P1"), Some("open"))))
}

pub fn parse_args_list_with_invalid_priority_and_valid_status_test() {
  cli.parse_args(["list", "--priority", "invalid", "--status", "open"])
  |> should.equal(Error(
    "Invalid priority value: invalid. Valid values are: P1, P2, P3",
  ))
}

pub fn parse_args_list_with_valid_priority_and_invalid_status_test() {
  cli.parse_args(["list", "--priority", "P1", "--status", "invalid"])
  |> should.equal(Error(
    "Invalid status value: invalid. Valid values are: open, in_progress, done",
  ))
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

// Edge cases: flag parsing with missing values
pub fn parse_args_new_flag_missing_value_test() {
  cli.parse_args(["new", "--slug"])
  |> should.equal(Error("--slug is required for new command"))
}

pub fn parse_args_new_short_flag_missing_value_test() {
  cli.parse_args(["new", "-s"])
  |> should.equal(Error("--slug is required for new command"))
}

pub fn parse_args_stage_flag_missing_value_test() {
  cli.parse_args(["stage", "--slug", "test-slug", "--stage"])
  |> should.equal(Error("--stage is required for stage command"))
}

pub fn parse_args_new_all_flags_test() {
  cli.parse_args([
    "new", "--slug", "test-slug", "--contract", "path/to/contract",
    "--interactive",
  ])
  |> should.equal(Ok(cli.NewTask("test-slug", Some("path/to/contract"), True)))
}

pub fn parse_args_new_short_contract_flag_test() {
  cli.parse_args(["new", "-s", "test-slug", "-c", "path/contract"])
  |> should.equal(Ok(cli.NewTask("test-slug", Some("path/contract"), False)))
}

pub fn parse_args_new_mixed_flag_order_test() {
  cli.parse_args([
    "new", "--interactive", "--slug", "test-slug", "--contract",
    "path/to/contract",
  ])
  |> should.equal(Ok(cli.NewTask("test-slug", Some("path/to/contract"), True)))
}

pub fn parse_args_stage_short_dry_run_test() {
  cli.parse_args(["stage", "-s", "test-slug", "--stage", "test", "-d"])
  |> should.equal(Ok(cli.RunStage("test-slug", "test", True, None, None)))
}

pub fn parse_args_stage_all_flags_test() {
  cli.parse_args([
    "stage", "-s", "test-slug", "--stage", "implement", "-d", "--from",
    "commit1", "--to", "commit2",
  ])
  |> should.equal(
    Ok(cli.RunStage(
      "test-slug",
      "implement",
      True,
      Some("commit1"),
      Some("commit2"),
    )),
  )
}

pub fn parse_args_approve_all_flags_test() {
  cli.parse_args(["approve", "-s", "test-slug", "--strategy", "gradual", "-f"])
  |> should.equal(Ok(cli.ApproveTask("test-slug", Some("gradual"), True)))
}

pub fn parse_args_show_detailed_test() {
  cli.parse_args(["show", "-s", "test-slug", "--detailed"])
  |> should.equal(Ok(cli.ShowTask("test-slug", True)))
}

pub fn parse_args_list_both_filters_p2_in_progress_test() {
  cli.parse_args(["list", "--priority", "P2", "--status", "in_progress"])
  |> should.equal(Ok(cli.ListTasks(Some("P2"), Some("in_progress"))))
}

pub fn parse_args_list_both_filters_p3_done_test() {
  cli.parse_args(["list", "--priority", "P3", "--status", "done"])
  |> should.equal(Ok(cli.ListTasks(Some("P3"), Some("done"))))
}

// Edge cases: strategy validation - all valid values
pub fn parse_args_approve_strategy_all_valid_test() {
  cli.parse_args(["approve", "-s", "slug1", "--strategy", "immediate"])
  |> should.equal(Ok(cli.ApproveTask("slug1", Some("immediate"), False)))

  cli.parse_args(["approve", "-s", "slug2", "--strategy", "gradual"])
  |> should.equal(Ok(cli.ApproveTask("slug2", Some("gradual"), False)))

  cli.parse_args(["approve", "-s", "slug3", "--strategy", "canary"])
  |> should.equal(Ok(cli.ApproveTask("slug3", Some("canary"), False)))
}

// Edge cases: strategy validation - various invalid values
pub fn parse_args_approve_strategy_uppercase_invalid_test() {
  cli.parse_args(["approve", "-s", "slug", "--strategy", "IMMEDIATE"])
  |> should.equal(Error(
    "Invalid strategy value: IMMEDIATE. Valid values are: immediate, gradual, canary",
  ))
}

pub fn parse_args_approve_strategy_empty_invalid_test() {
  cli.parse_args(["approve", "-s", "slug", "--strategy", ""])
  |> should.equal(Error(
    "Invalid strategy value: . Valid values are: immediate, gradual, canary",
  ))
}

pub fn parse_args_approve_strategy_partial_invalid_test() {
  cli.parse_args(["approve", "-s", "slug", "--strategy", "immed"])
  |> should.equal(Error(
    "Invalid strategy value: immed. Valid values are: immediate, gradual, canary",
  ))
}

// Edge cases: priority validation - all valid and invalid
pub fn parse_args_list_priority_uppercase_invalid_test() {
  cli.parse_args(["list", "--priority", "p1"])
  |> should.equal(Error(
    "Invalid priority value: p1. Valid values are: P1, P2, P3",
  ))
}

pub fn parse_args_list_priority_p0_invalid_test() {
  cli.parse_args(["list", "--priority", "P0"])
  |> should.equal(Error(
    "Invalid priority value: P0. Valid values are: P1, P2, P3",
  ))
}

pub fn parse_args_list_priority_p4_invalid_test() {
  cli.parse_args(["list", "--priority", "P4"])
  |> should.equal(Error(
    "Invalid priority value: P4. Valid values are: P1, P2, P3",
  ))
}

pub fn parse_args_list_priority_empty_invalid_test() {
  cli.parse_args(["list", "--priority", ""])
  |> should.equal(Error(
    "Invalid priority value: . Valid values are: P1, P2, P3",
  ))
}

// Edge cases: status validation - all valid and invalid
pub fn parse_args_list_status_uppercase_invalid_test() {
  cli.parse_args(["list", "--status", "OPEN"])
  |> should.equal(Error(
    "Invalid status value: OPEN. Valid values are: open, in_progress, done",
  ))
}

pub fn parse_args_list_status_partial_invalid_test() {
  cli.parse_args(["list", "--status", "in_prog"])
  |> should.equal(Error(
    "Invalid status value: in_prog. Valid values are: open, in_progress, done",
  ))
}

pub fn parse_args_list_status_empty_invalid_test() {
  cli.parse_args(["list", "--status", ""])
  |> should.equal(Error(
    "Invalid status value: . Valid values are: open, in_progress, done",
  ))
}

pub fn parse_args_list_status_pending_invalid_test() {
  cli.parse_args(["list", "--status", "pending"])
  |> should.equal(Error(
    "Invalid status value: pending. Valid values are: open, in_progress, done",
  ))
}

// Edge cases: help with various topics
pub fn parse_args_help_with_new_topic_test() {
  cli.parse_args(["help", "new"])
  |> should.equal(Ok(cli.Help(Some("new"))))
}

pub fn parse_args_help_with_stage_topic_test() {
  cli.parse_args(["help", "stage"])
  |> should.equal(Ok(cli.Help(Some("stage"))))
}

pub fn parse_args_help_with_approve_topic_test() {
  cli.parse_args(["help", "approve"])
  |> should.equal(Ok(cli.Help(Some("approve"))))
}

pub fn parse_args_help_with_show_topic_test() {
  cli.parse_args(["help", "show"])
  |> should.equal(Ok(cli.Help(Some("show"))))
}

pub fn parse_args_help_with_list_topic_test() {
  cli.parse_args(["help", "list"])
  |> should.equal(Ok(cli.Help(Some("list"))))
}

// Edge cases: unknown commands
pub fn parse_args_unknown_command_delete_test() {
  cli.parse_args(["delete", "--slug", "test"])
  |> should.equal(Error("Unknown command: delete"))
}

pub fn parse_args_unknown_command_edit_test() {
  cli.parse_args(["edit", "--slug", "test"])
  |> should.equal(Error("Unknown command: edit"))
}

pub fn parse_args_unknown_command_run_test() {
  cli.parse_args(["run", "--slug", "test"])
  |> should.equal(Error("Unknown command: run"))
}

pub fn parse_args_multiple_slugs_uses_first_test() {
  cli.parse_args(["new", "--slug", "slug1", "--slug", "slug2"])
  |> should.equal(Ok(cli.NewTask("slug1", None, False)))
}

pub fn parse_args_multiple_contracts_uses_first_test() {
  cli.parse_args([
    "new",
    "--slug",
    "test",
    "--contract",
    "contract1",
    "--contract",
    "contract2",
  ])
  |> should.equal(Ok(cli.NewTask("test", Some("contract1"), False)))
}

pub fn parse_args_multiple_stages_uses_first_test() {
  cli.parse_args([
    "stage",
    "--slug",
    "test",
    "--stage",
    "implement",
    "--stage",
    "review",
  ])
  |> should.equal(Ok(cli.RunStage("test", "implement", False, None, None)))
}

pub fn parse_args_new_contract_without_value_test() {
  cli.parse_args(["new", "--slug", "test", "--contract"])
  |> should.equal(Ok(cli.NewTask("test", None, False)))
}

pub fn parse_args_new_interactive_flag_position_test() {
  cli.parse_args(["new", "--interactive", "--slug", "test"])
  |> should.equal(Ok(cli.NewTask("test", None, True)))
}

pub fn parse_args_new_interactive_at_end_test() {
  cli.parse_args(["new", "--slug", "test", "--interactive"])
  |> should.equal(Ok(cli.NewTask("test", None, True)))
}

// Help text comprehensive checks
pub fn help_text_contains_all_commands_test() {
  let text = cli.help_text()
  text
  |> string.contains("new")
  |> should.equal(True)
  text
  |> string.contains("stage")
  |> should.equal(True)
  text
  |> string.contains("approve")
  |> should.equal(True)
  text
  |> string.contains("show")
  |> should.equal(True)
  text
  |> string.contains("list")
  |> should.equal(True)
}

pub fn help_text_contains_flag_descriptions_test() {
  let text = cli.help_text()
  text
  |> string.contains("--slug")
  |> should.equal(True)
  text
  |> string.contains("--stage")
  |> should.equal(True)
  text
  |> string.contains("--strategy")
  |> should.equal(True)
  text
  |> string.contains("--detailed")
  |> should.equal(True)
}

pub fn help_text_contains_short_flags_test() {
  let text = cli.help_text()
  text
  |> string.contains("-s")
  |> should.equal(True)
  text
  |> string.contains("-c")
  |> should.equal(True)
  text
  |> string.contains("-d")
  |> should.equal(True)
  text
  |> string.contains("-f")
  |> should.equal(True)
}

pub fn help_text_contains_examples_test() {
  let text = cli.help_text()
  text
  |> string.contains("factory new")
  |> should.equal(True)
  text
  |> string.contains("factory stage")
  |> should.equal(True)
  text
  |> string.contains("factory approve")
  |> should.equal(True)
  text
  |> string.contains("factory list")
  |> should.equal(True)
}

pub fn help_text_contains_valid_strategies_test() {
  let text = cli.help_text()
  text
  |> string.contains("immediate")
  |> should.equal(True)
  text
  |> string.contains("gradual")
  |> should.equal(True)
  text
  |> string.contains("canary")
  |> should.equal(True)
}

pub fn help_text_contains_valid_priorities_test() {
  let text = cli.help_text()
  text
  |> string.contains("P1")
  |> should.equal(True)
  text
  |> string.contains("P2")
  |> should.equal(True)
  text
  |> string.contains("P3")
  |> should.equal(True)
}

pub fn help_text_contains_valid_statuses_test() {
  let text = cli.help_text()
  text
  |> string.contains("open")
  |> should.equal(True)
  text
  |> string.contains("in_progress")
  |> should.equal(True)
  text
  |> string.contains("done")
  |> should.equal(True)
}

pub fn help_text_has_documentation_reference_test() {
  let text = cli.help_text()
  text
  |> string.contains("ARCHITECTURE.md")
  |> should.equal(True)
}

// Dry-run mode tests
pub fn parse_args_stage_with_dry_run_long_flag_test() {
  cli.parse_args([
    "stage",
    "--slug",
    "test-slug",
    "--stage",
    "implement",
    "--dry-run",
  ])
  |> should.equal(Ok(cli.RunStage("test-slug", "implement", True, None, None)))
}

pub fn parse_args_stage_with_dry_run_short_flag_test() {
  cli.parse_args(["stage", "--slug", "test-slug", "--stage", "implement", "-d"])
  |> should.equal(Ok(cli.RunStage("test-slug", "implement", True, None, None)))
}

pub fn parse_args_stage_dry_run_with_from_to_test() {
  cli.parse_args([
    "stage", "--slug", "test-slug", "--stage", "implement", "--dry-run",
    "--from", "start", "--to", "end",
  ])
  |> should.equal(
    Ok(cli.RunStage("test-slug", "implement", True, Some("start"), Some("end"))),
  )
}
