import error_messages
import gleam/string
import gleeunit/should

// === InvalidSlug Error Formatting ===

pub fn format_invalid_slug_includes_reason_test() {
  error_messages.format(error_messages.InvalidSlug("contains spaces"))
  |> string.contains("contains spaces")
  |> should.be_true
}

pub fn format_invalid_slug_provides_format_guidance_test() {
  let msg = error_messages.format(error_messages.InvalidSlug("bad"))
  msg |> string.contains("letters, numbers, hyphens") |> should.be_true
}

pub fn format_invalid_slug_shows_examples_test() {
  let msg = error_messages.format(error_messages.InvalidSlug("bad"))
  msg |> string.contains("my-feature-task") |> should.be_true
}

// === RepoNotFound Error Formatting ===

pub fn format_repo_not_found_mentions_project_files_test() {
  let msg = error_messages.format(error_messages.RepoNotFound)
  msg |> string.contains("gleam.toml") |> should.be_true
  msg |> string.contains("go.mod") |> should.be_true
  msg |> string.contains("Cargo.toml") |> should.be_true
}

pub fn format_repo_not_found_suggests_directory_check_test() {
  let msg = error_messages.format(error_messages.RepoNotFound)
  msg |> string.contains("project directory") |> should.be_true
}

// === TaskNotFound Error Formatting ===

pub fn format_task_not_found_includes_slug_test() {
  error_messages.format(error_messages.TaskNotFound("my-task"))
  |> string.contains("my-task")
  |> should.be_true
}

pub fn format_task_not_found_suggests_list_command_test() {
  let msg = error_messages.format(error_messages.TaskNotFound("x"))
  msg |> string.contains("factory list") |> should.be_true
}

// === StageNotFound Error Formatting ===

pub fn format_stage_not_found_includes_stage_name_test() {
  error_messages.format(error_messages.StageNotFound("deploy"))
  |> string.contains("deploy")
  |> should.be_true
}

pub fn format_stage_not_found_lists_available_stages_test() {
  let msg = error_messages.format(error_messages.StageNotFound("x"))
  msg |> string.contains("implement") |> should.be_true
  msg |> string.contains("unit-test") |> should.be_true
  msg |> string.contains("lint") |> should.be_true
}

// === InvalidStage Error Formatting ===

pub fn format_invalid_stage_includes_stage_name_test() {
  error_messages.format(error_messages.InvalidStage("review"))
  |> string.contains("review")
  |> should.be_true
}

pub fn format_invalid_stage_mentions_order_requirement_test() {
  let msg = error_messages.format(error_messages.InvalidStage("x"))
  msg |> string.contains("order") |> should.be_true
}

// === CommandFailed Error Formatting ===

pub fn format_command_failed_includes_command_name_test() {
  error_messages.format(error_messages.CommandFailed("gleam build", 1, ""))
  |> string.contains("gleam build")
  |> should.be_true
}

pub fn format_command_failed_includes_exit_code_test() {
  let msg = error_messages.format(error_messages.CommandFailed("cmd", 42, ""))
  msg |> string.contains("42") |> should.be_true
}

pub fn format_command_failed_includes_stderr_when_present_test() {
  error_messages.format(error_messages.CommandFailed("cmd", 1, "bad input"))
  |> string.contains("bad input")
  |> should.be_true
}

pub fn format_command_failed_suggests_verbose_when_no_stderr_test() {
  let msg = error_messages.format(error_messages.CommandFailed("cmd", 1, ""))
  msg |> string.contains("verbose") |> should.be_true
}

// === PermissionDenied Error Formatting ===

pub fn format_permission_denied_includes_path_test() {
  error_messages.format(error_messages.PermissionDenied("/etc/passwd"))
  |> string.contains("/etc/passwd")
  |> should.be_true
}

pub fn format_permission_denied_suggests_chmod_test() {
  let msg = error_messages.format(error_messages.PermissionDenied("/path"))
  msg |> string.contains("chmod") |> should.be_true
}

// === ResourceExhausted Error Formatting ===

pub fn format_resource_exhausted_includes_resource_name_test() {
  error_messages.format(error_messages.ResourceExhausted("workspaces"))
  |> string.contains("workspaces")
  |> should.be_true
}

// === ConfigError Error Formatting ===

pub fn format_config_error_includes_message_test() {
  error_messages.format(error_messages.ConfigError("invalid toml"))
  |> string.contains("invalid toml")
  |> should.be_true
}

pub fn format_config_error_mentions_config_file_test() {
  let msg = error_messages.format(error_messages.ConfigError("x"))
  msg |> string.contains("factory.toml") |> should.be_true
}

// === Timeout Error Formatting ===

pub fn format_timeout_includes_operation_name_test() {
  error_messages.format(error_messages.Timeout("build"))
  |> string.contains("build")
  |> should.be_true
}

pub fn format_timeout_suggests_retry_test() {
  let msg = error_messages.format(error_messages.Timeout("x"))
  msg |> string.contains("again") |> should.be_true
}

// === Hint Formatting Behavior ===

pub fn format_hint_for_invalid_slug_provides_tip_test() {
  let hint = error_messages.format_hint(error_messages.InvalidSlug("x"))
  hint |> string.contains("Tip") |> should.be_true
}

pub fn format_hint_for_repo_not_found_suggests_project_root_test() {
  let hint = error_messages.format_hint(error_messages.RepoNotFound)
  hint |> string.contains("root") |> should.be_true
}

pub fn format_hint_for_task_not_found_suggests_list_check_test() {
  let hint = error_messages.format_hint(error_messages.TaskNotFound("x"))
  hint |> string.contains("factory list") |> should.be_true
}

pub fn format_hint_for_stage_not_found_suggests_show_command_test() {
  let hint = error_messages.format_hint(error_messages.StageNotFound("x"))
  hint |> string.contains("factory show") |> should.be_true
}

pub fn format_hint_for_command_failed_suggests_tool_check_test() {
  let hint =
    error_messages.format_hint(error_messages.CommandFailed("x", 1, ""))
  hint |> string.contains("tools") |> should.be_true
}

pub fn format_hint_for_permission_denied_suggests_ls_test() {
  let hint = error_messages.format_hint(error_messages.PermissionDenied("x"))
  hint |> string.contains("ls") |> should.be_true
}

pub fn format_hint_for_resource_exhausted_suggests_cleanup_test() {
  let hint = error_messages.format_hint(error_messages.ResourceExhausted("x"))
  hint |> string.contains("cleanup") |> should.be_true
}

pub fn format_hint_for_timeout_mentions_system_load_test() {
  let hint = error_messages.format_hint(error_messages.Timeout("x"))
  hint |> string.contains("overloaded") |> should.be_true
}
