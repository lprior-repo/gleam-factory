// Factory - Main entry point
// Wires all modules together for CLI execution

import cli
import factory_supervisor
import gleam/io

pub fn main() {
  // Parse CLI and execute
  case cli.parse() {
    Ok(cmd) ->
      case cli.execute(cmd) {
        Ok(Nil) -> Nil
        Error(err) -> {
          io.println("Error: " <> err)
        }
      }
    Error(err) -> {
      io.println("Error: " <> err)
      io.println("")
      io.println(cli.help_text())
    }
  }
}

/// Execute a command using new CLI interface
pub fn execute_command(cmd: cli.Command) -> Result(Nil, String) {
  cli.execute(cmd)
}

/// Start the application supervisor tree
pub fn start_supervisor(
  _test_cmd: String,
  _test_interval_ms: Int,
  _golden_master_path: String,
) -> Result(factory_supervisor.Started, factory_supervisor.InitFailed) {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "gleam test",
      test_interval_ms: 5000,
      golden_master_path: "/tmp/factory-gleam",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 512,
      gpu_tickets: 1,
      beads_path: ".beads/issues.jsonl",
      beads_poll_interval_ms: 2000,
      workspace_root: ".",
    )
  factory_supervisor.start_link(config)
}
