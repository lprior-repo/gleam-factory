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
  test_cmd: String,
  test_interval_ms: Int,
  golden_master_path: String,
) -> Result(factory_supervisor.Started, factory_supervisor.InitFailed) {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd:,
      test_interval_ms:,
      golden_master_path:,
    )
  factory_supervisor.start_link(config)
}
