// Factory - Main entry point
// Wires all modules together for CLI execution

import cli
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
