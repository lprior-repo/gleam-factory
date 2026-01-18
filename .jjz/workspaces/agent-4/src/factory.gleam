// Factory - Main entry point
// Wires all modules together for CLI execution

import cli
import error_handling.{type Error, ErrorContext}
import factory_loop
import factory_supervisor
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import logging

pub fn main() {
  case cli.parse() {
    Ok(cmd) ->
      case cli.execute(cmd) {
        Ok(Nil) -> Nil
        Error(err) -> {
          let ctx = ErrorContext("factory", "main", Some(err))
          let error = error_handling.wrap_error_string(err, ctx)
          io.println("Error: " <> format_error(error))
        }
      }
    Error(err) -> {
      let ctx = ErrorContext("factory", "parse", Some(err))
      let error = error_handling.wrap_error_string(err, ctx)
      io.println("Error: " <> format_error(error))
      io.println("")
      io.println(cli.help_text())
    }
  }
}

fn format_error(error: Error) -> String {
  error.reason
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
      test_cmd: test_cmd,
      test_interval_ms: test_interval_ms,
      golden_master_path: golden_master_path,
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

pub type CompletionResult {
  Completed(cycles: Int)
  MaxCyclesReached(cycles: Int)
  LoopFailed(cycles: Int, phase: factory_loop.Phase)
  LoopUnresponsive(cycles: Int)
}

const default_max_cycles = 100

pub fn run_until_completion(
  loop: Subject(factory_loop.LoopMessage),
) -> CompletionResult {
  run_until_completion_with_max(loop, default_max_cycles)
}

pub fn run_until_completion_with_max(
  loop: Subject(factory_loop.LoopMessage),
  max_cycles: Int,
) -> CompletionResult {
  run_loop_cycle(loop, max_cycles, 0)
}

fn run_loop_cycle(
  loop: Subject(factory_loop.LoopMessage),
  max_cycles: Int,
  cycle: Int,
) -> CompletionResult {
  case cycle >= max_cycles {
    True -> {
      logging.log(
        logging.Info,
        "Max cycles reached",
        dict.from_list([#("cycles", int.to_string(cycle))]),
      )
      MaxCyclesReached(cycle)
    }
    False -> {
      case factory_loop.get_state(loop) {
        factory_loop.GetStateTimeout -> {
          logging.log(
            logging.Error,
            "Loop unresponsive",
            dict.from_list([#("cycle", int.to_string(cycle))]),
          )
          LoopUnresponsive(cycle)
        }
        factory_loop.GotState(state) -> {
          case state.phase {
            factory_loop.Completed -> {
              logging.log(
                logging.Info,
                "Loop completed",
                dict.from_list([#("cycles", int.to_string(cycle))]),
              )
              Completed(cycle)
            }
            factory_loop.Failed -> {
              logging.log(
                logging.Error,
                "Loop failed",
                dict.from_list([#("cycle", int.to_string(cycle))]),
              )
              LoopFailed(cycle, state.phase)
            }
            _ -> {
              process.sleep(100)
              run_loop_cycle(loop, max_cycles, cycle + 1)
            }
          }
        }
      }
    }
  }
}
