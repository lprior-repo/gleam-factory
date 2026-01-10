//// TCR runner - Test && Commit || Revert workflow execution.

import agent_executor
import agent_runners
import audit
import factory_loop
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/string
import shellout
import signal_bus
import signals

pub type TcrConfig {
  TcrConfig(max_iterations: Int, workspace_root: String, use_cli: Bool)
}

pub fn run_tcr_cycle(
  loop: Subject(factory_loop.LoopMessage),
  config: TcrConfig,
) -> Nil {
  let state = factory_loop.get_state(loop)
  case state.phase {
    factory_loop.Implementing -> run_implementing_phase(loop, state, config)
    factory_loop.TcrChecking -> run_tcr_checking_phase(loop, state, config)
    factory_loop.Reviewing -> run_reviewing_phase(loop, state, config)
    factory_loop.Pushing -> run_pushing_phase(loop, state, config)
    factory_loop.Rebasing -> run_rebasing_phase(loop, state, config)
    factory_loop.Completed -> Nil
    factory_loop.Failed -> Nil
  }
}

fn run_implementing_phase(
  loop: Subject(factory_loop.LoopMessage),
  state: factory_loop.FactoryLoopState,
  config: TcrConfig,
) -> Nil {
  io.println(
    "TCR Iteration "
    <> string.inspect(state.iteration)
    <> ": Running implementer",
  )

  let event = case config.use_cli {
    True -> agent_runners.run_implementer_cli(state, "")
    False -> agent_runners.run_implementer(state, "")
  }

  case event {
    factory_loop.TestPassed -> {
      io.println("Tests PASSED - code committed")
      factory_loop.advance(loop, factory_loop.TestPassed)
    }
    factory_loop.TestFailed -> {
      io.println("Tests FAILED - code reverted")
      case state.iteration >= config.max_iterations {
        True -> factory_loop.advance(loop, factory_loop.MaxIterationsReached)
        False -> factory_loop.advance(loop, factory_loop.TestFailed)
      }
    }
    _ -> Nil
  }
}

fn run_tcr_checking_phase(
  loop: Subject(factory_loop.LoopMessage),
  state: factory_loop.FactoryLoopState,
  _config: TcrConfig,
) -> Nil {
  io.println("TCR checking: running test cycle")
  case agent_executor.run_tests_in_worktree(state.workspace_path) {
    Ok(True) -> {
      io.println("TCR check PASSED")
      factory_loop.advance(loop, factory_loop.TestPassed)
    }
    Ok(False) -> {
      io.println("TCR check FAILED")
      factory_loop.advance(loop, factory_loop.TestFailed)
    }
    Error(_) -> {
      io.println("TCR check ERROR")
      factory_loop.advance(loop, factory_loop.TestFailed)
    }
  }
}

fn run_reviewing_phase(
  loop: Subject(factory_loop.LoopMessage),
  state: factory_loop.FactoryLoopState,
  config: TcrConfig,
) -> Nil {
  io.println("Running auditor review")

  let event = case config.use_cli {
    True -> agent_runners.run_auditor_cli(state, "")
    False -> agent_runners.run_auditor(state, "")
  }

  case event {
    factory_loop.TestPassed -> {
      io.println("Review PASSED - advancing to push")
      factory_loop.advance(loop, factory_loop.TestPassed)
    }
    factory_loop.TestFailed -> {
      io.println("Review FAILED")
      factory_loop.advance(loop, factory_loop.TestFailed)
    }
    _ -> Nil
  }
}

fn run_pushing_phase(
  loop: Subject(factory_loop.LoopMessage),
  state: factory_loop.FactoryLoopState,
  _config: TcrConfig,
) -> Nil {
  io.println("Pushing changes to remote")
  case shellout.command("git", ["push"], state.workspace_path, []) {
    Ok(_) -> {
      io.println("Push SUCCESS")
      factory_loop.advance(loop, factory_loop.PushSuccess)
    }
    Error(_) -> {
      io.println("Push CONFLICT - need rebase")
      factory_loop.advance(loop, factory_loop.PushConflict)
    }
  }
}

fn run_rebasing_phase(
  loop: Subject(factory_loop.LoopMessage),
  state: factory_loop.FactoryLoopState,
  _config: TcrConfig,
) -> Nil {
  io.println("Rebasing onto remote")
  case shellout.command("git", ["pull", "--rebase"], state.workspace_path, []) {
    Ok(_) -> {
      io.println("Rebase SUCCESS")
      factory_loop.advance(loop, factory_loop.RebaseSuccess)
    }
    Error(_) -> {
      io.println("Rebase CONFLICT - aborting")
      let _ =
        shellout.command("git", ["rebase", "--abort"], state.workspace_path, [])
      factory_loop.advance(loop, factory_loop.RebaseConflict)
    }
  }
}

pub fn run_full_tcr_loop(
  task_id: String,
  task_spec: String,
  workspace_path: String,
  config: TcrConfig,
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Nil, String) {
  let bead =
    signals.BeadAssigned(
      task_id: signals.task_id(task_id),
      spec: task_spec,
      requirements: [],
      priority: signals.P1,
      assigned_at: signals.timestamp(0),
    )

  case factory_loop.start_link(task_id, bead, workspace_path, bus) {
    Ok(loop) -> {
      let _ =
        audit.log_task_created(config.workspace_root, task_id, "gleam", task_id)
      run_until_completion(loop, config, 0)
      Ok(Nil)
    }
    Error(_) -> Error("Failed to start factory loop")
  }
}

fn run_until_completion(
  loop: Subject(factory_loop.LoopMessage),
  config: TcrConfig,
  cycle_count: Int,
) -> Nil {
  let state = factory_loop.get_state(loop)
  case state.phase {
    factory_loop.Completed -> io.println("TCR loop completed successfully")
    factory_loop.Failed -> io.println("TCR loop failed")
    _ -> {
      run_tcr_cycle(loop, config)
      process.sleep(1000)
      run_until_completion(loop, config, cycle_count + 1)
    }
  }
}
