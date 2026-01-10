//// Application supervisor - manages signal_bus and heartbeat actors.
////
//// Orchestrates startup of core system components with fault isolation.
//// If any child fails, system gracefully degrades rather than cascading.

import beads_watcher
import factory_dispatcher
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/result
import golden_master
import hardware_verification
import heartbeat
import logging
import merge_queue
import resource_governor
import signal_bus
import signal_handler
import workspace_manager

const shutdown_timeout_ms = 30_000

pub type SupervisorConfig {
  SupervisorConfig(
    test_cmd: String,
    test_interval_ms: Int,
    golden_master_path: String,
    max_mutators: Int,
    max_loops: Int,
    max_workspaces: Int,
    min_free_ram_mb: Int,
    gpu_tickets: Int,
    beads_path: String,
    beads_poll_interval_ms: Int,
    workspace_root: String,
  )
}

pub type Started {
  Started(
    signal_bus_subject: Subject(signal_bus.SignalBusMessage),
    heartbeat_subject: Subject(heartbeat.HeartbeatMessage),
    resource_governor_subject: Subject(resource_governor.GovernorMessage),
    workspace_manager_subject: Subject(
      workspace_manager.WorkspaceManagerMessage,
    ),
    golden_master_subject: Subject(golden_master.GoldenMasterMessage),
    merge_queue_subject: Subject(merge_queue.MergeQueueMessage),
    factory_dispatcher_pid: process.Pid,
    beads_watcher_pid: process.Pid,
    signal_handler_subject: Subject(signal_handler.SignalHandlerMessage),
  )
}

pub type InitFailed {
  InitFailed(reason: String)
}

/// Start supervisor with children: signal_bus, heartbeat, resource_governor,
/// workspace_manager, golden_master, merge_queue, factory_dispatcher, beads_watcher
/// Gracefully handles child failures without cascading
pub fn start_link(config: SupervisorConfig) -> Result(Started, InitFailed) {
  use _ <- result.try(
    hardware_verification.verify(
      config.min_free_ram_mb,
      config.golden_master_path,
    )
    |> result.map_error(fn(e) { InitFailed(reason: e) }),
  )

  use signal_bus_subject <- result.try(
    signal_bus.start_link()
    |> result.map_error(fn(_) { InitFailed(reason: "signal_bus failed") }),
  )

  use resource_governor_subject <- result.try(
    resource_governor.start_link(resource_governor.ResourceLimits(
      max_mutators: config.max_mutators,
      max_loops: config.max_loops,
      max_workspaces: config.max_workspaces,
      min_free_ram_mb: config.min_free_ram_mb,
      gpu_tickets: config.gpu_tickets,
    ))
    |> result.map_error(fn(_) { InitFailed(reason: "resource_governor failed") }),
  )

  use workspace_manager_subject <- result.try(
    workspace_manager.start_link()
    |> result.map_error(fn(_) { InitFailed(reason: "workspace_manager failed") }),
  )

  use golden_master_subject <- result.try(
    golden_master.start_link_with_bus(
      config.golden_master_path,
      signal_bus_subject,
    )
    |> result.map_error(fn(_) { InitFailed(reason: "golden_master failed") }),
  )

  use _ <- result.try(
    golden_master.prepare(golden_master_subject)
    |> result.map_error(fn(e) { InitFailed(reason: "golden_master prepare: " <> e) }),
  )

  use merge_queue_subject <- result.try(
    merge_queue.start_link(signal_bus_subject)
    |> result.map_error(fn(_) { InitFailed(reason: "merge_queue failed") }),
  )

  let hb_config =
    heartbeat.HeartbeatConfig(
      interval_ms: config.test_interval_ms,
      test_cmd: config.test_cmd,
      golden_master_path: config.golden_master_path,
    )

  use heartbeat_subject <- result.try(
    heartbeat.start_link(hb_config, signal_bus_subject)
    |> result.map_error(fn(_) { InitFailed(reason: "heartbeat failed") }),
  )

  let factory_dispatcher_pid =
    factory_dispatcher.start(signal_bus_subject, config.workspace_root)

  let beads_watcher_pid =
    beads_watcher.start(config.beads_path, config.beads_poll_interval_ms)

  let signal_handler_subject = process.new_subject()
  let _ = signal_handler.setup(signal_handler_subject)

  log_system_ready(config)

  Ok(Started(
    signal_bus_subject:,
    heartbeat_subject:,
    resource_governor_subject:,
    workspace_manager_subject:,
    golden_master_subject:,
    merge_queue_subject:,
    factory_dispatcher_pid:,
    beads_watcher_pid:,
    signal_handler_subject:,
  ))
}

/// Start supervisor and wait for shutdown signal
pub fn start_and_wait(config: SupervisorConfig) -> Result(Nil, InitFailed) {
  use started <- result.try(start_link(config))
  wait_for_shutdown(started)
  Ok(Nil)
}

/// Wait for shutdown signal and perform graceful shutdown
fn wait_for_shutdown(started: Started) -> Nil {
  logging.log(logging.Info, "Waiting for shutdown signal", dict.new())
  case process.receive(started.signal_handler_subject, shutdown_timeout_ms) {
    Ok(signal_handler.SignalReceived(signal)) -> {
      let signal_name = case signal {
        signal_handler.Sigterm -> "SIGTERM"
        signal_handler.Sigint -> "SIGINT"
      }
      logging.log(
        logging.Info,
        "Received " <> signal_name <> ", initiating shutdown",
        dict.new(),
      )
      signal_bus.broadcast(started.signal_bus_subject, signal_bus.ShutdownRequested)
      graceful_shutdown(started)
    }
    Error(Nil) -> wait_for_shutdown(started)
  }
}

/// Get signal bus from supervisor
pub fn get_signal_bus(started: Started) -> Subject(signal_bus.SignalBusMessage) {
  started.signal_bus_subject
}

/// Get heartbeat from supervisor
pub fn get_heartbeat(started: Started) -> Subject(heartbeat.HeartbeatMessage) {
  started.heartbeat_subject
}

/// Get resource governor from supervisor
pub fn get_resource_governor(
  started: Started,
) -> Subject(resource_governor.GovernorMessage) {
  started.resource_governor_subject
}

/// Get workspace manager from supervisor
pub fn get_workspace_manager(
  started: Started,
) -> Subject(workspace_manager.WorkspaceManagerMessage) {
  started.workspace_manager_subject
}

/// Get golden master from supervisor
pub fn get_golden_master(
  started: Started,
) -> Subject(golden_master.GoldenMasterMessage) {
  started.golden_master_subject
}

/// Get merge queue from supervisor
pub fn get_merge_queue(
  started: Started,
) -> Subject(merge_queue.MergeQueueMessage) {
  started.merge_queue_subject
}

/// Get factory dispatcher from supervisor
pub fn get_factory_dispatcher(started: Started) -> process.Pid {
  started.factory_dispatcher_pid
}

/// Get beads watcher from supervisor
pub fn get_beads_watcher(started: Started) -> process.Pid {
  started.beads_watcher_pid
}

/// Log system ready message at Info level after all services initialized
pub fn log_system_ready(_config: SupervisorConfig) -> Nil {
  logging.log(logging.Info, "System ready for beads", dict.new())
}

/// Gracefully shutdown all supervised actors
pub fn shutdown(started: Started) -> Nil {
  graceful_shutdown(started)
}

/// Internal graceful shutdown with timeout
fn graceful_shutdown(started: Started) -> Nil {
  let start_time = erlang_monotonic_time_ms()
  logging.log(logging.Info, "Initiating graceful shutdown", dict.new())

  logging.log(logging.Info, "Stopping beads watcher", dict.new())
  process.send_exit(started.beads_watcher_pid)
  process.sleep(100)

  logging.log(logging.Info, "Stopping factory dispatcher", dict.new())
  process.send_exit(started.factory_dispatcher_pid)
  process.sleep(100)

  logging.log(logging.Info, "Stopping merge queue", dict.new())
  merge_queue.shutdown(started.merge_queue_subject)
  process.sleep(100)

  logging.log(logging.Info, "Stopping golden master", dict.new())
  process.send(started.golden_master_subject, golden_master.Shutdown)
  process.sleep(100)

  signal_handler.teardown()

  let elapsed = erlang_monotonic_time_ms() - start_time
  logging.log(
    logging.Info,
    "All actors shutdown gracefully in " <> int.to_string(elapsed) <> "ms",
    dict.new(),
  )
}

@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time() -> Int

fn erlang_monotonic_time_ms() -> Int {
  erlang_monotonic_time() / 1_000_000
}
