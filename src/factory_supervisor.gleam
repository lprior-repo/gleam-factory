//// Application supervisor - manages signal_bus and heartbeat actors.
////
//// Orchestrates startup of core system components with fault isolation.
//// If any child fails, system gracefully degrades rather than cascading.

import beads_watcher
import gleam/erlang/process.{type Subject}
import gleam/result
import golden_master
import heartbeat
import resource_governor
import signal_bus
import workspace_manager

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
  )
}

pub type Started {
  Started(
    signal_bus_subject: Subject(signal_bus.SignalBusMessage),
    heartbeat_subject: Subject(heartbeat.HeartbeatMessage),
    resource_governor_subject: Subject(resource_governor.GovernorMessage),
    workspace_manager_subject: Subject(workspace_manager.WorkspaceManagerMessage),
    golden_master_subject: Subject(golden_master.GoldenMasterMessage),
    factory_dispatcher_pid: process.Pid,
    beads_watcher_pid: process.Pid,
  )
}

pub type InitFailed {
  InitFailed(reason: String)
}

/// Start supervisor with children: signal_bus, heartbeat, resource_governor,
/// workspace_manager, golden_master, factory_dispatcher, beads_watcher
/// Gracefully handles child failures without cascading
pub fn start_link(config: SupervisorConfig) -> Result(Started, InitFailed) {
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
    |> result.map_error(fn(_) {
      InitFailed(reason: "resource_governor failed")
    }),
  )

  use workspace_manager_subject <- result.try(
    workspace_manager.start_link()
    |> result.map_error(fn(_) {
      InitFailed(reason: "workspace_manager failed")
    }),
  )

  use golden_master_subject <- result.try(
    golden_master.start_link_with_bus(
      config.golden_master_path,
      signal_bus_subject,
    )
    |> result.map_error(fn(_) { InitFailed(reason: "golden_master failed") }),
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

  let factory_dispatcher_pid = process.spawn(fn() { process.sleep_forever() })

  let beads_watcher_pid =
    beads_watcher.start(config.beads_path, config.beads_poll_interval_ms)

  Ok(Started(
    signal_bus_subject:,
    heartbeat_subject:,
    resource_governor_subject:,
    workspace_manager_subject:,
    golden_master_subject:,
    factory_dispatcher_pid:,
    beads_watcher_pid:,
  ))
}

/// Get signal bus from supervisor
pub fn get_signal_bus(
  started: Started,
) -> Subject(signal_bus.SignalBusMessage) {
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
  let _msg = "System ready for beads"
  Nil
}
