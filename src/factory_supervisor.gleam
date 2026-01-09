//// Application supervisor - manages signal_bus and heartbeat actors.
////
//// Orchestrates startup of core system components with fault isolation.
//// If any child fails, system gracefully degrades rather than cascading.

import gleam/erlang/process.{type Subject}
import gleam/result
import heartbeat
import signal_bus

pub type SupervisorConfig {
  SupervisorConfig(
    test_cmd: String,
    test_interval_ms: Int,
    golden_master_path: String,
  )
}

pub type Started {
  Started(
    signal_bus_subject: Subject(signal_bus.SignalBusMessage),
    heartbeat_subject: Subject(heartbeat.HeartbeatMessage),
  )
}

pub type InitFailed {
  InitFailed(reason: String)
}

/// Start supervisor with children: signal_bus, heartbeat
/// Gracefully handles child failures without cascading
pub fn start_link(config: SupervisorConfig) -> Result(Started, InitFailed) {
  use signal_bus_subject <- result.try(
    signal_bus.start_link()
    |> result.map_error(fn(_) { InitFailed(reason: "signal_bus failed") }),
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

  Ok(Started(
    signal_bus_subject:,
    heartbeat_subject:,
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
