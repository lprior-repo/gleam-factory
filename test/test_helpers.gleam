import gleam/erlang/process.{type Subject}
import signal_bus

/// Create a signal bus for testing. Must call cleanup_bus when done.
pub fn setup_bus() -> Subject(signal_bus.SignalBusMessage) {
  let assert Ok(bus) = signal_bus.start_link()
  bus
}

/// Clean up a signal bus after testing.
pub fn cleanup_bus(bus: Subject(signal_bus.SignalBusMessage)) -> Nil {
  signal_bus.shutdown(bus)
}

/// Run a test with automatic bus setup and cleanup.
/// Passes the bus to the test_fn function, then cleans up.
pub fn with_bus(test_fn: fn(Subject(signal_bus.SignalBusMessage)) -> a) -> a {
  let bus = setup_bus()
  let result = test_fn(bus)
  cleanup_bus(bus)
  result
}
