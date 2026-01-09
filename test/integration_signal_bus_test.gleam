//// Integration tests for signal bus and pub/sub system
////
//// Verifies PubSub for signals: PatchAccepted, PatchRejected, TestFailure

import factory_supervisor
import gleam/erlang/process
import gleeunit
import gleeunit/should
import signal_bus

pub fn main() {
  gleeunit.main()
}

/// Test: Signal bus broadcasts TestFailure to multiple subscribers
pub fn signal_bus_broadcast_test_failure() {
  case signal_bus.start_link() {
    Error(_) -> should.fail()
    Ok(bus) -> {
      let sub1 = process.new_subject()
      let sub2 = process.new_subject()

      let _ = signal_bus.subscribe(bus, signal_bus.TestFailure, sub1)
      let _ = signal_bus.subscribe(bus, signal_bus.TestFailure, sub2)

      signal_bus.publish(bus, signal_bus.TestFailure)

      case process.receive(sub1, 1000) {
        Ok(signal_bus.TestFailure) -> Nil
        _ -> should.fail()
      }

      case process.receive(sub2, 1000) {
        Ok(signal_bus.TestFailure) -> Nil
        _ -> should.fail()
      }
    }
  }
}

/// Test: Signal bus broadcasts PatchAccepted to subscribers
pub fn signal_bus_broadcast_patch_accepted() {
  case signal_bus.start_link() {
    Error(_) -> should.fail()
    Ok(bus) -> {
      let subscriber = process.new_subject()
      let _ = signal_bus.subscribe(bus, signal_bus.PatchAccepted, subscriber)

      signal_bus.broadcast(bus, signal_bus.PatchAccepted)

      case process.receive(subscriber, 1000) {
        Ok(signal_bus.PatchAccepted) -> Nil
        _ -> should.fail()
      }
    }
  }
}

/// Test: Signal bus broadcasts PatchRejected to subscribers
pub fn signal_bus_broadcast_patch_rejected() {
  case signal_bus.start_link() {
    Error(_) -> should.fail()
    Ok(bus) -> {
      let subscriber = process.new_subject()
      let _ = signal_bus.subscribe(bus, signal_bus.PatchRejected, subscriber)

      signal_bus.broadcast(bus, signal_bus.PatchRejected)

      case process.receive(subscriber, 1000) {
        Ok(signal_bus.PatchRejected) -> Nil
        _ -> should.fail()
      }
    }
  }
}

/// Test: Supervisor with signal bus isolated from heartbeat
pub fn supervisor_signal_bus_isolated() {
  let config =
    factory_supervisor.SupervisorConfig(
      test_cmd: "true",
      test_interval_ms: 100,
      golden_master_path: "/tmp",
      max_mutators: 2,
      max_loops: 2,
      max_workspaces: 4,
      min_free_ram_mb: 100,
      gpu_tickets: 1,
      beads_path: "/tmp/.beads/issues.jsonl",
      beads_poll_interval_ms: 1000,
    )

  case factory_supervisor.start_link(config) {
    Error(_) -> should.fail()
    Ok(started) -> {
      let bus = factory_supervisor.get_signal_bus(started)
      let subscriber = process.new_subject()

      let _ = signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)

      signal_bus.broadcast(bus, signal_bus.TestFailure)

      case process.receive(subscriber, 1000) {
        Ok(signal_bus.TestFailure) -> Nil
        _ -> should.fail()
      }
    }
  }
}

/// Test: Multiple subscribers receive same signal
pub fn signal_bus_multiple_subscribers() {
  case signal_bus.start_link() {
    Error(_) -> should.fail()
    Ok(bus) -> {
      let sub1 = process.new_subject()
      let sub2 = process.new_subject()
      let sub3 = process.new_subject()

      let _ = signal_bus.subscribe(bus, signal_bus.TestPassing, sub1)
      let _ = signal_bus.subscribe(bus, signal_bus.TestPassing, sub2)
      let _ = signal_bus.subscribe(bus, signal_bus.TestPassing, sub3)

      signal_bus.publish(bus, signal_bus.TestPassing)

      case process.receive(sub1, 500) {
        Ok(signal_bus.TestPassing) -> {
          case process.receive(sub2, 500) {
            Ok(signal_bus.TestPassing) -> {
              case process.receive(sub3, 500) {
                Ok(signal_bus.TestPassing) -> Nil
                _ -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
  }
}

/// Test: Different signal types don't cross-deliver
pub fn signal_bus_type_isolation() {
  case signal_bus.start_link() {
    Error(_) -> should.fail()
    Ok(bus) -> {
      let sub_failure = process.new_subject()
      let sub_passing = process.new_subject()

      let _ = signal_bus.subscribe(bus, signal_bus.TestFailure, sub_failure)
      let _ = signal_bus.subscribe(bus, signal_bus.TestPassing, sub_passing)

      signal_bus.publish(bus, signal_bus.TestPassing)

      case process.receive(sub_failure, 500) {
        Error(Nil) -> Nil
        _ -> should.fail()
      }

      case process.receive(sub_passing, 500) {
        Ok(signal_bus.TestPassing) -> Nil
        _ -> should.fail()
      }
    }
  }
}
