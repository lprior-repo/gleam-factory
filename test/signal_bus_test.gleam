import gleam/erlang/process
import gleam/string
import gleeunit
import gleeunit/should
import signal_bus
import signals

pub fn main() {
  gleeunit.main()
}

pub fn start_link_creates_bus_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> signal_bus.shutdown(bus)
  }
}

pub fn start_link_timeout_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> signal_bus.shutdown(bus)
  }
}

pub fn publish_without_subscribers_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      signal_bus.publish(bus, signal_bus.TestPassing)
      |> should.equal(Nil)

      signal_bus.shutdown(bus)
    }
  }
}

pub fn subscribe_and_publish_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let subscriber = process.new_subject()
      case signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber) {
        Error(e) -> {
          signal_bus.shutdown(bus)
          panic as string.inspect(e)
        }
        Ok(Nil) -> {
          signal_bus.publish(bus, signal_bus.TestPassing)

          case process.receive(subscriber, 1000) {
            Ok(signal_bus.TestPassing) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected TestPassing signal"
            }
          }

          signal_bus.shutdown(bus)
        }
      }
    }
  }
}

pub fn multiple_subscribers_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let sub1 = process.new_subject()
      let sub2 = process.new_subject()
      let sub3 = process.new_subject()

      case
        signal_bus.subscribe(bus, signal_bus.TestFailure, sub1),
        signal_bus.subscribe(bus, signal_bus.TestFailure, sub2),
        signal_bus.subscribe(bus, signal_bus.TestFailure, sub3)
      {
        Ok(Nil), Ok(Nil), Ok(Nil) -> {
          signal_bus.publish(bus, signal_bus.TestFailure)

          case process.receive(sub1, 1000) {
            Ok(signal_bus.TestFailure) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "sub1 expected TestFailure"
            }
          }

          case process.receive(sub2, 1000) {
            Ok(signal_bus.TestFailure) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "sub2 expected TestFailure"
            }
          }

          case process.receive(sub3, 1000) {
            Ok(signal_bus.TestFailure) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "sub3 expected TestFailure"
            }
          }

          signal_bus.shutdown(bus)
        }
        _, _, _ -> {
          signal_bus.shutdown(bus)
          panic as "Failed to subscribe"
        }
      }
    }
  }
}

pub fn different_signal_types_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let sub_passing = process.new_subject()
      let sub_failure = process.new_subject()

      case
        signal_bus.subscribe(bus, signal_bus.TestPassing, sub_passing),
        signal_bus.subscribe(bus, signal_bus.TestFailure, sub_failure)
      {
        Ok(Nil), Ok(Nil) -> {
          signal_bus.publish(bus, signal_bus.TestPassing)

          case process.receive(sub_passing, 1000) {
            Ok(signal_bus.TestPassing) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected TestPassing"
            }
          }

          case process.receive(sub_failure, 100) {
            Error(Nil) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "sub_failure should not receive TestPassing"
            }
          }

          signal_bus.shutdown(bus)
        }
        _, _ -> {
          signal_bus.shutdown(bus)
          panic as "Failed to subscribe"
        }
      }
    }
  }
}

pub fn broadcast_alias_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let subscriber = process.new_subject()
      case signal_bus.subscribe(bus, signal_bus.Evolution, subscriber) {
        Error(e) -> {
          signal_bus.shutdown(bus)
          panic as string.inspect(e)
        }
        Ok(Nil) -> {
          signal_bus.broadcast(bus, signal_bus.Evolution)

          case process.receive(subscriber, 1000) {
            Ok(signal_bus.Evolution) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected Evolution signal"
            }
          }

          signal_bus.shutdown(bus)
        }
      }
    }
  }
}

pub fn subscribe_returns_ok_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let subscriber = process.new_subject()

      signal_bus.subscribe(bus, signal_bus.LoopSpawned, subscriber)
      |> should.be_ok()

      signal_bus.shutdown(bus)
    }
  }
}

pub fn multiple_signals_single_subscriber_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let subscriber = process.new_subject()

      let patch =
        signals.PatchAccepted(
          hash: signals.hash("test"),
          merged_at: signals.timestamp(0),
        )

      case
        signal_bus.subscribe(bus, signal_bus.PatchProposed, subscriber),
        signal_bus.subscribe(bus, signal_bus.PatchAccepted(patch), subscriber)
      {
        Ok(Nil), Ok(Nil) -> {
          signal_bus.publish(bus, signal_bus.PatchProposed)
          signal_bus.publish(bus, signal_bus.PatchAccepted(patch))

          case process.receive(subscriber, 1000) {
            Ok(signal_bus.PatchProposed) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected PatchProposed"
            }
          }

          case process.receive(subscriber, 1000) {
            Ok(signal_bus.PatchAccepted(_)) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected PatchAccepted"
            }
          }

          signal_bus.shutdown(bus)
        }
        _, _ -> {
          signal_bus.shutdown(bus)
          panic as "Failed to subscribe"
        }
      }
    }
  }
}

pub fn publish_to_empty_subscriptions_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      signal_bus.publish(bus, signal_bus.ResourceExhausted)
      signal_bus.publish(bus, signal_bus.GoldenMasterUpdated)
      signal_bus.publish(bus, signal_bus.LoopComplete)

      signal_bus.shutdown(bus)
    }
  }
}

pub fn subscriber_receives_in_order_test() {
  case signal_bus.start_link() {
    Error(e) -> panic as string.inspect(e)
    Ok(bus) -> {
      let subscriber = process.new_subject()

      case signal_bus.subscribe(bus, signal_bus.LoopSpawned, subscriber) {
        Error(e) -> {
          signal_bus.shutdown(bus)
          panic as string.inspect(e)
        }
        Ok(Nil) -> {
          signal_bus.publish(bus, signal_bus.LoopSpawned)
          signal_bus.publish(bus, signal_bus.LoopSpawned)

          case process.receive(subscriber, 1000) {
            Ok(signal_bus.LoopSpawned) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected first LoopSpawned"
            }
          }

          case process.receive(subscriber, 1000) {
            Ok(signal_bus.LoopSpawned) -> Nil
            _ -> {
              signal_bus.shutdown(bus)
              panic as "Expected second LoopSpawned"
            }
          }

          signal_bus.shutdown(bus)
        }
      }
    }
  }
}
