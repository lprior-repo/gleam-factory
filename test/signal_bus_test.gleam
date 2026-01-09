import gleam/erlang/process
import gleeunit
import gleeunit/should
import signal_bus

pub fn main() {
  gleeunit.main()
}

pub fn start_link_creates_bus_test() {
  signal_bus.start_link()
  |> should.be_ok()
}

pub fn start_link_timeout_test() {
  signal_bus.start_link()
  |> should.be_ok()
}

pub fn publish_without_subscribers_test() {
  let assert Ok(bus) = signal_bus.start_link()
  signal_bus.publish(bus, signal_bus.TestPassing)
  |> should.equal(Nil)
}

pub fn subscribe_and_publish_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, subscriber)

  signal_bus.publish(bus, signal_bus.TestPassing)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> panic as "Expected TestPassing signal"
  }
}

pub fn multiple_subscribers_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let sub1 = process.new_subject()
  let sub2 = process.new_subject()
  let sub3 = process.new_subject()

  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, sub1)
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, sub2)
  let assert Ok(Nil) = signal_bus.subscribe(bus, signal_bus.TestFailure, sub3)

  signal_bus.publish(bus, signal_bus.TestFailure)

  case process.receive(sub1, 1000) {
    Ok(signal_bus.TestFailure) -> Nil
    _ -> panic as "sub1 expected TestFailure"
  }

  case process.receive(sub2, 1000) {
    Ok(signal_bus.TestFailure) -> Nil
    _ -> panic as "sub2 expected TestFailure"
  }

  case process.receive(sub3, 1000) {
    Ok(signal_bus.TestFailure) -> Nil
    _ -> panic as "sub3 expected TestFailure"
  }
}

pub fn different_signal_types_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let sub_passing = process.new_subject()
  let sub_failure = process.new_subject()

  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestPassing, sub_passing)
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.TestFailure, sub_failure)

  signal_bus.publish(bus, signal_bus.TestPassing)

  case process.receive(sub_passing, 1000) {
    Ok(signal_bus.TestPassing) -> Nil
    _ -> panic as "Expected TestPassing"
  }

  case process.receive(sub_failure, 100) {
    Error(Nil) -> Nil
    _ -> panic as "sub_failure should not receive TestPassing"
  }
}

pub fn broadcast_alias_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.Evolution, subscriber)

  signal_bus.broadcast(bus, signal_bus.Evolution)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.Evolution) -> Nil
    _ -> panic as "Expected Evolution signal"
  }
}

pub fn subscribe_returns_ok_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()

  signal_bus.subscribe(bus, signal_bus.LoopSpawned, subscriber)
  |> should.be_ok()
}

pub fn multiple_signals_single_subscriber_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()

  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.PatchProposed, subscriber)
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.PatchAccepted, subscriber)

  signal_bus.publish(bus, signal_bus.PatchProposed)
  signal_bus.publish(bus, signal_bus.PatchAccepted)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.PatchProposed) -> Nil
    _ -> panic as "Expected PatchProposed"
  }

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.PatchAccepted) -> Nil
    _ -> panic as "Expected PatchAccepted"
  }
}

pub fn publish_to_empty_subscriptions_test() {
  let assert Ok(bus) = signal_bus.start_link()

  signal_bus.publish(bus, signal_bus.ResourceExhausted)
  signal_bus.publish(bus, signal_bus.GoldenMasterUpdated)
  signal_bus.publish(bus, signal_bus.LoopComplete)
}

pub fn subscriber_receives_in_order_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let subscriber = process.new_subject()

  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.LoopSpawned, subscriber)

  signal_bus.publish(bus, signal_bus.LoopSpawned)
  signal_bus.publish(bus, signal_bus.LoopSpawned)

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.LoopSpawned) -> Nil
    _ -> panic as "Expected first LoopSpawned"
  }

  case process.receive(subscriber, 1000) {
    Ok(signal_bus.LoopSpawned) -> Nil
    _ -> panic as "Expected second LoopSpawned"
  }
}
