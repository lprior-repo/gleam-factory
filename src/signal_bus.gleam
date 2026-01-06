//// Signal bus for pub/sub event distribution.
////
//// Provides a simple process-based subscription mechanism.

import gleam/dict
import gleam/erlang/process.{type Subject}

/// Signal types that can be published/subscribed.
pub type Signal {
  TestFailure
  TestPassing
  BeadAssigned
  PatchProposed
  PatchAccepted
  PatchRejected
  GoldenMasterUpdated
  Evolution
  LoopSpawned
  LoopComplete
  LoopFailed
  ResourceExhausted
}

/// Message type for signal bus actor.
pub type SignalBusMessage {
  Subscribe(signal: Signal, subscriber: Subject(Signal))
  Unsubscribe(signal: Signal, subscriber: Subject(Signal))
  Publish(signal: Signal)
  ListSubscriptions(reply_with: Subject(dict.Dict(Signal, List(Subject(Signal)))))
}

/// Error type for signal bus initialization.
pub type SignalBusError {
  InitFailed
}

/// Signal bus state.
type SignalBusState {
  SignalBusState(subscriptions: dict.Dict(Signal, List(Subject(Signal))))
}

/// Start the signal bus actor.
pub fn start_link() -> Result(Subject(SignalBusMessage), SignalBusError) {
  let initial_state = SignalBusState(subscriptions: dict.new())
  let parent_subject = process.new_subject()

  process.spawn(fn() {
    let child_subject = process.new_subject()
    process.send(parent_subject, child_subject)
    let selector = process.new_selector()
      |> process.select(child_subject)
    bus_loop(initial_state, selector)
  })

  case process.receive(parent_subject, 5000) {
    Ok(child_subject) -> Ok(child_subject)
    Error(Nil) -> Error(InitFailed)
  }
}

/// Subscribe to a signal type.
pub fn subscribe(
  bus: Subject(SignalBusMessage),
  signal: Signal,
  subscriber: Subject(Signal),
) -> Result(Nil, Nil) {
  process.send(bus, Subscribe(signal:, subscriber:))
  Ok(Nil)
}

/// Publish a signal to all subscribers.
pub fn publish(bus: Subject(SignalBusMessage), signal: Signal) -> Nil {
  process.send(bus, Publish(signal))
}

/// Alias for publish - broadcasts signal to all subscribers.
pub fn broadcast(bus: Subject(SignalBusMessage), signal: Signal) -> Nil {
  publish(bus, signal)
}

fn bus_loop(
  state: SignalBusState,
  selector: process.Selector(SignalBusMessage),
) -> Nil {
  case process.selector_receive_forever(selector) {
    Subscribe(signal, subscriber) -> {
      let subs = case dict.get(state.subscriptions, signal) {
        Ok(existing) -> [subscriber, ..existing]
        Error(Nil) -> [subscriber]
      }
      let new_subs = dict.insert(state.subscriptions, signal, subs)
      bus_loop(SignalBusState(subscriptions: new_subs), selector)
    }
    Unsubscribe(signal, subscriber) -> {
      let subs = case dict.get(state.subscriptions, signal) {
        Ok(existing) -> remove_subscriber(existing, subscriber, [])
        Error(Nil) -> []
      }
      let new_subs = dict.insert(state.subscriptions, signal, subs)
      bus_loop(SignalBusState(subscriptions: new_subs), selector)
    }
    Publish(signal) -> {
      case dict.get(state.subscriptions, signal) {
        Ok(subscribers) -> notify_all(subscribers, signal)
        Error(Nil) -> Nil
      }
      bus_loop(state, selector)
    }
    ListSubscriptions(reply_with) -> {
      process.send(reply_with, state.subscriptions)
      bus_loop(state, selector)
    }
  }
}

fn remove_subscriber(
  list: List(Subject(Signal)),
  target: Subject(Signal),
  acc: List(Subject(Signal)),
) -> List(Subject(Signal)) {
  case list {
    [] -> acc
    [head, ..tail] -> {
      case head == target {
        True -> remove_subscriber(tail, target, acc)
        False -> remove_subscriber(tail, target, [head, ..acc])
      }
    }
  }
}

fn notify_all(subscribers: List(Subject(Signal)), signal: Signal) -> Nil {
  case subscribers {
    [] -> Nil
    [sub, ..rest] -> {
      process.send(sub, signal)
      notify_all(rest, signal)
    }
  }
}
