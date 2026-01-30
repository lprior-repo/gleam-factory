//// Signal bus for pub/sub event distribution.
////
//// Provides a simple process-based subscription mechanism.

import gleam/dict
import gleam/erlang/process.{type Subject}
import logging

import signals

const startup_timeout_ms = 5000

const loop_timeout_ms = 100

/// Signal types that can be published/subscribed.
pub type Signal {
  TestFailure
  TestPassing
  BeadAssigned(signals.BeadAssigned)
  BeadRemoved(signals.BeadRemoved)
  PatchProposed
  PatchAccepted(signals.PatchAccepted)
  PatchRejected(reason: String)
  GoldenMasterUpdated
  Evolution
  LoopSpawned
  LoopComplete
  LoopFailed
  TDD15Started
  TDD15PhaseComplete(Int, Bool)
  ResourceExhausted
  ShutdownRequested
}

/// Signal type identifier for subscription purposes.
pub opaque type SignalType {
  SignalType(String)
}

/// Extract signal type identifier from a signal.
fn signal_type(sig: Signal) -> SignalType {
  case sig {
    TestFailure -> SignalType("TestFailure")
    TestPassing -> SignalType("TestPassing")
    BeadAssigned(_) -> SignalType("BeadAssigned")
    BeadRemoved(_) -> SignalType("BeadRemoved")
    PatchProposed -> SignalType("PatchProposed")
    PatchAccepted(_) -> SignalType("PatchAccepted")
    PatchRejected(_) -> SignalType("PatchRejected")
    GoldenMasterUpdated -> SignalType("GoldenMasterUpdated")
    Evolution -> SignalType("Evolution")
    LoopSpawned -> SignalType("LoopSpawned")
    LoopComplete -> SignalType("LoopComplete")
    LoopFailed -> SignalType("LoopFailed")
    TDD15Started -> SignalType("TDD15Started")
    TDD15PhaseComplete(_, _) -> SignalType("TDD15PhaseComplete")
    ResourceExhausted -> SignalType("ResourceExhausted")
    ShutdownRequested -> SignalType("ShutdownRequested")
  }
}

/// Message type for signal bus actor.
pub type SignalBusMessage {
  Subscribe(signal_type: SignalType, subscriber: Subject(Signal))
  Unsubscribe(signal_type: SignalType, subscriber: Subject(Signal))
  Publish(signal: Signal)
  ListSubscriptions(
    reply_with: Subject(dict.Dict(SignalType, List(Subject(Signal)))),
  )
  Shutdown
}

/// Error type for signal bus initialization.
pub type SignalBusError {
  InitFailed
}

/// Signal bus state.
type SignalBusState {
  SignalBusState(subscriptions: dict.Dict(SignalType, List(Subject(Signal))))
}

/// Start the signal bus actor.
pub fn start_link() -> Result(Subject(SignalBusMessage), SignalBusError) {
  let initial_state = SignalBusState(subscriptions: dict.new())
  let parent_subject = process.new_subject()

  let pid =
    process.spawn(fn() {
      let child_subject = process.new_subject()
      process.send(parent_subject, child_subject)
      let selector =
        process.new_selector()
        |> process.select(child_subject)
      bus_loop(initial_state, selector)
    })
  let _ = process.link(pid)

  case process.receive(parent_subject, startup_timeout_ms) {
    Ok(child_subject) -> {
      logging.log(logging.Info, "Signal bus started", dict.new())
      Ok(child_subject)
    }
    Error(Nil) -> {
      logging.log(logging.Error, "Signal bus startup failed", dict.new())
      Error(InitFailed)
    }
  }
}

/// Subscribe to a signal type.
pub fn subscribe(
  bus: Subject(SignalBusMessage),
  signal: Signal,
  subscriber: Subject(Signal),
) -> Result(Nil, Nil) {
  process.send(bus, Subscribe(signal_type: signal_type(signal), subscriber:))
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

/// Shutdown the signal bus actor.
pub fn shutdown(bus: Subject(SignalBusMessage)) -> Nil {
  process.send(bus, Shutdown)
}

fn bus_loop(
  state: SignalBusState,
  selector: process.Selector(SignalBusMessage),
) -> Nil {
  case process.selector_receive(selector, loop_timeout_ms) {
    Ok(Subscribe(sig_type, subscriber)) -> {
      let subs = case dict.get(state.subscriptions, sig_type) {
        Ok(existing) -> [subscriber, ..existing]
        Error(Nil) -> [subscriber]
      }
      let new_subs = dict.insert(state.subscriptions, sig_type, subs)
      bus_loop(SignalBusState(subscriptions: new_subs), selector)
    }
    Ok(Unsubscribe(sig_type, subscriber)) -> {
      let subs = case dict.get(state.subscriptions, sig_type) {
        Ok(existing) -> remove_subscriber(existing, subscriber, [])
        Error(Nil) -> []
      }
      let new_subs = dict.insert(state.subscriptions, sig_type, subs)
      bus_loop(SignalBusState(subscriptions: new_subs), selector)
    }
    Ok(Publish(signal)) -> {
      let sig_type = signal_type(signal)
      case dict.get(state.subscriptions, sig_type) {
        Ok(subscribers) -> notify_all(subscribers, signal)
        Error(Nil) -> Nil
      }
      bus_loop(state, selector)
    }
    Ok(ListSubscriptions(reply_with)) -> {
      process.send(reply_with, state.subscriptions)
      bus_loop(state, selector)
    }
    Ok(Shutdown) -> {
      logging.log(logging.Info, "Signal bus shutting down", dict.new())
      Nil
    }
    Error(Nil) -> {
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
