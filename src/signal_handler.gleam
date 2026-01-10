//// Signal handler for SIGINT/SIGTERM graceful shutdown.
////
//// Uses Erlang's erl_signal_server to intercept OS signals
//// and trigger graceful shutdown of all actors.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import logging

pub type ShutdownSignal {
  Sigterm
  Sigint
}

pub type SignalHandlerMessage {
  SignalReceived(ShutdownSignal)
}

/// Setup signal handlers that send messages to the given subject
pub fn setup(handler: Subject(SignalHandlerMessage)) -> Result(Nil, Nil) {
  let sigterm = atom.create_from_string("sigterm")
  let callback = fn(signal: Dynamic) {
    let shutdown_signal = case atom.cast_from_dynamic(signal) {
      Ok(sig) ->
        case sig == sigterm {
          True -> Sigterm
          False -> Sigint
        }
      Error(_) -> Sigint
    }
    process.send(handler, SignalReceived(shutdown_signal))
  }
  case setup_signal_handlers_ffi(callback) {
    SignalOk -> {
      logging.log(logging.Info, "Signal handlers installed", dict.new())
      Ok(Nil)
    }
    SignalError -> {
      logging.log(logging.Error, "Failed to install signal handlers", dict.new())
      Error(Nil)
    }
  }
}

/// Remove signal handlers and restore defaults
pub fn teardown() -> Nil {
  remove_signal_handlers_ffi()
  logging.log(logging.Info, "Signal handlers removed", dict.new())
}

@external(erlang, "signal_handler_ffi", "setup_signal_handlers")
fn setup_signal_handlers_ffi(callback: fn(Dynamic) -> Nil) -> SignalResult

@external(erlang, "signal_handler_ffi", "remove_signal_handlers")
fn remove_signal_handlers_ffi() -> Nil

type SignalResult {
  SignalOk
  SignalError
}
