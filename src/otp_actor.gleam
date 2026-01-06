//// OTP Actor wrapper - delegates to real gleam_otp actor functions via FFI.
//// This module exists because src/gleam/otp/actor.gleam shadows the stdlib.

import gleam/erlang/process.{type Subject}

// Re-export types
pub type Next(s, m)

pub type Started(data) {
  Started(pid: Pid, data: data)
}

pub type Pid

// Direct FFI to the REAL gleam_otp actor module in the dependency
@external(erlang, "gleam@otp@actor", "new")
pub fn new(state: state) -> builder

@external(erlang, "gleam@otp@actor", "on_message")
pub fn on_message(
  builder: builder,
  handler: fn(state, msg) -> Next(state, msg),
) -> builder

@external(erlang, "gleam@otp@actor", "start")
pub fn start(builder: builder) -> Result(Started(data), error)

@external(erlang, "gleam@otp@actor", "continue")
pub fn continue(state: state) -> Next(state, msg)

@external(erlang, "gleam@otp@actor", "stop")
pub fn stop(reason: reason) -> Next(state, msg)

pub fn send(subject: Subject(msg), msg: msg) -> Nil {
  process.send(subject, msg)
}
