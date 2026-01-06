//// OTP Actor wrapper - delegates to real gleam_otp actor functions via FFI.
//// This module exists because src/gleam/otp/actor.gleam shadows the stdlib.
//// Uses factory_actor_ffi which forces loading the real module.

import gleam/erlang/process.{type Subject}

// Re-export types
pub type Next(s, m)

pub type Started(data) {
  Started(pid: Pid, data: data)
}

pub type Pid

// FFI to factory_actor_ffi which loads and calls the REAL gleam_otp actor
@external(erlang, "factory_actor_ffi", "actor_new")
pub fn new(state: state) -> builder

@external(erlang, "factory_actor_ffi", "actor_on_message")
pub fn on_message(
  builder: builder,
  handler: fn(state, msg) -> Next(state, msg),
) -> builder

@external(erlang, "factory_actor_ffi", "actor_start")
pub fn start(builder: builder) -> Result(Started(data), error)

@external(erlang, "factory_actor_ffi", "actor_continue")
pub fn continue(state: state) -> Next(state, msg)

@external(erlang, "factory_actor_ffi", "actor_stop")
pub fn stop(reason: reason) -> Next(state, msg)

pub fn send(subject: Subject(msg), msg: msg) -> Nil {
  process.send(subject, msg)
}
