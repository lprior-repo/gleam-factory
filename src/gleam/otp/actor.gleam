//// Actor module wrapper that shadows the standard library
//// and provides call with reordered parameters.

import gleam/erlang/process

// Type aliases
pub type Subject(msg) = process.Subject(msg)

// Next type represents actor continuation
pub type Next(s, m)

// Started type - represents the process and its subject
pub type Started(msg) {
  Started(pid: Pid, subject: Subject(msg))
}

// Pid type (opaque)
pub opaque type Pid

// Direct FFI calls to the Erlang/Gleam OTP actor module
@external(erlang, "gleam@@otp@@actor", "new")
fn gleam_new(state: a) -> b

@external(erlang, "gleam@@otp@@actor", "on_message")
fn gleam_on_message(builder: a, handler: fn(b, c) -> d) -> e

@external(erlang, "gleam@@otp@@actor", "start")
fn gleam_start(builder: a) -> Result(Started(b), c)

@external(erlang, "gleam@@process", "send")
fn gleam_send(subject: a, msg: b) -> Nil

@external(erlang, "gleam@@otp@@actor", "continue")
fn gleam_continue(state: a) -> b

@external(erlang, "gleam@@otp@@actor", "stop")
fn gleam_stop(reason: a) -> b

// Public API that wraps the FFI calls
// Note: We use raw module imports to avoid cycles when implementing call

pub fn new(state: state) {
  gleam_new(state)
}

pub fn on_message(builder, handler) {
  gleam_on_message(builder, handler)
}

pub fn start(builder) {
  gleam_start(builder)
}

pub fn send(subject: Subject(msg), msg: msg) {
  gleam_send(subject, msg)
}

pub fn continue(state: state) -> Next(state, msg) {
  gleam_continue(state)
}

pub fn stop(reason: reason) -> Next(state, msg) {
  gleam_stop(reason)
}

/// Call function with reordered parameters.
/// Takes (subject, callback, timeout) instead of (subject, timeout, callback).
pub fn call(
  subject: Subject(msg),
  callback: fn(Subject(reply)) -> msg,
  timeout: Int,
) -> reply {
  // Call the Erlang process.call/3 with reordered parameters
  call_reordered(subject, timeout, callback)
}

@external(erlang, "gleam@erlang@process", "call")
fn call_reordered(subject: a, timeout: Int, callback: fn(b) -> c) -> d
