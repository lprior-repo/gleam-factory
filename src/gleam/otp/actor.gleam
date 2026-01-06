//// Actor module wrapper that re-exports from otp_actor.
//// This module exists at gleam/otp/actor path for import compatibility.

import gleam/erlang/process.{type Subject}
import otp_actor

// Re-export types from otp_actor
pub type Next(s, m) =
  otp_actor.Next(s, m)

pub type Started(data) =
  otp_actor.Started(data)

pub type Pid =
  otp_actor.Pid

// Re-export functions from otp_actor
pub fn new(state: state) {
  otp_actor.new(state)
}

pub fn on_message(builder, handler) {
  otp_actor.on_message(builder, handler)
}

pub fn start(builder) {
  otp_actor.start(builder)
}

pub fn send(subject: Subject(msg), msg: msg) {
  otp_actor.send(subject, msg)
}

pub fn continue(state: state) -> Next(state, msg) {
  otp_actor.continue(state)
}

pub fn stop(reason: reason) -> Next(state, msg) {
  otp_actor.stop(reason)
}
