//// Resource governor OTP actor for limiting concurrent resource usage.

import gleam/erlang/process.{type Subject}
import otp_actor as actor

pub type ResourceLimits {
  ResourceLimits(
    max_mutators: Int,
    max_loops: Int,
    max_workspaces: Int,
    min_free_ram_mb: Int,
    gpu_tickets: Int,
  )
}

pub type GovernorMessage {
  AcquireMutator(reply_with: Subject(Result(Nil, String)))
  AcquireLoop(reply_with: Subject(Result(Nil, String)))
  ReleaseMutator
  ReleaseLoop
}

type State {
  State(limits: ResourceLimits, mutators: Int, loops: Int)
}

pub fn start_link(
  config: ResourceLimits,
) -> Result(Subject(GovernorMessage), Nil) {
  let initial = State(limits: config, mutators: 0, loops: 0)
  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}

fn handle_message(state: State, msg: GovernorMessage) -> actor.Next(State, GovernorMessage) {
  case msg {
    AcquireMutator(reply) -> {
      case state.mutators < state.limits.max_mutators {
        True -> {
          process.send(reply, Ok(Nil))
          actor.continue(State(..state, mutators: state.mutators + 1))
        }
        False -> {
          process.send(reply, Error("mutator limit"))
          actor.continue(state)
        }
      }
    }
    AcquireLoop(reply) -> {
      case state.loops < state.limits.max_loops {
        True -> {
          process.send(reply, Ok(Nil))
          actor.continue(State(..state, loops: state.loops + 1))
        }
        False -> {
          process.send(reply, Error("loop limit"))
          actor.continue(state)
        }
      }
    }
    ReleaseMutator -> actor.continue(State(..state, mutators: state.mutators - 1))
    ReleaseLoop -> actor.continue(State(..state, loops: state.loops - 1))
  }
}

pub fn acquire_mutator(gov: Subject(GovernorMessage)) -> Result(Nil, String) {
  let reply = process.new_subject()
  process.send(gov, AcquireMutator(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn acquire_loop(gov: Subject(GovernorMessage)) -> Result(Nil, String) {
  let reply = process.new_subject()
  process.send(gov, AcquireLoop(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn release(gov: Subject(GovernorMessage), _ticket: Nil) -> Nil {
  process.send(gov, ReleaseMutator)
}
