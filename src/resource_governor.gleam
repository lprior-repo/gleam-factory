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

pub type Ticket {
  MutatorTicket
  LoopTicket
}

pub type SlotType {
  MutatorSlot
  LoopSlot
  WorkspaceSlot
  GpuSlot
}

pub type GovernorMessage {
  AcquireMutator(reply_with: Subject(Result(Ticket, String)))
  AcquireLoop(reply_with: Subject(Result(Ticket, String)))
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
          process.send(reply, Ok(MutatorTicket))
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
          process.send(reply, Ok(LoopTicket))
          actor.continue(State(..state, loops: state.loops + 1))
        }
        False -> {
          process.send(reply, Error("loop limit"))
          actor.continue(state)
        }
      }
    }
    ReleaseMutator -> {
      let new_count = case state.mutators - 1 < 0 {
        True -> 0
        False -> state.mutators - 1
      }
      actor.continue(State(..state, mutators: new_count))
    }
    ReleaseLoop -> {
      let new_count = case state.loops - 1 < 0 {
        True -> 0
        False -> state.loops - 1
      }
      actor.continue(State(..state, loops: new_count))
    }
  }
}

pub fn acquire_mutator(gov: Subject(GovernorMessage)) -> Result(Ticket, String) {
  let reply = process.new_subject()
  process.send(gov, AcquireMutator(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn acquire_loop(gov: Subject(GovernorMessage)) -> Result(Ticket, String) {
  let reply = process.new_subject()
  process.send(gov, AcquireLoop(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn release(gov: Subject(GovernorMessage), ticket: Ticket) -> Nil {
  case ticket {
    MutatorTicket -> process.send(gov, ReleaseMutator)
    LoopTicket -> process.send(gov, ReleaseLoop)
  }
}

pub fn release_slot(gov: Subject(GovernorMessage), slot_type: SlotType) -> Nil {
  case slot_type {
    MutatorSlot -> process.send(gov, ReleaseMutator)
    LoopSlot -> process.send(gov, ReleaseLoop)
    WorkspaceSlot -> Nil
    GpuSlot -> Nil
  }
}

pub fn request_loop_slot(gov: Subject(GovernorMessage)) -> Result(Ticket, String) {
  let reply = process.new_subject()
  process.send(gov, AcquireLoop(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn request_mutator_slot(gov: Subject(GovernorMessage)) -> Result(Ticket, String) {
  let reply = process.new_subject()
  process.send(gov, AcquireMutator(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}
