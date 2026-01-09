//// Resource governor OTP actor for limiting concurrent resource usage.

import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import otp_actor as actor
import simplifile

const timeout_ms = 5000

const kb_per_mb = 1024

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

pub type SlotId {
  SlotId(id: String)
}

pub type GovernorMessage {
  AcquireMutator(reply_with: Subject(Result(#(Ticket, SlotId), String)))
  AcquireLoop(reply_with: Subject(Result(#(Ticket, SlotId), String)))
  ReleaseMutator
  ReleaseLoop
  ReleaseSlot(slot_id: SlotId, reply_with: Subject(Result(Nil, String)))
}

type State {
  State(
    limits: ResourceLimits,
    mutators: Int,
    loops: Int,
    allocated_slots: dict.Dict(String, SlotType),
    slot_counter: Int,
  )
}

pub fn start_link(
  config: ResourceLimits,
) -> Result(Subject(GovernorMessage), Nil) {
  let initial =
    State(
      limits: config,
      mutators: 0,
      loops: 0,
      allocated_slots: dict.new(),
      slot_counter: 0,
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)
  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}

fn handle_message(
  state: State,
  msg: GovernorMessage,
) -> actor.Next(State, GovernorMessage) {
  case msg {
    AcquireMutator(reply) -> handle_acquire_mutator(state, reply)
    AcquireLoop(reply) -> handle_acquire_loop(state, reply)
    ReleaseMutator -> handle_release_mutator(state)
    ReleaseLoop -> handle_release_loop(state)
    ReleaseSlot(slot_id, reply) -> handle_release_slot(state, slot_id, reply)
  }
}

fn handle_acquire_mutator(
  state: State,
  reply: Subject(Result(#(Ticket, SlotId), String)),
) -> actor.Next(State, GovernorMessage) {
  case state.mutators < state.limits.max_mutators {
    True -> {
      let new_counter = state.slot_counter + 1
      let slot_id = SlotId("mutator:" <> int.to_string(new_counter))
      let new_slots =
        dict.insert(state.allocated_slots, slot_id.id, MutatorSlot)
      process.send(reply, Ok(#(MutatorTicket, slot_id)))
      actor.continue(
        State(
          ..state,
          mutators: state.mutators + 1,
          allocated_slots: new_slots,
          slot_counter: new_counter,
        ),
      )
    }
    False -> {
      process.send(reply, Error("mutator limit"))
      actor.continue(state)
    }
  }
}

fn handle_acquire_loop(
  state: State,
  reply: Subject(Result(#(Ticket, SlotId), String)),
) -> actor.Next(State, GovernorMessage) {
  case state.loops < state.limits.max_loops {
    True -> {
      let new_counter = state.slot_counter + 1
      let slot_id = SlotId("loop:" <> int.to_string(new_counter))
      let new_slots = dict.insert(state.allocated_slots, slot_id.id, LoopSlot)
      process.send(reply, Ok(#(LoopTicket, slot_id)))
      actor.continue(
        State(
          ..state,
          loops: state.loops + 1,
          allocated_slots: new_slots,
          slot_counter: new_counter,
        ),
      )
    }
    False -> {
      process.send(reply, Error("loop limit"))
      actor.continue(state)
    }
  }
}

fn handle_release_mutator(state: State) -> actor.Next(State, GovernorMessage) {
  let new_count = case state.mutators - 1 < 0 {
    True -> 0
    False -> state.mutators - 1
  }
  actor.continue(State(..state, mutators: new_count))
}

fn handle_release_loop(state: State) -> actor.Next(State, GovernorMessage) {
  let new_count = case state.loops - 1 < 0 {
    True -> 0
    False -> state.loops - 1
  }
  actor.continue(State(..state, loops: new_count))
}

fn handle_release_slot(
  state: State,
  slot_id: SlotId,
  reply: Subject(Result(Nil, String)),
) -> actor.Next(State, GovernorMessage) {
  let SlotId(id) = slot_id
  case dict.get(state.allocated_slots, id) {
    Ok(slot_type) -> {
      let new_state = update_state_for_slot_type(state, slot_type)
      let final_state =
        State(
          ..new_state,
          allocated_slots: dict.delete(new_state.allocated_slots, id),
        )
      process.send(reply, Ok(Nil))
      actor.continue(final_state)
    }
    Error(Nil) -> {
      process.send(reply, Ok(Nil))
      actor.continue(state)
    }
  }
}

fn update_state_for_slot_type(state: State, slot_type: SlotType) -> State {
  case slot_type {
    MutatorSlot -> {
      let new_mutators = case state.mutators - 1 < 0 {
        True -> 0
        False -> state.mutators - 1
      }
      State(..state, mutators: new_mutators)
    }
    LoopSlot -> {
      let new_loops = case state.loops - 1 < 0 {
        True -> 0
        False -> state.loops - 1
      }
      State(..state, loops: new_loops)
    }
    WorkspaceSlot -> state
    GpuSlot -> state
  }
}

pub fn acquire_mutator(
  gov: Subject(GovernorMessage),
) -> Result(#(Ticket, SlotId), String) {
  let reply = process.new_subject()
  process.send(gov, AcquireMutator(reply_with: reply))
  case process.receive(reply, timeout_ms) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn acquire_loop(
  gov: Subject(GovernorMessage),
) -> Result(#(Ticket, SlotId), String) {
  let reply = process.new_subject()
  process.send(gov, AcquireLoop(reply_with: reply))
  case process.receive(reply, timeout_ms) {
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

pub fn release_slot(
  gov: Subject(GovernorMessage),
  slot_id: SlotId,
) -> Result(Nil, String) {
  let reply = process.new_subject()
  process.send(gov, ReleaseSlot(slot_id: slot_id, reply_with: reply))
  case process.receive(reply, timeout_ms) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn check_free_ram() -> Result(Int, String) {
  use meminfo_content <- result.try(
    simplifile.read("/proc/meminfo")
    |> result.map_error(fn(_) { "Failed to read /proc/meminfo" }),
  )

  case
    meminfo_content
    |> string.split("\n")
    |> list.find_map(parse_meminfo_line)
  {
    Ok(mb) -> Ok(mb)
    Error(Nil) -> Error("MemAvailable not found in /proc/meminfo")
  }
}

fn parse_meminfo_line(line: String) -> Result(Int, Nil) {
  case string.starts_with(line, "MemAvailable:") {
    True -> {
      line
      |> string.replace("MemAvailable:", "")
      |> string.trim
      |> string.split(" ")
      |> list.first
      |> result.try(int.parse)
      |> result.map(fn(kb) { kb / kb_per_mb })
    }
    False -> Error(Nil)
  }
}

pub fn is_sufficient_ram(current_free_mb: Int, min_required_mb: Int) -> Bool {
  current_free_mb >= min_required_mb
}
