import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import otp_actor as actor
import signal_bus

type MergeQueueState {
  MergeQueueState(
    absorbing: Bool,
    signal_bus: Subject(signal_bus.SignalBusMessage),
    subscriber: Option(Subject(signal_bus.Signal)),
  )
}

pub type MergeQueueMessage {
  Shutdown
  GetAbsorbing(reply_with: Subject(Bool))
  HandlePatchProposed
}

pub type MergeQueueError {
  InitFailed
}

pub fn start_link(
  signal_bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(MergeQueueMessage), MergeQueueError) {
  let subscriber = process.new_subject()
  let initial =
    MergeQueueState(
      absorbing: False,
      signal_bus: signal_bus,
      subscriber: Some(subscriber),
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> {
      let _result =
        signal_bus.subscribe(signal_bus, signal_bus.PatchProposed, subscriber)
      Ok(started.data)
    }
    Error(_) -> Error(InitFailed)
  }
}

fn handle_message(
  state: MergeQueueState,
  msg: MergeQueueMessage,
) -> actor.Next(MergeQueueState, MergeQueueMessage) {
  case msg {
    Shutdown -> actor.stop(process.Normal)
    GetAbsorbing(reply) -> {
      process.send(reply, state.absorbing)
      actor.continue(state)
    }
    HandlePatchProposed -> {
      actor.continue(MergeQueueState(..state, absorbing: True))
    }
  }
}
