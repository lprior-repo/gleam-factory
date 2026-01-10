import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import otp_actor as actor
import signal_bus

type MergeQueueState {
  MergeQueueState(
    absorbing: Bool,
    signal_bus: Subject(signal_bus.SignalBusMessage),
    subscriber: Option(Subject(signal_bus.Signal)),
    current_patch_hash: Option(String),
  )
}

pub type MergeQueueMessage {
  Shutdown
  GetAbsorbing(reply_with: Subject(Bool))
  HandlePatchProposed(hash: String)
  PatchTestResult(hash: String, passed: Bool)
}

pub type MergeQueueError {
  InitFailed
}

pub fn start_link(
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(MergeQueueMessage), MergeQueueError) {
  let subscriber = process.new_subject()
  let initial =
    MergeQueueState(
      absorbing: False,
      signal_bus: bus,
      subscriber: Some(subscriber),
      current_patch_hash: None,
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> {
      let _result =
        signal_bus.subscribe(bus, signal_bus.PatchProposed, subscriber)
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
    HandlePatchProposed(hash) -> {
      case state.absorbing {
        False -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.PatchProposed)
          actor.continue(
            MergeQueueState(
              ..state,
              absorbing: True,
              current_patch_hash: Some(hash),
            ),
          )
        }
        True -> {
          actor.continue(state)
        }
      }
    }
    PatchTestResult(hash, passed) -> {
      case passed, state.current_patch_hash {
        True, Some(current) if hash == current -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.PatchAccepted)
          actor.continue(
            MergeQueueState(
              ..state,
              absorbing: False,
              current_patch_hash: None,
            ),
          )
        }
        False, Some(current) if hash == current -> {
          signal_bus.broadcast(state.signal_bus, signal_bus.PatchRejected)
          actor.continue(
            MergeQueueState(
              ..state,
              absorbing: False,
              current_patch_hash: None,
            ),
          )
        }
        _, _ -> actor.continue(state)
      }
    }
  }
}

pub fn is_absorbing(queue: Subject(MergeQueueMessage)) -> Bool {
  let reply = process.new_subject()
  process.send(queue, GetAbsorbing(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(absorbing) -> absorbing
    Error(Nil) -> False
  }
}

pub fn propose_patch(queue: Subject(MergeQueueMessage), hash: String) -> Nil {
  process.send(queue, HandlePatchProposed(hash))
}

pub fn report_test_result(
  queue: Subject(MergeQueueMessage),
  patch_hash: String,
  passed: Bool,
) -> Nil {
  process.send(queue, PatchTestResult(patch_hash, passed))
}

pub fn shutdown(queue: Subject(MergeQueueMessage)) -> Nil {
  process.send(queue, Shutdown)
}
