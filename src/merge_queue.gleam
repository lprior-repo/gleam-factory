import dict
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import logging
import otp_actor as actor
import signal_bus

type MergeQueueState {
  MergeQueueState(
    absorbing: Bool,
    signal_bus: Subject(signal_bus.SignalBusMessage),
    current_patch_hash: Option(String),
  )
}

pub type MergeQueueMessage {
  Shutdown
  GetAbsorbing(reply_with: Subject(Bool))
  GetCurrentPatch(reply_with: Subject(String))
  HandlePatchProposed(hash: String)
  PatchTestResult(hash: String, passed: Bool)
}

pub type MergeQueueError {
  InitFailed
}

pub fn start_link(
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(MergeQueueMessage), MergeQueueError) {
  let initial =
    MergeQueueState(
      absorbing: False,
      signal_bus: bus,
      current_patch_hash: None,
    )
  let builder = actor.new(initial) |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> {
      logging.log(logging.Info, "Merge queue started", dict.new())
      Ok(started.data)
    }
    Error(_) -> {
      logging.log(logging.Error, "Merge queue startup failed", dict.new())
      Error(InitFailed)
    }
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
    GetCurrentPatch(reply) -> {
      let patch = case state.current_patch_hash {
        Some(hash) -> hash
        None -> ""
      }
      process.send(reply, patch)
      actor.continue(state)
    }
    HandlePatchProposed(hash) -> {
      case state.absorbing {
        False -> {
          logging.log(
            logging.Info,
            "Patch proposed: " <> hash,
            dict.from_list([#("absorbing", "true")]),
          )
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
          logging.log(
            logging.Info,
            "Patch rejected (already absorbing): " <> hash,
            dict.new(),
          )
          actor.continue(state)
        }
      }
    }
    PatchTestResult(hash, passed) -> {
      case passed, state.current_patch_hash {
        True, Some(current) if hash == current -> {
          logging.log(
            logging.Info,
            "Patch accepted: " <> hash,
            dict.from_list([#("status", "passed")]),
          )
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
          let reason = "Tests failed for patch " <> hash
          logging.log(
            logging.Error,
            "Patch rejected: " <> reason,
            dict.from_list([#("status", "failed")]),
          )
          signal_bus.broadcast(state.signal_bus, signal_bus.PatchRejected(reason:))
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

pub fn get_current_patch(queue: Subject(MergeQueueMessage)) -> String {
  let reply = process.new_subject()
  process.send(queue, GetCurrentPatch(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(patch) -> patch
    Error(Nil) -> ""
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
