import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import otp_actor as actor
import types

type GoldenMasterState {
  GoldenMasterState(path: String, hash: Option(types.GitHash))
}

pub type GoldenMasterMessage {
  Shutdown
}

pub fn start_link(path: String) -> Result(Subject(GoldenMasterMessage), Nil) {
  let initial = GoldenMasterState(path: path, hash: None)
  let builder = actor.new(initial)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}

pub fn new_with_hash(
  path: String,
  hash: types.GitHash,
) -> Result(Subject(GoldenMasterMessage), Nil) {
  let initial = GoldenMasterState(path: path, hash: Some(hash))
  let builder = actor.new(initial)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}
