import gleam/erlang/process.{type Subject}
import otp_actor as actor

type GoldenMasterState {
  GoldenMasterState(path: String)
}

pub type GoldenMasterMessage {
  Shutdown
}

pub fn start_link(path: String) -> Result(Subject(GoldenMasterMessage), Nil) {
  let initial = GoldenMasterState(path: path)
  let builder = actor.new(initial)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}
