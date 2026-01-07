import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import otp_actor as actor
import process as shell_process
import signal_bus
import types

type GoldenMasterState {
  GoldenMasterState(
    path: String,
    hash: Option(types.GitHash),
    signal_bus: Option(Subject(signal_bus.SignalBusMessage)),
  )
}

pub type GoldenMasterMessage {
  Shutdown
  GetHash(reply_with: Subject(Result(types.GitHash, String)))
  Refresh(reply_with: Subject(Result(Nil, String)))
}

pub fn start_link(path: String) -> Result(Subject(GoldenMasterMessage), Nil) {
  let initial = GoldenMasterState(path: path, hash: None, signal_bus: None)
  let builder = actor.new(initial) |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}

pub fn start_link_with_bus(
  path: String,
  bus: Subject(signal_bus.SignalBusMessage),
) -> Result(Subject(GoldenMasterMessage), Nil) {
  let initial = GoldenMasterState(path: path, hash: None, signal_bus: Some(bus))
  let builder = actor.new(initial) |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}

pub fn new_with_hash(
  path: String,
  hash: types.GitHash,
) -> Result(Subject(GoldenMasterMessage), Nil) {
  let initial =
    GoldenMasterState(path: path, hash: Some(hash), signal_bus: None)
  let builder = actor.new(initial) |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> Ok(started.data)
    Error(_) -> Error(Nil)
  }
}

fn handle_message(
  state: GoldenMasterState,
  msg: GoldenMasterMessage,
) -> actor.Next(GoldenMasterState, GoldenMasterMessage) {
  case msg {
    Shutdown -> actor.stop(process.Normal)
    GetHash(reply) -> {
      let result = get_hash_from_jj(state.path)
      process.send(reply, result)
      actor.continue(state)
    }
    Refresh(reply) -> {
      case do_refresh(state) {
        Ok(new_state) -> {
          process.send(reply, Ok(Nil))
          actor.continue(new_state)
        }
        Error(e) -> {
          process.send(reply, Error(e))
          actor.continue(state)
        }
      }
    }
  }
}

fn get_hash_from_jj(path: String) -> Result(types.GitHash, String) {
  case shell_process.run_command("jj", ["log", "-r", "@", "-T", "commit_id"], path)
  {
    Ok(shell_process.Success(stdout, _, _)) -> {
      let hash = string.trim(stdout)
      types.git_hash_parse(hash)
    }
    Ok(shell_process.Failure(err, _)) -> Error("jj log failed: " <> err)
    Error(e) -> Error(e)
  }
}

fn do_refresh(
  state: GoldenMasterState,
) -> Result(GoldenMasterState, String) {
  let old_hash = state.hash

  use _ <- result.try(
    shell_process.run_command("jj", ["git", "fetch"], state.path)
    |> result.map_error(fn(_) { "fetch failed" })
    |> result.try(fn(r) {
      case r {
        shell_process.Success(_, _, _) -> Ok(Nil)
        shell_process.Failure(e, _) -> Error("fetch failed: " <> e)
      }
    }),
  )

  use _ <- result.try(
    shell_process.run_command("gleam", ["build"], state.path)
    |> result.map_error(fn(_) { "build failed" })
    |> result.try(fn(r) {
      case r {
        shell_process.Success(_, _, _) -> Ok(Nil)
        shell_process.Failure(e, _) -> Error("build failed: " <> e)
      }
    }),
  )

  use new_hash <- result.try(get_hash_from_jj(state.path))

  let hash_changed = case old_hash {
    Some(old) -> types.git_hash_to_string(old) != types.git_hash_to_string(new_hash)
    None -> True
  }

  case hash_changed, state.signal_bus {
    True, Some(bus) -> {
      signal_bus.broadcast(bus, signal_bus.GoldenMasterUpdated)
      Ok(GoldenMasterState(..state, hash: Some(new_hash)))
    }
    _, _ -> Ok(GoldenMasterState(..state, hash: Some(new_hash)))
  }
}

pub fn get_current_hash(
  master: Subject(GoldenMasterMessage),
) -> Result(types.GitHash, String) {
  let reply = process.new_subject()
  process.send(master, GetHash(reply_with: reply))
  case process.receive(reply, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}

pub fn refresh(master: Subject(GoldenMasterMessage)) -> Result(Nil, String) {
  let reply = process.new_subject()
  process.send(master, Refresh(reply_with: reply))
  case process.receive(reply, 30_000) {
    Ok(result) -> result
    Error(Nil) -> Error("timeout")
  }
}
