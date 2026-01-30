import gleam/option
import gleeunit/should
import types

pub fn git_hash_valid_test() {
  let hash = "a1b2c3d4e5f6789012345678901234567890abcd"
  let result = types.git_hash_parse(hash)
  result
  |> should.be_ok()
}

pub fn git_hash_invalid_length_test() {
  let hash = "a1b2c3d4e5f6789012345678901234567890abc"
  let result = types.git_hash_parse(hash)
  result
  |> should.be_error
}

pub fn git_hash_invalid_hex_test() {
  let hash = "A1B2C3D4E5F6789012345678901234567890ABCD"
  let result = types.git_hash_parse(hash)
  result
  |> should.be_error
}

pub fn git_hash_to_string_test() {
  let hash = "a1b2c3d4e5f6789012345678901234567890abcd"
  let result = types.git_hash_parse(hash)
  let parsed = case result {
    Ok(g) -> g
    Error(_) -> panic as "Should not happen"
  }
  types.git_hash_to_string(parsed)
  |> should.equal(hash)
}

pub fn workspace_id_new_test() {
  let id = types.new_workspace_id("test-workspace")
  let _ = id
  True
  |> should.equal(True)
}

pub fn acp_client_new_test() {
  let client = types.new_acp_client("http://localhost:8000")
  types.get_base_url(client)
  |> should.equal("http://localhost:8000")
}

pub fn acp_client_capabilities_none_test() {
  let client = types.AcpClient("http://localhost:8000")
  types.get_capabilities(client)
  |> should.equal(option.None)
}

pub fn acp_client_capabilities_some_test() {
  let client =
    types.AcpClientWithCaps("http://localhost:8000", ["cap1", "cap2"])
  types.get_capabilities(client)
  |> should.equal(option.Some(["cap1", "cap2"]))
}

pub fn acp_session_tracker_new_test() {
  let tracker = types.new_acp_session_tracker()
  let result = types.can_cancel(tracker, "nonexistent")
  result
  |> should.equal(Error("Session not found"))
}

pub fn acp_session_tracker_register_test() {
  let tracker = types.new_acp_session_tracker()
  let updated = types.register_session(tracker, "session-1", types.Running)
  let result = types.can_cancel(updated, "session-1")
  result
  |> should.equal(Ok(True))
}

pub fn acp_session_tracker_complete_test() {
  let tracker = types.new_acp_session_tracker()
  let updated = types.register_session(tracker, "session-1", types.Complete)
  let result = types.can_cancel(updated, "session-1")
  result
  |> should.equal(Ok(False))
}
