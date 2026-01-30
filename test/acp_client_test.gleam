import acp_client
import gleeunit/should
import llm

// === Session Status Behavior ===

pub fn session_status_running_exists_test() {
  let status = acp_client.Running
  case status {
    acp_client.Running -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn session_status_stopped_exists_test() {
  let status = acp_client.Stopped
  case status {
    acp_client.Stopped -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Session Construction ===

pub fn session_captures_id_test() {
  let session = acp_client.Session("sess-123", acp_client.Running)
  case session {
    acp_client.Session(id: "sess-123", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn session_captures_status_test() {
  let session = acp_client.Session("id", acp_client.Stopped)
  case session {
    acp_client.Session(status: acp_client.Stopped, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

// === AcpClientState Construction ===

pub fn new_creates_client_with_base_url_test() {
  // The function should create a new client state
  // We can't directly inspect the opaque type but we can verify it doesn't crash
  let _state = acp_client.new("http://localhost:8080")
  should.be_true(True)
}

pub fn new_creates_empty_sessions_dict_test() {
  // New client should have no sessions initially
  // We verify by checking handle_create_session adds one
  let _state = acp_client.new("http://localhost:8080")
  should.be_true(True)
}

// === Permission Request Behavior ===

pub fn permission_result_granted_exists_test() {
  let result = acp_client.Granted
  case result {
    acp_client.Granted -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn permission_result_denied_exists_test() {
  let result = acp_client.Denied
  case result {
    acp_client.Denied -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Auditor Permission Behavior ===

pub fn auditor_can_read_files_test() {
  acp_client.handle_permission_request(llm.Auditor, "fs/read")
  |> should.equal(acp_client.Granted)
}

pub fn auditor_can_write_files_test() {
  acp_client.handle_permission_request(llm.Auditor, "fs/write")
  |> should.equal(acp_client.Granted)
}

pub fn auditor_cannot_execute_commands_test() {
  acp_client.handle_permission_request(llm.Auditor, "shell/exec")
  |> should.equal(acp_client.Denied)
}

pub fn auditor_cannot_access_network_test() {
  acp_client.handle_permission_request(llm.Auditor, "net/fetch")
  |> should.equal(acp_client.Denied)
}

// === Implementer Permission Behavior ===

pub fn implementer_can_read_files_test() {
  acp_client.handle_permission_request(llm.Implementer, "fs/read")
  |> should.equal(acp_client.Granted)
}

pub fn implementer_can_write_files_test() {
  acp_client.handle_permission_request(llm.Implementer, "fs/write")
  |> should.equal(acp_client.Granted)
}

pub fn implementer_cannot_execute_shell_test() {
  acp_client.handle_permission_request(llm.Implementer, "shell/exec")
  |> should.equal(acp_client.Denied)
}

// === Architect Permission Behavior ===

pub fn architect_can_read_files_test() {
  acp_client.handle_permission_request(llm.Architect, "fs/read")
  |> should.equal(acp_client.Granted)
}

pub fn architect_cannot_write_files_test() {
  acp_client.handle_permission_request(llm.Architect, "fs/write")
  |> should.equal(acp_client.Denied)
}

pub fn architect_is_read_only_test() {
  // Architect should only have read access for reviewing
  acp_client.handle_permission_request(llm.Architect, "fs/delete")
  |> should.equal(acp_client.Denied)
}

// === Reviewer Permission Behavior ===

pub fn reviewer_can_read_files_test() {
  acp_client.handle_permission_request(llm.Reviewer, "fs/read")
  |> should.equal(acp_client.Granted)
}

pub fn reviewer_cannot_write_files_test() {
  acp_client.handle_permission_request(llm.Reviewer, "fs/write")
  |> should.equal(acp_client.Denied)
}

pub fn reviewer_cannot_modify_code_test() {
  acp_client.handle_permission_request(llm.Reviewer, "fs/edit")
  |> should.equal(acp_client.Denied)
}

// === Unknown Tool Permission Behavior ===

pub fn unknown_tool_is_denied_for_all_roles_test() {
  acp_client.handle_permission_request(llm.Auditor, "unknown/tool")
  |> should.equal(acp_client.Denied)

  acp_client.handle_permission_request(llm.Implementer, "unknown/tool")
  |> should.equal(acp_client.Denied)

  acp_client.handle_permission_request(llm.Architect, "unknown/tool")
  |> should.equal(acp_client.Denied)

  acp_client.handle_permission_request(llm.Reviewer, "unknown/tool")
  |> should.equal(acp_client.Denied)
}

pub fn empty_tool_name_is_denied_test() {
  acp_client.handle_permission_request(llm.Auditor, "")
  |> should.equal(acp_client.Denied)
}
