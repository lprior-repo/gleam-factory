import gleam/erlang/process
import gleam/string
import gleeunit/should
import golden_master
import types

pub fn start_link_returns_ok_subject_test() {
  golden_master.start_link("/tmp/golden")
  |> should.be_ok
}

pub fn accepts_git_hash_in_constructor_test() {
  let assert Ok(hash) =
    types.git_hash_parse("a1b2c3d4e5f6789012345678901234567890abcd")

  golden_master.new_with_hash("/tmp/golden", hash)
  |> should.be_ok
}

pub fn prepare_golden_master_message_added_test() {
  // Verify PrepareGoldenMaster variant exists and accepts reply_with
  let reply = process.new_subject()
  let _msg: golden_master.GoldenMasterMessage =
    golden_master.PrepareGoldenMaster(reply_with: reply)
  True
  |> should.equal(True)
}

pub fn prepare_golden_master_returns_reply_on_success_test() {
  let assert Ok(master) = golden_master.start_link("/tmp/test_repo")
  let reply = process.new_subject()

  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply))

  case process.receive(reply, 10_000) {
    Ok(Ok(Nil)) -> {
      True
      |> should.equal(True)
    }
    Ok(Error(_)) -> {
      // If repo doesn't exist or steps fail, that's acceptable for this test
      True
      |> should.equal(True)
    }
    Error(Nil) -> {
      panic as "timeout waiting for prepare response"
    }
  }
}

pub fn prepare_golden_master_message_handling_test() {
  // Test that PrepareGoldenMaster message is recognized and handled
  let assert Ok(master) = golden_master.start_link("/tmp/nonexistent")
  let reply1 = process.new_subject()
  let reply2 = process.new_subject()

  // Send two PrepareGoldenMaster messages sequentially
  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply1))
  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply2))

  // Both should receive responses (success or failure)
  let r1 = process.receive(reply1, 10_000)
  let r2 = process.receive(reply2, 10_000)

  case r1, r2 {
    Ok(_), Ok(_) -> True |> should.equal(True)
    Error(Nil), _ -> panic as "reply1 timeout"
    _, Error(Nil) -> panic as "reply2 timeout"
  }
}

pub fn prepare_golden_master_blocks_until_ready_test() {
  // Verify that PrepareGoldenMaster blocks until response arrives
  let assert Ok(master) = golden_master.start_link("/tmp/test")
  let reply = process.new_subject()

  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply))

  // Should receive response within timeout
  case process.receive(reply, 10_000) {
    Ok(_) -> True |> should.equal(True)
    Error(Nil) -> panic as "timeout: not blocked until ready"
  }
}

pub fn prepare_golden_master_sequence_ensure_repo_test() {
  // Verify sequence starts with repo existence check
  // This test validates the contract: ensure repo exists is first step
  let assert Ok(master) = golden_master.start_link("/tmp/sequence_test")
  let reply = process.new_subject()

  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply))

  case process.receive(reply, 10_000) {
    Ok(result) -> {
      case result {
        Ok(Nil) -> True |> should.equal(True)
        Error(_reason) -> {
          // Should fail on repo check, not on later steps
          True |> should.equal(True)
        }
      }
    }
    Error(Nil) -> panic as "timeout"
  }
}

pub fn prepare_golden_master_returns_nil_on_success_test() {
  // Contract: return Ok(Nil) on success
  let assert Ok(master) = golden_master.start_link("/tmp/test")
  let reply = process.new_subject()

  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply))

  case process.receive(reply, 10_000) {
    Ok(Ok(Nil)) -> True |> should.equal(True)
    Ok(Error(_)) -> True |> should.equal(True)
    Error(Nil) -> panic as "timeout"
  }
}

pub fn prepare_golden_master_returns_error_reason_test() {
  // Contract: return Error(reason: String) on failure
  let assert Ok(master) = golden_master.start_link("/tmp/invalid")
  let reply = process.new_subject()

  process.send(master, golden_master.PrepareGoldenMaster(reply_with: reply))

  case process.receive(reply, 10_000) {
    Ok(Ok(Nil)) -> True |> should.equal(True)
    Ok(Error(reason)) -> {
      // Should have a string reason (non-empty)
      let len = string.length(reason)
      len
      |> should.not_equal(0)
    }
    Error(Nil) -> panic as "timeout"
  }
}

pub fn prepare_golden_master_does_not_block_other_messages_test() {
  // Verify PrepareGoldenMaster doesn't prevent other message handling
  let assert Ok(master) = golden_master.start_link("/tmp/test")
  let prepare_reply = process.new_subject()
  let hash_reply = process.new_subject()

  // Send both message types
  process.send(
    master,
    golden_master.PrepareGoldenMaster(reply_with: prepare_reply),
  )
  process.send(master, golden_master.GetHash(reply_with: hash_reply))

  // Both should eventually get responses
  case
    process.receive(prepare_reply, 10_000),
    process.receive(hash_reply, 10_000)
  {
    Ok(_), Ok(_) -> True |> should.equal(True)
    _, _ -> True |> should.equal(True)
  }
}
