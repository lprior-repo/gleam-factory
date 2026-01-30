import gleam/erlang/process
import gleeunit
import gleeunit/should
import merge_queue
import signal_bus
import signals

pub fn main() {
  gleeunit.main()
}

pub fn start_link_returns_ok_subject_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let result = merge_queue.start_link(bus)
  result |> should.be_ok()
  case result {
    Ok(queue) -> merge_queue.shutdown(queue)
    Error(_) -> Nil
  }
  signal_bus.shutdown(bus)
}

pub fn initial_state_not_absorbing_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let assert Ok(queue) = merge_queue.start_link(bus)

  let absorbing = merge_queue.is_absorbing(queue)
  absorbing |> should.equal(False)

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}

pub fn propose_patch_changes_absorbing_to_true_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  let absorbing = merge_queue.is_absorbing(queue)
  absorbing |> should.equal(True)

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}

pub fn report_test_result_success_broadcasts_patch_accepted_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let accepted_sub = process.new_subject()
  let test_patch =
    signals.PatchAccepted(
      hash: signals.hash("test"),
      merged_at: signals.timestamp(0),
    )
  let assert Ok(Nil) =
    signal_bus.subscribe(
      bus,
      signal_bus.PatchAccepted(test_patch),
      accepted_sub,
    )

  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  merge_queue.report_test_result(queue, "hash1", True)
  process.sleep(100)

  case process.receive(accepted_sub, 500) {
    Ok(signal_bus.PatchAccepted(_)) -> Nil
    _ -> {
      merge_queue.shutdown(queue)
      signal_bus.shutdown(bus)
      panic as "Expected PatchAccepted signal"
    }
  }

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}

pub fn report_test_result_failure_broadcasts_patch_rejected_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let rejected_sub = process.new_subject()

  // Subscribe with the expected rejection reason
  let expected_reason = "Tests failed for patch hash1"
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.PatchRejected(""), rejected_sub)

  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  merge_queue.report_test_result(queue, "hash1", False)
  process.sleep(100)

  case process.receive(rejected_sub, 500) {
    Ok(signal_bus.PatchRejected(reason)) -> {
      reason |> should.equal(expected_reason)
    }
    _ -> {
      merge_queue.shutdown(queue)
      signal_bus.shutdown(bus)
      panic as "Expected PatchRejected signal"
    }
  }

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}

pub fn patch_accepted_changes_absorbing_to_false_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  let absorbing_before = merge_queue.is_absorbing(queue)
  absorbing_before |> should.equal(True)

  merge_queue.report_test_result(queue, "hash1", True)
  process.sleep(100)

  let absorbing_after = merge_queue.is_absorbing(queue)
  absorbing_after |> should.equal(False)

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}

pub fn only_matching_patch_hash_accepts_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let accepted_sub = process.new_subject()
  let test_patch =
    signals.PatchAccepted(
      hash: signals.hash("test"),
      merged_at: signals.timestamp(0),
    )
  let assert Ok(Nil) =
    signal_bus.subscribe(
      bus,
      signal_bus.PatchAccepted(test_patch),
      accepted_sub,
    )

  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  // Report different hash - should not accept
  merge_queue.report_test_result(queue, "hash2", True)
  process.sleep(100)

  let absorbing = merge_queue.is_absorbing(queue)
  absorbing |> should.equal(True)

  // Verify no PatchAccepted was sent
  case process.receive(accepted_sub, 100) {
    Ok(_) -> {
      merge_queue.shutdown(queue)
      signal_bus.shutdown(bus)
      panic as "Should not have sent PatchAccepted for different hash"
    }
    Error(Nil) -> Nil
  }

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}

pub fn first_wins_ignores_subsequent_patches_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let assert Ok(queue) = merge_queue.start_link(bus)

  // First patch wins
  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  let absorbing_after_first = merge_queue.is_absorbing(queue)
  absorbing_after_first |> should.equal(True)

  // Second patch arrives while absorbing - should be ignored
  merge_queue.propose_patch(queue, "hash2")
  process.sleep(100)

  // Should still be absorbing hash1
  let current_patch = merge_queue.get_current_patch(queue)
  current_patch |> should.equal("hash1")

  // Complete first patch
  merge_queue.report_test_result(queue, "hash1", True)
  process.sleep(100)

  let absorbing_after_complete = merge_queue.is_absorbing(queue)
  absorbing_after_complete |> should.equal(False)

  merge_queue.shutdown(queue)
  signal_bus.shutdown(bus)
}
