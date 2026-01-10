import gleam/erlang/process
import gleeunit
import gleeunit/should
import merge_queue
import signal_bus

pub fn main() {
  gleeunit.main()
}

pub fn start_link_returns_ok_subject_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let result = merge_queue.start_link(bus)
  result |> should.be_ok()
}

pub fn initial_state_not_absorbing_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let assert Ok(queue) = merge_queue.start_link(bus)

  let absorbing = merge_queue.is_absorbing(queue)
  absorbing |> should.equal(False)
}

pub fn propose_patch_changes_absorbing_to_true_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  let absorbing = merge_queue.is_absorbing(queue)
  absorbing |> should.equal(True)
}

pub fn report_test_result_success_broadcasts_patch_accepted_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let accepted_sub = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.PatchAccepted, accepted_sub)

  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  merge_queue.report_test_result(queue, "hash1", True)
  process.sleep(100)

  case process.receive(accepted_sub, 500) {
    Ok(signal_bus.PatchAccepted) -> Nil
    _ -> panic as "Expected PatchAccepted signal"
  }
}

pub fn report_test_result_failure_broadcasts_patch_rejected_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let rejected_sub = process.new_subject()

  // Subscribe to any PatchRejected signal (with any reason)
  // We need to create a dummy signal for subscription
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.PatchRejected(""), rejected_sub)

  let assert Ok(queue) = merge_queue.start_link(bus)

  merge_queue.propose_patch(queue, "hash1")
  process.sleep(100)

  merge_queue.report_test_result(queue, "hash1", False)
  process.sleep(100)

  case process.receive(rejected_sub, 500) {
    Ok(signal_bus.PatchRejected(reason)) -> {
      should.string_contains(reason, "hash1")
    }
    _ -> panic as "Expected PatchRejected signal"
  }
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
}

pub fn only_matching_patch_hash_accepts_test() {
  let assert Ok(bus) = signal_bus.start_link()
  let accepted_sub = process.new_subject()
  let assert Ok(Nil) =
    signal_bus.subscribe(bus, signal_bus.PatchAccepted, accepted_sub)

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
    Ok(_) -> panic as "Should not have sent PatchAccepted for different hash"
    Error(Nil) -> Nil
  }
}
