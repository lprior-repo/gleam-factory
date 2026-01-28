import gleeunit/should
import signals

pub fn hash_unwrap_test() {
  let h = signals.hash("test")
  signals.unwrap_hash(h)
  |> should.equal("test")
}

pub fn task_id_unwrap_test() {
  let tid = signals.task_id("task-123")
  signals.unwrap_task_id(tid)
  |> should.equal("task-123")
}

pub fn loop_id_unwrap_test() {
  let lid = signals.loop_id("loop-456")
  signals.unwrap_loop_id(lid)
  |> should.equal("loop-456")
}

pub fn timestamp_unwrap_test() {
  let ts = signals.timestamp(1_234_567_890)
  signals.unwrap_timestamp(ts)
  |> should.equal(1_234_567_890)
}
