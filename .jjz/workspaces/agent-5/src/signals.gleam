pub opaque type Hash {
  Hash(String)
}

pub opaque type TaskId {
  TaskId(String)
}

pub opaque type LoopId {
  LoopId(String)
}

pub type Priority {
  P0
  P1
  P2
  P3
  P4
}

pub opaque type Timestamp {
  Timestamp(Int)
}

pub fn hash(s: String) -> Hash {
  Hash(s)
}

pub fn unwrap_hash(h: Hash) -> String {
  let Hash(s) = h
  s
}

pub fn task_id(s: String) -> TaskId {
  TaskId(s)
}

pub fn unwrap_task_id(t: TaskId) -> String {
  let TaskId(s) = t
  s
}

pub fn loop_id(s: String) -> LoopId {
  LoopId(s)
}

pub fn unwrap_loop_id(l: LoopId) -> String {
  let LoopId(s) = l
  s
}

pub fn timestamp(i: Int) -> Timestamp {
  Timestamp(i)
}

pub fn unwrap_timestamp(t: Timestamp) -> Int {
  let Timestamp(i) = t
  i
}

pub type TestFailure {
  TestFailure(
    file: String,
    error: String,
    context_hash: Hash,
    timestamp: Timestamp,
  )
}

pub type TestPassing {
  TestPassing(hash: Hash, timestamp: Timestamp)
}

pub type BeadAssigned {
  BeadAssigned(
    task_id: TaskId,
    spec: String,
    requirements: List(String),
    priority: Priority,
    assigned_at: Timestamp,
  )
}

pub type PatchProposed {
  PatchProposed(diff: String, author_pid: String, workspace: String, hash: Hash)
}

pub type PatchAccepted {
  PatchAccepted(hash: Hash, merged_at: Timestamp)
}

pub type PatchRejected {
  PatchRejected(reason: String)
}

pub type GoldenMasterUpdated {
  GoldenMasterUpdated(old_hash: Hash, new_hash: Hash)
}

pub type Evolution {
  Evolution(new_hash: Hash, cause: String)
}

pub type LoopSpawned {
  LoopSpawned(loop_id: LoopId, task_id: TaskId, phase: String)
}

pub type LoopComplete {
  LoopComplete(
    loop_id: LoopId,
    task_id: TaskId,
    commits: Int,
    reverts: Int,
    duration_ms: Int,
  )
}

pub type LoopFailed {
  LoopFailed(loop_id: LoopId, reason: String)
}

pub type ResourceExhausted {
  ResourceExhausted(resource: String, current: Int, limit: Int)
}

pub type BeadRemoved {
  BeadRemoved(task_id: TaskId, removed_at: Timestamp)
}
