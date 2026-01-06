pub type TestFailure {
  TestFailure(
    file: String,
    error: String,
    context_hash: String,
    timestamp: String,
  )
}

pub type TestPassing {
  TestPassing(hash: String, timestamp: String)
}

pub type BeadAssigned {
  BeadAssigned(
    task_id: String,
    spec: String,
    requirements: List(String),
    priority: String,
    assigned_at: String,
  )
}

pub type PatchProposed {
  PatchProposed(
    diff: String,
    author_pid: String,
    workspace: String,
    hash: String,
  )
}

pub type PatchAccepted {
  PatchAccepted(hash: String, merged_at: String)
}

pub type PatchRejected {
  PatchRejected(reason: String)
}

pub type GoldenMasterUpdated {
  GoldenMasterUpdated(old_hash: String, new_hash: String)
}

pub type Evolution {
  Evolution(new_hash: String, cause: String)
}

pub type LoopSpawned {
  LoopSpawned(loop_id: String, task_id: String, phase: String)
}

pub type LoopComplete {
  LoopComplete(
    loop_id: String,
    task_id: String,
    commits: Int,
    reverts: Int,
    duration_ms: Int,
  )
}

pub type LoopFailed {
  LoopFailed(loop_id: String, reason: String)
}

pub type ResourceExhausted {
  ResourceExhausted(resource: String, current: Int, limit: Int)
}
