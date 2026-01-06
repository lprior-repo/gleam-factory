pub type TestFailure {
  TestFailure(
    file: String,
    error: String,
    context_hash: String,
    timestamp: String,
  )
}

pub type TestPassing {
  TestPassing(
    hash: String,
    timestamp: String,
  )
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
