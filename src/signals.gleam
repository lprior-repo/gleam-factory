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
