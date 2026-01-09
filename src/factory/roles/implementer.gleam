//// Implementer role configuration for TCR workflow.
////
//// Defines model, tools, and filesystem access for code implementation agents.

pub type ImplementerConfig {
  ImplementerConfig(
    model: String,
    allowed_tools: List(String),
    filesystem_locks: FilesystemLocks,
  )
}

pub type FilesystemLocks {
  FilesystemLocks(src: AccessMode, tests: AccessMode)
}

pub type AccessMode {
  ReadWrite
  ReadOnly
  NoAccess
}

pub fn default_config() -> ImplementerConfig {
  ImplementerConfig(
    model: "qwen-32b",
    allowed_tools: [
      "read_file",
      "write_file",
      "list_directory",
      "run_tests",
      "git_diff",
      "git_status",
    ],
    filesystem_locks: FilesystemLocks(src: ReadWrite, tests: ReadOnly),
  )
}

pub fn with_model(config: ImplementerConfig, model: String) -> ImplementerConfig {
  ImplementerConfig(..config, model:)
}

pub fn can_write_src(config: ImplementerConfig) -> Bool {
  case config.filesystem_locks.src {
    ReadWrite -> True
    _ -> False
  }
}

pub fn can_write_tests(config: ImplementerConfig) -> Bool {
  case config.filesystem_locks.tests {
    ReadWrite -> True
    _ -> False
  }
}

pub fn can_read_src(config: ImplementerConfig) -> Bool {
  case config.filesystem_locks.src {
    ReadWrite | ReadOnly -> True
    NoAccess -> False
  }
}

pub fn can_read_tests(config: ImplementerConfig) -> Bool {
  case config.filesystem_locks.tests {
    ReadWrite | ReadOnly -> True
    NoAccess -> False
  }
}
