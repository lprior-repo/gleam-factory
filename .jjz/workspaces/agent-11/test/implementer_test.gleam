import factory/roles/implementer
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn default_config_test() {
  let config = implementer.default_config()

  config.model
  |> should.equal("qwen-32b")

  config.allowed_tools
  |> should.equal([
    "read_file",
    "write_file",
    "list_directory",
    "run_tests",
    "git_diff",
    "git_status",
  ])
}

pub fn filesystem_locks_test() {
  let config = implementer.default_config()

  implementer.can_write_src(config)
  |> should.be_true

  implementer.can_read_src(config)
  |> should.be_true

  implementer.can_write_tests(config)
  |> should.be_false

  implementer.can_read_tests(config)
  |> should.be_true
}

pub fn with_model_test() {
  let config =
    implementer.default_config()
    |> implementer.with_model("llama-70b")

  config.model
  |> should.equal("llama-70b")
}

pub fn readonly_filesystem_locks_test() {
  let locks =
    implementer.FilesystemLocks(
      src: implementer.ReadOnly,
      tests: implementer.NoAccess,
    )
  let config =
    implementer.ImplementerConfig(
      model: "test",
      allowed_tools: [],
      filesystem_locks: locks,
    )

  implementer.can_write_src(config)
  |> should.be_false

  implementer.can_read_src(config)
  |> should.be_true

  implementer.can_read_tests(config)
  |> should.be_false
}
