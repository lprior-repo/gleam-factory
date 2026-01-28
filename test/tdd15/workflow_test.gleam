import gleam/json
import gleeunit/should
import tdd15/workflow.{PhaseConfig, PhaseResult}

pub fn phase_config_test() {
  let config =
    PhaseConfig(
      bead_id: "test-bead",
      workspace_path: "/tmp/test",
      llm_config: llm_router.new_config(
        gpu_governor: types.Disabled,
        local_url: "http://localhost:8000",
        anthropic_url: "http://localhost:8000",
        anthropic_key: "test-key",
      ),
    )

  config.bead_id
  |> should.equal("test-bead")

  config.workspace_path
  |> should.equal("/tmp/test")
}

pub fn phase_result_test() {
  let result =
    PhaseResult(
      success: True,
      message: "All good",
      data: json.object([#("test", json.string("value"))]),
    )

  result.success
  |> should.be_true

  result.message
  |> should.equal("All good")
}

pub fn phase_config_fields_test() {
  let config =
    PhaseConfig(
      bead_id: "bead-123",
      workspace_path: "/workspace",
      llm_config: llm_router.new_config(
        gpu_governor: types.Disabled,
        local_url: "http://localhost:8000",
        anthropic_url: "http://localhost:8000",
        anthropic_key: "test-key",
      ),
    )

  config.bead_id
  |> should.equal("bead-123")

  config.workspace_path
  |> should.equal("/workspace")
}

pub fn phase_result_data_test() {
  let data =
    json.object([
      #("phase", json.int(4)),
      #("tokens", json.int(100)),
    ])

  let result = PhaseResult(success: True, message: "Phase complete", data: data)

  result.data
  |> should.equal(data)
}
