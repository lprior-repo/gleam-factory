import gleam/json
import gleeunit/should
import tdd15/workflow.{PhaseConfig, PhaseResult}

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

pub fn phase_result_false_test() {
  let result =
    PhaseResult(success: False, message: "Failed", data: json.object([]))

  result.success
  |> should.be_false

  result.message
  |> should.equal("Failed")
}
