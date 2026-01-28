import gleam/json
import tdd15/types

pub fn execute_phase4_red(_config: types.PhaseConfig) -> Result(types.PhaseResult, String) {
  Ok(types.PhaseResult(success: True, message: "Phase 4 RED: Write failing tests", data: json.object([])))
}

pub fn execute_phase5_green(_config: types.PhaseConfig) -> Result(types.PhaseResult, String) {
  Ok(types.PhaseResult(success: True, message: "Phase 5 GREEN: Minimal implementation", data: json.object([])))
}

pub fn execute_phase6_refactor(_config: types.PhaseConfig) -> Result(types.PhaseResult, String) {
  Ok(types.PhaseResult(success: True, message: "Phase 6 REFACTOR: Code cleanup", data: json.object([])))
}
