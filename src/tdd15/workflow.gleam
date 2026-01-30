import gleam/dict
import gleam/int
import gleam/result
import llm_router
import tdd15/phase0_triage.{execute_phase0_triage}
import tdd15/phase10_fp_gates.{execute_phase10_fp_gates}
import tdd15/phase12_martin_fowler.{execute_phase12_martin_fowler}
import tdd15/phase15_landing.{execute_phase15_landing}
import tdd15/phase7_martin_fowler.{execute_phase7_martin_fowler}
import tdd15/phases
import tdd15/phases_4_5_6.{
  execute_phase4_red, execute_phase5_green, execute_phase6_refactor,
}
import tdd15/state
import tdd15/types

pub type WorkflowResult {
  WorkflowComplete
  WorkflowFailed(String)
  WorkflowInProgress(phases.Phase)
}

pub type WorkflowState {
  WorkflowState(
    current_phase: phases.Phase,
    route: phases.Route,
    results: dict.Dict(Int, WorkflowPhaseResult),
  )
}

pub type WorkflowPhaseResult {
  TriageResult(types.TriageResult)
  PhaseResult(types.PhaseResult)
  QualityGateResult(types.QualityGateResult)
  FPGateResult(types.FPGateResult)
  LandingResult(types.LandingResult)
}

pub fn execute_workflow(
  config: types.PhaseConfig,
) -> Result(WorkflowResult, String) {
  let bead_id = config.bead_id
  use progress: state.Progress <- result.try(
    state.load_progress(bead_id)
    |> result.map_error(fn(_) { "Failed to load progress for workflow" }),
  )
  let start_num = progress.current_phase
  use start_phase <- result.try(
    phases.phase_by_number(start_num)
    |> result.map_error(fn(_) { "Failed to get phase by number" }),
  )
  let complexity = progress.complexity
  let phases_complexity = case complexity {
    state.Simple -> phases.Simple
    state.Medium -> phases.Medium
    state.Complex -> phases.Complex
  }
  let route = phases.route_for_complexty(phases_complexity)
  let workflow_state =
    WorkflowState(current_phase: start_phase, route:, results: dict.new())
  execute_phase(workflow_state, config)
}

fn execute_phase(
  state: WorkflowState,
  config: types.PhaseConfig,
) -> Result(WorkflowResult, String) {
  let phases.Phase(number: num, ..) = state.current_phase
  let result = case num {
    0 -> execute_phase0(config)
    4 -> execute_phase4(config)
    5 -> execute_phase5(config)
    6 -> execute_phase6(config)
    7 -> execute_phase7(config)
    10 -> execute_phase10(config)
    12 -> execute_phase12(config)
    15 -> execute_phase15(config)
    _ -> Error("Invalid phase: " <> int.to_string(num))
  }
  case result {
    Ok(phase_result) -> {
      let updated_results = dict.insert(state.results, num, phase_result)
      let updated_state = WorkflowState(..state, results: updated_results)
      advance_workflow(updated_state, config, num)
    }
    Error(err) -> Ok(WorkflowFailed(err))
  }
}

fn advance_workflow(
  state: WorkflowState,
  config: types.PhaseConfig,
  _completed_num: Int,
) -> Result(WorkflowResult, String) {
  case phases.next_phase(state.current_phase, state.route) {
    Ok(next_phase) -> {
      let updated_state = WorkflowState(..state, current_phase: next_phase)
      execute_phase(updated_state, config)
    }
    Error(_) -> Ok(WorkflowComplete)
  }
}

fn execute_phase0(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase0_triage(config) {
    Ok(result) -> Ok(QualityGateResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase4(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase4_red(config) {
    Ok(result) -> Ok(PhaseResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase5(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase5_green(config) {
    Ok(result) -> Ok(PhaseResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase6(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase6_refactor(config) {
    Ok(result) -> Ok(PhaseResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase7(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase7_martin_fowler(config) {
    Ok(result) -> Ok(QualityGateResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase10(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase10_fp_gates(config) {
    Ok(result) -> Ok(FPGateResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase12(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase12_martin_fowler(config) {
    Ok(result) -> Ok(QualityGateResult(result))
    Error(err) -> Error(err)
  }
}

fn execute_phase15(
  config: types.PhaseConfig,
) -> Result(WorkflowPhaseResult, String) {
  case execute_phase15_landing(config) {
    Ok(result) -> Ok(LandingResult(result))
    Error(err) -> Error(err)
  }
}

pub fn create_phase_config(
  bead_id: String,
  workspace_path: String,
  llm_config: llm_router.RouterConfig,
) -> types.PhaseConfig {
  types.PhaseConfig(bead_id:, workspace_path:, llm_config:)
}

pub fn get_current_phase(bead_id: String) -> Result(phases.Phase, String) {
  case state.load_progress(bead_id) {
    Ok(progress) -> {
      case phases.phase_by_number(progress.current_phase) {
        Ok(phase) -> Ok(phase)
        Error(_) -> Error("Failed to get phase from number")
      }
    }
    Error(_) -> Error("Failed to load progress")
  }
}

pub fn get_phase_status(
  bead_id: String,
  phase_num: Int,
) -> Result(state.PhaseStatus, String) {
  case state.load_progress(bead_id) {
    Ok(progress) -> {
      case dict.get(progress.phases, phase_num) {
        Ok(phase_state) -> Ok(phase_state.status)
        Error(_) -> Ok(state.Pending)
      }
    }
    Error(_) -> Error("Failed to load progress")
  }
}
