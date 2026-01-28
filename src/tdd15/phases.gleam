import gleam/int
import gleam/list
import gleam/result

/// TDD15 Phase definition with gate, agent type, and model
pub type Phase {
  Phase0Triage
  Phase1Research
  Phase2Plan
  Phase3Verify
  Phase4Red
  Phase5Green
  Phase6Refactor
  Phase7MartinFowler1
  Phase8Implement
  Phase9Verify
  Phase10FpGates
  Phase11Qa
  Phase12MartinFowler2
  Phase13Consistency
  Phase14Liability
  Phase15Landing
}

/// Phase metadata
pub type PhaseMeta {
  PhaseMeta(
    number: Int,
    name: String,
    gate: String,
    agent_type: String,
    model: String,
  )
}

/// Complexity routing levels
pub type Complexity {
  Simple
  Medium
  Complex
}

/// Phase execution status
pub type PhaseStatus {
  Pending
  InProgress
  Completed
  Skipped
}

/// Route: ordered list of phase numbers
pub type Route {
  Route(List(Int))
}

/// Get metadata for a phase
pub fn phase_meta(phase: Phase) -> PhaseMeta {
  case phase {
    Phase0Triage ->
      PhaseMeta(
        0,
        "TRIAGE",
        "complexity_assessed",
        "triage",
        "claude-3-5-sonnet",
      )
    Phase1Research ->
      PhaseMeta(
        1,
        "RESEARCH",
        "sufficient_context",
        "researcher",
        "claude-3-5-sonnet",
      )
    Phase2Plan ->
      PhaseMeta(2, "PLAN", "plan_verified", "architect", "claude-3-5-sonnet")
    Phase3Verify ->
      PhaseMeta(
        3,
        "VERIFY",
        "plan_verified_llm",
        "verifier",
        "claude-3-5-sonnet",
      )
    Phase4Red -> PhaseMeta(4, "RED", "tests_fail", "tdd", "claude-3-5-sonnet")
    Phase5Green ->
      PhaseMeta(5, "GREEN", "tests_pass", "tdd", "claude-3-5-sonnet")
    Phase6Refactor ->
      PhaseMeta(6, "REFACTOR", "tests_green", "tdd", "claude-3-5-sonnet")
    Phase7MartinFowler1 ->
      PhaseMeta(7, "MF#1", "martin_fowler_1", "refactorer", "claude-3-5-sonnet")
    Phase8Implement ->
      PhaseMeta(
        8,
        "IMPLEMENT",
        "implementation_complete",
        "implementer",
        "claude-3-5-sonnet",
      )
    Phase9Verify ->
      PhaseMeta(9, "VERIFY", "criteria_met", "verifier", "claude-3-5-sonnet")
    Phase10FpGates ->
      PhaseMeta(
        10,
        "FP-GATES",
        "no_critical_issues",
        "fp_checker",
        "claude-3-5-sonnet",
      )
    Phase11Qa -> PhaseMeta(11, "QA", "qa_pass", "qa", "claude-3-5-sonnet")
    Phase12MartinFowler2 ->
      PhaseMeta(
        12,
        "MF#2",
        "martin_fowler_2",
        "refactorer",
        "claude-3-opus-20240229",
      )
    Phase13Consistency ->
      PhaseMeta(
        13,
        "CONSISTENCY",
        "standards_met",
        "consistency",
        "claude-3-5-sonnet",
      )
    Phase14Liability ->
      PhaseMeta(14, "LIABILITY", "minimized", "lawyer", "claude-3-5-sonnet")
    Phase15Landing ->
      PhaseMeta(15, "LANDING", "push_succeeded", "lander", "claude-3-5-sonnet")
  }
}

/// Get phase by number
pub fn phase_by_number(number: Int) -> Result(Phase, String) {
  case number {
    0 -> Ok(Phase0Triage)
    1 -> Ok(Phase1Research)
    2 -> Ok(Phase2Plan)
    3 -> Ok(Phase3Verify)
    4 -> Ok(Phase4Red)
    5 -> Ok(Phase5Green)
    6 -> Ok(Phase6Refactor)
    7 -> Ok(Phase7MartinFowler1)
    8 -> Ok(Phase8Implement)
    9 -> Ok(Phase9Verify)
    10 -> Ok(Phase10FpGates)
    11 -> Ok(Phase11Qa)
    12 -> Ok(Phase12MartinFowler2)
    13 -> Ok(Phase13Consistency)
    14 -> Ok(Phase14Liability)
    15 -> Ok(Phase15Landing)
    _ -> Error("Invalid phase number: " <> int_to_string(number))
  }
}

/// Get route for complexity level
pub fn route_for_complexity(complexity: Complexity) -> Route {
  case complexity {
    Simple -> Route([0, 4, 5, 6, 14, 15])
    Medium -> Route([0, 1, 2, 4, 5, 6, 7, 9, 11, 15])
    Complex -> Route([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])
  }
}

/// Get next phase in route, returns Error if at end
pub fn next_phase(current: Phase, route: Route) -> Result(Phase, String) {
  let Route(numbers) = route
  let current_num = phase_meta(current).number

  numbers
  |> list.drop_while(fn(n) { n != current_num })
  |> list.drop(1)
  |> list.first
  |> result.map_error(fn(_) { "No next phase in route" })
  |> result.try(phase_by_number)
}

/// Get all phases in route as Phase types
pub fn route_phases(route: Route) -> List(Phase) {
  let Route(numbers) = route
  numbers
  |> list.map(fn(n) {
    case phase_by_number(n) {
      Ok(phase) -> phase
      _ -> panic as "Invalid phase number in route"
    }
  })
}

/// Parse complexity from string
pub fn parse_complexity(s: String) -> Result(Complexity, String) {
  case s {
    "SIMPLE" -> Ok(Simple)
    "MEDIUM" -> Ok(Medium)
    "COMPLEX" -> Ok(Complex)
    _ ->
      Error(
        "Invalid complexity: " <> s <> " (expected: SIMPLE, MEDIUM, COMPLEX)",
      )
  }
}

/// Convert complexity to string
pub fn complexity_to_string(complexity: Complexity) -> String {
  case complexity {
    Simple -> "SIMPLE"
    Medium -> "MEDIUM"
    Complex -> "COMPLEX"
  }
}

/// Parse phase status from string
pub fn parse_phase_status(s: String) -> Result(PhaseStatus, String) {
  case s {
    "pending" -> Ok(Pending)
    "in_progress" -> Ok(InProgress)
    "completed" -> Ok(Completed)
    "skipped" -> Ok(Skipped)
    _ ->
      Error(
        "Invalid phase status: "
        <> s
        <> " (expected: pending, in_progress, completed, skipped)",
      )
  }
}

/// Convert phase status to string
pub fn phase_status_to_string(status: PhaseStatus) -> String {
  case status {
    Pending -> "pending"
    InProgress -> "in_progress"
    Completed -> "completed"
    Skipped -> "skipped"
  }
}

/// Check if phase is in route
pub fn phase_in_route(phase: Phase, route: Route) -> Bool {
  let Route(numbers) = route
  let num = phase_meta(phase).number
  list.contains(numbers, num)
}

/// Count phases in route
pub fn route_length(route: Route) -> Int {
  let Route(numbers) = route
  list.length(numbers)
}

/// Get position of phase in route (0-indexed), returns Error if not found
pub fn phase_position(phase: Phase, route: Route) -> Result(Int, String) {
  let Route(numbers) = route
  let target_num = phase_meta(phase).number

  numbers
  |> list.index_map(fn(n, idx) { #(n, idx) })
  |> list.find(fn(pair) {
    let #(n, _) = pair
    n == target_num
  })
  |> result.map(fn(pair) {
    let #(_, idx) = pair
    idx
  })
  |> result.map_error(fn(_) { "Phase not in route" })
}

/// Get first phase in route
pub fn route_start(route: Route) -> Result(Phase, String) {
  let Route(numbers) = route
  case numbers {
    [first, ..] -> phase_by_number(first)
    [] -> Error("Empty route has no start phase")
  }
}

/// Get last phase in route
pub fn route_end(route: Route) -> Result(Phase, String) {
  let Route(numbers) = route
  case numbers {
    [] -> Error("Empty route has no end phase")
    _ -> {
      let last = list.last(numbers)
      case last {
        Ok(n) -> phase_by_number(n)
        _ -> Error("Cannot get last phase")
      }
    }
  }
}

fn int_to_string(i: Int) -> String {
  int.to_string(i)
}
