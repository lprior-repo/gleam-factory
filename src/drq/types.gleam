import gleam/int
import gleam/option

pub type Champion {
  Champion(
    version: String,
    commit: String,
    fitness_score: FitnessScore,
    timestamp: String,
  )
}

pub type TestCase {
  TestCase(id: String, name: String, scenario: String, expected_outcome: String)
}

pub type TestBank {
  TestBank(
    tests: List(TestCase),
    regressions: List(TestCase),
    history: List(#(TestCase, Round)),
  )
}

pub type Patch {
  Patch(
    id: String,
    diff: String,
    author: String,
    timestamp: String,
    validation_status: ValidationStatus,
  )
}

pub type SurfaceType {
  Cli
  Http
  Ui
}

pub type Arena {
  Arena(surface_type: SurfaceType, interface_definition: String)
}

pub type Round {
  Round(
    number: Int,
    champion_before: Champion,
    candidate: Patch,
    tests_added: List(TestCase),
    outcome: RoundOutcome,
  )
}

pub type RoundOutcome {
  ChampionDefended
  ChampionDethroned(new_champion: Patch, fitness_delta: FitnessScore)
  Inconclusive(reason: String)
}

pub type FitnessScore {
  FitnessScore(
    passed: Int,
    total: Int,
    coverage: Float,
    performance: Float,
    regressions_avoided: Int,
  )
}

pub type ValidationStatus {
  Accepted
  Rejected(reason: String)
  Pending
}

pub type ValidationResult {
  ValidationResult(
    status: ValidationStatus,
    reason: String,
    artifact_path: option.Option(String),
  )
}

pub fn new_test_bank() -> TestBank {
  TestBank(tests: [], regressions: [], history: [])
}

pub fn add_test(bank: TestBank, t: TestCase) -> TestBank {
  TestBank(..bank, tests: [t, ..bank.tests])
}

pub fn add_regression(bank: TestBank, t: TestCase) -> TestBank {
  TestBank(..bank, regressions: [t, ..bank.regressions])
}

pub fn fitness_score_ratio(score: FitnessScore) -> Float {
  case score.total {
    0 -> 0.0
    _ -> {
      let passed = int.to_float(score.passed)
      let total = int.to_float(score.total)
      passed /. total
    }
  }
}
