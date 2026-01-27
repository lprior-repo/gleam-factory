import gleam/float

pub type Champion {
  Champion(commit: String, fitness: Float, test_pass_rate: Float)
}

pub type PatchProposal {
  PatchProposal(diff: String, files: List(String), rationale: String)
}

pub type RegressionTest {
  RegressionTest(name: String, path: String, code: String, scenario: String)
}

pub type TestBank {
  TestBank(
    existing: List(String),
    regressions: List(RegressionTest),
    pending: List(RegressionTest),
  )
}

pub type LlmClient {
  LlmClient(api_key: String, model: String)
}

pub type Fitness {
  Fitness(score: Float, test_pass_rate: Float, coverage: Float)
}

pub type GateResult {
  GateResult(performance: Bool, security: Bool, maintainability: Bool)
}

pub type TestResult {
  TestResult(passed: Int, failed: Int, skipped: Int, failures: List(String))
}

pub type RoundOutcome {
  Accepted(new_champion: Champion)
  Rejected(reason: String)
  RoundError(reason: String)
}

pub type RoundResult {
  RoundResult(
    outcome: RoundOutcome,
    new_tests: List(RegressionTest),
    fitness: Float,
  )
}

pub type RoundExecutor {
  RoundExecutor(workspace: String, llm: LlmClient, test_bank: TestBank)
}

pub fn new_executor(
  workspace: String,
  llm: LlmClient,
  initial_tests: List(String),
) -> RoundExecutor {
  let bank = TestBank(existing: initial_tests, regressions: [], pending: [])
  RoundExecutor(workspace:, llm:, test_bank: bank)
}

pub fn execute_round(
  executor: RoundExecutor,
  current_champion: Champion,
) -> RoundResult {
  let proposal = propose_patch(executor, current_champion)

  case proposal {
    Error(reason) ->
      RoundResult(outcome: RoundError(reason:), new_tests: [], fitness: 0.0)
    Ok(patch_and_tests) -> {
      let new_tests = patch_and_tests.tests

      case validate_new_tests(executor, new_tests, current_champion) {
        Error(reason) ->
          RoundResult(
            outcome: RoundError("Test validation failed: " <> reason),
            new_tests: [],
            fitness: 0.0,
          )
        Ok(True) -> {
          let fitness = calculate_test_fitness(executor)

          case fitness >=. current_champion.fitness {
            True -> {
              let new_champion =
                Champion(commit: "new", fitness:, test_pass_rate: fitness)
              RoundResult(
                outcome: Accepted(new_champion:),
                new_tests:,
                fitness:,
              )
            }
            False ->
              RoundResult(
                outcome: Rejected("Fitness regression"),
                new_tests:,
                fitness:,
              )
          }
        }
        Ok(False) ->
          RoundResult(
            outcome: Rejected("New tests don't fail on old champion"),
            new_tests: [],
            fitness: 0.0,
          )
      }
    }
  }
}

type PatchAndTests {
  PatchAndTests(patch: PatchProposal, tests: List(RegressionTest))
}

fn propose_patch(
  executor: RoundExecutor,
  champion: Champion,
) -> Result(PatchAndTests, String) {
  let prompt = build_proposal_prompt(executor, champion)

  case call_llm(executor.llm, prompt) {
    Error(reason) -> Error(reason)
    Ok(response) -> parse_proposal(response)
  }
}

fn validate_new_tests(
  _executor: RoundExecutor,
  tests: List(RegressionTest),
  _champion: Champion,
) -> Result(Bool, String) {
  case tests {
    [] -> Ok(True)
    _ -> Ok(all_fail(tests))
  }
}

fn calculate_test_fitness(_executor: RoundExecutor) -> Float {
  0.85
}

fn all_fail(_tests: List(RegressionTest)) -> Bool {
  True
}

fn build_proposal_prompt(_executor: RoundExecutor, champion: Champion) -> String {
  "Current champion fitness: " <> float.to_string(champion.fitness)
}

fn call_llm(_llm: LlmClient, _prompt: String) -> Result(String, String) {
  Ok("stub response")
}

fn parse_proposal(_response: String) -> Result(PatchAndTests, String) {
  Ok(
    PatchAndTests(
      patch: PatchProposal(diff: "", files: [], rationale: ""),
      tests: [],
    ),
  )
}
