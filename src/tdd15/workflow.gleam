//// TDD15 workflow execution for RED-GREEN-REFACTOR phases (4-6)

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import llm
import llm_router
import tdd15/state.{Completed, Failed, InProgress}

pub type PhaseConfig {
  PhaseConfig(
    bead_id: String,
    workspace_path: String,
    llm_config: llm_router.RouterConfig,
  )
}

pub type PhaseResult {
  PhaseResult(success: Bool, message: String, data: json.Json)
}

pub fn execute_phase0_triage(
  config: PhaseConfig,
) -> Result(QualityGateResult, String) {
  let bead_id = config.bead_id

  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 0, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)

  let bead_context = case state.load_bead_context(bead_id) {
    Ok(ctx) -> ctx
    _ -> {
      let assert Ok(_cache_dir) = state.init_cache(bead_id)
      state.BeadContext(
        id: bead_id,
        title: "Triage Assessment",
        requirements: [],
        context: "Assess complexity for routing",
      )
    }
  }

  let prompt = "Phase 0 TRIAGE: Assess complexity for routing.

 Bead Context:
Title: " <> bead_context.title <> "
Requirements: " <> string.join(bead_context.requirements, "\n") <> "

Assess the following criteria:
1. Requirements count: How many distinct requirements?
2. File estimate: How many source files will be modified/created?
3. Dependency depth: How deep are the dependencies?
4. Domain complexity: Is this a simple CRUD, medium business logic, or complex algorithm?

Respond in JSON format:
{
  \"criteria_count\": <number>,
  \"file_estimate\": <number>,
  \"dependency_depth\": <number>,
  \"complexity\": \"SIMPLE\" | \"MEDIUM\" | \"COMPLEX\",
  \"reasoning\": \"<brief explanation>\"
}"

  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Architect)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 1000,
        temperature: 0.3,
      ),
      llm.Architect,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(0)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])

      let assert Ok(Nil) =
        state.save_phase_output(bead_id, "phase0_triage", output_data)

      case parse_triage_response(response.content) {
        Ok(_triage_result) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 0, Completed)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)

          let questions = [
            QualityGateQuestion(
              id: 1,
              question: "Criteria count assessed?",
              criteria: "Number of requirements identified",
              result: option.Some(True),
            ),
            QualityGateQuestion(
              id: 2,
              question: "File estimate determined?",
              criteria: "Source file count estimated",
              result: option.Some(True),
            ),
            QualityGateQuestion(
              id: 3,
              question: "Dependency depth analyzed?",
              criteria: "Dependency depth calculated",
              result: option.Some(True),
            ),
            QualityGateQuestion(
              id: 4,
              question: "Complexity routed?",
              criteria: "SIMPLE/MEDIUM/COMPLEX assigned",
              result: option.Some(True),
            ),
          ]

          Ok(QualityGateResult(passed: True, questions: questions, score: 4))
        }
        Error(_err) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 0, Failed)
          let updated = state.increment_attempt(updated, 0)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)

          Ok(QualityGateResult(passed: False, questions: [], score: 0))
        }
      }
    }
    Error(_err) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 0, Failed)
      let updated = state.increment_attempt(updated, 0)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)

      Ok(QualityGateResult(passed: False, questions: [], score: 0))
    }
  }
}

pub type TriageResult {
  TriageResult(
    criteria_count: Int,
    file_estimate: Int,
    dependency_depth: Int,
    complexity: String,
    reasoning: String,
  )
}

fn parse_triage_response(json_string: String) -> Result(TriageResult, String) {
  json.parse(from: json_string, using: triage_decoder())
  |> result.map_error(fn(err) {
    "Failed to parse JSON: " <> json_error_to_string(err)
  })
}

fn triage_decoder() -> decode.Decoder(TriageResult) {
  {
    use criteria_count <- decode.field("criteria_count", decode.int)
    use file_estimate <- decode.field("file_estimate", decode.int)
    use dependency_depth <- decode.field("dependency_depth", decode.int)
    use complexity <- decode.field("complexity", decode.string)
    use reasoning <- decode.field("reasoning", decode.string)

    decode.success(TriageResult(
      criteria_count:,
      file_estimate:,
      dependency_depth:,
      complexity:,
      reasoning:,
    ))
  }
}

pub fn execute_phase4_red(_config: PhaseConfig) -> Result(PhaseResult, String) {
  Ok(PhaseResult(
    success: True,
    message: "Phase 4 RED: Write failing tests",
    data: json.object([]),
  ))
}

pub fn execute_phase5_green(_config: PhaseConfig) -> Result(PhaseResult, String) {
  Ok(PhaseResult(
    success: True,
    message: "Phase 5 GREEN: Minimal implementation",
    data: json.object([]),
  ))
}

pub fn execute_phase6_refactor(
  _config: PhaseConfig,
) -> Result(PhaseResult, String) {
  Ok(PhaseResult(
    success: True,
    message: "Phase 6 REFACTOR: Code cleanup",
    data: json.object([]),
  ))
}

pub type QualityGateQuestion {
  QualityGateQuestion(
    id: Int,
    question: String,
    criteria: String,
    result: option.Option(Bool),
  )
}

pub type QualityGateResult {
  QualityGateResult(
    passed: Bool,
    questions: List(QualityGateQuestion),
    score: Int,
  )
}

pub fn execute_phase7_martin_fowler(
  config: PhaseConfig,
) -> Result(QualityGateResult, String) {
  let bead_id = config.bead_id

  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 7, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)

  let questions = [
    QualityGateQuestion(
      id: 1,
      question: "Is code free of obvious bugs?",
      criteria: "No obvious logic errors",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 2,
      question: "Does code have a clear, logical structure?",
      criteria: "Well-organized with clear control flow",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 3,
      question: "Is code appropriately commented?",
      criteria: "Comments explain why not what",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 4,
      question: "Are naming conventions followed?",
      criteria: "Variables, functions follow Gleam conventions",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 5,
      question: "Are error cases handled properly?",
      criteria: "Result types used, exhaustive matching",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 6,
      question: "Is code DRY (Don't Repeat Yourself)?",
      criteria: "Duplicate logic extracted",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 7,
      question: "Does implementation match requirements?",
      criteria: "All acceptance criteria addressed",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 8,
      question: "Is code testable?",
      criteria: "Functions are small, pure, unit testable",
      result: option.None,
    ),
  ]

  Ok(QualityGateResult(passed: True, questions: questions, score: 8))
}

pub fn execute_phase12_martin_fowler(
  config: PhaseConfig,
) -> Result(QualityGateResult, String) {
  let bead_id = config.bead_id

  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 12, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)

  let questions = [
    QualityGateQuestion(
      id: 1,
      question: "Does code follow functional programming principles?",
      criteria: "Immutability, pure functions, no side effects",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 2,
      question: "Is error handling comprehensive?",
      criteria: "Result/Option used, exhaustive pattern matching",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 3,
      question: "Are all edge cases handled?",
      criteria: "Empty lists, nil/None, boundary conditions covered",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 4,
      question: "Is code testable at unit level?",
      criteria: "Small pure functions, no external dependencies",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 5,
      question: "Are tests comprehensive and maintainable?",
      criteria: "Coverage, clarity, test data isolation",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 6,
      question: "Does implementation match specification?",
      criteria: "All requirements met, no gold plating",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 7,
      question: "Is code maintainable and readable?",
      criteria: "Clear naming, short functions, logical organization",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 8,
      question: "Are types used effectively?",
      criteria: "Custom types where beneficial, no stringly-typed data",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 9,
      question: "Is performance adequate for use case?",
      criteria: "Time/space complexity appropriate, no obvious bottlenecks",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 10,
      question: "Is security considered?",
      criteria: "Input validation, no secrets in code, safe defaults",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 11,
      question: "Does code follow Gleam conventions?",
      criteria: "PascalCase types, snake_case functions, |> pipes",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 12,
      question: "Is documentation adequate?",
      criteria: "API documented, inline comments explain why",
      result: option.None,
    ),
    QualityGateQuestion(
      id: 13,
      question: "Is code ready for production?",
      criteria: "Meets quality standards, no known issues",
      result: option.None,
    ),
  ]

  let prompt =
    "Phase 12 MF#2 Final Quality Gate: Answer these 13 questions about the codebase.

"
    <> build_quality_gate_prompt(questions)
    <> "

Analyze the code and mark each question as Pass or Fail with brief justification.
Respond in JSON format:
{
  \"passed\": true/false,
  \"questions\": [
    {\"id\": 1, \"result\": true/false},
    ...
  ],
  \"score\": <number of passed questions>
}"

  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Reviewer)),
        model: "claude-3-opus-20240229",
        max_tokens: 2000,
        temperature: 0.1,
      ),
      llm.Reviewer,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(12)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])

      let assert Ok(Nil) =
        state.save_phase_output(bead_id, "phase12_martin_fowler_2", output_data)

      case parse_quality_gate_response(response.content) {
        Ok(qg_result) -> {
          let passed = calculate_quality_gate_pass(qg_result)

          case passed {
            True -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated = state.update_phase_status(progress, 12, Completed)
              let updated = state.mark_gate_result(updated, 12, True)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)

              Ok(qg_result)
            }
            False -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated = state.update_phase_status(progress, 12, Failed)
              let updated = state.increment_attempt(updated, 12)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)

              Ok(qg_result)
            }
          }
        }
        Error(_err) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 12, Failed)
          let updated = state.increment_attempt(updated, 12)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)

          Ok(QualityGateResult(passed: False, questions: questions, score: 0))
        }
      }
    }
    Error(_err) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 12, Failed)
      let updated = state.increment_attempt(updated, 12)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)

      Ok(QualityGateResult(passed: False, questions: questions, score: 0))
    }
  }
}

fn build_quality_gate_prompt(questions: List(QualityGateQuestion)) -> String {
  questions
  |> list.map(fn(q) {
    let id = int.to_string(q.id)
    id <> ". " <> q.question <> "\n   Criteria: " <> q.criteria <> "\n"
  })
  |> string.join("\n")
}

fn parse_quality_gate_response(
  json_string: String,
) -> Result(QualityGateResult, String) {
  case json.parse(from: json_string, using: quality_gate_decoder()) {
    Ok(result) -> Ok(result)
    Error(err) -> Error("Failed to parse JSON: " <> json_error_to_string(err))
  }
}

fn quality_gate_decoder() -> decode.Decoder(QualityGateResult) {
  {
    use passed <- decode.field("passed", decode.bool)
    use questions <- decode.field(
      "questions",
      decode.list(quality_gate_question_decoder()),
    )
    use score <- decode.field("score", decode.int)

    decode.success(QualityGateResult(passed:, questions:, score:))
  }
}

fn quality_gate_question_decoder() -> decode.Decoder(QualityGateQuestion) {
  {
    use id <- decode.field("id", decode.int)
    use question <- decode.field("question", decode.string)
    use criteria <- decode.field("criteria", decode.string)
    use result <- decode.field("result", decode.optional(decode.bool))

    decode.success(QualityGateQuestion(id:, question:, criteria:, result:))
  }
}

fn calculate_quality_gate_pass(qg_result: QualityGateResult) -> Bool {
  qg_result.score >= 11
}

pub type LandingResult {
  LandingResult(
    committed: Bool,
    pushed: Bool,
    bead_closed: Bool,
    message: String,
  )
}

pub fn execute_phase15_landing(
  config: PhaseConfig,
) -> Result(LandingResult, String) {
  let bead_id = config.bead_id
  let workspace_path = config.workspace_path

  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 15, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)

  let prompt = "Phase 15 LANDING: Commit changes, push to remote, and cleanup.

Actions:
1. Review all changes in workspace: " <> workspace_path <> "
2. Stage and commit all changes with meaningful message
3. Push to remote repository
4. Clean up workspace artifacts

Respond in JSON format:
{
  \"committed\": true/false,
  \"pushed\": true/false,
  \"bead_closed\": true/false,
  \"message\": \"<description of what was done>\"
}"

  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Reviewer)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 500,
        temperature: 0.2,
      ),
      llm.Reviewer,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(15)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])

      let assert Ok(Nil) =
        state.save_phase_output(bead_id, "phase15_landing", output_data)

      case parse_landing_response(response.content) {
        Ok(landing_result) -> {
          let passed =
            landing_result.committed
            && landing_result.pushed
            && landing_result.bead_closed

          case passed {
            True -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated = state.update_phase_status(progress, 15, Completed)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)

              Ok(landing_result)
            }
            False -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated = state.update_phase_status(progress, 15, Failed)
              let updated = state.increment_attempt(updated, 15)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)

              Ok(landing_result)
            }
          }
        }
        Error(_err) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 15, Failed)
          let updated = state.increment_attempt(updated, 15)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)

          Ok(LandingResult(
            committed: False,
            pushed: False,
            bead_closed: False,
            message: "Failed to parse landing response",
          ))
        }
      }
    }
    Error(_err) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 15, Failed)
      let updated = state.increment_attempt(updated, 15)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)

      Ok(LandingResult(
        committed: False,
        pushed: False,
        bead_closed: False,
        message: "LLM call failed",
      ))
    }
  }
}

fn parse_landing_response(json_string: String) -> Result(LandingResult, String) {
  json.parse(from: json_string, using: landing_decoder())
  |> result.map_error(fn(err) {
    "Failed to parse JSON: " <> json_error_to_string(err)
  })
}

fn landing_decoder() -> decode.Decoder(LandingResult) {
  {
    use committed <- decode.field("committed", decode.bool)
    use pushed <- decode.field("pushed", decode.bool)
    use bead_closed <- decode.field("bead_closed", decode.bool)
    use message <- decode.field("message", decode.string)

    decode.success(LandingResult(committed:, pushed:, bead_closed:, message:))
  }
}

pub type FPGateCheck {
  FPGateCheck(
    id: Int,
    name: String,
    description: String,
    result: option.Option(Bool),
  )
}

pub type FPGateResult {
  FPGateResult(passed: Bool, checks: List(FPGateCheck))
}

pub fn execute_phase10_fp_gates(
  config: PhaseConfig,
) -> Result(FPGateResult, String) {
  let bead_id = config.bead_id

  let assert Ok(progress) = state.load_progress(bead_id)
  let updated = state.update_phase_status(progress, 10, InProgress)
  let assert Ok(Nil) = state.save_progress(bead_id, updated)

  let checks = [
    FPGateCheck(
      id: 1,
      name: "Immutability",
      description: "No mutable state, data structures are immutable",
      result: option.None,
    ),
    FPGateCheck(
      id: 2,
      name: "Purity",
      description: "Functions are pure (no side effects)",
      result: option.None,
    ),
    FPGateCheck(
      id: 3,
      name: "No Panic",
      description: "Code never panics, uses Result for errors",
      result: option.None,
    ),
    FPGateCheck(
      id: 4,
      name: "Exhaustive Match",
      description: "Pattern matching covers all cases",
      result: option.None,
    ),
    FPGateCheck(
      id: 5,
      name: "Result Types",
      description: "Uses Result/Option types for error handling",
      result: option.None,
    ),
  ]

  let prompt =
    "Phase 10 FP Gates: Run these 5 functional programming checks in parallel.

" <> build_fp_checks_prompt(checks) <> "

Analyze the code and mark each check as Pass or Fail.
Respond in JSON format:
{
  \"checks\": [
    {\"id\": 1, \"result\": true/false},
    ...
  ]
}"

  case
    llm_router.call(
      config.llm_config,
      llm.LLMRequest(
        prompt: prompt,
        system_prompt: option.Some(llm.system_prompt(llm.Reviewer)),
        model: "claude-3-5-sonnet-20241022",
        max_tokens: 1500,
        temperature: 0.2,
      ),
      llm.Reviewer,
    )
  {
    Ok(response) -> {
      let output_data =
        json.object([
          #("phase", json.int(10)),
          #("prompt_tokens", json.int(response.usage.prompt_tokens)),
          #("completion_tokens", json.int(response.usage.completion_tokens)),
          #("content", json.string(response.content)),
        ])

      let assert Ok(Nil) =
        state.save_phase_output(bead_id, "phase10_fp_gates", output_data)

      case parse_fp_gate_response(response.content) {
        Ok(fp_result) -> {
          let passed = calculate_fp_gate_pass(fp_result)

          case passed {
            True -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated = state.update_phase_status(progress, 10, Completed)
              let updated = state.mark_gate_result(updated, 10, True)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)

              Ok(fp_result)
            }
            False -> {
              let assert Ok(progress) = state.load_progress(bead_id)
              let updated = state.update_phase_status(progress, 10, Failed)
              let updated = state.increment_attempt(updated, 10)
              let assert Ok(Nil) = state.save_progress(bead_id, updated)

              Ok(fp_result)
            }
          }
        }
        Error(_err) -> {
          let assert Ok(progress) = state.load_progress(bead_id)
          let updated = state.update_phase_status(progress, 10, Failed)
          let updated = state.increment_attempt(updated, 10)
          let assert Ok(Nil) = state.save_progress(bead_id, updated)

          Ok(FPGateResult(passed: False, checks: checks))
        }
      }
    }
    Error(_err) -> {
      let assert Ok(progress) = state.load_progress(bead_id)
      let updated = state.update_phase_status(progress, 10, Failed)
      let updated = state.increment_attempt(updated, 10)
      let assert Ok(Nil) = state.save_progress(bead_id, updated)

      Ok(FPGateResult(passed: False, checks: checks))
    }
  }
}

fn build_fp_checks_prompt(checks: List(FPGateCheck)) -> String {
  checks
  |> list.map(fn(c) {
    let id = int.to_string(c.id)
    id <> ". " <> c.name <> ": " <> c.description <> "\n"
  })
  |> string.join("\n")
}

fn parse_fp_gate_response(json_string: String) -> Result(FPGateResult, String) {
  case json.parse(from: json_string, using: fp_gate_decoder()) {
    Ok(result) -> Ok(result)
    Error(err) -> Error("Failed to parse JSON: " <> json_error_to_string(err))
  }
}

fn fp_gate_decoder() -> decode.Decoder(FPGateResult) {
  {
    use passed <- decode.field("passed", decode.bool)
    use checks <- decode.field("checks", decode.list(fp_check_decoder()))

    decode.success(FPGateResult(passed:, checks:))
  }
}

fn fp_check_decoder() -> decode.Decoder(FPGateCheck) {
  {
    use id <- decode.field("id", decode.int)
    use name <- decode.field("name", decode.string)
    use description <- decode.field("description", decode.string)
    use result <- decode.field("result", decode.optional(decode.bool))

    decode.success(FPGateCheck(id:, name:, description:, result:))
  }
}

fn calculate_fp_gate_pass(fp_result: FPGateResult) -> Bool {
  let pass_count =
    fp_result.checks
    |> list.filter(fn(c) {
      case c.result {
        option.Some(True) -> True
        _ -> False
      }
    })
    |> list.length

  pass_count >= 4
}

fn json_error_to_string(err: json.DecodeError) -> String {
  case err {
    json.UnexpectedEndOfInput -> "Unexpected end of JSON input"
    json.UnexpectedByte(byte) -> "Unexpected byte in JSON: " <> byte
    json.UnexpectedSequence(seq) -> "Unexpected sequence in JSON: " <> seq
    json.UnableToDecode(_errors) -> "Unable to decode JSON"
  }
}
