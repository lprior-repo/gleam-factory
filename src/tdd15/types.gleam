import gleam/dynamic/decode
import gleam/json
import gleam/option
import llm_router

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

pub type TriageResult {
  TriageResult(
    criteria_count: Int,
    file_estimate: Int,
    dependency_depth: Int,
    complexity: String,
    reasoning: String,
  )
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

pub type LandingResult {
  LandingResult(
    committed: Bool,
    pushed: Bool,
    bead_closed: Bool,
    message: String,
  )
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

pub fn triage_decoder() -> decode.Decoder(TriageResult) {
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

pub fn quality_gate_decoder() -> decode.Decoder(QualityGateResult) {
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

pub fn landing_decoder() -> decode.Decoder(LandingResult) {
  {
    use committed <- decode.field("committed", decode.bool)
    use pushed <- decode.field("pushed", decode.bool)
    use bead_closed <- decode.field("bead_closed", decode.bool)
    use message <- decode.field("message", decode.string)

    decode.success(LandingResult(committed:, pushed:, bead_closed:, message:))
  }
}

pub fn fp_gate_decoder() -> decode.Decoder(FPGateResult) {
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

pub fn json_error_to_string(err: json.DecodeError) -> String {
  case err {
    json.UnexpectedEndOfInput -> "Unexpected end of JSON input"
    json.UnexpectedByte(byte) -> "Unexpected byte in JSON: " <> byte
    json.UnexpectedSequence(seq) -> "Unexpected sequence in JSON: " <> seq
    json.UnableToDecode(_errors) -> "Unable to decode JSON"
  }
}
