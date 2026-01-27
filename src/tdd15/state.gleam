import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import simplifile

/// Phase status in the TDD15 workflow
pub type PhaseStatus {
  Pending
  InProgress
  Completed
  Failed
  Skipped
}

/// Per-phase state tracking
pub type PhaseState {
  PhaseState(status: PhaseStatus, gate: Bool, attempts: Int)
}

/// Main progress tracking type
pub type Progress {
  Progress(
    bead_id: String,
    language: Language,
    complexity: Complexity,
    route: List(Int),
    phases: dict.Dict(Int, PhaseState),
    current_phase: Int,
    last_commit: String,
  )
}

/// Language detection
pub type Language {
  Gleam
  Rust
  Mixed
  Unknown
}

/// Complexity assessment
pub type Complexity {
  Simple
  Medium
  Complex
}

/// Bead context from bd show --json
pub type BeadContext {
  BeadContext(
    id: String,
    title: String,
    requirements: List(String),
    context: String,
  )
}

/// Cache directory operations
pub type CacheDir {
  CacheDir(base_path: String, bead_id: String)
}

/// Error types for state operations
pub type StateError {
  CacheDirNotFound
  InvalidJson(String)
  IoError(String)
}

/// Initialize cache directory for a bead
pub fn init_cache(bead_id: String) -> Result(CacheDir, StateError) {
  let base_path = ".tdd15-cache/" <> bead_id
  let cache_dir = CacheDir(base_path, bead_id)

  simplifile.create_directory_all(base_path)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
  |> result.map(fn(_) { cache_dir })
}

/// Get cache directory for existing bead
pub fn get_cache(bead_id: String) -> Result(CacheDir, StateError) {
  let base_path = ".tdd15-cache/" <> bead_id

  simplifile.verify_is_directory(base_path)
  |> result.map_error(fn(_) { CacheDirNotFound })
  |> result.map(fn(_) { CacheDir(base_path, bead_id) })
}

/// Load progress from progress.json
pub fn load_progress(bead_id: String) -> Result(Progress, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let progress_path = cache_dir.base_path <> "/progress.json"

  simplifile.read(progress_path)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
  |> result.try(parse_progress)
}

/// Parse progress from JSON string
pub fn parse_progress(json_string: String) -> Result(Progress, StateError) {
  json.parse(from: json_string, using: progress_decoder())
  |> result.map_error(fn(err) { InvalidJson(json_error_to_string(err)) })
}

/// Save progress to progress.json
pub fn save_progress(
  bead_id: String,
  progress: Progress,
) -> Result(Nil, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let progress_path = cache_dir.base_path <> "/progress.json"
  let json_string = progress |> encode_progress

  simplifile.write(progress_path, json_string)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
}

/// Update phase status in progress
pub fn update_phase_status(
  progress: Progress,
  phase: Int,
  status: PhaseStatus,
) -> Progress {
  let existing_phase = dict.get(progress.phases, phase)

  let new_phase_state = case existing_phase {
    Ok(PhaseState(_, gate, attempts)) -> PhaseState(status, gate, attempts)
    Error(_) -> PhaseState(status, False, 0)
  }

  Progress(
    ..progress,
    phases: dict.insert(progress.phases, phase, new_phase_state),
    current_phase: phase,
  )
}

/// Mark gate result for a phase
pub fn mark_gate_result(progress: Progress, phase: Int, pass: Bool) -> Progress {
  let existing_phase = dict.get(progress.phases, phase)

  let new_phase_state = case existing_phase {
    Ok(PhaseState(status, _, attempts)) -> PhaseState(status, pass, attempts)
    Error(_) -> PhaseState(Pending, pass, 0)
  }

  Progress(
    ..progress,
    phases: dict.insert(progress.phases, phase, new_phase_state),
  )
}

/// Increment attempt counter for a phase
pub fn increment_attempt(progress: Progress, phase: Int) -> Progress {
  let existing_phase = dict.get(progress.phases, phase)

  let new_phase_state = case existing_phase {
    Ok(PhaseState(status, gate, attempts)) ->
      PhaseState(status, gate, attempts + 1)
    Error(_) -> PhaseState(Pending, False, 1)
  }

  Progress(
    ..progress,
    phases: dict.insert(progress.phases, phase, new_phase_state),
  )
}

/// Encode progress to JSON
pub fn encode_progress(progress: Progress) -> String {
  progress
  |> progress_encoder
  |> json.to_string
}

/// Save bead context to bead.json
pub fn save_bead_context(
  bead_id: String,
  context: BeadContext,
) -> Result(Nil, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let bead_path = cache_dir.base_path <> "/bead.json"
  let json_string = encode_bead_context(context)

  simplifile.write(bead_path, json_string)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
}

/// Load bead context from bead.json
pub fn load_bead_context(bead_id: String) -> Result(BeadContext, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let bead_path = cache_dir.base_path <> "/bead.json"

  simplifile.read(bead_path)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
  |> result.try(parse_bead_context)
}

/// Save phase output JSON
pub fn save_phase_output(
  bead_id: String,
  phase_name: String,
  data: json.Json,
) -> Result(Nil, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let output_path = cache_dir.base_path <> "/" <> phase_name <> ".json"
  let json_string = json.to_string(data)

  simplifile.write(output_path, json_string)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
}

/// Load phase output JSON as dynamic
pub fn load_phase_output(
  bead_id: String,
  phase_name: String,
) -> Result(json.Json, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let output_path = cache_dir.base_path <> "/" <> phase_name <> ".json"

  simplifile.read(output_path)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
  |> result.try(fn(json_string) { Ok(json.string(json_string)) })
}

/// Write summary.txt for debugging
pub fn write_summary(
  bead_id: String,
  content: String,
) -> Result(Nil, StateError) {
  use cache_dir <- result.try(get_cache(bead_id))

  let summary_path = cache_dir.base_path <> "/summary.txt"

  simplifile.write(summary_path, content)
  |> result.map_error(fn(err) { IoError(simplifile.describe_error(err)) })
}

// JSON Decoders

fn progress_decoder() -> decode.Decoder(Progress) {
  {
    use bead_id <- decode.field("bead_id", decode.string)
    use language <- decode.field("language", language_decoder())
    use complexity <- decode.field("complexity", complexity_decoder())
    use route <- decode.field("route", decode.list(decode.int))
    use phases <- decode.field("phases", phases_dict_decoder())
    use current_phase <- decode.field("current_phase", decode.int)
    use last_commit <- decode.field("last_commit", decode.string)

    decode.success(Progress(
      bead_id: bead_id,
      language: language,
      complexity: complexity,
      route: route,
      phases: phases,
      current_phase: current_phase,
      last_commit: last_commit,
    ))
  }
}

fn phases_dict_decoder() -> decode.Decoder(dict.Dict(Int, PhaseState)) {
  decode.dict(decode.int, phase_state_decoder())
}

fn phase_state_decoder() -> decode.Decoder(PhaseState) {
  {
    use status <- decode.field("status", phase_status_decoder())
    use gate <- decode.field("gate", decode.bool)
    use attempts <- decode.field("attempts", decode.int)

    decode.success(PhaseState(status: status, gate: gate, attempts: attempts))
  }
}

fn language_decoder() -> decode.Decoder(Language) {
  decode.string
  |> decode.then(fn(s) {
    case s {
      "gleam" -> decode.success(Gleam)
      "rust" -> decode.success(Rust)
      "mixed" -> decode.success(Mixed)
      _ -> decode.success(Unknown)
    }
  })
}

fn complexity_decoder() -> decode.Decoder(Complexity) {
  decode.string
  |> decode.then(fn(s) {
    case s {
      "simple" -> decode.success(Simple)
      "medium" -> decode.success(Medium)
      "complex" -> decode.success(Complex)
      _ -> decode.success(Unknown) |> decode.map(fn(_) { Simple })
    }
  })
}

fn phase_status_decoder() -> decode.Decoder(PhaseStatus) {
  decode.string
  |> decode.then(fn(s) {
    case s {
      "pending" -> decode.success(Pending)
      "in_progress" -> decode.success(InProgress)
      "completed" -> decode.success(Completed)
      "failed" -> decode.success(Failed)
      "skipped" -> decode.success(Skipped)
      _ -> decode.success(Pending)
    }
  })
}

fn bead_context_decoder() -> decode.Decoder(BeadContext) {
  {
    use id <- decode.field("id", decode.string)
    use title <- decode.field("title", decode.string)
    use requirements <- decode.field("requirements", decode.list(decode.string))
    use context <- decode.field("context", decode.string)

    decode.success(BeadContext(
      id: id,
      title: title,
      requirements: requirements,
      context: context,
    ))
  }
}

// JSON Encoders

fn progress_encoder(progress: Progress) -> json.Json {
  let obj = [
    #("bead_id", json.string(progress.bead_id)),
    #("language", encode_language(progress.language)),
    #("complexity", encode_complexity(progress.complexity)),
    #("route", encode_int_list(progress.route)),
    #("phases", encode_phases_dict(progress.phases)),
    #("current_phase", json.int(progress.current_phase)),
    #("last_commit", json.string(progress.last_commit)),
  ]

  json.object(obj)
}

fn encode_language(lang: Language) -> json.Json {
  case lang {
    Gleam -> json.string("gleam")
    Rust -> json.string("rust")
    Mixed -> json.string("mixed")
    Unknown -> json.string("unknown")
  }
}

fn encode_complexity(complexity: Complexity) -> json.Json {
  case complexity {
    Simple -> json.string("simple")
    Medium -> json.string("medium")
    Complex -> json.string("complex")
  }
}

fn encode_int_list(items: List(Int)) -> json.Json {
  json.array(items, json.int)
}

fn encode_phases_dict(phases: dict.Dict(Int, PhaseState)) -> json.Json {
  let pairs =
    phases
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(key, value) = pair
      #(int_to_string(key), encode_phase_state(value))
    })

  json.object(pairs)
}

fn encode_phase_state(state: PhaseState) -> json.Json {
  let obj = [
    #("status", encode_phase_status(state.status)),
    #("gate", json.bool(state.gate)),
    #("attempts", json.int(state.attempts)),
  ]

  json.object(obj)
}

fn encode_phase_status(status: PhaseStatus) -> json.Json {
  case status {
    Pending -> json.string("pending")
    InProgress -> json.string("in_progress")
    Completed -> json.string("completed")
    Failed -> json.string("failed")
    Skipped -> json.string("skipped")
  }
}

fn encode_bead_context(context: BeadContext) -> String {
  let obj = [
    #("id", json.string(context.id)),
    #("title", json.string(context.title)),
    #("requirements", encode_string_list(context.requirements)),
    #("context", json.string(context.context)),
  ]

  json.object(obj)
  |> json.to_string
}

fn encode_string_list(items: List(String)) -> json.Json {
  json.array(items, json.string)
}

fn parse_bead_context(json_string: String) -> Result(BeadContext, StateError) {
  json.parse(from: json_string, using: bead_context_decoder())
  |> result.map_error(fn(err) { InvalidJson(json_error_to_string(err)) })
}

fn json_error_to_string(err: json.DecodeError) -> String {
  case err {
    json.UnexpectedEndOfInput -> "Unexpected end of JSON input"
    json.UnexpectedByte(byte) -> "Unexpected byte in JSON: " <> byte
    json.UnexpectedSequence(seq) -> "Unexpected sequence in JSON: " <> seq
    json.UnableToDecode(errors) ->
      "Unable to decode JSON: "
      <> string.join(list.map(errors, decode_error_to_string), ", ")
  }
}

fn decode_error_to_string(err: decode.DecodeError) -> String {
  case err {
    _ -> "decode error"
  }
}

fn int_to_string(i: Int) -> String {
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "10"
    11 -> "11"
    12 -> "12"
    13 -> "13"
    14 -> "14"
    15 -> "15"
    _ -> {
      let negative = i < 0
      let n = case negative {
        True -> 0 - i
        False -> i
      }
      let digit = int_to_string(n % 10)
      let rest = int_to_string(n / 10)
      let str = rest <> digit
      case negative {
        True -> "-" <> str
        False -> str
      }
    }
  }
}
