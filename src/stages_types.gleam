import domain
import gleam/list
import gleam/string

pub type StageError {
  CommandNotFound(command: String)
  CommandFailed(stage: String, exit_code: Int, output: String)
  InvalidStage(stage: String, language: String)
  StageTransitionError(from: String, to: String, reason: String)
}

pub fn format_stage_error(err: StageError) -> String {
  case err {
    CommandNotFound(cmd) ->
      "Command not found: '" <> cmd <> "'. Please install it and try again."
    CommandFailed(stage, code, output) ->
      "Stage '"
      <> stage
      <> "' failed (exit "
      <> string.inspect(code)
      <> "): "
      <> string.slice(output, 0, 200)
    InvalidStage(stage, lang) ->
      "Unknown stage '"
      <> stage
      <> "' for language '"
      <> lang
      <> "'. Valid stages: implement, unit-test, coverage, lint, static, integration, security, review, accept"
    StageTransitionError(from, to, reason) ->
      "Cannot transition from '" <> from <> "' to '" <> to <> "': " <> reason
  }
}

pub fn validate_stage_transition(
  from_stage: String,
  to_stage: String,
) -> Result(Nil, String) {
  let pipeline = domain.standard_pipeline()
  let from_idx = domain.find_stage_index(pipeline, from_stage)
  let to_idx = domain.find_stage_index(pipeline, to_stage)

  case from_idx, to_idx {
    Ok(fi), Ok(ti) ->
      case fi < ti {
        True -> Ok(Nil)
        False ->
          Error(
            "invalid stage transition: cannot move from "
            <> from_stage
            <> " to "
            <> to_stage,
          )
      }
    Error(_), _ -> Error("unknown stage: " <> from_stage)
    _, Error(_) -> Error("unknown stage: " <> to_stage)
  }
}

pub type StagePreview {
  StagePreview(name: String, command: String, estimated_duration: Int)
}

pub fn execute_stages_dry_run(
  stages: List(domain.Stage),
  language: domain.Language,
) -> List(StagePreview) {
  stages
  |> list.map(fn(stage) { stage_preview(stage, language) })
}

fn stage_preview(stage: domain.Stage, language: domain.Language) -> StagePreview {
  let domain.Stage(name, _, _) = stage
  let cmd = stage_command(name, language)
  let duration = estimate_duration(name)
  StagePreview(name, cmd, duration)
}

fn stage_command(stage_name: String, language: domain.Language) -> String {
  case language, stage_name {
    domain.Gleam, "implement" -> "gleam build"
    domain.Gleam, "unit-test" -> "gleam test"
    domain.Gleam, "coverage" -> "find . -name *_test.gleam"
    domain.Gleam, "lint" -> "gleam format --check ."
    domain.Gleam, "static" -> "gleam check"
    domain.Gleam, "integration" -> "gleam test"
    domain.Gleam, "security" -> "gleam deps download"
    domain.Gleam, "review" -> "grep -r TODO|FIXME|XXX|HACK"
    domain.Gleam, "accept" ->
      "gleam build && gleam test && gleam format --check ."
    domain.Go, "implement" -> "go build ./..."
    domain.Go, "unit-test" -> "go test -v -short ./..."
    domain.Go, "coverage" -> "go test -coverprofile=coverage.out ./..."
    domain.Go, "lint" -> "gofmt -l ."
    domain.Go, "static" -> "go vet ./..."
    domain.Go, "integration" -> "go test -v ./..."
    domain.Go, "security" -> "gosec ./..."
    domain.Go, "review" -> "grep -r TODO|FIXME|XXX|HACK"
    domain.Go, "accept" ->
      "go build ./... && go test -v -short ./... && gofmt -l ."
    domain.Rust, "implement" -> "cargo build"
    domain.Rust, "unit-test" -> "cargo test"
    domain.Rust, "coverage" -> "cargo tarpaulin --out Xml"
    domain.Rust, "lint" -> "cargo fmt --check"
    domain.Rust, "static" -> "cargo clippy --all-targets"
    domain.Rust, "integration" -> "cargo test --all"
    domain.Rust, "security" -> "cargo audit"
    domain.Rust, "review" -> "grep -r TODO|FIXME|XXX|HACK"
    domain.Rust, "accept" -> "cargo build && cargo test && cargo fmt --check"
    domain.Python, "implement" -> "python -m py_compile ."
    domain.Python, "unit-test" -> "python -m pytest -v"
    domain.Python, "coverage" -> "python -m coverage run -m pytest"
    domain.Python, "lint" -> "black --check ."
    domain.Python, "static" -> "mypy ."
    domain.Python, "integration" -> "python -m pytest -v"
    domain.Python, "security" -> "bandit -r ."
    domain.Python, "review" -> "grep -r TODO|FIXME|XXX|HACK"
    domain.Python, "accept" ->
      "python -m py_compile . && python -m pytest -v && black --check ."
    domain.Javascript, "implement" -> "npm run build"
    domain.Javascript, "unit-test" -> "npm test"
    domain.Javascript, "coverage" -> "npm run test:coverage"
    domain.Javascript, "lint" -> "npm run lint"
    domain.Javascript, "static" -> "npm run typecheck"
    domain.Javascript, "integration" -> "npm test -- integration"
    domain.Javascript, "security" -> "npm audit"
    domain.Javascript, "review" -> "grep -r TODO|FIXME|XXX|HACK"
    domain.Javascript, "accept" -> "npm run build && npm test && npm run lint"
    _, _ -> "unknown"
  }
}

fn estimate_duration(stage_name: String) -> Int {
  case stage_name {
    "implement" -> 5000
    "unit-test" -> 3000
    "coverage" -> 8000
    "lint" -> 2000
    "static" -> 4000
    "integration" -> 10_000
    "security" -> 6000
    "review" -> 1000
    "accept" -> 7000
    _ -> 1000
  }
}
