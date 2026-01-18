//// Verification gauntlet for running ordered verification stages.
////
//// Runs build, test, lint stages in order, stopping on first failure.

import process as shell_process

pub type GauntletStage {
  Build
  Test
  Lint
  Format
}

pub type GauntletResult {
  Passed(stages_run: Int)
  Failed(stage: GauntletStage, error: String)
}

pub fn run_gauntlet(
  workspace_path: String,
  language: String,
) -> Result(GauntletResult, String) {
  let stages = stages_for_language(language)
  run_stages(workspace_path, stages, 0)
}

fn stages_for_language(language: String) -> List(GauntletStage) {
  case language {
    "gleam" -> [Build, Test, Format]
    "rust" -> [Build, Test, Lint]
    "go" -> [Build, Test, Lint, Format]
    _ -> [Build, Test]
  }
}

fn run_stages(
  path: String,
  stages: List(GauntletStage),
  count: Int,
) -> Result(GauntletResult, String) {
  case stages {
    [] -> Ok(Passed(stages_run: count))
    [stage, ..rest] -> {
      case run_stage(path, stage) {
        Ok(Nil) -> run_stages(path, rest, count + 1)
        Error(err) -> Ok(Failed(stage:, error: err))
      }
    }
  }
}

fn run_stage(path: String, stage: GauntletStage) -> Result(Nil, String) {
  let #(cmd, args) = stage_command(stage)
  case shell_process.run_command(cmd, args, path) {
    Ok(shell_process.Success(_, _, _)) -> Ok(Nil)
    Ok(shell_process.Failure(err, _)) -> Error(err)
    Error(e) -> Error(e)
  }
}

fn stage_command(stage: GauntletStage) -> #(String, List(String)) {
  case stage {
    Build -> #("gleam", ["build"])
    Test -> #("gleam", ["test"])
    Lint -> #("gleam", ["check"])
    Format -> #("gleam", ["format", "--check"])
  }
}

pub fn stage_name(stage: GauntletStage) -> String {
  case stage {
    Build -> "build"
    Test -> "test"
    Lint -> "lint"
    Format -> "format"
  }
}

pub fn is_passed(result: GauntletResult) -> Bool {
  case result {
    Passed(_) -> True
    Failed(_, _) -> False
  }
}
