import gleam/int
import gleam/result
import process
import stages_test_mode

pub fn execute_gleam_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "implement" -> gleam_implement(cwd)
    "unit-test" -> gleam_unit_test(cwd)
    "coverage" -> gleam_coverage(cwd)
    "lint" -> gleam_lint(cwd)
    "static" -> gleam_static(cwd)
    "integration" -> gleam_integration(cwd)
    "security" -> gleam_security(cwd)
    "review" -> gleam_review(cwd)
    "accept" -> gleam_accept(cwd)
    other -> Error("Unknown Gleam stage: " <> other)
  }
}

fn gleam_implement(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("gleam"))
  use cmd_result <- result.try(process.run_command("gleam", ["build"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Code does not compile" })
}

fn gleam_unit_test(cwd: String) -> Result(Nil, String) {
  case stages_test_mode.is_test_mode() {
    True -> Ok(Nil)
    False -> {
      use _ <- result.try(process.command_exists("gleam"))
      use cmd_result <- result.try(process.run_command("gleam", ["test"], cwd))
      process.check_success(cmd_result)
      |> result.map_error(fn(_) { "Gleam: Tests failed" })
    }
  }
}

fn gleam_coverage(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "find",
    [".", "-name", "*_test.gleam", "-o", "-name", "test_*.gleam"],
    cwd,
  ))
  case cmd_result {
    process.Success(_, _, 0) -> Ok(Nil)
    process.Success(_, _, 1) -> Error("Gleam: No test files found")
    process.Success(_, _, code) ->
      Error("Gleam: find failed with code: " <> int.to_string(code))
    process.Failure(err, _) -> Error(err)
  }
}

fn gleam_lint(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "gleam",
    ["format", "--check", "."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) {
    "Gleam: Code formatting issues. Run: gleam format ."
  })
}

fn gleam_static(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("gleam", ["check"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Type checking failed" })
}

fn gleam_integration(cwd: String) -> Result(Nil, String) {
  case stages_test_mode.is_test_mode() {
    True -> Ok(Nil)
    False -> {
      use cmd_result <- result.try(process.run_command("gleam", ["test"], cwd))
      process.check_success(cmd_result)
      |> result.map_error(fn(_) { "Gleam: Integration tests failed" })
    }
  }
}

fn gleam_security(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "gleam",
    ["deps", "download"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Gleam: Dependency validation failed" })
}

fn gleam_review(cwd: String) -> Result(Nil, String) {
  case
    process.run_command(
      "grep",
      ["-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.gleam", "."],
      cwd,
    )
  {
    Ok(process.Success(_, _, 0)) ->
      Error("Gleam: TODO/FIXME/XXX/HACK markers found")
    Ok(process.Success(_, _, 1)) -> Ok(Nil)
    Ok(process.Success(_, _, code)) ->
      Error("Grep failed with code: " <> int.to_string(code))
    Ok(process.Failure(err, _)) -> Error(err)
    Error(e) -> Error(e)
  }
}

fn gleam_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(gleam_implement(cwd))
  use _ <- result.try(gleam_unit_test(cwd))
  use _ <- result.try(gleam_lint(cwd))
  Ok(Nil)
}
