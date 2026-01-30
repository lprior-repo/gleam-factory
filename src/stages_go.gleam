import gleam/int
import gleam/result
import gleam/string
import process

pub fn execute_go_stage(stage_name: String, cwd: String) -> Result(Nil, String) {
  case stage_name {
    "implement" -> go_implement(cwd)
    "unit-test" -> go_unit_test(cwd)
    "coverage" -> go_coverage(cwd)
    "lint" -> go_lint(cwd)
    "static" -> go_static(cwd)
    "integration" -> go_integration(cwd)
    "security" -> go_security(cwd)
    "review" -> go_review(cwd)
    "accept" -> go_accept(cwd)
    other -> Error("Unknown Go stage: " <> other)
  }
}

fn go_implement(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("go"))
  use cmd_result <- result.try(process.run_command(
    "go",
    ["build", "./..."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Code does not compile" })
}

fn go_unit_test(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("go"))
  use cmd_result <- result.try(process.run_command(
    "go",
    ["test", "-v", "-short", "./..."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Tests failed" })
}

fn go_coverage(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "go",
    ["test", "-coverprofile=/tmp/coverage.out", "./..."],
    cwd,
  ))
  case cmd_result {
    process.Success(_, _, 0) -> Ok(Nil)
    process.Success(_, _, code) ->
      Error("Go: Tests failed with exit code: " <> int.to_string(code))
    process.Failure(err, _) -> Error(err)
  }
}

fn go_lint(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("gofmt", ["-l", "."], cwd))
  case cmd_result {
    process.Success(stdout, _, _) ->
      case string.is_empty(string.trim(stdout)) {
        True -> Ok(Nil)
        False -> Error("Go: Unformatted files:\n" <> stdout)
      }
    process.Failure(stderr, _) -> Error("Go: gofmt failed: " <> stderr)
  }
}

fn go_static(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("go", ["vet", "./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: go vet failed" })
}

fn go_integration(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "go",
    ["test", "-v", "./..."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Integration tests failed" })
}

fn go_security(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("gosec", ["./..."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Go: Security check failed" })
}

fn go_review(cwd: String) -> Result(Nil, String) {
  case
    process.run_command(
      "grep",
      ["-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.go", "."],
      cwd,
    )
  {
    Ok(process.Success(_, _, 0)) ->
      Error("Go: TODO/FIXME/XXX/HACK markers found")
    Ok(process.Success(_, _, 1)) -> Ok(Nil)
    Ok(process.Success(_, _, code)) ->
      Error("Grep failed with code: " <> int.to_string(code))
    Ok(process.Failure(err, _)) -> Error(err)
    Error(e) -> Error(e)
  }
}

fn go_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(go_implement(cwd))
  use _ <- result.try(go_unit_test(cwd))
  use _ <- result.try(go_lint(cwd))
  Ok(Nil)
}
