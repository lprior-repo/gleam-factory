import gleam/int
import gleam/result
import process

pub fn execute_python_stage(stage_name: String, cwd: String) -> Result(Nil, String) {
  case stage_name {
    "implement" -> python_implement(cwd)
    "unit-test" -> python_unit_test(cwd)
    "coverage" -> python_coverage(cwd)
    "lint" -> python_lint(cwd)
    "static" -> python_static(cwd)
    "integration" -> python_integration(cwd)
    "security" -> python_security(cwd)
    "review" -> python_review(cwd)
    "accept" -> python_accept(cwd)
    other -> Error("Unknown Python stage: " <> other)
  }
}

fn python_implement(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("python"))
  use cmd_result <- result.try(process.run_command(
    "python",
    ["-m", "py_compile", "."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Code does not compile" })
}

fn python_unit_test(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "python",
    ["-m", "pytest", "-v"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Tests failed" })
}

fn python_coverage(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "python",
    ["-m", "coverage", "run", "-m", "pytest"],
    cwd,
  ))
  case cmd_result {
    process.Success(_, _, 0) -> Ok(Nil)
    process.Success(_, _, code) ->
      Error("Python: Coverage tests failed with code: " <> int.to_string(code))
    process.Failure(err, _) -> Error(err)
  }
}

fn python_lint(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "black",
    ["--check", "."],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Code formatting issues" })
}

fn python_static(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("mypy", ["."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Type checking failed" })
}

fn python_integration(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "python",
    ["-m", "pytest", "-v"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Integration tests failed" })
}

fn python_security(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("bandit", ["-r", "."], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Python: Security scan failed" })
}

fn python_review(cwd: String) -> Result(Nil, String) {
  case
    process.run_command(
      "grep",
      ["-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.py", "."],
      cwd,
    )
  {
    Ok(process.Success(_, _, 0)) ->
      Error("Python: TODO/FIXME/XXX/HACK markers found")
    Ok(process.Success(_, _, 1)) -> Ok(Nil)
    Ok(process.Success(_, _, code)) ->
      Error("Grep failed with code: " <> int.to_string(code))
    Ok(process.Failure(err, _)) -> Error(err)
    Error(e) -> Error(e)
  }
}

fn python_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(python_implement(cwd))
  use _ <- result.try(python_unit_test(cwd))
  use _ <- result.try(python_lint(cwd))
  Ok(Nil)
}
