import gleam/int
import gleam/result
import process

pub fn execute_javascript_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "implement" -> javascript_implement(cwd)
    "unit-test" -> javascript_unit_test(cwd)
    "coverage" -> javascript_coverage(cwd)
    "lint" -> javascript_lint(cwd)
    "static" -> javascript_static(cwd)
    "integration" -> javascript_integration(cwd)
    "security" -> javascript_security(cwd)
    "review" -> javascript_review(cwd)
    "accept" -> javascript_accept(cwd)
    other -> Error("Unknown JavaScript stage: " <> other)
  }
}

fn javascript_implement(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("npm"))
  use cmd_result <- result.try(process.run_command("npm", ["run", "build"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "JavaScript: Build failed" })
}

fn javascript_unit_test(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("npm"))
  use cmd_result <- result.try(process.run_command("npm", ["test"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "JavaScript: Tests failed" })
}

fn javascript_coverage(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "npm",
    ["run", "test:coverage"],
    cwd,
  ))
  case cmd_result {
    process.Success(_, _, 0) -> Ok(Nil)
    process.Success(_, _, code) ->
      Error(
        "JavaScript: Coverage tests failed with code: " <> int.to_string(code),
      )
    process.Failure(err, _) -> Error(err)
  }
}

fn javascript_lint(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("npm", ["run", "lint"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "JavaScript: Linting failed" })
}

fn javascript_static(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "npm",
    ["run", "typecheck"],
    cwd,
  ))
  case cmd_result {
    process.Success(_, _, 0) -> Ok(Nil)
    process.Success(_, _, code) ->
      Error(
        "JavaScript: Type checking failed with code: " <> int.to_string(code),
      )
    process.Failure(err, _) -> Error(err)
  }
}

fn javascript_integration(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "npm",
    ["test", "--", "integration"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "JavaScript: Integration tests failed" })
}

fn javascript_security(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("npm", ["audit"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "JavaScript: Security audit failed" })
}

fn javascript_review(cwd: String) -> Result(Nil, String) {
  case
    process.run_command(
      "grep",
      [
        "-r",
        "TODO\\|FIXME\\|XXX\\|HACK",
        "--include=*.js",
        "--include=*.ts",
        ".",
      ],
      cwd,
    )
  {
    Ok(process.Success(_, _, 0)) ->
      Error("JavaScript: TODO/FIXME/XXX/HACK markers found")
    Ok(process.Success(_, _, 1)) -> Ok(Nil)
    Ok(process.Success(_, _, code)) ->
      Error("Grep failed with code: " <> int.to_string(code))
    Ok(process.Failure(err, _)) -> Error(err)
    Error(e) -> Error(e)
  }
}

fn javascript_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(javascript_implement(cwd))
  use _ <- result.try(javascript_unit_test(cwd))
  use _ <- result.try(javascript_lint(cwd))
  Ok(Nil)
}
