import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import process

pub fn execute_rust_stage(
  stage_name: String,
  cwd: String,
) -> Result(Nil, String) {
  case stage_name {
    "implement" -> rust_implement(cwd)
    "unit-test" -> rust_unit_test(cwd)
    "coverage" -> rust_coverage(cwd)
    "lint" -> rust_lint(cwd)
    "static" -> rust_static(cwd)
    "integration" -> rust_integration(cwd)
    "security" -> rust_security(cwd)
    "review" -> rust_review(cwd)
    "accept" -> rust_accept(cwd)
    other -> Error("Unknown Rust stage: " <> other)
  }
}

fn rust_implement(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(process.command_exists("cargo"))
  use cmd_result <- result.try(process.run_command("cargo", ["build"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Code does not compile" })
}

fn rust_unit_test(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("cargo", ["test"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Tests failed" })
}

fn rust_coverage(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "cargo",
    ["tarpaulin", "--out", "Xml"],
    cwd,
  ))
  case cmd_result {
    process.Success(_, _, 0) -> Ok(Nil)
    process.Success(_, _, code) ->
      Error("Rust: Coverage check failed with code: " <> int.to_string(code))
    process.Failure(err, _) -> Error(err)
  }
}

fn rust_lint(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "cargo",
    ["fmt", "--check"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Code formatting issues" })
}

fn rust_static(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "cargo",
    ["clippy", "--all-targets"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Clippy failed" })
}

fn rust_integration(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command(
    "cargo",
    ["test", "--all"],
    cwd,
  ))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Integration tests failed" })
}

fn rust_security(cwd: String) -> Result(Nil, String) {
  use cmd_result <- result.try(process.run_command("cargo", ["audit"], cwd))
  process.check_success(cmd_result)
  |> result.map_error(fn(_) { "Rust: Security audit failed" })
}

fn rust_review(cwd: String) -> Result(Nil, String) {
  case
    process.run_command(
      "grep",
      ["-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.rs", "."],
      cwd,
    )
  {
    Ok(process.Success(_, _, 0)) ->
      Error("Rust: TODO/FIXME/XXX/HACK markers found")
    Ok(process.Success(_, _, 1)) -> Ok(Nil)
    Ok(process.Success(_, _, code)) ->
      Error("Grep failed with code: " <> int.to_string(code))
    Ok(process.Failure(err, _)) -> Error(err)
    Error(e) -> Error(e)
  }
}

fn rust_accept(cwd: String) -> Result(Nil, String) {
  use _ <- result.try(rust_implement(cwd))
  use _ <- result.try(rust_unit_test(cwd))
  use _ <- result.try(rust_lint(cwd))
  Ok(Nil)
}
