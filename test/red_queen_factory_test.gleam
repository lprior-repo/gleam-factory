import gleam/int
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import process

pub fn main() {
  gleeunit.main()
}

fn factory_run(args: List(String)) -> Result(String, String) {
  use result <- result.try(process.run_command(
    "gleam",
    ["run", "--module=factory/main", ..args],
    "",
  ))
  case result {
    process.Success(stdout, _, code) if code == 0 -> Ok(stdout)
    process.Failure(stderr, code) ->
      Error("Exit code " <> int.to_string(code) <> ": " <> stderr)
    _ -> Error("Unexpected error")
  }
}

fn factory_run_expect_success(args: List(String)) -> String {
  case factory_run(args) {
    Ok(output) -> output
    Error(msg) -> panic as msg
  }
}

fn factory_run_expect_error(args: List(String)) -> String {
  case factory_run(args) {
    Error(msg) -> msg
    Ok(_) -> panic as "Expected error but got success"
  }
}

pub fn red_queen_attack_001_happy_path_single_stage() {
  let output = factory_run_expect_success(["new", "-s", "rq-001"])
  string.contains(output, "Created:")
  |> should.be_true()
  string.contains(output, "Branch:")
  |> should.be_true()

  let show_output = factory_run_expect_success(["show", "-s", "rq-001"])
  string.contains(show_output, "rq-001")
  |> should.be_true()
  string.contains(show_output, "created")
  |> should.be_true()
}

pub fn red_queen_attack_002_stage_range_from_to() {
  factory_run_expect_success(["new", "-s", "rq-002"])

  let dry_run =
    factory_run_expect_success([
      "stage",
      "-s",
      "rq-002",
      "--stage",
      "unit-test",
      "--from",
      "implement",
      "--to",
      "lint",
      "-d",
    ])

  string.contains(dry_run, "DRY RUN")
  |> should.be_true()
}

pub fn red_queen_attack_003_missing_slug_fails() {
  let error = factory_run_expect_error(["new"])
  string.contains(error, "--slug is required")
  |> should.be_true()
}

pub fn red_queen_attack_004_invalid_slug_chars() {
  let error = factory_run_expect_error(["new", "-s", "invalid!slug"])
  string.contains(error, "invalid characters")
  |> should.be_true()
}

pub fn red_queen_attack_005_empty_slug_fails() {
  let error = factory_run_expect_error(["new", "-s", ""])
  string.contains(error, "cannot be empty")
  |> should.be_true()
}

pub fn red_queen_attack_006_stage_range_invalid() {
  factory_run_expect_success(["new", "-s", "rq-006"])

  let error =
    factory_run_expect_error([
      "stage",
      "-s",
      "rq-006",
      "--stage",
      "implement",
      "--from",
      "lint",
      "--to",
      "implement",
    ])

  string.contains(error, "must come before")
  |> should.be_true()
}

pub fn red_queen_attack_007_nonexistent_stage() {
  factory_run_expect_success(["new", "-s", "rq-007"])

  let error =
    factory_run_expect_error(["stage", "-s", "rq-007", "--stage", "fake-stage"])

  string.contains(error, "unknown stage")
  |> should.be_true()
}

pub fn red_queen_attack_008_approve_without_stages() {
  factory_run_expect_success(["new", "-s", "rq-008"])

  let error = factory_run_expect_error(["approve", "-s", "rq-008"])
  string.contains(error, "no stages have been passed")
  |> should.be_true()
}

pub fn red_queen_attack_009_force_approve_bypasses() {
  factory_run_expect_success(["new", "-s", "rq-009"])

  let output = factory_run_expect_success(["approve", "-s", "rq-009", "-f"])
  string.contains(output, "Approved")
  |> should.be_true()
}

pub fn red_queen_attack_010_list_filters() {
  factory_run_expect_success(["new", "-s", "rq-010-a"])
  factory_run_expect_success(["new", "-s", "rq-010-b"])

  let output = factory_run_expect_success(["list", "--status", "open"])
  string.contains(output, "rq-010-a")
  |> should.be_true()
  string.contains(output, "rq-010-b")
  |> should.be_true()
}

pub fn red_queen_attack_011_stage_from_without_to() {
  factory_run_expect_success(["new", "-s", "rq-011"])

  let dry_run =
    factory_run_expect_success([
      "stage",
      "-s",
      "rq-011",
      "--stage",
      "lint",
      "--from",
      "implement",
      "-d",
    ])

  string.contains(dry_run, "DRY RUN")
  |> should.be_true()
}

pub fn red_queen_attack_012_stage_to_without_from() {
  factory_run_expect_success(["new", "-s", "rq-012"])

  let dry_run =
    factory_run_expect_success([
      "stage",
      "-s",
      "rq-012",
      "--stage",
      "implement",
      "--to",
      "unit-test",
      "-d",
    ])

  string.contains(dry_run, "DRY RUN")
  |> should.be_true()
}
