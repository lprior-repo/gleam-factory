import gleam/dict
import gleam/option

pub type ArenaSurface {
  CliCommand(CliCommandData)
  HttpEndpoint
  UiFlow
}

pub type CliCommandData {
  CliCommandData(
    name: String,
    args_schema: List(ArgSpec),
    stdin_behavior: StdinBehavior,
    stdout_format: OutputFormat,
  )
}

pub type ArgSpec {
  RequiredArg(name: String, help: String)
  OptionalArg(name: String, default: String, help: String)
  Flag(name: String, short: String, help: String)
}

pub type StdinBehavior {
  NoStdin
  OptionalStdin
  RequiredStdin
}

pub type OutputFormat {
  PlainText
  JsonOutput
  ColorizedText
  HumanReadable
}

pub type TestCase {
  TestCase(
    surface: ArenaSurface,
    input: TestInput,
    expected_output: ExpectedOutput,
    timeout_ms: Int,
  )
}

pub type TestInput {
  CliInput(args: List(String), stdin: option.Option(String))
  HttpInput(body: String, headers: dict.Dict(String, String))
  UiInput(actions: List(String), state: dict.Dict(String, String))
}

pub type ExpectedOutput {
  ExitCode(code: Int)
  OutputContains(text: String)
  OutputMatches(pattern: String)
}

pub type TestResult {
  TestPassed(duration_ms: Int)
  TestFailed(reason: String, output: String)
  TestTimeout(duration_ms: Int)
}

pub type ArenaError {
  InvalidSurface(surface: String)
  ExecutionFailed(msg: String)
  ValidationFailed(msg: String)
}

pub fn gleam_run_command() -> ArenaSurface {
  CliCommand(CliCommandData(
    name: "gleam run",
    args_schema: [
      OptionalArg(name: "module", default: "", help: "Module to run"),
      Flag(name: "target", short: "-t", help: "Target runtime"),
    ],
    stdin_behavior: NoStdin,
    stdout_format: PlainText,
  ))
}

pub fn gleam_test_command() -> ArenaSurface {
  CliCommand(CliCommandData(
    name: "gleam test",
    args_schema: [
      OptionalArg(name: "target", default: "erlang", help: "Target runtime"),
      Flag(name: "coverage", short: "-c", help: "Enable coverage"),
    ],
    stdin_behavior: NoStdin,
    stdout_format: ColorizedText,
  ))
}

pub fn all_gleam_commands() -> List(ArenaSurface) {
  [
    gleam_run_command(),
    gleam_test_command(),
  ]
}
