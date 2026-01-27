import gleam/dict
import gleam/list
import gleam/option
import gleam/string

pub type TestCase {
  TestCase(test_name: String, file_path: String, line_number: Int)
}

pub type Patch {
  Patch(commit_hash: String, patch_id: String, timestamp: String)
}

pub type Severity {
  Critical
  Major
  Minor
}

pub type Regression {
  Regression(
    test_case: TestCase,
    introduced_in: Patch,
    first_seen_at: String,
    severity: Severity,
    failure_output: String,
  )
}

pub type RegressionHistory {
  RegressionHistory(
    regressions: List(Regression),
    by_test_case: dict.Dict(String, List(Regression)),
    by_severity: dict.Dict(String, List(Regression)),
  )
}

pub type TestResult {
  TestResult(
    test_case: TestCase,
    passed: Bool,
    output: String,
    executed_at: String,
  )
}

pub type TestRun {
  TestRun(patch: Patch, results: List(TestResult))
}

pub fn new_history() -> RegressionHistory {
  RegressionHistory(
    regressions: [],
    by_test_case: dict.new(),
    by_severity: dict.new(),
  )
}

pub fn detect_regressions(
  old_run: TestRun,
  new_run: TestRun,
) -> List(Regression) {
  let old_results = results_by_test_key(old_run.results)
  let _new_results = results_by_test_key(new_run.results)

  new_run.results
  |> list.filter(fn(result) {
    case dict.get(old_results, test_key(result.test_case)) {
      Ok(old_result) -> {
        !result.passed && old_result.passed
      }
      Error(_) -> False
    }
  })
  |> list.map(fn(failure) { promote_to_regression(failure, new_run.patch) })
}

pub fn is_permanent(regression: Regression) -> Bool {
  case regression.severity {
    Critical -> True
    Major -> True
    Minor -> False
  }
}

pub fn promote_to_regression(failure: TestResult, patch: Patch) -> Regression {
  let severity = classify_severity(failure)

  Regression(
    test_case: failure.test_case,
    introduced_in: patch,
    first_seen_at: failure.executed_at,
    severity: severity,
    failure_output: failure.output,
  )
}

pub fn classify_severity(result: TestResult) -> Severity {
  let output = result.output

  case is_critical(output) {
    True -> Critical
    False ->
      case is_major(output) {
        True -> Major
        False -> Minor
      }
  }
}

fn is_critical(output: String) -> Bool {
  string.contains(output, "panic:") || string.contains(output, "fatal")
}

fn is_major(output: String) -> Bool {
  string.contains(output, "Error:") || string.contains(output, "FAILED")
}

pub fn record_regression(
  history: RegressionHistory,
  regression: Regression,
) -> RegressionHistory {
  let key = test_key(regression.test_case)
  let severity_key = severity_to_string(regression.severity)

  RegressionHistory(
    regressions: [regression, ..history.regressions],
    by_test_case: dict.upsert(history.by_test_case, key, fn(opt) {
      case opt {
        option.Some(list) -> [regression, ..list]
        option.None -> [regression]
      }
    }),
    by_severity: dict.upsert(history.by_severity, severity_key, fn(opt) {
      case opt {
        option.Some(list) -> [regression, ..list]
        option.None -> [regression]
      }
    }),
  )
}

fn test_key(test_case: TestCase) -> String {
  test_case.file_path <> ":" <> test_case.test_name
}

fn severity_to_string(severity: Severity) -> String {
  case severity {
    Critical -> "critical"
    Major -> "major"
    Minor -> "minor"
  }
}

fn results_by_test_key(
  results: List(TestResult),
) -> dict.Dict(String, TestResult) {
  results
  |> list.map(fn(r) { #(test_key(r.test_case), r) })
  |> dict.from_list
}
