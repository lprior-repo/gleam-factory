import domain
import gleam/option.{None, Some}
import gleeunit/should
import stage_runner

// === StageResult Type Behavior ===

pub fn stage_success_captures_iterations_test() {
  let result = stage_runner.StageSuccess(iterations: 3, tokens_used: 100)
  case result {
    stage_runner.StageSuccess(iterations: 3, ..) -> should.be_true(True)
    stage_runner.StageSuccess(..) -> should.fail()
  }
}

pub fn stage_success_captures_tokens_used_test() {
  let result = stage_runner.StageSuccess(iterations: 1, tokens_used: 500)
  case result {
    stage_runner.StageSuccess(tokens_used: 500, ..) -> should.be_true(True)
    stage_runner.StageSuccess(..) -> should.fail()
  }
}

pub fn stage_failed_captures_reason_test() {
  let result =
    stage_runner.StageFailed(
      reason: "tests failed",
      iterations: 5,
      tokens_used: 1000,
    )
  case result {
    stage_runner.StageFailed(reason: "tests failed", ..) -> should.be_true(True)
    stage_runner.StageFailed(..) -> should.fail()
  }
}

pub fn stage_failed_captures_iterations_test() {
  let result =
    stage_runner.StageFailed(reason: "error", iterations: 10, tokens_used: 200)
  case result {
    stage_runner.StageFailed(iterations: 10, ..) -> should.be_true(True)
    stage_runner.StageFailed(..) -> should.fail()
  }
}

pub fn stage_failed_captures_tokens_used_test() {
  let result =
    stage_runner.StageFailed(reason: "error", iterations: 1, tokens_used: 2000)
  case result {
    stage_runner.StageFailed(tokens_used: 2000, ..) -> should.be_true(True)
    stage_runner.StageFailed(..) -> should.fail()
  }
}

// === CodeBlock Type Behavior ===

pub fn code_block_captures_content_test() {
  let block =
    stage_runner.CodeBlock(
      filename: None,
      content: "pub fn main() {}",
      lang: "gleam",
    )
  case block {
    stage_runner.CodeBlock(content: "pub fn main() {}", ..) ->
      should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_captures_language_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "rust")
  case block {
    stage_runner.CodeBlock(lang: "rust", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_captures_optional_filename_test() {
  let block =
    stage_runner.CodeBlock(
      filename: Some("src/main.gleam"),
      content: "code",
      lang: "gleam",
    )
  case block {
    stage_runner.CodeBlock(filename: Some("src/main.gleam"), ..) ->
      should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_can_have_no_filename_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "go")
  case block {
    stage_runner.CodeBlock(filename: None, ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

// === apply_fix Behavior ===

pub fn apply_fix_errors_when_no_code_blocks_test() {
  stage_runner.apply_fix(
    "Just some text without code",
    "/tmp/test",
    domain.Gleam,
  )
  |> should.be_error
}

pub fn apply_fix_errors_with_only_prose_test() {
  let response = "I'll fix this by implementing the function properly."
  stage_runner.apply_fix(response, "/tmp/test", domain.Go)
  |> should.be_error
}

// === Code Block Edge Cases ===

pub fn code_block_with_empty_content_test() {
  let block =
    stage_runner.CodeBlock(
      filename: Some("empty.gleam"),
      content: "",
      lang: "gleam",
    )
  case block {
    stage_runner.CodeBlock(content: "", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_with_multiline_content_test() {
  let content = "pub fn main() {\n  io.println(\"Hello\")\n}"
  let block =
    stage_runner.CodeBlock(filename: None, content: content, lang: "gleam")
  case block {
    stage_runner.CodeBlock(content: c, ..) -> {
      c |> should.equal(content)
    }
  }
}

// === Different Language Code Blocks ===

pub fn code_block_supports_gleam_lang_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "gleam")
  case block {
    stage_runner.CodeBlock(lang: "gleam", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_supports_go_lang_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "go")
  case block {
    stage_runner.CodeBlock(lang: "go", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_supports_rust_lang_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "rust")
  case block {
    stage_runner.CodeBlock(lang: "rust", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_supports_python_lang_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "python")
  case block {
    stage_runner.CodeBlock(lang: "python", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

pub fn code_block_supports_javascript_lang_test() {
  let block =
    stage_runner.CodeBlock(filename: None, content: "code", lang: "javascript")
  case block {
    stage_runner.CodeBlock(lang: "javascript", ..) -> should.be_true(True)
    stage_runner.CodeBlock(..) -> should.fail()
  }
}

// === StageResult Discrimination ===

pub fn can_discriminate_success_from_failure_test() {
  let success = stage_runner.StageSuccess(iterations: 1, tokens_used: 50)
  let failure =
    stage_runner.StageFailed(reason: "oops", iterations: 1, tokens_used: 50)

  case success {
    stage_runner.StageSuccess(..) -> should.be_true(True)
  }

  case failure {
    stage_runner.StageFailed(..) -> should.be_true(True)
  }
}

pub fn stage_result_zero_iterations_is_valid_test() {
  // Zero iterations means immediate success without feedback loop
  let result = stage_runner.StageSuccess(iterations: 0, tokens_used: 0)
  case result {
    stage_runner.StageSuccess(iterations: 0, tokens_used: 0) ->
      should.be_true(True)
    stage_runner.StageSuccess(..) -> should.fail()
  }
}
