//// Stage Runner - Connects stages to feedback loop for auto-heal.
////
//// Executes stages with retry via LLM on failure.

import domain
import feedback_loop
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile
import stages

/// Result of running a stage with retries
pub type StageResult {
  StageSuccess(iterations: Int, tokens_used: Int)
  StageFailed(reason: String, iterations: Int, tokens_used: Int)
}

/// Run a stage with feedback loop retry on failure
pub fn run_stage(
  stage_name: String,
  language: domain.Language,
  workspace: String,
  config: feedback_loop.FeedbackConfig,
  task_spec: String,
) -> StageResult {
  case stages.execute_stage(stage_name, language, workspace) {
    Ok(_) -> StageSuccess(iterations: 0, tokens_used: 0)
    Error(reason) ->
      run_with_feedback(
        stage_name,
        language,
        workspace,
        config,
        task_spec,
        reason,
      )
  }
}

fn run_with_feedback(
  stage_name: String,
  language: domain.Language,
  workspace: String,
  config: feedback_loop.FeedbackConfig,
  task_spec: String,
  _initial_error: String,
) -> StageResult {
  let test_cmd = stage_to_command(stage_name, language)
  let test_args = stage_to_args(stage_name, language)

  let apply = fn(response: String) -> Result(Nil, String) {
    apply_fix(response, workspace, language)
  }

  case
    feedback_loop.run_loop(
      config,
      task_spec,
      test_cmd,
      test_args,
      workspace,
      apply,
    )
  {
    feedback_loop.Success(iters, tokens) -> StageSuccess(iters, tokens)
    feedback_loop.Failure(reason, iters, tokens) ->
      StageFailed(reason, iters, tokens)
  }
}

fn stage_to_command(_stage_name: String, language: domain.Language) -> String {
  case language {
    domain.Gleam -> "gleam"
    domain.Go -> "go"
    domain.Rust -> "cargo"
    domain.Python -> "python"
  }
}

fn stage_to_args(stage_name: String, language: domain.Language) -> List(String) {
  case language, stage_name {
    domain.Gleam, "implement" -> ["build"]
    domain.Gleam, "unit-test" -> ["test"]
    domain.Gleam, "lint" -> ["format", "--check", "."]
    domain.Gleam, "static" -> ["check"]
    domain.Gleam, _ -> ["test"]
    domain.Go, "implement" -> ["build", "./..."]
    domain.Go, "unit-test" -> ["test", "-v", "-short", "./..."]
    domain.Go, "lint" -> ["-l", "."]
    domain.Go, "static" -> ["vet", "./..."]
    domain.Go, _ -> ["test", "./..."]
    domain.Rust, "implement" -> ["build"]
    domain.Rust, "unit-test" -> ["test"]
    domain.Rust, "lint" -> ["fmt", "--check"]
    domain.Rust, "static" -> ["clippy", "--all-targets"]
    domain.Rust, _ -> ["test"]
    domain.Python, "implement" -> ["-m", "py_compile", "."]
    domain.Python, "unit-test" -> ["-m", "pytest", "-v"]
    domain.Python, "lint" -> ["-m", "black", "--check", "."]
    domain.Python, "static" -> ["-m", "mypy", "."]
    domain.Python, _ -> ["-m", "pytest"]
  }
}

/// Parse LLM response for code blocks and write to files
pub fn apply_fix(
  response: String,
  workspace: String,
  language: domain.Language,
) -> Result(Nil, String) {
  let blocks = extract_code_blocks(response)
  case blocks {
    [] -> Error("No code blocks found in response")
    _ -> write_code_blocks(blocks, workspace, language)
  }
}

/// Code block with optional filename
pub type CodeBlock {
  CodeBlock(filename: Option(String), content: String, lang: String)
}

/// Extract fenced code blocks from markdown response
fn extract_code_blocks(text: String) -> List(CodeBlock) {
  extract_blocks_acc(text, [])
}

fn extract_blocks_acc(text: String, acc: List(CodeBlock)) -> List(CodeBlock) {
  case find_code_block(text) {
    None -> acc
    Some(#(block, rest)) -> extract_blocks_acc(rest, [block, ..acc])
  }
}

fn find_code_block(text: String) -> Option(#(CodeBlock, String)) {
  case string.split_once(text, "```") {
    Error(_) -> None
    Ok(#(_, after_fence)) -> parse_block_content(after_fence)
  }
}

fn parse_block_content(text: String) -> Option(#(CodeBlock, String)) {
  case string.split_once(text, "\n") {
    Error(_) -> None
    Ok(#(first_line, rest)) -> {
      let lang_and_file = string.trim(first_line)
      case string.split_once(rest, "```") {
        Error(_) -> None
        Ok(#(content, remaining)) -> {
          let #(lang, filename) = parse_lang_line(lang_and_file)
          Some(#(
            CodeBlock(filename:, content: string.trim(content), lang:),
            remaining,
          ))
        }
      }
    }
  }
}

fn parse_lang_line(line: String) -> #(String, Option(String)) {
  case string.split_once(line, " ") {
    Error(_) -> #(line, None)
    Ok(#(lang, rest)) -> {
      let trimmed = string.trim(rest)
      case trimmed {
        "" -> #(lang, None)
        filename -> #(lang, Some(filename))
      }
    }
  }
}

fn write_code_blocks(
  blocks: List(CodeBlock),
  workspace: String,
  language: domain.Language,
) -> Result(Nil, String) {
  case blocks {
    [] -> Ok(Nil)
    [block, ..rest] -> {
      case write_block(block, workspace, language) {
        Ok(_) -> write_code_blocks(rest, workspace, language)
        Error(e) -> Error(e)
      }
    }
  }
}

fn write_block(
  block: CodeBlock,
  workspace: String,
  language: domain.Language,
) -> Result(Nil, String) {
  case block.filename {
    Some(filename) -> write_to_file(workspace, filename, block.content)
    None -> infer_and_write(block, workspace, language)
  }
}

fn write_to_file(
  workspace: String,
  filename: String,
  content: String,
) -> Result(Nil, String) {
  let path = build_path(workspace, filename)
  simplifile.write(path, content)
  |> result.map_error(fn(_) { "Failed to write: " <> path })
}

fn build_path(workspace: String, filename: String) -> String {
  case string.ends_with(workspace, "/") {
    True -> workspace <> filename
    False -> workspace <> "/" <> filename
  }
}

fn infer_and_write(
  block: CodeBlock,
  workspace: String,
  language: domain.Language,
) -> Result(Nil, String) {
  let ext = language_extension(language)
  case block.lang == ext || block.lang == language_name(language) {
    True -> {
      let filename = "src/fix" <> "." <> ext
      write_to_file(workspace, filename, block.content)
    }
    False -> Ok(Nil)
  }
}

fn language_extension(lang: domain.Language) -> String {
  case lang {
    domain.Gleam -> "gleam"
    domain.Go -> "go"
    domain.Rust -> "rs"
    domain.Python -> "py"
  }
}

fn language_name(lang: domain.Language) -> String {
  case lang {
    domain.Gleam -> "gleam"
    domain.Go -> "go"
    domain.Rust -> "rust"
    domain.Python -> "python"
  }
}
