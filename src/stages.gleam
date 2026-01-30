import domain
import gleam/dict
import logging
import stages_gleam
import stages_go
import stages_javascript
import stages_python
import stages_rust

fn log_stage_start(stage_name: String, lang_str: String, worktree_path: String) {
  logging.log(
    logging.Info,
    "Stage starting",
    dict.from_list([
      #("stage", stage_name),
      #("language", lang_str),
      #("path", worktree_path),
    ]),
  )
}

fn log_stage_complete(stage_name: String, lang_str: String) {
  logging.log(
    logging.Info,
    "Stage completed",
    dict.from_list([#("stage", stage_name), #("language", lang_str)]),
  )
}

fn log_stage_failed(stage_name: String, lang_str: String, error: String) {
  logging.log(
    logging.Error,
    "Stage failed",
    dict.from_list([
      #("stage", stage_name),
      #("language", lang_str),
      #("error", error),
    ]),
  )
}

pub fn execute_stage(
  stage_name: String,
  language: domain.Language,
  worktree_path: String,
) -> Result(Nil, String) {
  let lang_str = domain.language_display_name(language)
  log_stage_start(stage_name, lang_str, worktree_path)

  let result = case language {
    domain.Go -> stages_go.execute_go_stage(stage_name, worktree_path)
    domain.Gleam -> stages_gleam.execute_gleam_stage(stage_name, worktree_path)
    domain.Rust -> stages_rust.execute_rust_stage(stage_name, worktree_path)
    domain.Python ->
      stages_python.execute_python_stage(stage_name, worktree_path)
    domain.Javascript ->
      stages_javascript.execute_javascript_stage(stage_name, worktree_path)
  }

  case result {
    Ok(Nil) -> log_stage_complete(stage_name, lang_str)
    Error(err) -> log_stage_failed(stage_name, lang_str, err)
  }

  result
}
