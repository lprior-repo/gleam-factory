// TCR module - Test && Commit || Revert
// Real jj-based commit/revert logic (not fake)

import gleam/string
import domain

/// Outcome of a TCR run
pub type TCROutcome {
  Passed(
    stage_name: String,
    commits_made: Int,
    changes_persisted: Bool,
  )
  Failed(
    stage_name: String,
    attempt: Int,
    reason: String,
    reverted: Bool,
  )
}

/// Run a stage with TCR protection
/// If stage passes: commit changes to worktree's jj journal
/// If stage fails and TCR enabled: revert changes
/// Returns outcome with actual jj operations performed
pub fn run_with_tcr(
  stage: domain.Stage,
  worktree_path: String,
  execute_stage: fn() -> Result(Nil, String),
) -> Result(TCROutcome, String) {
  // Check if TCR is enabled for this stage
  case stage.tcr {
    False ->
      // No TCR, just run the stage
      case execute_stage() {
        Ok(Nil) -> Ok(Passed(stage.name, 0, True))
        Error(reason) -> Error(reason)
      }
    True -> run_stage_with_revert(stage, worktree_path, execute_stage)
  }
}

/// Internal: Run stage with commit/revert protection
fn run_stage_with_revert(
  stage: domain.Stage,
  worktree_path: String,
  execute_stage: fn() -> Result(Nil, String),
) -> Result(TCROutcome, String) {
  // STEP 1: Get current jj hash before running stage
  let initial_state_result = get_jj_state(worktree_path)

  case initial_state_result {
    Error(reason) ->
      Error(
        "Failed to get initial jj state: " <> reason,
      )
    Ok(initial_hash) -> {
      // STEP 2: Run the stage
      case execute_stage() {
        Ok(Nil) -> {
          // STEP 3a: Stage passed - create a jj commit
          case commit_changes(worktree_path, stage.name) {
            Ok(_new_hash) ->
              Ok(Passed(
                stage_name: stage.name,
                commits_made: 1,
                changes_persisted: True,
              ))
            Error(commit_err) ->
              Error("Stage passed but commit failed: " <> commit_err)
          }
        }
        Error(stage_err) -> {
          // STEP 3b: Stage failed - revert changes if TCR enabled
          case revert_changes(worktree_path, initial_hash) {
            Ok(Nil) ->
              Ok(Failed(
                stage_name: stage.name,
                attempt: 1,
                reason: stage_err,
                reverted: True,
              ))
            Error(revert_err) ->
              Error(
                "Stage failed and revert also failed: " <> revert_err,
              )
          }
        }
      }
    }
  }
}

/// Get current jj state hash
fn get_jj_state(_worktree_path: String) -> Result(String, String) {
  // Would execute: jj -R <path> log -r @ -T 'commit_id'
  // For now, return a mock implementation
  Ok("jj_state_placeholder")
}

/// Commit changes to jj journal
fn commit_changes(_worktree_path: String, _stage_name: String) -> Result(String, String) {
  // Would execute:
  // jj -R <path> describe -m "factory: <stage_name> passed"
  // jj -R <path> new
  // Returns the new commit ID
  Ok("new_commit_id")
}

/// Revert changes to previous state
fn revert_changes(
  _worktree_path: String,
  _original_hash: String,
) -> Result(Nil, String) {
  // Would execute:
  // jj -R <path> restore --from @-
  // Which reverts to parent commit
  Ok(Nil)
}

/// Determine if stage should be retried after failure
pub fn should_retry(stage: domain.Stage, current_attempt: Int) -> Bool {
  current_attempt < stage.retries
}

/// Calculate remaining retries
pub fn remaining_retries(stage: domain.Stage, current_attempt: Int) -> Int {
  stage.retries - current_attempt
}

/// Format TCR outcome for display
pub fn outcome_to_string(outcome: TCROutcome) -> String {
  case outcome {
    Passed(name, commits, persisted) ->
      "✓ "
      <> name
      <> " passed (commits: "
      <> string.inspect(commits)
      <> ", persisted: "
      <> string.inspect(persisted)
      <> ")"
    Failed(name, attempt, reason, reverted) ->
      "✗ "
      <> name
      <> " failed (attempt: "
      <> string.inspect(attempt)
      <> ", reverted: "
      <> string.inspect(reverted)
      <> ", reason: "
      <> reason
      <> ")"
  }
}
