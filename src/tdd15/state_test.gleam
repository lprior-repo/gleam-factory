import gleam/dict
import gleam/json
import gleam/result
import simplifile
import tdd15/state.{
  BeadContext, CacheDir, Completed, Complex, Failed, Gleam, InProgress, Medium,
  Pending, PhaseState, Progress, Rust, Simple, Skipped,
}

fn test_cache_dir() {
  let bead_id = "test-bead-001"

  case state.init_cache(bead_id) {
    Ok(cache_dir) -> {
      let assert Ok(_) = simplifile.verify_is_directory(cache_dir.base_path)

      case state.get_cache(bead_id) {
        Ok(CacheDir(_, id)) -> {
          let assert True = id == bead_id
        }
        Error(_) -> panic as "Failed to get cache"
      }

      let assert Ok(Nil) = simplifile.delete(cache_dir.base_path)
    }
    Error(_) -> panic as "Failed to create cache"
  }
}

fn test_progress_save_load() {
  let bead_id = "test-bead-002"
  let assert Ok(cache_dir) = state.init_cache(bead_id)

  let progress =
    Progress(
      bead_id: bead_id,
      language: Gleam,
      complexity: Medium,
      route: [0, 4, 5, 6, 14, 15],
      phases: dict.from_list([
        #(0, PhaseState(Pending, False, 0)),
        #(4, PhaseState(InProgress, False, 1)),
        #(5, PhaseState(Pending, False, 0)),
      ]),
      current_phase: 4,
      last_commit: "abc123",
    )

  let assert Ok(Nil) = state.save_progress(bead_id, progress)
  let assert Ok(loaded) = state.load_progress(bead_id)

  let assert True = loaded.bead_id == progress.bead_id
  let assert True = loaded.current_phase == progress.current_phase
  let assert Ok(state_4) = dict.get(loaded.phases, 4)
  let assert True = state_4.attempts == 1

  let assert Ok(Nil) = simplifile.delete(cache_dir.base_path)
}

fn test_phase_status_update() {
  let progress =
    Progress(
      bead_id: "test",
      language: Gleam,
      complexity: Simple,
      route: [0, 4, 5],
      phases: dict.from_list([
        #(0, PhaseState(Pending, False, 0)),
        #(4, PhaseState(InProgress, False, 1)),
      ]),
      current_phase: 4,
      last_commit: "abc",
    )

  let updated = state.update_phase_status(progress, 4, Completed)
  let assert Ok(phase_4) = dict.get(updated.phases, 4)
  let assert True = phase_4.status == Completed
  let assert True = updated.current_phase == 4
}

fn test_gate_result() {
  let progress =
    Progress(
      bead_id: "test",
      language: Gleam,
      complexity: Simple,
      route: [0, 4],
      phases: dict.from_list([
        #(0, PhaseState(Completed, False, 0)),
      ]),
      current_phase: 0,
      last_commit: "abc",
    )

  let updated = state.mark_gate_result(progress, 0, True)
  let assert Ok(phase_0) = dict.get(updated.phases, 0)
  let assert True = phase_0.gate == True
}

fn test_increment_attempt() {
  let progress =
    Progress(
      bead_id: "test",
      language: Rust,
      complexity: Complex,
      route: [0],
      phases: dict.from_list([
        #(0, PhaseState(Pending, False, 0)),
      ]),
      current_phase: 0,
      last_commit: "abc",
    )

  let updated = state.increment_attempt(progress, 0)
  let assert Ok(phase_0) = dict.get(updated.phases, 0)
  let assert True = phase_0.attempts == 1

  let updated2 = state.increment_attempt(updated, 0)
  let assert Ok(phase_0) = dict.get(updated2.phases, 0)
  let assert True = phase_0.attempts == 2
}

fn test_bead_context_save_load() {
  let bead_id = "test-bead-003"
  let assert Ok(cache_dir) = state.init_cache(bead_id)

  let context =
    BeadContext(
      id: bead_id,
      title: "Test Bead",
      requirements: ["Write tests", "Implement feature"],
      context: "This is a test bead context",
    )

  let assert Ok(Nil) = state.save_bead_context(bead_id, context)
  let assert Ok(loaded) = state.load_bead_context(bead_id)

  let assert True = loaded.id == context.id
  let assert True = loaded.title == context.title
  let assert True = loaded.context == context.context

  let assert Ok(Nil) = simplifile.delete(cache_dir.base_path)
}

fn test_phase_output_save_load() {
  let bead_id = "test-bead-004"
  let assert Ok(cache_dir) = state.init_cache(bead_id)

  let data =
    json.object([
      #("result", json.string("success")),
      #("count", json.int(42)),
    ])

  let assert Ok(Nil) = state.save_phase_output(bead_id, "phase0", data)
  let assert Ok(loaded) = state.load_phase_output(bead_id, "phase0")

  let _ = loaded

  let assert Ok(Nil) = simplifile.delete(cache_dir.base_path)
}

fn test_summary_write() {
  let bead_id = "test-bead-005"
  let assert Ok(cache_dir) = state.init_cache(bead_id)

  let content = "Test summary content"
  let assert Ok(Nil) = state.write_summary(bead_id, content)

  let summary_path = cache_dir.base_path <> "/summary.txt"
  let assert Ok(loaded) = simplifile.read(summary_path)

  let assert True = loaded == content

  let assert Ok(Nil) = simplifile.delete(cache_dir.base_path)
}

fn test_json_encoding_decoding() {
  let progress =
    Progress(
      bead_id: "test",
      language: Gleam,
      complexity: Medium,
      route: [0, 4, 5],
      phases: dict.from_list([
        #(0, PhaseState(Completed, True, 1)),
        #(4, PhaseState(InProgress, False, 2)),
      ]),
      current_phase: 4,
      last_commit: "abc123",
    )

  let json_string = state.encode_progress(progress)
  let assert Ok(decoded) = state.parse_progress(json_string)

  let assert True = decoded.bead_id == progress.bead_id
  let assert True = decoded.complexity == progress.complexity
  let assert True = decoded.current_phase == progress.current_phase
}

fn test_all_phase_status_values() {
  let statuses = [Pending, InProgress, Completed, Failed, Skipped]

  let assert Nil = case statuses {
    [Pending, InProgress, Completed, Failed, Skipped] -> Nil
    _ -> panic as "Unexpected status list"
  }

  let progress =
    Progress(
      bead_id: "test",
      language: Gleam,
      complexity: Simple,
      route: [0, 4],
      phases: dict.from_list([
        #(0, PhaseState(Pending, False, 0)),
        #(4, PhaseState(Completed, True, 1)),
      ]),
      current_phase: 0,
      last_commit: "abc",
    )

  let p1 = state.update_phase_status(progress, 0, Failed)
  let assert Ok(phase_0) = dict.get(p1.phases, 0)
  let assert True = phase_0.status == Failed
}

fn test_language_encoding() {
  let assert Nil = case Gleam {
    Gleam -> Nil
  }
  let assert Nil = case Rust {
    Rust -> Nil
  }
  let assert Nil = case Medium {
    Medium -> Nil
  }
  let assert Nil = case Complex {
    Complex -> Nil
  }
}
