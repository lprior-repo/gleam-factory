import drq/types

pub type Config {
  Config(arena: types.Arena, max_rounds: Int)
}

pub type DrqResult {
  DrqResult(rounds_completed: Int, final_fitness: Float, regressions_found: Int)
}

pub type DrqError {
  ArenaError(String)
  TestError(String)
  LlmError(String)
}

pub fn run_drq(_config: Config) -> Result(DrqResult, DrqError) {
  Ok(DrqResult(rounds_completed: 0, final_fitness: 0.0, regressions_found: 0))
}
