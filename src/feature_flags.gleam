import gleam/dict
import gleam/float
import gleam/int
import gleam/result

pub type Stage {
  Initial
  OnePercent
  TenPercent
  Complete
}

pub type RolloutError {
  InvalidConfig
  MonitoringGateFailed(reason: String)
  RollbackTriggered(message: String)
}

pub type HealthMetrics {
  HealthMetrics(
    error_rate: Float,
    request_count: Int,
    error_count: Int,
    latency_p99: Int,
  )
}

pub type RolloutState {
  RolloutState(current_percentage: Int, stage: Stage, should_rollback: Bool)
}

pub type RolloutConfig {
  RolloutConfig(
    flag: String,
    initial_percentage: Int,
    error_threshold: Float,
    monitoring_interval: Int,
  )
}

pub opaque type RolloutContext {
  RolloutContext(
    state: RolloutState,
    metrics: dict.Dict(Stage, HealthMetrics),
    error_history: List(Float),
  )
}

pub fn validate_config(config: RolloutConfig) -> Result(Nil, RolloutError) {
  case
    config.initial_percentage >= 0
    && config.initial_percentage <= 100
    && config.error_threshold >=. 0.0
    && config.error_threshold <=. 1.0
  {
    True -> Ok(Nil)
    False -> Error(InvalidConfig)
  }
}

pub fn rollout_feature(
  _flag: String,
  config: RolloutConfig,
) -> Result(RolloutState, RolloutError) {
  use _ <- result.try(validate_config(config))

  Ok(RolloutState(
    current_percentage: config.initial_percentage,
    stage: Initial,
    should_rollback: False,
  ))
}

pub fn new_context(_flag: String, config: RolloutConfig) -> RolloutContext {
  let initial_state =
    RolloutState(
      current_percentage: config.initial_percentage,
      stage: Initial,
      should_rollback: False,
    )

  let metrics =
    dict.new()
    |> dict.insert(Initial, HealthMetrics(0.0, 0, 0, 0))

  RolloutContext(state: initial_state, metrics: metrics, error_history: [])
}

pub fn get_state(ctx: RolloutContext) -> RolloutState {
  ctx.state
}

pub fn get_error_history(ctx: RolloutContext) -> List(Float) {
  ctx.error_history
}

pub fn calculate_error_rate(metrics: HealthMetrics) -> Float {
  case metrics.request_count {
    0 -> 0.0
    total -> {
      let errors = metrics.error_count
      int.to_float(errors) /. int.to_float(total)
    }
  }
}

pub fn advance_stage(
  ctx: RolloutContext,
  next_percentage: Int,
  next_stage: Stage,
) -> RolloutContext {
  let new_state =
    RolloutState(
      current_percentage: next_percentage,
      stage: next_stage,
      should_rollback: ctx.state.should_rollback,
    )
  RolloutContext(..ctx, state: new_state)
}

pub fn should_trigger_rollback(
  current_metrics: HealthMetrics,
  error_threshold: Float,
) -> Bool {
  let error_rate = calculate_error_rate(current_metrics)
  error_rate >. error_threshold
}

pub fn advance_rollout(
  ctx: RolloutContext,
  config: RolloutConfig,
  next_percentage: Int,
  next_stage: Stage,
  gate_metrics: HealthMetrics,
) -> Result(RolloutContext, RolloutError) {
  case should_trigger_rollback(gate_metrics, config.error_threshold) {
    True ->
      Error(RollbackTriggered(
        "Error threshold exceeded: "
        <> float.to_string(calculate_error_rate(gate_metrics)),
      ))
    False -> {
      let updated_metrics =
        ctx.metrics
        |> dict.insert(next_stage, gate_metrics)
      let updated_ctx = advance_stage(ctx, next_percentage, next_stage)
      let with_metrics = RolloutContext(..updated_ctx, metrics: updated_metrics)
      Ok(with_metrics)
    }
  }
}
