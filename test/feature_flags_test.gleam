import gleam/dict
import gleam/float
import gleam/int
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

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
  RolloutState(
    current_percentage: Int,
    stage: Stage,
    should_rollback: Bool,
  )
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

fn new_context(_flag: String, config: RolloutConfig) -> RolloutContext {
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

fn calculate_error_rate(
  metrics: HealthMetrics,
) -> Float {
  case metrics.request_count {
    0 -> 0.0
    total -> {
      let errors = metrics.error_count
      int.to_float(errors) /. int.to_float(total)
    }
  }
}

fn advance_stage(
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

fn should_trigger_rollback(
  current_metrics: HealthMetrics,
  error_threshold: Float,
) -> Bool {
  let error_rate = calculate_error_rate(current_metrics)
  error_rate >. error_threshold
}

pub fn rollout_feature(
  flag: String,
  config: RolloutConfig,
) -> Result(RolloutState, RolloutError) {
  case config.initial_percentage < 0 || config.initial_percentage > 100 {
    True -> Error(InvalidConfig)
    False -> {
      let ctx = new_context(flag, config)
      Ok(ctx.state)
    }
  }
}

fn advance_rollout(
  ctx: RolloutContext,
  config: RolloutConfig,
  next_percentage: Int,
  next_stage: Stage,
  gate_metrics: HealthMetrics,
) -> Result(RolloutContext, RolloutError) {
  case should_trigger_rollback(gate_metrics, config.error_threshold) {
    True ->
      Error(RollbackTriggered(
        "Error threshold exceeded: " <>
          float.to_string(calculate_error_rate(gate_metrics)),
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

pub fn rollout_advances_from_1_to_10_percent_test() {
  let config =
    RolloutConfig(
      flag: "test_feature",
      initial_percentage: 1,
      error_threshold: 0.05,
      monitoring_interval: 5000,
    )

  let stage_1_metrics =
    HealthMetrics(
      error_rate: 0.02,
      request_count: 1000,
      error_count: 20,
      latency_p99: 150,
    )

  case rollout_feature("test_feature", config) {
    Error(_) -> should.fail()
    Ok(initial_state) -> {
      initial_state.current_percentage
      |> should.equal(1)

      initial_state.stage
      |> should.equal(Initial)

      let ctx = new_context("test_feature", config)
      case advance_rollout(ctx, config, 10, TenPercent, stage_1_metrics) {
        Error(_) -> should.fail()
        Ok(next_ctx) -> {
          next_ctx.state.current_percentage
          |> should.equal(10)

          next_ctx.state.stage
          |> should.equal(TenPercent)

          next_ctx.state.should_rollback
          |> should.equal(False)
        }
      }
    }
  }
}

pub fn rollout_holds_at_stage_on_errors_test() {
  let config =
    RolloutConfig(
      flag: "test_feature",
      initial_percentage: 1,
      error_threshold: 0.05,
      monitoring_interval: 5000,
    )

  let high_error_metrics =
    HealthMetrics(
      error_rate: 0.08,
      request_count: 500,
      error_count: 40,
      latency_p99: 500,
    )

  case rollout_feature("test_feature", config) {
    Error(_) -> should.fail()
    Ok(_) -> {
      let ctx = new_context("test_feature", config)
      case advance_rollout(ctx, config, 10, TenPercent, high_error_metrics) {
        Ok(_) -> should.fail()
        Error(RollbackTriggered(_)) -> Nil
        Error(_) -> should.fail()
      }
    }
  }
}

pub fn rollout_triggers_rollback_on_threshold_test() {
  let critical_error_metrics =
    HealthMetrics(
      error_rate: 0.15,
      request_count: 2000,
      error_count: 300,
      latency_p99: 3000,
    )

  let should_rollback = should_trigger_rollback(critical_error_metrics, 0.05)
  should_rollback
  |> should.equal(True)
}

pub fn rollout_completes_to_100_percent_test() {
  let config =
    RolloutConfig(
      flag: "test_feature",
      initial_percentage: 10,
      error_threshold: 0.05,
      monitoring_interval: 5000,
    )

  let clean_metrics_1 =
    HealthMetrics(
      error_rate: 0.01,
      request_count: 5000,
      error_count: 50,
      latency_p99: 120,
    )

  let clean_metrics_2 =
    HealthMetrics(
      error_rate: 0.015,
      request_count: 10_000,
      error_count: 150,
      latency_p99: 140,
    )

  case rollout_feature("test_feature", config) {
    Error(_) -> should.fail()
    Ok(_) -> {
      let ctx = new_context("test_feature", config)

      case advance_rollout(ctx, config, 10, TenPercent, clean_metrics_1) {
        Error(_) -> should.fail()
        Ok(ctx_10) -> {
          case advance_rollout(ctx_10, config, 100, Complete, clean_metrics_2) {
            Error(_) -> should.fail()
            Ok(ctx_final) -> {
              ctx_final.state.current_percentage
              |> should.equal(100)

              ctx_final.state.stage
              |> should.equal(Complete)

              ctx_final.state.should_rollback
              |> should.equal(False)
            }
          }
        }
      }
    }
  }
}

pub fn rollout_respects_monitoring_gates_test() {
  let metrics_below_threshold =
    HealthMetrics(
      error_rate: 0.03,
      request_count: 1000,
      error_count: 30,
      latency_p99: 140,
    )

  let metrics_at_threshold =
    HealthMetrics(
      error_rate: 0.05,
      request_count: 1000,
      error_count: 50,
      latency_p99: 160,
    )

  let metrics_above_threshold =
    HealthMetrics(
      error_rate: 0.06,
      request_count: 1000,
      error_count: 60,
      latency_p99: 180,
    )

  let below = should_trigger_rollback(metrics_below_threshold, 0.05)
  below
  |> should.equal(False)

  let at = should_trigger_rollback(metrics_at_threshold, 0.05)
  at
  |> should.equal(False)

  let above = should_trigger_rollback(metrics_above_threshold, 0.05)
  above
  |> should.equal(True)
}
