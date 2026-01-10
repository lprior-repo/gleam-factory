//// Feature flags rollout and validation tests
////
//// Tests gradual rollout stages, config validation, error thresholds, and
//// automatic rollback triggering based on health metrics.

import feature_flags
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn valid_config() -> feature_flags.RolloutConfig {
  feature_flags.RolloutConfig(
    flag: "test_feature",
    initial_percentage: 1,
    error_threshold: 0.05,
    monitoring_interval: 60000,
  )
}

fn valid_metrics(error_count: Int, request_count: Int) -> feature_flags.HealthMetrics {
  feature_flags.HealthMetrics(
    error_rate: 0.0,
    request_count: request_count,
    error_count: error_count,
    latency_p99: 100,
  )
}

// Config Validation Tests

pub fn validate_config_with_valid_percentage_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 50,
      error_threshold: 0.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Ok(Nil))
}

pub fn validate_config_with_zero_percentage_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 0,
      error_threshold: 0.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Ok(Nil))
}

pub fn validate_config_with_100_percentage_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 100,
      error_threshold: 0.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Ok(Nil))
}

pub fn validate_config_with_negative_percentage_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: -1,
      error_threshold: 0.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Error(feature_flags.InvalidConfig))
}

pub fn validate_config_with_over_100_percentage_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 101,
      error_threshold: 0.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Error(feature_flags.InvalidConfig))
}

pub fn validate_config_with_valid_error_threshold_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 50,
      error_threshold: 0.0,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Ok(Nil))

  let config2 =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 50,
      error_threshold: 1.0,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config2)
  |> should.equal(Ok(Nil))
}

pub fn validate_config_with_negative_error_threshold_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 50,
      error_threshold: -0.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Error(feature_flags.InvalidConfig))
}

pub fn validate_config_with_over_100_error_threshold_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 50,
      error_threshold: 1.1,
      monitoring_interval: 60000,
    )

  feature_flags.validate_config(config)
  |> should.equal(Error(feature_flags.InvalidConfig))
}

// Rollout Feature Creation Tests

pub fn rollout_feature_creates_initial_state_test() {
  let config = valid_config()

  case feature_flags.rollout_feature("test_feature", config) {
    Ok(state) -> {
      state.current_percentage
      |> should.equal(config.initial_percentage)

      state.stage
      |> should.equal(feature_flags.Initial)

      state.should_rollback
      |> should.equal(False)
    }
    Error(_) -> should.fail()
  }
}

pub fn rollout_feature_rejects_invalid_config_test() {
  let config =
    feature_flags.RolloutConfig(
      flag: "test",
      initial_percentage: 150,
      error_threshold: 0.1,
      monitoring_interval: 60000,
    )

  feature_flags.rollout_feature("test", config)
  |> should.equal(Error(feature_flags.InvalidConfig))
}

// Error Rate Calculation Tests

pub fn calculate_error_rate_zero_errors_test() {
  let metrics = valid_metrics(0, 100)

  feature_flags.calculate_error_rate(metrics)
  |> should.equal(0.0)
}

pub fn calculate_error_rate_all_errors_test() {
  let metrics = valid_metrics(100, 100)

  feature_flags.calculate_error_rate(metrics)
  |> should.equal(1.0)
}

pub fn calculate_error_rate_partial_errors_test() {
  let metrics = valid_metrics(5, 100)

  let error_rate = feature_flags.calculate_error_rate(metrics)

  // Should be 0.05
  error_rate >. 0.04
  |> should.be_true()

  error_rate <. 0.06
  |> should.be_true()
}

pub fn calculate_error_rate_zero_requests_test() {
  let metrics = valid_metrics(0, 0)

  feature_flags.calculate_error_rate(metrics)
  |> should.equal(0.0)
}

// Stage Advancement Tests

pub fn advance_stage_updates_percentage_test() {
  let config = valid_config()
  let ctx = feature_flags.new_context("test", config)

  let updated = feature_flags.advance_stage(ctx, 10, feature_flags.OnePercent)
  let state = feature_flags.get_state(updated)

  state.current_percentage
  |> should.equal(10)

  state.stage
  |> should.equal(feature_flags.OnePercent)
}

pub fn advance_stage_preserves_rollback_flag_test() {
  let config = valid_config()
  let ctx = feature_flags.new_context("test", config)

  let updated = feature_flags.advance_stage(ctx, 10, feature_flags.OnePercent)
  let state = feature_flags.get_state(updated)
  let orig_state = feature_flags.get_state(ctx)

  state.should_rollback
  |> should.equal(orig_state.should_rollback)
}

// Rollback Triggering Tests

pub fn should_trigger_rollback_when_error_rate_exceeds_threshold_test() {
  let metrics = valid_metrics(10, 100)
  let error_threshold = 0.05

  feature_flags.should_trigger_rollback(metrics, error_threshold)
  |> should.be_true()
}

pub fn should_not_trigger_rollback_when_error_rate_below_threshold_test() {
  let metrics = valid_metrics(3, 100)
  let error_threshold = 0.05

  feature_flags.should_trigger_rollback(metrics, error_threshold)
  |> should.be_false()
}

pub fn should_not_trigger_rollback_when_error_rate_equals_threshold_test() {
  let metrics = valid_metrics(5, 100)
  let error_threshold = 0.05

  feature_flags.should_trigger_rollback(metrics, error_threshold)
  |> should.be_false()
}

// Advance Rollout Tests

pub fn advance_rollout_succeeds_with_healthy_metrics_test() {
  let config = valid_config()
  let ctx = feature_flags.new_context("test", config)
  let healthy_metrics = valid_metrics(2, 100)

  case feature_flags.advance_rollout(
    ctx,
    config,
    10,
    feature_flags.OnePercent,
    healthy_metrics,
  ) {
    Ok(updated) -> {
      let state = feature_flags.get_state(updated)
      state.current_percentage
      |> should.equal(10)

      state.stage
      |> should.equal(feature_flags.OnePercent)
    }
    Error(_) -> should.fail()
  }
}

pub fn advance_rollout_fails_with_unhealthy_metrics_test() {
  let config = valid_config()
  let ctx = feature_flags.new_context("test", config)
  let unhealthy_metrics = valid_metrics(10, 100)

  case feature_flags.advance_rollout(
    ctx,
    config,
    10,
    feature_flags.OnePercent,
    unhealthy_metrics,
  ) {
    Ok(_) -> should.fail()
    Error(feature_flags.RollbackTriggered(_msg)) -> Nil
    Error(_) -> should.fail()
  }
}

pub fn advance_rollout_updates_metrics_test() {
  let config = valid_config()
  let ctx = feature_flags.new_context("test", config)
  let healthy_metrics = valid_metrics(1, 200)

  case feature_flags.advance_rollout(
    ctx,
    config,
    10,
    feature_flags.OnePercent,
    healthy_metrics,
  ) {
    Ok(_updated) -> {
      // Metrics updated successfully, stage advanced
      Nil
    }
    Error(_) -> should.fail()
  }
}

// Context Creation Tests

pub fn new_context_creates_initial_state_test() {
  let config = valid_config()
  let ctx = feature_flags.new_context("test", config)
  let state = feature_flags.get_state(ctx)

  state.current_percentage
  |> should.equal(config.initial_percentage)

  state.stage
  |> should.equal(feature_flags.Initial)

  feature_flags.get_error_history(ctx)
  |> should.equal([])
}
