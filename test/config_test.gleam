import config
import gleeunit/should

// === Priority Type Behavior ===

pub fn priority_p1_is_highest_priority_test() {
  // P1 should be the highest priority (most urgent)
  let priority = config.P1
  case priority {
    config.P1 -> should.be_true(True)
  }
}

pub fn priority_p2_is_medium_priority_test() {
  let priority = config.P2
  case priority {
    config.P2 -> should.be_true(True)
  }
}

pub fn priority_p3_is_lowest_priority_test() {
  let priority = config.P3
  case priority {
    config.P3 -> should.be_true(True)
  }
}

// === Default Config Behavior ===

pub fn default_config_uses_factory_data_directory_test() {
  config.default_config()
  |> config.get_data_dir
  |> should.equal(".factory")
}

pub fn default_config_uses_p2_as_default_priority_test() {
  let cfg = config.default_config()
  case cfg {
    config.Config(default_priority: priority, ..) ->
      priority |> should.equal(config.P2)
  }
}

pub fn default_config_has_verbose_disabled_test() {
  let cfg = config.default_config()
  case cfg {
    config.Config(verbose: verbose, ..) -> verbose |> should.equal(False)
  }
}

// === Config Construction Behavior ===

pub fn config_can_be_created_with_custom_data_dir_test() {
  let cfg = config.Config(data_dir: "/custom/path", default_priority: config.P1, verbose: True)
  config.get_data_dir(cfg)
  |> should.equal("/custom/path")
}

pub fn config_preserves_all_field_values_test() {
  let cfg = config.Config(data_dir: "/tmp", default_priority: config.P3, verbose: True)
  config.get_data_dir(cfg) |> should.equal("/tmp")
  case cfg {
    config.Config(default_priority: priority, verbose: verbose, ..) -> {
      priority |> should.equal(config.P3)
      verbose |> should.equal(True)
    }
  }
}

// === get_data_dir Behavior ===

pub fn get_data_dir_returns_exact_path_from_config_test() {
  let cfg = config.Config(data_dir: "./relative/path", default_priority: config.P2, verbose: False)
  config.get_data_dir(cfg)
  |> should.equal("./relative/path")
}

pub fn get_data_dir_handles_empty_path_test() {
  let cfg = config.Config(data_dir: "", default_priority: config.P2, verbose: False)
  config.get_data_dir(cfg)
  |> should.equal("")
}

pub fn get_data_dir_handles_absolute_path_test() {
  let cfg = config.Config(data_dir: "/home/user/.factory", default_priority: config.P2, verbose: False)
  config.get_data_dir(cfg)
  |> should.equal("/home/user/.factory")
}
