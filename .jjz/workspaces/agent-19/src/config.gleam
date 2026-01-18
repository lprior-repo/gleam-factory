// Configuration module for Factory
// Provides configuration types and default values

/// Priority levels for tasks
pub type Priority {
  P1
  P2
  P3
}

/// Configuration for Factory
pub type Config {
  Config(data_dir: String, default_priority: Priority, verbose: Bool)
}

/// Returns a Config with sensible default values
pub fn default_config() -> Config {
  Config(data_dir: ".factory", default_priority: P2, verbose: False)
}

/// Returns the data directory from a config
pub fn get_data_dir(cfg: Config) -> String {
  cfg.data_dir
}
