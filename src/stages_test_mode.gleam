const test_mode_env = "FACTORY_TEST_MODE"

pub fn is_test_mode() -> Bool {
  case get_env(test_mode_env) {
    Ok("1") -> True
    _ -> False
  }
}

@external(erlang, "os", "getenv")
fn get_env(key: String) -> Result(String, Nil)

@external(erlang, "os", "putenv")
fn set_env(key: String, value: String) -> Bool

pub fn set_test_mode() -> Nil {
  let _ = set_env(test_mode_env, "1")
  Nil
}

pub fn clear_test_mode() -> Nil {
  let _ = set_env(test_mode_env, "0")
  Nil
}
