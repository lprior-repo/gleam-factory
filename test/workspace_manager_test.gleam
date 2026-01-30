import types
import workspace_manager

pub fn workspace_type_test() {
  let _jj = types.Jj
  let _reflink = types.Reflink
  Nil
}

pub fn resolve_auto_strategy_shm_exists_test() {
  let _strategy = workspace_manager.resolve_auto_strategy()
  Nil
}
