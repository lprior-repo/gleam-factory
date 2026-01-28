import gleeunit/should
import types
import workspace_manager

pub fn workspace_type_test() {
  let jj = types.Jj
  let reflink = types.Reflink
  jj
  //   |> should.be_an_instance_of(types.WorkspaceType)
  reflink
  //   |> should.be_an_instance_of(types.WorkspaceType)
}

pub fn resolve_auto_strategy_shm_exists_test() {
  let strategy = workspace_manager.resolve_auto_strategy()
  strategy
  //   |> should.be_an_instance_of(types.WorkspaceType)
}
