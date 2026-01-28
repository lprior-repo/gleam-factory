import domain
import gleeunit/should
import worktree

pub fn worktrees_base_test() {
  let result = worktree.worktrees_base("/repo/root")
  result
  |> should.equal("/repo/root/.factory-workspaces")
}
