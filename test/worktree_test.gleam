import domain
import gleeunit/should
import worktree

// === worktrees_base Path Construction ===

pub fn worktrees_base_appends_workspaces_dir_test() {
  worktree.worktrees_base("/repo/root")
  |> should.equal("/repo/root/.factory-workspaces")
}

pub fn worktrees_base_handles_trailing_slash_test() {
  // Note: current implementation doesn't strip trailing slash
  worktree.worktrees_base("/repo/root/")
  |> should.equal("/repo/root//.factory-workspaces")
}

pub fn worktrees_base_handles_relative_path_test() {
  worktree.worktrees_base(".")
  |> should.equal("./.factory-workspaces")
}

pub fn worktrees_base_handles_home_path_test() {
  worktree.worktrees_base("/home/user/project")
  |> should.equal("/home/user/project/.factory-workspaces")
}

pub fn worktrees_base_handles_nested_path_test() {
  worktree.worktrees_base("/a/b/c/d/e")
  |> should.equal("/a/b/c/d/e/.factory-workspaces")
}

// === Worktree Type Construction ===

pub fn worktree_captures_slug_test() {
  let wt = worktree.Worktree("my-task", "/path", "feat/my-task", domain.Gleam)
  case wt {
    worktree.Worktree(slug: "my-task", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_captures_path_test() {
  let wt = worktree.Worktree("slug", "/full/path/to/worktree", "branch", domain.Go)
  case wt {
    worktree.Worktree(path: "/full/path/to/worktree", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_captures_branch_test() {
  let wt = worktree.Worktree("slug", "/path", "feat/feature-x", domain.Rust)
  case wt {
    worktree.Worktree(branch: "feat/feature-x", ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_captures_language_test() {
  let wt = worktree.Worktree("slug", "/path", "branch", domain.Python)
  case wt {
    worktree.Worktree(language: domain.Python, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Worktree Type with Different Languages ===

pub fn worktree_can_have_gleam_language_test() {
  let wt = worktree.Worktree("gleam-task", "/path", "branch", domain.Gleam)
  case wt {
    worktree.Worktree(language: domain.Gleam, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_can_have_go_language_test() {
  let wt = worktree.Worktree("go-task", "/path", "branch", domain.Go)
  case wt {
    worktree.Worktree(language: domain.Go, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_can_have_rust_language_test() {
  let wt = worktree.Worktree("rust-task", "/path", "branch", domain.Rust)
  case wt {
    worktree.Worktree(language: domain.Rust, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_can_have_python_language_test() {
  let wt = worktree.Worktree("python-task", "/path", "branch", domain.Python)
  case wt {
    worktree.Worktree(language: domain.Python, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn worktree_can_have_javascript_language_test() {
  let wt = worktree.Worktree("js-task", "/path", "branch", domain.Javascript)
  case wt {
    worktree.Worktree(language: domain.Javascript, ..) -> should.be_true(True)
    _ -> should.fail()
  }
}

// === Worktree Field Access Patterns ===

pub fn worktree_all_fields_accessible_via_pattern_match_test() {
  let wt = worktree.Worktree("slug", "/path", "branch", domain.Gleam)
  case wt {
    worktree.Worktree(slug: s, path: p, branch: b, language: l) -> {
      s |> should.equal("slug")
      p |> should.equal("/path")
      b |> should.equal("branch")
      case l {
        domain.Gleam -> should.be_true(True)
        _ -> should.fail()
      }
    }
  }
}
