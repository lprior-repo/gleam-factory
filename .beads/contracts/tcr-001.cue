package contracts

#TcrCommitVerificationTest: {
  setup: {
    temp_dir: string & =~"^/tmp/tcr-test-[0-9]+$"
    jj_init_cmd: ["jj", "git", "init", temp_dir]
  }
  
  execution: {
    test_file_path: string & =~"^.*/test\\.txt$"
    test_file_content: "initial content"
    mock_stage: {
      name: "test-stage"
      tcr: true
    }
    execute_fn: "fn() -> Result(Nil, String) { Ok(Nil) }"
  }
  
  assertions: {
    jj_log_output: string & =~"factory: test-stage passed"
    outcome_commits_made: 1
  }
}
