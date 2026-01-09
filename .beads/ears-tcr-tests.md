# TCR Integration Testing Gaps

## EAR-001: Real jj Commit Verification Missing
**Event**: Developer runs stage that passes tests
**Action**: TCR commits changes to jj journal
**Result**: jj log shows commit with message "factory: {stage} passed"

**Inversion**: What if jj describe fails silently? What if jj new never creates working copy? Current tests mock phase transitions - never verify jj state. Result: TCR claims to commit but silently fails, changes lost forever.

**Bead**: test/tcr_jj_commit_verification_test.gleam
- Create temp jj repo with jj git init
- Make file change in worktree
- Run tcr.run_with_tcr with mock passing stage
- Shell out: jj -R <worktree> log -r '@-' to verify commit exists
- Assert commit message contains stage name
- Assert new working copy created

---

## EAR-002: Real jj Revert Verification Missing
**Event**: Developer runs stage that fails tests  
**Action**: TCR reverts all working directory changes
**Result**: Working directory restored to exact pre-execution state

**Inversion**: What if jj restore --from silently fails? What if it partially reverts? Current tests never touch filesystem. Result: Failed TCR leaves dirty state, corrupts worktree, breaks subsequent stages.

**Bead**: test/tcr_jj_revert_verification_test.gleam
- Create temp jj repo
- Get initial jj state hash
- Write new file "poison.txt" to worktree
- Run tcr.run_with_tcr with mock failing stage
- Assert poison.txt no longer exists
- Assert working directory matches initial hash exactly
- Run 'jj status' and verify "No changes"

---

## EAR-003: TCR Never Leaves Dirty State (Property)
**Event**: Any stage execution (pass or fail)
**Action**: TCR completes (commit or revert)
**Result**: Working directory always in clean jj state

**Inversion**: What if crash during jj describe? What if SIGTERM during restore? Current tests never verify cleanup. Result: Half-committed state, unrecoverable worktree, factory stuck forever.

**Bead**: test/qcheck/property_tcr_invariants_test.gleam
```gleam
// Property: After TCR, jj status always shows "No changes"
pub fn prop_tcr_leaves_clean_state__test()
  use pass <- qcheck.given(qcheck.bool())
  create temp worktree
  run tcr with passing=pass stage
  stdout = run "jj status"
  assert string.contains(stdout, "No changes")
```

---

## EAR-004: Pipeline Stage Invariants Unverified
**Event**: Load standard_pipeline()
**Action**: Validate all stages conform to TCR requirements
**Result**: Every stage has tcr=True, retries>0

**Inversion**: What if someone adds Stage("manual", gate, 0, False)? Current property tests skip this. Result: Non-TCR stage breaks factory assumptions, silent failures cascade.

**Bead**: test/qcheck/property_pipeline_invariants_test.gleam
```gleam
pub fn prop_all_stages_tcr_enabled__test()
  let p = domain.standard_pipeline()
  assert list.all(p, fn(s) { s.tcr == True })

pub fn prop_all_stages_have_retries__test()
  let p = domain.standard_pipeline()
  assert list.all(p, fn(s) { s.retries > 0 })
```

---

## EAR-005: End-to-End TCR Cycle Missing
**Event**: Bead assigned to factory loop
**Action**: Loop runs implement → TCR commit → next stage
**Result**: jj journal shows N commits matching N passed stages

**Inversion**: What if factory_loop.advance mocks succeed but jj never commits? Current integration tests use `factory_loop.advance(TestPassed)` - pure message passing, no filesystem. Result: Tests pass, production fails, zero commits reach jj.

**Bead**: test/e2e_real_tcr_cycle_test.gleam
- Create real jj worktree
- Spawn factory_loop with real workspace_path
- Mock LLM to return passing code
- Advance through Implementing → TcrChecking
- Shell out: jj -R <path> log --no-graph
- Parse output, count commits
- Assert commit_count in state matches actual jj commits
- Assert each commit message matches stage name

---

## EAR-006: Concurrent TCR Isolation Unverified
**Event**: Two loops run TCR in different worktrees simultaneously
**Action**: Both commit to separate jj journals
**Result**: No cross-contamination, each worktree independent

**Inversion**: What if jj -R flag ignored? What if worktrees share journal? Current tests spawn concurrent loops but never verify filesystem isolation. Result: Loop A's commit corrupts Loop B's worktree, cascading failures.

**Bead**: test/integration_tcr_isolation_test.gleam
- Create two temp jj repos: /tmp/wt-a, /tmp/wt-b
- Spawn two factory loops with different workspace_paths
- Run TCR in both concurrently
- Verify jj log in wt-a shows only wt-a commits
- Verify jj log in wt-b shows only wt-b commits
- Assert no "error: ambiguous revision" from jj

---

## EAR-007: Golden Master for jj Output Format
**Event**: Run jj log after TCR cycle
**Action**: Parse commit output to extract stage names
**Result**: Commit message format stable, parseable

**Inversion**: What if jj changes output format? What if commit message missing stage name? Current code assumes jj output stable. Result: Factory can't parse jj log, loses all commit history.

**Bead**: test/golden_master_jj_output_test.gleam
- Create known jj repo state
- Run jj log -r @ -T 'commit_id'
- Save output as golden_master/jj_log_format.txt
- Future tests compare against golden
- Fail loudly if jj output format drifts
