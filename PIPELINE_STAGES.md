# Factory Gleam - Pipeline Stages Detail

**Complete specification of all 10 pipeline stages**

**Last Updated**: January 5, 2026

---

## Pipeline Overview

Factory uses a **10-stage standard pipeline** that all tasks execute in sequence. Each stage has specific responsibilities, language-specific implementations, and clear success/failure criteria.

```
┌──────────────┐
│ Stage 1      │  Implement (Compilation)
│ Implement    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 2      │  Unit Test
│ Unit Test    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 3      │  Coverage Analysis
│ Coverage     │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 4      │  Lint & Format
│ Lint         │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 5      │  Integration Tests
│ Integration  │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 6      │  Code Review Checks
│ Review       │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 7      │  Boundary Enforcement
│ Boundary     │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 8      │  Merge Integration
│ Integrate    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 9      │  TCR (Full test)
│ TCR          │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Stage 10     │  Deployment Prep
│ Deploy       │
└──────────────┘
```

---

## Stage 1: Implement (Compilation)

**Purpose**: Verify that code compiles and basic structure is correct

**Criteria for Success**:
- Code compiles without errors
- No syntax errors
- Type checking passes

**Gleam**:
```bash
# Build for JavaScript target
gleam build --target javascript
# Exit code must be 0

# Build for Erlang target
gleam build --target erlang
# Exit code must be 0
```

**Go**:
```bash
go build ./...
# Must succeed without errors
```

**Rust**:
```bash
cargo build --release
# Must compile successfully
```

**Python**:
```bash
python -m py_compile src/
# Syntax check all Python files
```

**Success Metrics**:
- Exit code: 0
- No compilation errors
- Output: "Build successful"

**Failure Handling**:
- Extract and show first compilation error
- Example: `Error in src/web/handlers.gleam:47: Expected 'pub fn'`
- Full error output saved to `.factory/{slug}/stage-implement-full.log`

**Decision Points**:
- This is a **hard requirement** - if implement fails, pipeline stops
- No code continues to stage 2 if stage 1 fails

---

## Stage 2: Unit Test

**Purpose**: Run all unit tests to verify functionality

**Criteria for Success**:
- All unit tests pass
- No test failures
- No test timeouts

**Gleam**:
```bash
gleam test
# Runs all test modules
# Output shows: "Passed: X, Failed: 0"
```

**Go**:
```bash
go test ./...
# Runs all test packages
# Must have exit code 0
```

**Rust**:
```bash
cargo test --lib
# Runs library tests only (not integration)
# All tests must pass
```

**Python**:
```bash
python -m pytest test/ -v
# Runs all tests in test/ directory
# Shows verbose output
```

**Success Metrics**:
- Exit code: 0
- No failing tests
- Test count: N
- Duration: X.XXs

**Failure Handling**:
- Extract failing test names
- Example: `test_concurrent_requests failed (3 assertions)`
- Show which assertions failed
- Link to test file and line number

**Decision Points**:
- **Hard requirement** - if any test fails, pipeline stops
- Failures trigger TCR revert

---

## Stage 3: Coverage

**Purpose**: Measure test coverage and ensure minimum coverage is met

**Criteria for Success**:
- Coverage meets contract minimum (default 80%)
- Coverage percentage >= required_test_coverage

**Gleam**:
```bash
gleam test --cover
# Generates coverage data
# Output shows: "Coverage: X.X%"
```

**Go**:
```bash
go test -cover ./...
# Shows coverage for each package
# Example: "coverage: 87.5% of statements"
```

**Rust**:
```bash
cargo tarpaulin --out Html --output-dir coverage
# Generates HTML coverage report
# Shows line coverage percentage
```

**Python**:
```bash
coverage run -m pytest test/
coverage report --precision=2
# Shows module-by-module coverage
# Final: "TOTAL  89%"
```

**Success Metrics**:
- Coverage %: N.N%
- Covered lines: X
- Uncovered lines: Y
- Critical path coverage: Z%

**Failure Handling**:
- If below minimum: `Coverage 75% < required 80%`
- Show which modules are uncovered
- Suggest tests for uncovered code
- Not hard failure (can approve despite low coverage)

**Decision Points**:
- Can proceed to stage 4 even if coverage is low
- But human must explicitly accept the risk in approval stage

---

## Stage 4: Lint & Format

**Purpose**: Check code style and automatically fixable issues

**Criteria for Success**:
- Code follows style guide
- No formatting violations
- No obvious pattern violations

**Gleam**:
```bash
gleam format --check src/
# Checks if code matches gleam format style
# Shows: "All files formatted correctly" or lists violations
```

**Go**:
```bash
gofmt -l src/
go vet ./...
# gofmt checks formatting
# go vet checks for suspicious constructs
```

**Rust**:
```bash
cargo fmt --check
cargo clippy -- -D warnings
# fmt: check formatting
# clippy: lint warnings as errors
```

**Python**:
```bash
black --check src/
flake8 src/
# black: formatting check
# flake8: style enforcement
```

**Success Metrics**:
- Exit code: 0
- No style violations
- No lint warnings

**Failure Handling**:
- Show specific violations
- Example: `src/file.gleam:42: Line too long (112 chars, max 100)`
- Can auto-fix some issues
- Not hard failure (linting issues aren't functional problems)

**Decision Points**:
- Can proceed despite lint failures
- But human sees them in approval stage

---

## Stage 5: Integration

**Purpose**: Run integration tests against real dependencies

**Criteria for Success**:
- All integration tests pass
- External services accessible
- No flaky test failures

**Gleam**:
```bash
gleam test --target javascript  # If integration tests use JS
# Or specific integration test file
```

**Go**:
```bash
go test -tags=integration ./...
# Runs tests tagged with "integration"
```

**Rust**:
```bash
cargo test --test '*'
# Runs all integration tests in tests/ directory
```

**Python**:
```bash
python -m pytest test/integration/ -v
# Runs tests in integration subdirectory
```

**Success Metrics**:
- Exit code: 0
- All integration tests pass
- No service connection failures

**Failure Handling**:
- Different from unit test failures
- May indicate external service issues
- Show which integration point failed
- Example: `Failed: test_database_connection (connection refused)`

**Decision Points**:
- Can retry if external service was temporarily down
- Hard failure if the code itself is broken

---

## Stage 6: Review (Automated Code Review)

**Purpose**: Automated checks for common issues that code review might catch

**Checks**:

1. **Naming Conventions**
   - Variables follow style (snake_case, camelCase, etc.)
   - Functions have clear names
   - No cryptic abbreviations

2. **Documentation**
   - Public functions have doc comments
   - Complex logic has explanations
   - No obvious missing documentation

3. **Performance Patterns**
   - No obvious O(n²) loops
   - No quadratic algorithms
   - No unnecessary allocations

4. **Security Patterns**
   - No hardcoded secrets
   - No SQL injection patterns
   - No unsafe operations (if applicable)

5. **Error Handling**
   - All Result types handled
   - All Option types handled
   - Meaningful error messages

**Gleam**:
```bash
# Custom implementation using AST analysis
# Checks for:
# - Unhandled Result/Option
# - Missing doc comments on pub functions
# - Suspicious patterns
```

**Success Metrics**:
- Issues found: 0
- Warnings: N (acceptable)

**Failure Handling**:
- List each issue found
- Example: `src/handlers.gleam:42: Missing doc comment on public function`
- Provide suggestions for fixes
- Not hard failure (review issues aren't blockers)

**Decision Points**:
- Can approve with review issues
- Shows up in approval metrics as risk factor

---

## Stage 7: Boundary Enforcement

**Purpose**: Ensure code respects contract boundaries (no scope creep)

**Checks**:

1. **File Boundaries**
   - Only modified files match `allowed_files` list
   - No files in `forbidden_files` list touched
   - No new files created outside allowed directories

2. **Dependency Boundaries**
   - If `new_dependencies_allowed: false`, no new imports
   - New dependencies match approved list if restricted
   - No circular dependencies introduced

3. **Size Boundaries**
   - Lines changed < `max_lines_changed`
   - No single function grows too large
   - No scope creep via size

4. **Contract Compliance**
   - Code follows contract requirements
   - Custom constraints met
   - All contract promises kept

**Implementation**:
```gleam
// Pseudo-code
fn validate_boundaries(modified_files: List(String), contract: Contract) -> Result(Nil, String) {
  // Check each modified file
  modified_files
  |> list.each(fn(file) {
    if list.any(contract.forbidden_files, fn(pattern) { matches(file, pattern) }) {
      Error("File " <> file <> " is forbidden by contract")
    }
    if !list.any(contract.allowed_files, fn(pattern) { matches(file, pattern) }) {
      Error("File " <> file <> " not in allowed_files")
    }
  })
  |> Ok
}
```

**Success Metrics**:
- All files within boundaries: ✓
- No forbidden files touched: ✓
- Lines changed: N (< max)
- New dependencies: 0 (or approved)

**Failure Handling**:
- Stop immediately if boundary violated
- Example: `ERROR: Modified README.md (forbidden by contract)`
- Hard failure - cannot proceed if boundary is violated

**Decision Points**:
- **This is hard enforced** - no approval can override
- Prevents accidental scope creep

---

## Stage 8: Integrate (Merge Integration)

**Purpose**: Verify no merge conflicts with current main branch

**Criteria for Success**:
- No merge conflicts with main
- Can cleanly integrate changes
- All dependencies resolved

**Implementation**:
```bash
# Attempt merge
git merge origin/main --no-commit --no-ff
# or
jj rebase --to origin/main

# Check for conflicts
# If none, success
# If conflicts, fail
```

**Success Metrics**:
- Merge conflicts: 0
- Integration status: "Ready to merge"

**Failure Handling**:
- Show which files have conflicts
- Example: `Conflict in src/web/handlers.gleam (2 hunks)`
- Not hard failure (conflicts can be resolved)

**Decision Points**:
- Can retry after conflict resolution
- Usually indicates main branch changed significantly

---

## Stage 9: TCR (Test-Code-Revert)

**Purpose**: Final validation that everything works together

**Process**:
1. Run full test suite one more time
2. Run integration tests one more time
3. Run coverage one more time
4. Aggregate all results
5. If all pass: Mark ready for deployment
6. If any fail: Revert all changes (TCR discipline)

**Implementation**:
```gleam
fn tcr_stage(workspace: Workspace) -> Result(Nil, String) {
  // Run all tests
  case run_all_tests(workspace) {
    Ok(results) if all_passed(results) -> {
      // Success - code is ready
      Ok(Nil)
    }
    _ -> {
      // Failure - revert everything
      jj_restore_from_previous(workspace)
      Error("Tests failed, workspace reverted")
    }
  }
}
```

**Success Metrics**:
- All tests pass: ✓
- No regressions: ✓
- Coverage maintained: ✓
- Ready for deployment: ✓

**Failure Handling**:
- If any test fails: Revert workspace with `jj restore --from @-`
- Show what failed
- Mark task as failed
- Prompt for rework

**Decision Points**:
- **Hard failure** - if TCR fails, nothing is deployed
- This is the safety mechanism preventing broken code

---

## Stage 10: Deploy (Deployment Preparation)

**Purpose**: Prepare for deployment with feature flags and monitoring

**What It Does**:

1. **Generate Feature Flag Configuration**
```yaml
feature_flags:
  factory_bd_52_1:
    initial_percentage: 1
    gradual_rollout:
      - percentage: 1
        duration_minutes: 5
        monitoring_gates:
          - error_rate < 1%
          - latency_p99 < 100ms
      - percentage: 10
        duration_minutes: 5
      - percentage: 100
```

2. **Generate Deployment Plan**
```yaml
deployment_plan:
  change_summary: "Added batch upload handler"
  files: 2
  lines_changed: 47
  tests: 15
  coverage: 89%
  rollout_strategy: "gradual (1% → 10% → 100%)"
  estimated_time: 15 minutes
```

3. **Set Up Monitoring Alerts**
```yaml
monitoring:
  alerts:
    - error_rate > 2% → automatic rollback
    - latency_p99 > 200ms → automatic pause at current %
    - memory_increase > 50% → investigate
```

4. **Generate Approval Prompt**
```
Ready for deployment:
  Task: batch-upload
  Files: 2 modified
  Tests: 15 passed
  Coverage: 89%

  Approve for gradual rollout? (1% → 10% → 100%)
  [y/n]
```

**Success Metrics**:
- Feature flag created: ✓
- Monitoring alerts configured: ✓
- Deployment plan generated: ✓
- Awaiting human approval

**Failure Handling**:
- If any prerequisite failed: Cannot reach stage 10
- If deployment preparation fails: Show error details

**Decision Points**:
- **Human approval required** - nothing deploys without `factory approve`
- This is where human oversight happens

---

## Stage Failure & Recovery

### If a Stage Fails at Different Points

**Stage 1-2 (Implement, Unit Test) Failure**:
- Code is broken
- Workspace is reverted
- Task marked as failed
- Developer must fix issues

**Stage 3 (Coverage) Failure**:
- Code works but not well tested
- Human sees low coverage in approval
- Can approve anyway (accepts risk)
- Or reject and request more tests

**Stage 4 (Lint) Failure**:
- Code is working but doesn't follow style
- Can auto-fix and retry
- Or human approves despite lint issues
- Not a blocker

**Stage 5-6 (Integration, Review) Failure**:
- Code has issues but not critical
- Human sees in approval metrics
- Can approve or request fixes

**Stage 7 (Boundary) Failure**:
- Code violates contract
- **Cannot approve** - hard stop
- Must rework to respect boundaries
- This prevents scope creep

**Stage 8 (Integrate) Failure**:
- Merge conflicts exist
- Can retry after resolution
- Human decides if resolution is needed

**Stage 9 (TCR) Failure**:
- Something breaks in full integration
- **Automatic revert** - code doesn't commit
- Forces fixes before any deployment

**Stage 10 (Deploy) Reached**:
- All technical checks passed
- **Human decides** on deployment
- Feature flag controls blast radius

---

## Monitoring During Deployment

### Gradual Rollout Gates

After `factory approve`:

1. **Enable for 1% of users**
   - Monitor for 5 minutes
   - Check: error_rate, latency, memory
   - If healthy: proceed to 10%
   - If unhealthy: automatic rollback

2. **Enable for 10% of users**
   - Monitor for 5 minutes
   - Same health checks
   - If healthy: proceed to 100%
   - If unhealthy: automatic rollback

3. **Enable for 100% of users**
   - Full rollout complete
   - Continue monitoring
   - Can manually rollback if needed

---

## Custom Pipelines

Tasks can override the standard pipeline:

```yaml
# contract.yaml
contract:
  language: Gleam
  pipeline:
    - name: implement
      command: "gleam build --target javascript"
    - name: custom_unit_test
      command: "gleam test --filter package_name"
    - name: custom_lint
      command: "gleam format --check"
    # Skip coverage for this task
    # Custom stages added
```

This allows:
- Skipping unnecessary stages
- Adding custom validation
- Reordering stages
- Using different commands

---

## Summary

The 10-stage pipeline provides:

1. ✅ **Compilation verification** (Stage 1)
2. ✅ **Correctness assurance** (Stages 2, 9)
3. ✅ **Quality metrics** (Stages 3-6)
4. ✅ **Safety enforcement** (Stages 7-8)
5. ✅ **Human oversight** (Stage 10)

Each stage is **autonomous and fast** (typically < 2 minutes each), giving humans clear, objective data for 2-minute approval decisions instead of 30-minute deep reviews.
