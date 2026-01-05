# Factory v2: Implementation Roadmap

**Purpose**: Concrete, prioritized implementation plan addressing all 10 critical problems

**Status**: Ready for development

**Timeline**: Phased approach, one phase at a time

---

## The 10 Critical Problems (Summary)

| # | Problem | v1 Issue | v2 Solution |
|---|---------|---------|------------|
| 1 | Auto-Commit Trap | Self-approval loop | REMOVE auto-commit, require manual approval |
| 2 | Fake Confidence | 0.92 precision = guess | Show metrics, you decide |
| 3 | Learning Backfires | General caution | Surgical: add specific test per incident |
| 4 | Monitoring Lag | 5-60 min delay | Feature flags (instant control) |
| 5 | Audit Confusion | System auto-updates | Explicit statuses + person tracking |
| 6 | No Boundary Enforce | Hope prevents scope creep | Pre-commit hook enforcement |
| 7 | Coverage Meaningless | Just percentage | Critical paths analysis |
| 8 | Error Output Noise | Buried errors | Extract + summarize key info |
| 9 | Language Agnostic | Hardcoded for Go | Auto-detect + generate config |
| 10 | Rollback Unclear | Mysterious revert | Explicit jj commands documented |

---

## Phase 1: Safety Guarantees (BLOCKING)

**Duration**: 2-3 days of work

**Why first**: Without these, the system can cause harm. Everything else depends on safety.

### Problem 1: Remove Auto-Commit

**Changes needed**:

```gleam
// Before (v1)
if confidence > 0.90 {
  auto_commit()
}

// After (v2)
// Just provide metrics, no auto-commit at all
show_deployment_readiness_metrics()
wait_for_manual_approval()
```

**Deliverable**:
- [ ] Remove auto-commit logic from factory.gleam
- [ ] Add `factory approve <task>` command
- [ ] Display metrics before asking for approval
- [ ] Require manual confirmation

**Test**:
```bash
factory run batch-upload
# ... stages pass ...
# Output shows metrics, not auto-deploying
# Requires: factory approve batch-upload
```

---

### Problem 4: Feature Flags Mandatory

**Changes needed**:

Create feature flag layer:

```gleam
// Add to factory.gleam

type DeploymentStrategy {
  Canary(percentage: Int, duration_minutes: Int)  // 1% for 5 min
  GradualRollout                                   // 1% → 10% → 100%
  Manual                                           // You control percentage
}

fn deploy_with_feature_flag(
  task: Task,
  strategy: DeploymentStrategy,
) -> Result(DeploymentResult, Error) {
  // 1. Enable feature flag at percentage
  // 2. Start monitoring
  // 3. Collect metrics
  // 4. Ask for next decision
}
```

**Configuration**:

```yaml
# .factory/bd-52.1/deployment.yaml

deployment:
  strategy: gradual

  stage_1_canary:
    percentage: 1
    duration: 5_minutes
    auto_rollback_on:
      - error_rate_spike > 10%
      - panic_detected

  stage_2_early_adopters:
    percentage: 10
    duration: 30_minutes
    requires_your_decision: true

  stage_3_production:
    percentage: 100
    requires_your_approval: true
```

**Deliverable**:
- [ ] Feature flag abstraction in code
- [ ] Integration with flag service (Firebase, LaunchDarkly, or custom)
- [ ] Automatic canary deployment
- [ ] Metrics collection during rollout
- [ ] Instant rollback capability

**Test**:
```bash
factory deploy --approve batch-upload
# Enables flag to 1% automatically
# Monitors for 5 minutes
# Asks: "Ready for 10%?"
```

---

### Problem 6: Code Boundary Enforcement

**Changes needed**:

Pre-commit hook that reads contract:

```bash
# .git/hooks/pre-commit (generated per workspace)

#!/bin/bash

TASK_ID="${FACTORY_TASK_ID}"
CONTRACT_FILE=".factory/${TASK_ID}/contract.yaml"

# Parse allowed/forbidden files from contract
ALLOWED_FILES=$(yq '.contract.boundaries.allowed_files[]' "$CONTRACT_FILE")
FORBIDDEN_FILES=$(yq '.contract.boundaries.forbidden_files[]' "$CONTRACT_FILE")

# Get staged files
STAGED=$(git diff --cached --name-only)

# Check each file
for file in $STAGED; do
  if echo "$FORBIDDEN_FILES" | grep -q "^${file}$"; then
    echo "✗ BLOCKED: ${TASK_ID} cannot modify $file"
    echo "✗ Allowed files:"
    echo "$ALLOWED_FILES" | sed 's/^/  /'
    exit 1
  fi
done

exit 0
```

**Deliverable**:
- [ ] Generate pre-commit hook per workspace
- [ ] Parse contract YAML for boundaries
- [ ] Block commits that violate boundaries
- [ ] Clear error messages with allowed files

**Test**:
```bash
factory new batch-upload --from bd-52.1
cd batch-upload
git add src/core/rate_limiter.gleam  # Wrong file!
git commit -m "..."

# Hook fires:
# ✗ BLOCKED: bd-52.1 cannot modify src/core/rate_limiter.gleam
# ✗ Allowed files:
#   - src/web/handlers.gleam
```

---

### Problem 5: Explicit Status Lifecycle

**Changes needed**:

Update Beads integration to track person + decision:

```gleam
// Add to persistence.gleam

type TaskStatus {
  Created
  ReadyForDecision
  Approved(approved_by: String, timestamp: String)
  Deployed(deployed_by: String, timestamp: String)
  Monitoring(started_at: String)
  MonitoringComplete(ended_at: String)
  FailedMonitoring(reason: String)
}

fn update_beads_status(task: Task, status: TaskStatus) -> Result(Nil, Error) {
  // Write to beads with:
  // - Status
  // - Person who made decision
  // - Timestamp
  // - Reason/notes
}
```

**Deliverable**:
- [ ] Extend TaskStatus enum with all lifecycle states
- [ ] Track "set_by" person on state changes
- [ ] Store timestamp for every status change
- [ ] Integrate with Beads for audit trail
- [ ] Generate audit report

**Test**:
```bash
factory audit bd-52.1

Status history:
✓ 10:30 created (system)
✓ 14:20 ready_for_decision (system)
✓ 14:22 approved (lewis)
✓ 14:25 deployed (lewis)
✓ 14:30 monitoring (system)
```

---

## Phase 2: Core Functionality (3-4 days)

**Why now**: Depends on Phase 1 safety guarantees

### Problem 8: Error Extraction & Summarization

**Changes needed**:

```gleam
// Add to stages.gleam

type StageOutput {
  stage_name: String,
  exit_code: Int,
  duration_ms: Int,

  // Key fields extracted
  root_cause: Option(String),
  error_context: Option(String),

  // Full output if needed
  full_output: String,
  full_log_path: Option(String),
}

fn extract_error_summary(output: String, stage: String) -> Option(String) {
  // Look for first "Error:" or "panic:" or "failed"
  // Extract next 5-10 lines for context
  // Return summarized version
}
```

**Deliverable**:
- [ ] Parse command output for errors
- [ ] Extract root cause and context
- [ ] Summarize to 5-10 critical lines
- [ ] Save full output to .factory/logs/
- [ ] Format as clear, actionable message

**Test**:
```
Stage 3: unit-test FAILED

ROOT CAUSE:
test_concurrent_requests_50 failed
race condition in rate limiter
src/core/rate_limiter.gleam:87

ACTION:
factory stage bd-52.2 unit-test  # Retry
factory debug bd-52.2            # Debug mode
factory explain bd-52.2          # Ask Claude

FULL LOG: .factory/bd-52.2/stage-3-failure.log
```

---

### Problem 9: Language Detection & Auto-Config

**Changes needed**:

```gleam
// Add to repo.gleam

type Language {
  Gleam
  Go
  Rust
  Python
}

fn detect_language() -> Result(Language, Error) {
  // Check for manifest files in order:
  // 1. gleam.toml → Gleam
  // 2. go.mod → Go
  // 3. Cargo.toml → Rust
  // 4. pyproject.toml → Python
}

fn generate_language_config(lang: Language) -> Config {
  // Return correct stages + commands for language
}
```

**Configuration templates**:

```yaml
# templates/gleam.yaml
language: gleam
stages:
  implement: "gleam check && gleam build"
  unit_test: "gleam test"
  coverage: "gleam test --coverage"
  lint: "gleam format --check"
  static: "gleam check"

# templates/go.yaml
language: go
stages:
  implement: "go build ./..."
  unit_test: "go test -v ./..."
  coverage: "go test -cover ./..."
  lint: "gofmt -l ."
  static: "go vet ./..."

# templates/rust.yaml
language: rust
stages:
  implement: "cargo build"
  unit_test: "cargo test"
  coverage: "cargo tarpaulin"
  lint: "cargo fmt -- --check"
  static: "cargo clippy"

# templates/python.yaml
language: python
stages:
  implement: "python -m py_compile src/"
  unit_test: "pytest"
  coverage: "pytest --cov"
  lint: "black --check ."
  static: "mypy src/"
```

**Deliverable**:
- [ ] Implement language detection algorithm
- [ ] Create config templates for 4 languages
- [ ] Auto-generate .factory/<language>.yaml on workspace creation
- [ ] Generate .moon/tasks/factory.yml with correct commands
- [ ] Support custom commands via .factory.toml override

**Test**:
```bash
factory new batch-upload  # In Gleam repo
# Detects: gleam.toml
# Generates: .factory/gleam.yaml
# Stages use: gleam check, gleam build, gleam test, etc.

factory new feature  # In Go repo
# Detects: go.mod
# Generates: .factory/go.yaml
# Stages use: go build, go test, gofmt, etc.
```

---

### Problem 10: Explicit TCR Specifications

**Changes needed**:

```gleam
// Add to domain.gleam

type TCRBehavior {
  Enabled: {
    on_failure: {
      command: String,      // "jj restore --from @-"
      restore_to: String,   // "@-" (parent change)
      meaning: String,      // Explanation
    },
    on_success: {
      action: String,       // "jj describe -m '...'"
      creates_snapshot: Bool,
      advances_to_next: Bool,
    }
  },
  Disabled: { reason: String }
}

fn apply_tcr(stage: Stage, result: StageResult) -> Result(TCRResult, Error) {
  case stage.tcr {
    Enabled(tcr_config) if result.exit_code != 0 ->
      run_command(tcr_config.on_failure.command)

    Enabled(tcr_config) if result.exit_code == 0 ->
      // Create snapshot, advance to next change
      Ok(TCRPassed)

    Disabled(_) ->
      // No revert, just report result
      Ok(NoTCR)
  }
}
```

**Contract specification**:

```yaml
# .factory/bd-52.1/contract.yaml

tcr_behavior:
  implement:
    enabled: true
    on_failure:
      command: "jj restore --from @-"
      restore_to: "@-"
      meaning: "Revert to parent change (last known good)"
    on_success:
      action: "jj describe -m 'TCR pass: implement'"
      creates_snapshot: true

  unit-test:
    enabled: true
    on_failure:
      command: "jj restore --from @-"
      restore_to: "@-"
      meaning: "Revert failed test changes"

  tdd-setup:
    enabled: false
    reason: "Tests must FAIL at start (red state)"

  accept:
    enabled: false
    reason: "Final gate, no auto-revert"
```

**Deliverable**:
- [ ] Document TCR behavior per stage in contract
- [ ] Make TCR revert automatic and transparent
- [ ] Show exactly what gets reverted
- [ ] Display before/after state
- [ ] Update Beads with revert metadata

**Test**:
```bash
factory run bd-52.1
# Stage 3 fails
# TCR triggered: jj restore --from @-
# Output shows what was reverted
# Status returned to last test-passing state
```

---

## Phase 3: Polish (2-3 days)

**Why later**: Nice to have, not critical for functionality

### Problem 2: Metrics Dashboard

**Implementation**:

```gleam
// Add to factory.gleam

fn show_deployment_readiness(task: Task, metrics: Metrics) {
  // Display formatted table:
  // ✓ Test coverage: 89% (threshold: 80%)
  // ✓ Lines changed: 47 (small ✓)
  // ✓ New dependencies: 0 (none ✓)
  // ⚠️ Introduces new patterns (requires review)
  // Risk level: MEDIUM (6/10)
}
```

**Deliverable**:
- [ ] Collect metrics during pipeline
- [ ] Format as readable dashboard
- [ ] Show green/yellow/red indicators
- [ ] Include decision guidance
- [ ] Estimate review time

---

### Problem 7: Coverage Analysis

**Implementation**:

```gleam
// Add to stages.gleam

type CoverageAnalysis {
  overall_percent: Float,
  critical_paths: List(#(String, Float, Option(String))), // (name, %, gaps)
  by_function: List(#(String, Float)),
  recommendation: String,
}

fn analyze_coverage(coverage_output: String) -> CoverageAnalysis {
  // Parse coverage output
  // Identify critical functions
  // Highlight what's missing
  // Recommend action
}
```

**Deliverable**:
- [ ] Parse coverage output (JSON format)
- [ ] Identify critical vs non-critical code
- [ ] Show what's tested vs untested
- [ ] Recommend specific tests to add
- [ ] Block deployment if critical paths untested

---

### Problem 3: Surgical Learning

**Implementation**:

```gleam
// Add to incident_learning.gleam

fn create_learning_from_incident(incident: Incident) -> TaskRequirement {
  // Extract root cause
  // Create specific test requirement
  // Apply to future tasks
  // Track effectiveness
}
```

**Deliverable**:
- [ ] Incident analysis system
- [ ] Extract root cause
- [ ] Create specific test requirements
- [ ] Apply to future contract templates
- [ ] Track learning effectiveness

---

## Phase 4: Integration (Ongoing)

**Duration**: Parallel with other phases

### Beads Integration
- [ ] Link factory workspaces to beads issues
- [ ] Auto-update beads when stages pass/fail
- [ ] Create child issues for discovered work
- [ ] Track metrics in beads

### Codanna Integration
- [ ] Search for similar patterns before starting work
- [ ] Validate code follows existing patterns
- [ ] Detect scope creep via semantic analysis
- [ ] Suggest improvements based on codebase patterns

### Intent Spec Integration
- [ ] Auto-decompose intent specs into beads issues
- [ ] Generate factory stages from intent specs
- [ ] Validate code against intent rules
- [ ] Monitor production against intent spec

### Moon Integration
- [ ] Generate .moon/tasks/factory.yml per workspace
- [ ] Call moon run for each stage
- [ ] Aggregate results from moon
- [ ] Integrate feature flags with moon

---

## Implementation Order (Step-by-Step)

### Week 1: Safety First
1. **Remove auto-commit** (Problem 1)
   - Delete auto-commit logic
   - Add `factory approve` command
   - Test with sample task

2. **Add feature flags** (Problem 4)
   - Create flag abstraction
   - Implement canary deployment (1% for 5 min)
   - Test gradual rollout

3. **Boundary enforcement** (Problem 6)
   - Generate pre-commit hook
   - Test boundary violation blocking
   - Verify clear error messages

4. **Status lifecycle** (Problem 5)
   - Extend TaskStatus enum
   - Track person + timestamp
   - Integrate with Beads

### Week 2: Core Functionality
5. **Error summarization** (Problem 8)
   - Extract root cause
   - Summarize to 5-10 lines
   - Save full logs

6. **Language detection** (Problem 9)
   - Implement language detection
   - Create config templates
   - Auto-generate configs

7. **TCR specifications** (Problem 10)
   - Document TCR per stage
   - Make revert automatic
   - Show what gets reverted

### Week 3: Polish
8. **Metrics dashboard** (Problem 2)
   - Collect metrics
   - Format readably
   - Include decision guidance

9. **Coverage analysis** (Problem 7)
   - Parse coverage output
   - Identify critical paths
   - Recommend tests

10. **Surgical learning** (Problem 3)
    - Incident analysis
    - Root cause extraction
    - Create test requirements

### Ongoing: Integration
- Beads, Codanna, Intent, Moon integration
- These can happen in parallel with core work

---

## Testing Strategy

### Unit Tests
- Language detection
- Error extraction
- Coverage analysis
- TCR logic

### Integration Tests
- Full pipeline with feature flags
- Boundary enforcement + commit
- Status lifecycle with Beads
- Moon task execution

### Manual Tests
- Create real workspace
- Work on actual task
- Run full pipeline
- Approve and deploy
- Monitor rollout

---

## Success Criteria for v2

### Phase 1 (Safety)
- [ ] No auto-commits
- [ ] Feature flags mandatory
- [ ] Boundary enforcement working
- [ ] Audit trail accurate

### Phase 2 (Core)
- [ ] Language agnostic (Gleam, Go, Rust, Python)
- [ ] Error messages clear and actionable
- [ ] TCR behavior explicit
- [ ] All stages run correctly per language

### Phase 3 (Polish)
- [ ] Metrics dashboard informative
- [ ] Coverage analysis actionable
- [ ] Surgical learning creating requirements

### Phase 4 (Integration)
- [ ] Beads issues link to workspaces
- [ ] Codanna finds patterns
- [ ] Intent specs auto-decompose
- [ ] Moon executes all stages

---

## Risk Mitigation

### Risk: Breaking existing code during refactor
**Mitigation**: Keep all old code, add new code alongside, migrate gradually

### Risk: Feature flags don't work with existing tools
**Mitigation**: Start with Firebase (free, proven, simple), can swap later

### Risk: Pre-commit hook too strict, blocks legitimate work
**Mitigation**: Provide escape hatch for legitimate scope expansion (create new issue)

### Risk: TCR reverts too aggressive
**Mitigation**: Make TCR revert always transparent, show what was lost, can undo with git log

---

## What We're NOT Changing

- ✅ Jujutsu workspaces (still best for isolation)
- ✅ 10-stage pipeline (still gold standard)
- ✅ TCR discipline (still core enforcement)
- ✅ Contract-driven development (still foundation)
- ✅ Gleam as implementation language (still fits perfectly)

---

## Summary

**v2 is not a rewrite. It's a hardening.**

We're taking the brilliant v1 architecture and fixing the 10 critical problems:
1. Removing auto-commit trap
2. Replacing fake confidence with real metrics
3. Making learning surgical instead of general
4. Adding feature flag control over monitoring lag
5. Creating explicit audit trails
6. Enforcing boundaries at commit time
7. Making coverage analysis meaningful
8. Extracting errors from noise
9. Supporting all languages
10. Making TCR behavior transparent

**Result**: A system that's safer, faster, clearer, and genuinely ready for solo founder scaling.

