# Factory Gleam - System Architecture

**Status**: Design phase with v1 implementation complete
**Last Updated**: January 5, 2026
**Version**: v2 (in planning phase)

---

## Executive Summary

Factory is a contract-driven, TCR-based CI/CD system built in Gleam that manages automated code generation, testing, and deployment with human oversight. It enforces strict boundaries, dependency awareness, and explicit approval gates.

### Three Core Principles

1. **Contracts First**: Every task has a written contract defining what's allowed (files, dependencies, test coverage)
2. **TCR Discipline**: Test-Code-Revert cycle ensures code only commits when tests pass
3. **Human Oversight**: System prepares everything, humans make deployment decisions

---

## System Architecture

### High-Level Flow

```
┌─────────────────┐
│  User Request   │  (via CLI or Beads task)
│  (Beads task)   │
└────────┬────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│         Factory Orchestrator             │
│  (factory.gleam - main entry point)     │
│  - Parse CLI args                        │
│  - Load task contract                    │
│  - Create jj workspace                   │
│  - Run pipeline stages                   │
└────────┬────────────────────────────────┘
         │
         ├─ Stage 1: Implement ────→ Code generation
         ├─ Stage 2: Unit Test ────→ Run tests
         ├─ Stage 3: Coverage ─────→ Measure coverage
         ├─ Stage 4: Lint ────────→ Style check
         ├─ Stage 5: Integration ─→ Integration tests
         ├─ Stage 6: Review ──────→ Code review checks
         ├─ Stage 7: Boundary ────→ Scope enforcement
         ├─ Stage 8: Integrate ───→ Merge conflicts check
         ├─ Stage 9: TCR ────────→ Full test run
         └─ Stage 10: Deploy ────→ Prepare deployment
         │
         ▼
┌─────────────────────────────────────────┐
│     Human Approval Required              │
│  - Review metrics                        │
│  - Decide on deployment                  │
│  - Enable feature flags (gradual rollout)│
└────────┬────────────────────────────────┘
         │
         ├─ Approved ─→ Feature flag 1%
         │              Monitor 5 min
         │              → 10% → 100%
         │
         └─ Rejected ─→ Mark task failed
                        Return for rework
```

### Core Modules

```
src/
├── factory.gleam       # Main orchestrator and CLI entry point
├── domain.gleam        # Type definitions (Task, Stage, Language, etc.)
├── cli.gleam           # CLI argument parsing
├── process.gleam       # Process execution and result handling
├── repo.gleam          # Repository operations
├── worktree.gleam      # Jujutsu workspace management
├── stages.gleam        # Pipeline stage implementations
├── tcr.gleam           # Test-Code-Revert discipline
├── persistence.gleam   # Task state and result storage
├── integration.gleam   # External tool integrations (moon.dev, Beads)
└── main.gleam          # Entry point stub
```

---

## Domain Model

### Core Types

#### Task
```gleam
pub type Task {
  Task(
    slug: Slug,              // Unique identifier (e.g., "bd-52.1")
    title: String,           // Human-readable title
    description: String,     // Full description
    contract: Contract,      // What's allowed/required
    status: TaskStatus,      // Current state
    created_at: Int,         // Unix timestamp
    started_at: Option(Int), // When work started
    completed_at: Option(Int),
  )
}
```

#### Contract
```gleam
pub type Contract {
  Contract(
    allowed_files: List(String),        // Glob patterns
    forbidden_files: List(String),      // Blocks these
    required_test_coverage: Int,        // Minimum %
    new_dependencies_allowed: Bool,
    max_lines_changed: Int,
    language: Language,                 // Detected or specified
    pipeline: List(Stage),              // Custom or standard
  )
}
```

#### Language
```gleam
pub type Language {
  Go
  Gleam
  Rust
  Python
}
```

#### TaskStatus
```gleam
pub type TaskStatus {
  Created                 // Just created
  Ready                   // Dependencies satisfied, can start
  InProgress              // Work has begun
  ReadyForReview          // All stages passed
  ReviewApproved          // Human approved
  Deploying               // Deployment in progress
  DeployedToStaging       // 1% of users
  DeployedToProduction    // 100% of users
  Done                    // Complete
  Failed(reason: String)  // Error explanation
  Reverted                // TCR reverted changes
}
```

---

## Pipeline Stages (10-Stage Standard)

### Stage 1: Implement
**Purpose**: Verify code compiles and basic structure is correct

**For Gleam**:
```bash
gleam build --target javascript
gleam build --target erlang
```

**For other languages**: Similar build commands

**Output**: Compilation errors or success

---

### Stage 2: Unit Test
**Purpose**: Run all unit tests

**For Gleam**:
```bash
gleam test
```

**Output**: Test results and failures

---

### Stage 3: Coverage
**Purpose**: Measure code coverage

**For Gleam**:
```bash
gleam test --cover
```

**Output**: Coverage percentage and uncovered lines

---

### Stage 4: Lint
**Purpose**: Style and pattern checking

**For Gleam**:
```bash
gleam format --check src/
```

**Output**: Formatting or style violations

---

### Stage 5: Integration
**Purpose**: Integration tests against real dependencies

**Output**: Integration test results

---

### Stage 6: Review
**Purpose**: Automated code review checks

**Checks**:
- Naming conventions
- Documentation completeness
- Performance patterns
- Security patterns

---

### Stage 7: Boundary
**Purpose**: Enforce contract boundaries

**Checks**:
- Modified files match contract
- No new dependencies added (if not allowed)
- Lines changed < max

---

### Stage 8: Integrate
**Purpose**: Check for merge conflicts

**Output**: Merge status

---

### Stage 9: TCR (Test-Code-Revert)
**Purpose**: Final validation before deployment

**Process**:
1. Run full test suite
2. If pass: Mark ready for deployment
3. If fail: Revert all changes

---

### Stage 10: Deploy
**Purpose**: Prepare deployment package

**Output**: Feature flag configuration, rollout plan

---

## Module Responsibilities

### factory.gleam (Main Orchestrator)
- Parse CLI arguments
- Load task and contract from filesystem
- Create jj workspace
- Run pipeline stages in sequence
- Track results
- Report final status

### domain.gleam (Type System)
- All type definitions
- Validation functions
- Pipeline construction
- Standard contracts

### stages.gleam (Pipeline Implementation)
- Execute each stage
- Collect output and errors
- Compare against contract
- Update status

### repo.gleam (Repository Operations)
- Detect language from files
- Get current git state
- Identify changed files
- Track file modifications

### worktree.gleam (Jujutsu Integration)
- Create isolated workspaces with `jj new`
- Manage revisions
- Handle TCR reverts
- Clean up workspaces

### tcr.gleam (TCR Discipline)
- Monitor test status
- Revert on failure
- Track revert history

### persistence.gleam (State Management)
- Load task definitions
- Save execution results
- Track audit trail
- Load/save metrics

### cli.gleam (Argument Parsing)
- Parse `factory new`
- Parse `factory stage`
- Parse `factory approve`
- Validate arguments

### process.gleam (Process Execution)
- Run shell commands
- Capture stdout/stderr
- Handle timeouts
- Track execution time

### integration.gleam (External Tools)
- Call moon.dev tasks
- Update Beads status
- Send metrics to monitoring
- Enable feature flags

---

## Data Flow for a Task

### 1. Task Creation
```
User creates Beads task "bd-52.1"
    ↓
Factory reads from .beads/beads.db
    ↓
Creates contract.yaml defining boundaries
    ↓
User runs: factory new bd-52.1
```

### 2. Workspace Creation
```
User runs: factory new bd-52.1
    ↓
Factory creates: jj new --at bd-52.1
    ↓
Creates isolated workspace
    ↓
Creates .factory/bd-52.1/ directory
```

### 3. Pipeline Execution
```
Factory runs: factory stage bd-52.1 implement
    ↓
Runs gleam build (or go build, cargo build, etc.)
    ↓
Captures output
    ↓
Saves to .factory/bd-52.1/stage-implement.json
    ↓
Repeats for each stage
```

### 4. Results Aggregation
```
After all stages complete
    ↓
Gather metrics:
  - Coverage: 89%
  - Lines: 47
  - Dependencies: 0
  - Files touched: 2
    ↓
Generate approval prompt
    ↓
Show to human
```

### 5. Approval
```
Human reviews metrics
    ↓
Runs: factory approve bd-52.1
    ↓
Feature flag enabled (1% of users)
    ↓
Monitoring begins
    ↓
After 5 minutes with no errors: 10%
    ↓
After 5 minutes with no errors: 100%
```

---

## Safety Mechanisms

### 1. Boundary Enforcement (Pre-Commit Hook)
```gleam
// In stages.gleam - boundary stage
fn validate_boundaries(task: Task, modified_files: List(String)) -> Result(Nil, String) {
  case contract_allows_files(task.contract, modified_files) {
    Ok(Nil) -> Ok(Nil)
    Error(msg) -> Error("Boundary violation: " <> msg)
  }
}
```

**Effect**: Prevents scope creep before commit

### 2. Feature Flags (Mandatory)
```yaml
# .factory/bd-52.1/feature-flags.yaml
gradual_rollout:
  - percentage: 1
    duration_minutes: 5
    monitoring_gates: [no_errors]
  - percentage: 10
    duration_minutes: 5
    monitoring_gates: [no_errors]
  - percentage: 100
```

**Effect**: Limits blast radius, allows rollback

### 3. Explicit Approval Gate
```gleam
// Only humans can call this
pub fn approve_deployment(task: Task) -> Result(FeatureFlagState, String) {
  // This is NOT automatic
}
```

**Effect**: Prevents auto-deployment

### 4. TCR Discipline
```gleam
// In tcr.gleam
pub fn test_then_commit(workspace: Workspace) -> Result(Nil, String) {
  let result = run_tests(workspace)
  case result {
    Ok(test_result) -> commit_workspace(workspace)
    Error(_) -> revert_workspace(workspace)
  }
}
```

**Effect**: Code only commits when tests pass

### 5. Audit Trail
```yaml
# .factory/bd-52.1/audit.yaml
events:
  - timestamp: 2025-01-15T14:30:00Z
    event: task_created
    actor: system
  - timestamp: 2025-01-15T14:31:00Z
    event: workspace_created
    actor: system
  - timestamp: 2025-01-15T14:45:00Z
    event: stage_completed
    actor: system
    stage: implement
    result: success
  - timestamp: 2025-01-15T15:00:00Z
    event: approval_requested
    actor: system
    required_fields: [coverage, lines, dependencies]
  - timestamp: 2025-01-15T15:02:00Z
    event: approved_for_deployment
    actor: lewis
    decision: approve
    rollout_strategy: gradual
```

**Effect**: Clear record of who decided what

---

## Error Handling

### Three-Level Error Response

#### Level 1: Extraction (Immediate)
Extract root cause from error output:
```
❌ Compilation failed
   Error in src/web/handlers.gleam:47
   Unexpected token: `let`
   Expected: `pub fn` or `import`
```

#### Level 2: Summarization
```
Summary for human:
  - Problem: Syntax error in handler definition
  - Location: src/web/handlers.gleam:47
  - Fix: Check function declaration syntax
  - Similar: 3 other functions in file use correct syntax
```

#### Level 3: Full Log (For Deep Debugging)
```
Full output saved to: .factory/bd-52.1/stage-implement-full.log
(Contains complete compiler output for investigation)
```

---

## Extension Points

### Adding a New Language
1. Add to `Language` type in domain.gleam
2. Implement detection in `repo.gleam`
3. Add build/test commands in `stages.gleam`
4. Add to `language_default_config()` in persistence.gleam

### Adding a Custom Stage
1. Define in contract.yaml for specific task
2. Implement in stages.gleam
3. Track output in persistence.gleam

### Adding Integration
1. Define interface in integration.gleam
2. Implement external tool calls
3. Handle errors and retries

---

## Deployment Architecture (with moon.dev)

### Factory as Orchestrator
```
┌─────────────────┐
│ Factory         │  - Manages tasks
│ (Gleam app)     │  - Runs pipeline
│                 │  - Calls moon
└────────┬────────┘
         │
         │ moon run factory:stage-implement
         │ moon run factory:stage-unit-test
         ▼
┌─────────────────┐
│ Moon Repository │  - Caches results
│ Task Scheduler  │  - Handles parallelism
│ (CI/CD)         │  - Cross-repo aware
└─────────────────┘
```

### Task Contract with moon
```yaml
# moon.yml
factory:
  stages:
    implement:
      command: factory stage ${task} implement
      inputs: [src/**/*.gleam, gleam.toml]
      outputs: [.factory/${task}/implement.json]

    unit-test:
      command: factory stage ${task} unit-test
      inputs: [src/**/*.gleam, test/**/*.gleam]
      outputs: [.factory/${task}/unit-test.json]
```

---

## Configuration Files

### .factory/contract.yaml
Defines what a task can and cannot do:
```yaml
allowed_files:
  - src/**
  - test/**

forbidden_files:
  - README.md
  - moon.yml

contract:
  min_coverage: 80
  max_lines: 500
  new_deps: false
```

### .factory/bd-52.1/metrics.yaml
Generated by pipeline, shows decision-making data:
```yaml
test_coverage: 89%
cyclomatic_complexity: +2
lines_changed: 47
new_dependencies: 0
files_modified:
  - src/web/handlers.gleam
  - src/web/handlers_test.gleam
```

### .factory/bd-52.1/feature-flags.yaml
Defines rollout strategy:
```yaml
gradual_rollout:
  - percentage: 1
    duration_minutes: 5
  - percentage: 10
    duration_minutes: 5
  - percentage: 100
```

---

## Summary

Factory is a **human-in-the-loop** system that:

1. **Creates** tasks with clear contracts
2. **Runs** a 10-stage pipeline in isolated jj workspaces
3. **Collects** metrics (coverage, lines, dependencies)
4. **Presents** data to humans for decision-making
5. **Executes** approved deployments with gradual rollout
6. **Maintains** audit trail of all decisions
7. **Enforces** boundaries and TCR discipline

The goal: **10x leverage without giving up control.**
