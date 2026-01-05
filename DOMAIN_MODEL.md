# Factory Gleam - Domain Model

**Complete specification of all types and data structures**

**Last Updated**: January 5, 2026

---

## Core Entities

### Language

Represents supported programming languages for code generation.

```gleam
pub type Language {
  Go
  Gleam
  Rust
  Python
}
```

**Detection Order** (for ambiguous repos):
1. Check for language-specific files:
   - `go.mod` → Gleam
   - `gleam.toml` → Gleam (highest priority)
   - `Cargo.toml` → Rust
   - `pyproject.toml` or `requirements.txt` → Python

2. If multiple detected:
   - Return error asking for explicit specification
   - Or use primary language heuristic

3. If none detected:
   - Return error, require explicit specification

**Configuration by Language**:

| Language | Build | Test | Lint | Format |
|----------|-------|------|------|--------|
| Gleam | `gleam build --target javascript` | `gleam test` | `gleam format --check` | `gleam format` |
| Go | `go build ./...` | `go test ./...` | `go vet ./...` | `gofmt` |
| Rust | `cargo build --release` | `cargo test --lib` | `cargo clippy` | `cargo fmt` |
| Python | `python -m py_compile` | `pytest test/` | `flake8` | `black` |

---

### Slug

Validated unique identifier for tasks, typically from Beads.

```gleam
pub type Slug
  // Wrapped String type
  // Valid: alphanumeric, hyphen, dot
  // Invalid: spaces, special chars, empty
  // Examples: "bd-52", "bd-52.1", "task-01"
```

**Validation Rules**:
- Must be 1-50 characters
- Only: `[a-zA-Z0-9\-\.]`
- Must start with letter
- Cannot be empty

**Construction**:
```gleam
// Unsafe - don't use
let slug = "bd-52.1"  // String

// Safe - use this
let slug = validate_slug("bd-52.1")  // Result(Slug, String)
```

**Usage**:
```gleam
// Get string back
let slug_str = slug_to_string(slug)  // "bd-52.1"

// Use in paths
let path = ".factory/" <> slug_to_string(slug)
```

---

### Stage

Represents a single pipeline stage.

```gleam
pub type Stage {
  Stage(
    name: String,              // "implement", "unit-test", "coverage"
    description: String,       // "Run Gleam build"
    command: String,           // Actual command to execute
    timeout_seconds: Int,      // Max time for stage
    required_success: Bool,    // Must pass or pipeline fails
  )
}
```

**Examples**:

```gleam
// Stage 1: Implement
Stage(
  name: "implement",
  description: "Compile code",
  command: "gleam build --target javascript && gleam build --target erlang",
  timeout_seconds: 60,
  required_success: True,  // Pipeline stops if this fails
)

// Stage 3: Coverage
Stage(
  name: "coverage",
  description: "Measure test coverage",
  command: "gleam test --cover",
  timeout_seconds: 120,
  required_success: False,  // Can proceed even if coverage low
)

// Stage 4: Lint
Stage(
  name: "lint",
  description: "Check code style",
  command: "gleam format --check src/",
  timeout_seconds: 30,
  required_success: False,  // Just informational
)
```

**Required vs Optional**:
- `required_success: True` → Pipeline stops if fails
- `required_success: False` → Pipeline continues, human sees issue
- Only stages 1, 2, 7, 9 are typically hard-required

---

### Contract

Defines what a task is allowed to do and what it must achieve.

```gleam
pub type Contract {
  Contract(
    allowed_files: List(String),        // Glob patterns: src/**, test/**
    forbidden_files: List(String),      // Never touch: README.md, moon.yml
    required_test_coverage: Int,        // Percentage: 80
    new_dependencies_allowed: Bool,     // Can add imports?
    max_lines_changed: Int,             // Scope limit: 500
    language: Language,                 // Go, Gleam, Rust, Python
    pipeline: List(Stage),              // Custom stages or standard_pipeline()
    custom_fields: Dict(String, String),// Project-specific fields
  )
}
```

**Example Contracts**:

```gleam
// Strict contract for critical infrastructure
let strict = Contract(
  allowed_files: ["src/web/handlers.gleam", "src/web/handlers_test.gleam"],
  forbidden_files: ["src/core/**", "src/database/**"],
  required_test_coverage: 95,
  new_dependencies_allowed: False,
  max_lines_changed: 100,
  language: Gleam,
  pipeline: standard_pipeline(),
  custom_fields: dict.new(),
)

// Loose contract for documentation
let loose = Contract(
  allowed_files: ["doc/**", "README.md", "examples/**"],
  forbidden_files: ["src/**", ".moon/tasks/**"],
  required_test_coverage: 0,  // No tests needed
  new_dependencies_allowed: False,
  max_lines_changed: 5000,
  language: Gleam,
  pipeline: [  // Custom pipeline without code checks
    Stage("format", "Format markdown", "...", 30, True),
    Stage("validate", "Check links", "...", 60, False),
  ],
  custom_fields: dict.new(),
)
```

**File Patterns**:
```gleam
// Glob syntax supported
"src/**"              // Everything in src/ and subdirs
"src/*.gleam"         // Only .gleam files directly in src/
"test/**/*_test.gleam"  // Only test files with _test.gleam suffix
"!src/internal/**"    // Everything except src/internal/
```

---

### TaskStatus

Represents the state of a task through its lifecycle.

```gleam
pub type TaskStatus {
  Created              // Just created, waiting for dependencies
  Ready                // Dependencies satisfied, can start work
  InProgress           // Work has begun, running stages
  ReadyForReview       // All stages passed, awaiting human approval
  ReviewApproved       // Human approved the changes
  Deploying            // Deployment in progress (feature flags rolling out)
  DeployedToStaging    // 1% of users have new version
  DeployedToProduction // 100% of users have new version
  Done                 // Task complete and fully deployed
  Failed(reason: String)  // Task failed with this reason
  Reverted             // TCR reverted changes due to failure
}
```

**Status Transitions**:

```
Created
  ↓
Ready (when dependencies satisfied)
  ↓
InProgress (when work starts)
  ├─ Implement fails → Failed("compilation error")
  ├─ Unit test fails → Failed("3 tests failed") → (revert) → Reverted
  ├─ Boundary fails → Failed("file xxx forbidden")
  │
  └─ All stages pass → ReadyForReview
     ├─ Human rejects → (return to InProgress for rework)
     │
     └─ Human approves → ReviewApproved
        ↓
        Deploying (feature flag rollout)
        ├─ 1% for 5 min → DeployedToStaging
        ├─ 10% for 5 min → (still DeployedToStaging)
        ├─ Error detected at 1% → Reverted (auto-rollback)
        │
        └─ 100% healthy → DeployedToProduction
           ↓
           Done
```

**Important**:
- States are **immutable** - only tracked in persistence layer
- Transitions are **driven by stages** or **human decisions**
- **No backward transitions** except Reverted (which creates new task)

---

### Task

The complete definition of a task from creation to completion.

```gleam
pub type Task {
  Task(
    slug: Slug,                     // Unique ID: "bd-52.1"
    title: String,                  // "Implement batch upload handler"
    description: String,            // Full description from Beads
    contract: Contract,             // What's allowed/required
    status: TaskStatus,             // Current state
    created_at: Int,                // Unix timestamp
    started_at: Option(Int),        // When work began
    completed_at: Option(Int),      // When finished
  )
}
```

**Lifecycle**:
1. Created (timestamp: 2025-01-15 14:30:00)
2. Started (timestamp: 2025-01-15 14:31:00)
3. Completed (timestamp: 2025-01-15 15:45:00)

**Duration Calculation**:
```gleam
fn duration_seconds(task: Task) -> Int {
  case task.completed_at, task.started_at {
    option.Some(end), option.Some(start) -> end - start
    _, _ -> 0  // Not completed yet
  }
}
```

---

### StageResult

Result from executing a single pipeline stage.

```gleam
pub type StageResult {
  StageResult(
    stage_name: String,        // "implement", "unit-test"
    status: String,            // "success" | "failure" | "timeout" | "skipped"
    stdout: String,            // Captured output
    stderr: String,            // Captured errors
    duration_ms: Int,          // How long it took
    metrics: Dict(String, String),  // Stage-specific metrics
  )
}
```

**Example Results**:

```gleam
// Successful compilation
StageResult(
  stage_name: "implement",
  status: "success",
  stdout: "Compiled successfully\nGenerated 2 targets",
  stderr: "",
  duration_ms: 2340,
  metrics: dict.from_list([
    #("targets_generated", "2"),
    #("warnings", "0"),
  ]),
)

// Test failure
StageResult(
  stage_name: "unit-test",
  status: "failure",
  stdout: "Running tests...\n",
  stderr: "Test failed: test_concurrent_requests\n  Expected: true\n  Got: false",
  duration_ms: 5678,
  metrics: dict.from_list([
    #("total_tests", "15"),
    #("passed", "12"),
    #("failed", "3"),
  ]),
)

// Timeout
StageResult(
  stage_name: "integration",
  status: "timeout",
  stdout: "Starting integration tests...",
  stderr: "Process killed after 60000ms",
  duration_ms: 60000,
  metrics: dict.from_list([
    #("timeout_seconds", "60"),
  ]),
)
```

**Metrics by Stage**:

| Stage | Metrics |
|-------|---------|
| implement | targets_generated, warnings, errors |
| unit-test | total_tests, passed, failed, duration |
| coverage | coverage_percent, covered_lines, uncovered_lines |
| lint | violations_count, violations_list |
| integration | tests_run, passed, failed |
| review | issues_count, issue_types |
| boundary | files_checked, boundary_violations |
| integrate | conflicts, merge_status |
| tcr | test_rerun_status, revert_needed |
| deploy | flags_created, rollout_plan_generated |

---

### TaskResult

Aggregated result from entire pipeline execution.

```gleam
pub type TaskResult {
  TaskResult(
    task: Task,                    // Updated task with final status
    stages: List(StageResult),     // All stage results in order
    overall_status: String,        // "success" | "failure" | "reverted"
    total_duration_ms: Int,        // Total time for all stages
    metrics: Dict(String, String), // Aggregated metrics
    errors: List(String),          // User-facing error summaries
  )
}
```

**Example**:

```gleam
TaskResult(
  task: Task(...),
  stages: [
    StageResult(...),  // implement: success
    StageResult(...),  // unit-test: success
    StageResult(...),  // coverage: success (89%)
    StageResult(...),  // lint: success
    // ... more stages
  ],
  overall_status: "success",
  total_duration_ms: 125000,
  metrics: dict.from_list([
    #("coverage_percent", "89"),
    #("lines_changed", "47"),
    #("files_modified", "2"),
    #("dependencies_added", "0"),
    #("total_tests", "15"),
    #("tests_passed", "15"),
    #("stage_count", "10"),
  ]),
  errors: [],  // No errors
)
```

---

### ExecutionOptions

Configuration for executing a shell command.

```gleam
pub type ExecutionOptions {
  ExecutionOptions(
    cwd: String,                    // Working directory
    timeout_seconds: Int,           // Max execution time
    capture_stdout: Bool,           // Save standard output?
    capture_stderr: Bool,           // Save standard error?
    env_vars: Dict(String, String), // Environment variables
  )
}
```

**Example**:

```gleam
ExecutionOptions(
  cwd: "/home/lewis/workspace/bd-52.1",
  timeout_seconds: 60,
  capture_stdout: True,
  capture_stderr: True,
  env_vars: dict.from_list([
    #("GLEAM_TARGET", "javascript"),
    #("RUST_BACKTRACE", "1"),
  ]),
)
```

---

### AuditEvent

Records approval and deployment decisions for audit trail.

```gleam
pub type AuditEvent {
  TaskCreated(timestamp: Int)
  WorkspaceCreated(workspace_id: String, timestamp: Int)
  StageCompleted(stage: String, status: String, timestamp: Int)
  ApprovalRequested(timestamp: Int)
  ApprovedByHuman(actor: String, decision: String, timestamp: Int)
  DeploymentStarted(percentage: Int, timestamp: Int)
  DeploymentCompleted(percentage: Int, timestamp: Int)
  DeploymentRolledBack(reason: String, timestamp: Int)
}
```

**Example Audit Trail**:

```
2025-01-15 14:30:00 UTC: TaskCreated
2025-01-15 14:31:00 UTC: WorkspaceCreated (bd-52.1)
2025-01-15 14:32:00 UTC: StageCompleted (implement: success)
2025-01-15 14:35:00 UTC: StageCompleted (unit-test: success)
2025-01-15 14:40:00 UTC: StageCompleted (coverage: success - 89%)
2025-01-15 14:45:00 UTC: StageCompleted (lint: success)
2025-01-15 14:50:00 UTC: StageCompleted (integration: success)
2025-01-15 14:52:00 UTC: StageCompleted (review: success)
2025-01-15 14:53:00 UTC: StageCompleted (boundary: success)
2025-01-15 14:54:00 UTC: StageCompleted (integrate: success)
2025-01-15 14:55:00 UTC: StageCompleted (tcr: success)
2025-01-15 14:56:00 UTC: StageCompleted (deploy: success)
2025-01-15 14:57:00 UTC: ApprovalRequested
2025-01-15 14:59:00 UTC: ApprovedByHuman (lewis, "Looks good")
2025-01-15 14:59:30 UTC: DeploymentStarted (1%)
2025-01-15 15:04:30 UTC: DeploymentCompleted (1% - healthy)
2025-01-15 15:04:31 UTC: DeploymentStarted (10%)
2025-01-15 15:09:31 UTC: DeploymentCompleted (10% - healthy)
2025-01-15 15:09:32 UTC: DeploymentStarted (100%)
2025-01-15 15:14:32 UTC: DeploymentCompleted (100% - complete)
```

---

### Workspace

Represents an isolated jj workspace for a task.

```gleam
pub type Workspace {
  Workspace(
    id: String,              // Task slug: "bd-52.1"
    path: String,            // Full filesystem path
    parent_commit: String,   // What this branched from
    created_at: Int,         // Unix timestamp
  )
}
```

**Location**: `.factory/{slug}/workspace/`

**Properties**:
- Completely isolated from other workspaces
- Can work on multiple simultaneously
- Easy rollback with `jj restore --from @-`
- Clean up with `rm -rf` when done

---

### CommandResult

Result from executing a shell command.

```gleam
pub type CommandResult {
  Success(stdout: String)
  Failure(exit_code: Int, stderr: String)
  Timeout
}
```

**Examples**:

```gleam
// Successful command
Success("Compiled successfully\nWarnings: 0")

// Failed command
Failure(1, "error[E0425]: cannot find value `x` in scope")

// Timeout
Timeout
```

---

## Data Flow

### Task Creation

```
User creates Beads task "bd-52.1" with description

↓

Factory loads from Beads:
- slug: "bd-52.1"
- title: "Batch upload handler"
- description: "Add file upload..."

↓

Factory creates Contract from .factory/contract.yaml:
- allowed_files: ["src/web/**", "test/web/**"]
- forbidden_files: ["README.md"]
- required_test_coverage: 80
- language: Gleam

↓

Factory creates Task:
- slug: bd-52.1
- status: Created

↓

Factory creates Workspace:
- id: bd-52.1
- path: /home/lewis/workspace/bd-52.1/
- parent_commit: abc123
```

### Pipeline Execution

```
For each stage in contract.pipeline:

  Execute Stage:
  - Load command from Stage
  - Create ExecutionOptions
  - Run command in workspace
  - Capture output, errors, duration

  ↓

  Create StageResult with metrics

  ↓

  Save to .factory/{slug}/stage-{name}.json

  ↓

  Check if required_success:
    - True: Pipeline fails if status != "success"
    - False: Pipeline continues

↓

After all stages: Create TaskResult with:
- All StageResults
- Aggregated metrics
- Overall status
- Error summaries
```

---

## Type Safety Benefits

Factory uses **strong typing** to prevent errors:

```gleam
// This is safe - type system prevents mistakes
let slug = validate_slug("bd-52.1")  // Result(Slug, String)

case slug {
  Ok(valid_slug) -> {
    // Can only use valid_slug here
    let path = ".factory/" <> slug_to_string(valid_slug)
  }
  Error(msg) -> {
    // Must handle error case
    println("Invalid slug: " <> msg)
  }
}

// This is wrong - won't compile
let slug_str = "bd-52.1"
let path = ".factory/" <> slug_str  // ERROR: Slug expected, not String
```

**Benefits**:
- Invalid slugs caught at compile time
- Status transitions explicit
- No null pointer errors (uses Option)
- No unhandled Result types
- Exhaustive pattern matching

---

## Summary

The domain model provides:

1. **Strong typing** - Impossible states are unrepresentable
2. **Validation** - Invalid data rejected at boundaries
3. **Clear semantics** - Each type has one meaning
4. **Audit trail** - Complete history of decisions
5. **Language flexibility** - Abstract over Go/Gleam/Rust/Python
6. **Safety enforcement** - Contracts prevent mistakes

Result: A system you can reason about and trust.
