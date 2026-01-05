# Factory Gleam - Module Reference

**Complete guide to all modules and their responsibilities**

**Last Updated**: January 5, 2026

---

## Module Overview

| Module | Responsibility | Status | LOC |
|--------|---------------|--------|-----|
| factory.gleam | Main orchestrator and CLI | Partial | ~200 |
| domain.gleam | Type system and validation | Complete | ~400 |
| cli.gleam | Argument parsing | Partial | ~150 |
| process.gleam | Shell execution | Partial | ~100 |
| repo.gleam | Repository operations | Partial | ~150 |
| worktree.gleam | Jujutsu workspace management | Partial | ~200 |
| stages.gleam | Pipeline stage implementations | Partial | ~400 |
| tcr.gleam | Test-Code-Revert discipline | Design | ~100 |
| persistence.gleam | State and result storage | Partial | ~250 |
| integration.gleam | External tool integrations | Design | ~200 |

---

## factory.gleam - Main Orchestrator

**Purpose**: Entry point that coordinates the entire workflow

**Responsibilities**:
1. Parse CLI commands (`new`, `stage`, `approve`)
2. Load task definition and contract
3. Create jj workspace
4. Execute pipeline in sequence
5. Report status and metrics
6. Handle errors and rollbacks

**Public Functions**:

```gleam
pub fn main() -> Nil
  Runs CLI and handles all commands

pub fn new_task(slug: String, contract: Contract) -> Result(Task, String)
  Creates new task with jj workspace

pub fn run_pipeline(task: Task) -> Result(TaskResult, String)
  Executes all pipeline stages in sequence
  Returns aggregated metrics

pub fn approve_deployment(task: Task, strategy: RolloutStrategy) -> Result(Nil, String)
  Approves task for deployment (MANUAL CALL ONLY)
  Enables feature flags and monitoring
```

**Dependencies**:
- domain.gleam (types)
- cli.gleam (argument parsing)
- worktree.gleam (jj operations)
- stages.gleam (pipeline execution)
- persistence.gleam (save results)
- integration.gleam (external tools)

**Example Flow**:
```
$ factory new bd-52.1
  → Loads task from Beads
  → Creates jj workspace
  → Returns workspace ID

$ factory stage bd-52.1 implement
  → Loads workspace
  → Runs compile check
  → Saves result
  → Returns exit code

$ factory approve bd-52.1 --strategy gradual
  → Validates all stages passed
  → Enables feature flag to 1%
  → Sets monitoring alerts
  → Returns approval confirmation
```

---

## domain.gleam - Type System

**Purpose**: All type definitions and validation logic

**Complete Types**:

```gleam
pub type Language {
  Go
  Gleam
  Rust
  Python
}

pub type Slug
  // Validated task identifier like "bd-52.1"
  // Cannot be empty, must be alphanumeric + hyphen/dot

pub type Stage {
  Stage(
    name: String,           // "implement", "unit-test", etc.
    description: String,
    command: String,        // Shell command to run
    timeout_seconds: Int,
    required_success: Bool, // Must pass or whole pipeline fails
  )
}

pub type TaskStatus {
  Created
  Ready
  InProgress
  ReadyForReview
  ReviewApproved
  Deploying
  DeployedToStaging  // 1% users
  DeployedToProduction  // 100% users
  Done
  Failed(reason: String)
  Reverted
}

pub type Task {
  Task(
    slug: Slug,
    title: String,
    description: String,
    contract: Contract,
    status: TaskStatus,
    created_at: Int,
    started_at: Option(Int),
    completed_at: Option(Int),
  )
}

pub type Contract {
  Contract(
    allowed_files: List(String),     // Glob patterns
    forbidden_files: List(String),
    required_test_coverage: Int,     // Percentage
    new_dependencies_allowed: Bool,
    max_lines_changed: Int,
    language: Language,
    pipeline: List(Stage),
    custom_fields: Dict(String, String),
  )
}

pub type StageResult {
  StageResult(
    stage_name: String,
    status: String,  // "success" | "failure" | "skipped"
    stdout: String,
    stderr: String,
    duration_ms: Int,
    metrics: Dict(String, String),
  )
}

pub type TaskResult {
  TaskResult(
    task: Task,
    stages: List(StageResult),
    overall_status: String,
    total_duration_ms: Int,
    metrics: Dict(String, String),
    errors: List(String),
  )
}
```

**Public Functions**:

```gleam
pub fn parse_language(s: String) -> Result(Language, String)
  Converts "go", "gleam", etc. to Language type
  Error if unrecognized

pub fn detect_language_from_files(file_paths: List(String)) -> Result(Language, String)
  Looks at files and guesses language
  Examines: go.mod, gleam.toml, Cargo.toml, pyproject.toml

pub fn validate_slug(s: String) -> Result(Slug, String)
  Ensures slug is valid format
  Allows: alphanumeric, hyphen, dot
  Rejects: spaces, special chars, empty

pub fn slug_to_string(slug: Slug) -> String
  Converts slug back to string

pub fn standard_pipeline() -> List(Stage)
  Returns the 10-stage standard pipeline
  Used if no custom pipeline in contract

pub fn get_stage(name: String, pipeline: List(Stage)) -> Result(Stage, String)
  Looks up a stage by name
  Error if not found

pub fn filter_stages(from: String, to: String, pipeline: List(Stage)) -> Result(List(Stage), String)
  Gets all stages from X to Y (inclusive)
  Example: filter_stages("implement", "lint")

pub fn stage_requires_success(stage: Stage) -> Bool
  Returns true if stage failure should fail entire pipeline
```

**Design Notes**:
- All types are immutable records
- Validation happens at type boundaries
- Language detection is heuristic-based (best-effort)
- Slug is a newtype wrapper (can't construct invalid ones)

---

## cli.gleam - Argument Parsing

**Purpose**: Parse command-line arguments and routing

**Current Commands**:

```bash
factory new <slug>                    # Create new task
factory stage <slug> <stage-name>     # Run specific stage
factory approve <slug>                # Approve for deployment
factory show <slug>                   # Show task status
factory list                          # List all tasks
```

**Public Functions**:

```gleam
pub fn parse_command(args: List(String)) -> Result(Command, String)
  Parses CLI arguments into Command type
  Validates argument count and names

pub type Command {
  New(slug: String, contract_path: Option(String))
  Stage(slug: String, stage_name: String)
  Approve(slug: String, strategy: Option(String))
  Show(slug: String)
  List
}
```

**Implementation Notes**:
- Uses gleam pattern matching for routing
- Each command has its own validation
- Returns helpful error messages for invalid input

---

## process.gleam - Process Execution

**Purpose**: Run shell commands safely with timeout and error handling

**Public Types**:

```gleam
pub type CommandResult {
  Success(stdout: String)
  Failure(exit_code: Int, stderr: String)
  Timeout
}

pub type ExecutionOptions {
  ExecutionOptions(
    cwd: String,                    // Working directory
    timeout_seconds: Int,
    capture_stdout: Bool,
    capture_stderr: Bool,
    env_vars: Dict(String, String),
  )
}
```

**Public Functions**:

```gleam
pub fn run_command(
  cmd: String,
  args: List(String),
  options: ExecutionOptions,
) -> Result(CommandResult, String)
  Executes a command with timeout
  Captures output
  Returns success or failure

pub fn extract_error_summary(stderr: String) -> String
  Takes full error output
  Extracts root cause (first line of actual error)
  Removes noise and duplicate messages
  Returns 1-2 line summary
```

**Safety Considerations**:
- Command injection: Arguments are passed separately, not interpolated
- Timeout: Prevents stuck processes
- Output capture: Full output saved for debugging
- Environment: Can control env variables for reproducibility

---

## repo.gleam - Repository Operations

**Purpose**: Interact with the repository to gather information

**Public Functions**:

```gleam
pub fn detect_language(repo_path: String) -> Result(Language, String)
  Looks for language indicators
  Checks for: go.mod, gleam.toml, Cargo.toml, pyproject.toml
  Returns most likely language

pub fn get_modified_files(repo_path: String) -> Result(List(String), String)
  Gets files changed in current workspace
  Returns relative paths from repo root

pub fn get_file_diff(repo_path: String, file_path: String) -> Result(String, String)
  Gets diff for a specific file
  Shows additions and deletions

pub fn get_repo_root(start_path: String) -> Result(String, String)
  Finds the repository root directory
  Starts from start_path, walks up until finding .git or .jj

pub fn file_matches_pattern(file_path: String, pattern: String) -> Bool
  Glob-style pattern matching
  Supports * and ** wildcards
  Case-sensitive matching
```

**Pattern Matching**:
```
src/**/*.gleam      → matches all gleam files under src
src/*.test.gleam    → matches test files directly under src
README.*            → matches README with any extension
```

---

## worktree.gleam - Jujutsu Workspace Management

**Purpose**: Create and manage isolated jj workspaces for each task

**Public Functions**:

```gleam
pub fn create_workspace(task: Task) -> Result(Workspace, String)
  Creates new jj workspace for this task
  Returns workspace ID and path

pub fn delete_workspace(workspace: Workspace) -> Result(Nil, String)
  Deletes workspace and all contents
  Cannot be undone

pub fn get_workspace_path(workspace_id: String) -> String
  Returns full path to workspace directory

pub fn restore_from_previous(
  workspace: Workspace,
  num_commits: Int,
) -> Result(Nil, String)
  Reverts workspace to N commits ago
  Used by TCR on failure
  Command: jj restore --from @-

pub type Workspace {
  Workspace(
    id: String,              // e.g., "bd-52.1"
    path: String,            // Full path to workspace
    parent_commit: String,   // What this branched from
    created_at: Int,
  )
}
```

**Jujutsu Integration**:
```bash
# Create workspace
jj new --at <parent-commit> -d <workspace-id>

# Run commands in workspace
cd <workspace-path>
gleam build  # or whatever

# Revert on failure (TCR)
jj restore --from @-

# Commit when ready
jj commit -m "message"

# Cleanup
cd ..
rm -rf <workspace-path>
```

**Why Jujutsu?**
- Isolated workspaces prevent interference
- Can work on multiple tasks simultaneously
- Easy rollback with `jj restore`
- Clear commit history
- Integrates with moon.dev

---

## stages.gleam - Pipeline Implementation

**Purpose**: Execute each stage of the pipeline with language-specific commands

**Public Functions**:

```gleam
pub fn run_stage(
  task: Task,
  workspace: Workspace,
  stage: Stage,
) -> Result(StageResult, String)
  Executes a single pipeline stage
  Runs appropriate command for language
  Captures metrics
  Returns result

pub fn run_all_stages(
  task: Task,
  workspace: Workspace,
) -> Result(List(StageResult), String)
  Runs entire pipeline
  Stops on first failure if required_success is true
  Returns list of all results

pub fn build_language_config(language: Language) -> Dict(String, String)
  Creates language-specific configuration
  Returns command overrides for each stage
```

**Stage Implementations**:

```gleam
// Stage 1: Implement (Compilation)
fn run_implement(language: Language, path: String) -> Result(String, String)
  For Gleam: gleam build --target javascript && gleam build --target erlang
  For Go: go build ./...
  For Rust: cargo build
  For Python: python -m py_compile src/

// Stage 2: Unit Test
fn run_unit_test(language: Language, path: String) -> Result(String, String)
  For Gleam: gleam test
  For Go: go test ./...
  For Rust: cargo test --lib
  For Python: python -m pytest test/

// Stage 3: Coverage
fn run_coverage(language: Language, path: String) -> Result(Int, String)
  For Gleam: gleam test --cover → parse output for %
  For Go: go test -cover ./...
  For Rust: cargo tarpaulin
  For Python: coverage run -m pytest, coverage report
  Returns percentage as integer

// Stages 4-10: Similar pattern
```

**Error Extraction**:
```gleam
fn extract_root_error(full_output: String) -> String
  From compiler error output:
    INPUT: "error[E0425]: cannot find value `x` in this scope
             --> src/main.rs:2:5
             |
             2 | println!(\"{}\", x);
               |                ^
               |
            help: you might have meant to use one of these variants"

    OUTPUT: "error[E0425]: cannot find value `x` in this scope"

  From test failure:
    INPUT: "test result: FAILED. 5 passed; 3 failed; 0 ignored"
    OUTPUT: "3 tests failed (5 passed)"
```

---

## tcr.gleam - Test-Code-Revert Discipline

**Purpose**: Enforce TCR cycle: Test → Code → Revert pattern

**Public Functions**:

```gleam
pub fn tcr_cycle(
  workspace: Workspace,
  max_attempts: Int,
) -> Result(Nil, String)
  1. Run test suite
  2. If pass: Mark ready for commit
  3. If fail: Revert and try again
  4. After max_attempts: Return error

pub fn run_tcr_tests(workspace: Workspace) -> Result(Bool, String)
  Runs full test suite
  Returns true if all pass, false otherwise

pub fn revert_to_previous(
  workspace: Workspace,
) -> Result(Nil, String)
  Reverts workspace to previous commit
  Uses: jj restore --from @-
```

**TCR Guarantees**:
- Code only commits when all tests pass
- No broken code reaches deployment
- Failures are immediately visible
- Easy rollback to known-good state

---

## persistence.gleam - State Management

**Purpose**: Load and save task state, results, and audit trail

**File Structure**:
```
.factory/
├── contract.yaml           # Global contract rules
├── tasks.db               # SQLite with task definitions
├── bd-52.1/
│   ├── task.yaml          # Task definition
│   ├── contract.yaml      # Task-specific contract
│   ├── status.yaml        # Current status
│   ├── metrics.yaml       # Generated metrics
│   ├── audit.yaml         # Approval history
│   ├── feature-flags.yaml # Rollout strategy
│   ├── stage-implement.json      # Stage result
│   ├── stage-unit-test.json      # Stage result
│   └── stage-implement-full.log  # Full compiler output
└── bd-52.2/
    └── (same structure)
```

**Public Functions**:

```gleam
pub fn load_task(slug: String) -> Result(Task, String)
  Loads task from .factory/{slug}/task.yaml
  Returns complete task definition

pub fn load_contract(slug: String) -> Result(Contract, String)
  Loads contract from .factory/{slug}/contract.yaml
  Falls back to global contract.yaml

pub fn save_task_status(task: Task) -> Result(Nil, String)
  Saves task status to status.yaml
  Updates timestamp

pub fn save_stage_result(slug: String, result: StageResult) -> Result(Nil, String)
  Saves individual stage result
  Also saves full output to -full.log

pub fn save_metrics(slug: String, metrics: Dict(String, String)) -> Result(Nil, String)
  Saves metrics.yaml with decision-making data
  Includes: coverage, lines changed, dependencies

pub fn load_audit_trail(slug: String) -> Result(List(AuditEvent), String)
  Loads complete approval history
  Shows who approved what and when

pub fn add_audit_event(
  slug: String,
  event: AuditEvent,
) -> Result(Nil, String)
  Appends event to audit trail
```

**Audit Events**:
```gleam
pub type AuditEvent {
  TaskCreated(timestamp: Int)
  WorkspaceCreated(timestamp: Int)
  StageCompleted(stage: String, result: String, timestamp: Int)
  ApprovalRequested(timestamp: Int)
  ApprovedByHuman(actor: String, timestamp: Int)
  DeploymentStarted(percentage: Int, timestamp: Int)
  DeploymentCompleted(timestamp: Int)
}
```

---

## integration.gleam - External Tool Integration

**Purpose**: Coordinate with moon.dev, Beads, monitoring systems

**Moon Integration**:

```gleam
pub fn call_moon_task(task_name: String, args: Dict(String, String)) -> Result(String, String)
  Calls moon run with task and arguments
  Example: call_moon_task("factory:stage-implement", {"task" → "bd-52.1"})

pub fn moon_task_for_stage(slug: String, stage_name: String) -> String
  Generates moon task name for a stage
  Example: "factory:stage-implement-bd-52.1"
```

**Beads Integration**:

```gleam
pub fn update_beads_status(task_slug: String, status: TaskStatus) -> Result(Nil, String)
  Updates Beads task with current status
  Syncs: Created → Ready → InProgress → Done

pub fn link_workspace(task_slug: String, workspace_id: String) -> Result(Nil, String)
  Records workspace in Beads for traceability
```

**Feature Flags**:

```gleam
pub fn enable_feature_flag(
  flag_name: String,
  percentage: Int,
) -> Result(Nil, String)
  Enables feature flag for percentage of users
  Example: enable_feature_flag("bd-52-batch-upload", 1)

pub fn verify_flag_status(flag_name: String) -> Result(FlagStatus, String)
  Checks current status of feature flag
  Returns: percentage, errors, successes
```

**Monitoring**:

```gleam
pub fn send_metric(metric_name: String, value: Float) -> Result(Nil, String)
  Sends metric to monitoring system
  Example: send_metric("factory.stage.duration", 45.2)

pub fn check_deployment_health(flag_name: String) -> Result(HealthStatus, String)
  Checks if current rollout is healthy
  Returns: error rate, latency, critical issues
```

---

## How Modules Work Together

### Complete Pipeline Flow

```
User: factory new bd-52.1
  ↓
CLI.gleam: Parses "new" command with slug
  ↓
Factory.gleam: Calls new_task()
  ↓
Persistence.gleam: Loads task and contract
  ↓
Repo.gleam: Detects language from files
  ↓
Worktree.gleam: Creates jj workspace
  ↓
Integration.gleam: Updates Beads status
  ↓
Return: Workspace created successfully

---

User: factory stage bd-52.1 implement
  ↓
CLI.gleam: Parses stage command
  ↓
Factory.gleam: Calls run_stage()
  ↓
Persistence.gleam: Loads task and workspace
  ↓
Stages.gleam: Gets language-specific commands
  ↓
Process.gleam: Runs gleam build with timeout
  ↓
Persistence.gleam: Saves result and full output
  ↓
Return: Build succeeded/failed

---

User: factory approve bd-52.1
  ↓
CLI.gleam: Parses approve command
  ↓
Factory.gleam: Validates all stages passed
  ↓
Persistence.gleam: Loads metrics
  ↓
Integration.gleam: Enables feature flag (1%)
  ↓
Integration.gleam: Updates Beads to "DeployedToStaging"
  ↓
Persistence.gleam: Records approval in audit trail
  ↓
Return: Deployment in progress, monitoring started
```

---

## Summary

Each module has a single responsibility:

- **factory.gleam**: Orchestration and routing
- **domain.gleam**: Type system and validation
- **cli.gleam**: User input parsing
- **process.gleam**: Safe command execution
- **repo.gleam**: Repository inspection
- **worktree.gleam**: Workspace isolation
- **stages.gleam**: Pipeline execution
- **tcr.gleam**: Test-Code-Revert discipline
- **persistence.gleam**: State management
- **integration.gleam**: External tools

Together they form a cohesive system for **human-in-the-loop code generation and deployment**.
