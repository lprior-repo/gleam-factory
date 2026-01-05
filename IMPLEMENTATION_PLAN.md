# Factory Gleam Rewrite - Implementation Plan

## What We've Completed ✅

### 1. Project Structure
- ✅ Created Gleam project at `/home/lewis/src/factory-gleam`
- ✅ Set up gleam.toml with dependencies (simplifile for file I/O)
- ✅ Minimal, clean structure ready for expansion

### 2. Domain Model (domain.gleam) - COMPLETE & COMPILING
- ✅ `Language` type with Go, Gleam, Rust, Python
- ✅ `parse_language()` - validates language from string
- ✅ `detect_language_from_files()` - auto-detects from repo files
- ✅ `Slug` type - validated task identifiers
- ✅ `validate_slug()` - ensures valid identifiers
- ✅ `Stage` type - pipeline stage definition
- ✅ `TaskStatus` type - complete task lifecycle states
- ✅ `Task` type - represents a complete task with all metadata
- ✅ `standard_pipeline()` - 10-stage pipeline definition
- ✅ `get_stage()` - lookup by name
- ✅ `filter_stages()` - get range of stages by name

**Key improvement over Go version:** Pure data types + pure functions. No interactive prompts are possible in the type system.

## What Still Needs Building

### Phase 1: Core Execution (CRITICAL PATH)

#### 1a. Shell Command Execution Module (`shell.gleam`)
**Purpose:** Execute actual commands for real pipeline validation

```gleam
pub type CommandResult {
  Success(stdout: String)
  Failure(exit_code: Int, stderr: String)
}

pub fn run_command(cmd: String, args: List(String), cwd: String) -> Result(CommandResult, String)
```

**Use cases:**
- `gleam build` - for Gleam projects
- `go build ./...` - for Go projects
- `cargo build` - for Rust
- `python -m pytest` - for Python
- `jj` commands for VCS operations

#### 1b. Real Language-Specific Stages (`stages.gleam`)
**Purpose:** Replace fake "echo" stages with real commands

For Gleam:
```gleam
pub fn stage_lint(cwd: String) -> Result(CommandResult, String) {
  // gleam format --check
}

pub fn stage_static(cwd: String) -> Result(CommandResult, String) {
  // gleam check (type checker)
}

pub fn stage_security(cwd: String) -> Result(CommandResult, String) {
  // Check gleam dependencies for vulnerabilities
}
```

Same pattern for Go, Rust, Python - each with real linters/checkers.

#### 1c. TCR Implementation (`tcr.gleam`)
**Purpose:** Real Test && Commit || Revert with jj

```gleam
pub fn tcr_run(
  cwd: String,
  stage_name: String
) -> Result(TCROutcome, String)

pub type TCROutcome {
  Passed
  Failed(reason: String, reverted: Bool)
}
```

**Implementation:**
1. Run stage command
2. If passes: `jj describe -m "tcr: stage passed"` + `jj new`
3. If fails: `jj restore --from @-` + return Error
4. No "fake reverts" - actual jj commits

#### 1d. Integration Testing (`integration.gleam`)
**Purpose:** Verify changes work when merged back to main

```gleam
pub fn test_integration(
  task: Task,
  repo_root: String
) -> Result(IntegrationResult, String)

pub type IntegrationResult {
  Passed
  Failed(reason: String)
}
```

**Implementation:**
1. Create temporary merge branch from task branch
2. Merge task branch into temp branch
3. Run full test suite in merged state
4. Delete temp branch
5. Return Pass/Fail

### Phase 2: Persistence

#### 2a. Task Status Persistence (`persistence.gleam`)
**Purpose:** Track which stages passed/failed per task (JSON-based)

```gleam
pub type TaskRecord {
  slug: String
  language: String
  status: String
  stages: Map(String, StageRecord)
  created_at: String
  updated_at: String
}

pub type StageRecord {
  status: String // "passed", "failed", "pending"
  attempts: Int
  last_error: Option(String)
}

pub fn save_task_record(record: TaskRecord, path: String) -> Result(Nil, String)
pub fn load_task_record(slug: String, path: String) -> Result(TaskRecord, String)
pub fn list_all_tasks(path: String) -> Result(List(TaskRecord), String)
```

**Storage:** `.factory/tasks.json` in repo root

```json
{
  "my-feature": {
    "slug": "my-feature",
    "language": "gleam",
    "status": "in_progress",
    "created_at": "2025-01-04T20:00:00Z",
    "updated_at": "2025-01-04T20:15:30Z",
    "stages": {
      "tdd-setup": {"status": "passed", "attempts": 1},
      "implement": {"status": "passed", "attempts": 1},
      "unit-test": {"status": "failed", "attempts": 2, "last_error": "test assertion failed"}
    }
  }
}
```

### Phase 3: CLI Interface (NO PROMPTS)

#### 3a. CLI Module (`cli.gleam`)
**Purpose:** Handle all arguments, no interactive prompts

```gleam
pub type Command {
  New(slug: String, language: Option(String))
  Run(slug: String)
  Stage(slug: String, stage_name: String)
  Range(slug: String, start: String, end: String)
  List
  Status(slug: String)
  Clean(slug: String)
  Init(repo_path: String)
  Provision(repo_path: String, language: Option(String))
}

pub fn parse_args(args: List(String)) -> Result(Command, String)
```

**Key design:**
- All parameters from command line
- No prompts anywhere
- AI-friendly: `factory run my-feature` runs headless
- Structured output for parsing (JSON or plain text options)

#### 3b. Main Entry Point (`factory.gleam`)
```gleam
pub fn main() {
  // Parse CLI args
  // Get command
  // Execute command
  // Print results (structured output)
  // Exit with proper code
}
```

### Phase 4: Worktree Management (`worktree.gleam`)

```gleam
pub type Worktree {
  Worktree(
    slug: String,
    path: String,
    branch: String,
    language: Language,
  )
}

pub fn create_worktree(
  slug: String,
  language: Language,
  repo_root: String
) -> Result(Worktree, String)

pub fn get_worktree(slug: String, repo_root: String) -> Result(Worktree, String)

pub fn remove_worktree(slug: String, repo_root: String) -> Result(Nil, String)

pub fn list_worktrees(repo_root: String) -> Result(List(Worktree), String)
```

### Phase 5: Repository Integration (`repo.gleam`)

```gleam
pub fn detect_repo_root() -> Result(String, String)

pub fn detect_repo_language(repo_root: String) -> Result(Language, String)

pub fn get_base_branch() -> Result(String, String)
```

## Implementation Order (MVP)

1. **shell.gleam** - Execute commands
2. **stages.gleam** - Real stage execution
3. **tcr.gleam** - Real TCR logic
4. **worktree.gleam** - Workspace management
5. **integration.gleam** - Integration testing
6. **persistence.gleam** - Save/load task status
7. **repo.gleam** - Repository detection
8. **cli.gleam** - Command parsing
9. **factory.gleam** - Wire everything together

##  Testing Strategy

Each module gets unit tests:
- `domain_test.gleam` - validation logic
- `stages_test.gleam` - command building
- `tcr_test.gleam` - state transitions
- `persistence_test.gleam` - serialization
- `cli_test.gleam` - argument parsing

E2E test: Create task, run full pipeline, verify results in persistence file.

## Build Output

Target: `factory` executable that runs as CLI tool

```bash
cd /home/lewis/src/factory-gleam
gleam build --target javascript
# Output: ./gleam-stdlib/
# OR
gleam build --target erlang
# Output: Erlang BEAM file
```

For Go interop (calling `go build`, `jj`, etc.):
- If targeting JavaScript: wrapper in Node.js
- If targeting Erlang: native port or system calls

**Recommendation:** Target Erlang for reliability on CLI.

## Advantages of Gleam Rewrite

1. **Impossible Prompts** - No `user.input()` in language, so AI safety guaranteed
2. **Type Safety** - Invalid states can't compile (Language validation, Slug validation)
3. **Immutability** - No hidden state mutations
4. **Pattern Matching** - Command handling is explicit and exhaustive
5. **Real TCR** - jj operations are pure effects, easy to test
6. **No False Validation** - Each stage truly validates the code

## Known Limitations

- Gleam target (JavaScript vs Erlang) - must be decided
- No native subprocess handling - will use system calls via ports
- Hex package ecosystem still growing

## Success Criteria

✅ `factory init` - Detect repo language auto-magically
✅ `factory run my-feature` - Run full pipeline, no prompts
✅ `factory status` - Show which stages passed/failed per task
✅ Real linting/checking - Not echo statements
✅ Real TCR - Actual commits with jj
✅ Integration tests - Verify merged code works
✅ Zero interactive prompts - AI-safe by design

