# Factory

Contract-driven CI/CD pipeline with Test-Commit-Revert (TCR) workflow for multi-language projects.

## Architecture

Factory implements an automated TCR pipeline that:
- Creates isolated jj worktrees per task
- Runs language-specific validation stages
- Auto-commits passing changes, reverts failures
- Tracks progress via .factory/tasks.json

### Core Modules

**domain.gleam** - Type-safe core model
- Language types: Go, Gleam, Rust, Python
- Task status state machine: Created → InProgress → PassedPipeline/FailedPipeline → Integrated
- Pipeline stage definitions with retry counts and TCR flags
- Validated slug types (1-50 chars, [a-z0-9_-])

**cli.gleam** - Command interface
- `new` - Create task with isolated worktree
- `stage` - Run pipeline stage (implement, unit-test, coverage, lint, static, integration, security, review, accept)
- `approve` - Mark task ready for integration
- `show` - Display task status
- `list` - Query tasks by priority/status

**worktree.gleam** - Workspace isolation via jj
- Creates `.factory-workspaces/<slug>-<id>` directories
- Manages jj workspace operations and bookmarks
- Symlinks `.factory/<slug>` for easy access

**stages.gleam** - Language-specific execution
- **Gleam**: gleam build, test, format --check, check
- **Go**: go build, test -short, test -coverprofile, gofmt -l, go vet, gosec
- **Rust**: cargo build, test, fmt --check, clippy, tarpaulin, audit
- **Python**: py_compile, pytest, coverage, black --check, mypy, bandit

**tcr.gleam** - Test-commit-revert logic
- Captures jj state before stage execution
- On pass: `jj describe` + `jj new` (commit)
- On fail: `jj restore --from <hash>` (revert)
- Returns TCROutcome tracking commits/reverts

**persistence.gleam** - Task state tracking
- JSON serialization to `.factory/tasks.json`
- TaskRecord: slug, language, status, timestamps, stage results
- Atomic updates via record replacement

**repo.gleam** - Repository detection
- Auto-detect language from gleam.toml, go.mod, Cargo.toml, pyproject.toml
- Find repo root via jj/git

**audit.gleam** - Event logging
- Task lifecycle: created, approved
- Stage execution: started, passed, failed with duration

**process.gleam** - Command execution
- Wraps external commands with error handling
- CommandResult: Success(stdout, stderr, code) | Failure(msg, code)

## Usage

```bash
# Create new task (auto-detects language)
factory new -s my-feature

# Run implementation stage with TCR
factory stage -s my-feature --stage implement

# Run specific stage range
factory stage -s my-feature --stage unit-test --from implement --to lint

# Show task status
factory show -s my-feature --detailed

# List all tasks
factory list --priority P1 --status in_progress

# Approve for integration
factory approve -s my-feature --strategy gradual

# Help
factory help
```

## Pipeline Stages

All stages support TCR (auto-commit on pass, revert on fail):

1. **implement** - Code compiles (5 retries)
2. **unit-test** - All tests pass (3 retries)
3. **coverage** - 80% coverage (5 retries)
4. **lint** - Code formatted (3 retries)
5. **static** - Static analysis passes (3 retries)
6. **integration** - Integration tests pass (3 retries)
7. **security** - No vulnerabilities (2 retries)
8. **review** - Code review passes (3 retries)
9. **accept** - Ready for merge (1 retry)

Each stage uses language-appropriate tooling. Retries happen automatically with exponential backoff.

## Design Decisions

**Why jj over git?**
- Native worktree support without git's complexity
- Journal-based history tracking per workspace
- Simpler restore/revert semantics

**Why opaque Slug type?**
- Makes invalid task IDs unrepresentable at compile time
- Validation happens once at creation

**Why Result everywhere?**
- Explicit error handling, no exceptions
- Forces callers to handle failure cases
- Composable via use <- result.try()

**Why TCR on all stages?**
- Prevents broken code from persisting
- Creates audit trail of working increments
- Enforces discipline: tests must pass before commit

**Stage-specific retry counts?**
- Different stages have different flakiness (network, timing)
- implement (5): compiler often needs multiple fixes
- security (2): usually deterministic, rarely flaky

## Testing Strategy

- **Unit tests**: domain logic, validation, JSON serialization
- **Integration tests**: real jj repos, actual command execution
- **Property tests**: slug validation, stage transitions via qcheck
- **Golden master**: CLI output snapshots

## Build & Run

```bash
# Build
gleam build

# Run tests
gleam test

# Format
gleam format

# Create executable
gleam export erlang-shipment
./build/erlang-shipment/entrypoint.sh run -m factory/main

# Or via gleam directly
gleam run -- new -s test-task
```

## Directory Structure

```
.factory/               # Task metadata
  tasks.json            # Current task states
  audit.log             # Event log
  <slug>               # Symlinks to worktrees

.factory-workspaces/    # Isolated work directories
  <slug>-<id>/         # Per-task jj workspace
```

## Requirements

- **Gleam**: >=0.44.0
- **jj**: Latest stable (for worktree management)
- **Language-specific tools**:
  - Go: go, gofmt, gosec
  - Rust: cargo, rustfmt, clippy, cargo-tarpaulin, cargo-audit
  - Python: python, pytest, coverage, black, mypy, bandit
  - Gleam: gleam

## License

Apache-2.0
