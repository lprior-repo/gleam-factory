# Factory Gleam - Beads Task Management Guide

## What is Beads?

Beads is a distributed, git-backed issue tracking system designed for AI coding agents. It uses Git as the database and stores issues as JSONL files in `.beads/` directory. Unlike traditional todo systems, Beads provides:

- **Hierarchical issues**: Epics → Issues → Subtasks
- **Dependency-aware**: Track blockers and dependencies between tasks
- **Hash-based IDs**: Zero merge conflicts (e.g., `factory-gleam-cwl`)
- **Priority levels**: P0, P1, P2, P3 for task prioritization
- **Batch operations**: Update multiple tasks at once
- **Memory decay**: Closed issues summarized for context preservation

## Installation

Beads is already installed at `/home/lewis/.local/bin/bd`

Three installation methods:
- npm: `npm install -g @beads/bd`
- Homebrew: `brew install steveyegge/beads/bd`
- Go: `go install github.com/steveyegge/beads/cmd/bd@latest`

## Setup (Already Complete)

```bash
bd init --stealth    # Initialized in stealth mode (local only, no git commits)
```

## Core Commands

### View Tasks

```bash
# Show all tasks ready to work (no blockers)
bd ready

# List all issues
bd list

# List by priority
bd list -p 1          # P1 only
bd list -p "1,2"      # P1 and P2

# Show issue details and history
bd show factory-gleam-cwl

# Search by title
bd search "JSON"
```

### Create & Update Tasks

```bash
# Create a new task
bd create "Task title" -p 1 --description "Detailed description"

# Edit an issue
bd edit factory-gleam-cwl --title "New title" --description "Updated description"

# Update status (open, in_progress, done, blocked, wontfix)
bd edit factory-gleam-cwl --status in_progress

# Change priority
bd edit factory-gleam-cwl --priority 2
```

### Dependency Management

```bash
# Add dependency (factory-gleam-duh blocks factory-gleam-0t5)
bd dep add factory-gleam-duh factory-gleam-0t5

# Show blockers for a task
bd show factory-gleam-0t5    # Will show "blocked by" if dependencies exist

# Remove dependency
bd dep rm factory-gleam-duh factory-gleam-0t5
```

### Task Organization

```bash
# Create parent-child relationship (epic/subtask)
bd parent factory-gleam-7eg factory-gleam-0t5    # 0t5 becomes child of 7eg

# List children
bd show factory-gleam-7eg    # Shows all child tasks

# Change parent
bd reparent factory-gleam-0t5 factory-gleam-mi1
```

### Batch Operations

```bash
# Close multiple tasks at once
bd edit factory-gleam-cwl factory-gleam-duh --status done

# Set multiple tasks to in_progress
bd edit factory-gleam-4rv factory-gleam-0t5 factory-gleam-mi1 --status in_progress

# Add multiple tasks to parent
bd parent factory-gleam-7eg factory-gleam-4rv factory-gleam-0t5 factory-gleam-mi1
```

### Advanced Features

```bash
# Show all dependencies
bd deps

# Check status
bd status           # Shows overall project health

# Garbage collection (clean up closed tasks)
bd gc

# View git-backed database
ls -la .beads/
cat .beads/beads.db  # JSONL format
```

## Priority Levels

- **P0**: Critical blockers (none currently)
- **P1**: High priority (5 tasks - core implementation)
- **P2**: Medium priority (10 tasks - testing & verification)
- **P3**: Lower priority (6 tasks - features & polish)

## Current Task Breakdown

### P1 Core Implementation (5 tasks)
1. `factory-gleam-cwl` - JSON parsing in persistence
2. `factory-gleam-duh` - Subprocess execution via Erlang ports
3. `factory-gleam-7eg` - Real stage execution (Gleam/Go/Rust/Python)
4. `factory-gleam-6ug` - CLI argument parsing at entry point
5. `factory-gleam-j7c` - Real JSON parsing for persistence

### P2 Testing & Build (11 tasks)
6-16. Unit tests for all 10 modules + E2E tests + CLI testing + build verification

### P3 Features & Polish (6 tasks)
17-21. Documentation, benchmarks, dry-run, filtering, retries

## Workflow Examples

### Start Working on a Task

```bash
# Pick highest priority task without blockers
bd ready | head -1

# Show details to understand scope
bd show factory-gleam-cwl

# Mark as in_progress
bd edit factory-gleam-cwl --status in_progress
```

### Create Related Tasks

```bash
# Create subtask for a larger task
bd create "Implement json_to_record function" -p 1 --description "Parse TaskRecord from JSON string"

# Make it a child of the main task
bd parent factory-gleam-j7c <new-task-id>
```

### Block/Unblock Tasks

```bash
# Task A cannot start until Task B is done
bd dep add factory-gleam-duh factory-gleam-j7c

# Check what's blocking task A
bd show factory-gleam-j7c    # Shows: "blocked by factory-gleam-duh"

# Task B is done - remove blocker
bd edit factory-gleam-duh --status done
bd dep rm factory-gleam-duh factory-gleam-j7c
```

### Complete & Clean Up

```bash
# Mark multiple related tests as done
bd edit factory-gleam-4rv factory-gleam-0t5 factory-gleam-mi1 --status done

# See remaining work
bd ready

# Garbage collect closed tasks (optional)
bd gc
```

## Pro Tips

1. **Use priorities wisely**: P1 = must do, P2 = important, P3 = nice to have
2. **Chain dependencies**: Create logical dependencies for work sequences
3. **Parent large tasks**: Group related subtasks under an epic
4. **Status tracking**: Keep status updated for accurate "ready" list
5. **Descriptions matter**: Good descriptions save context between sessions
6. **Batch operations**: Update multiple tasks at once for efficiency
7. **Search before creating**: Use `bd search` to avoid duplicates

## Database Format

Beads stores everything as JSONL in `.beads/beads.db`:

```json
{"id":"factory-gleam-cwl","title":"Implement real JSON parsing...","priority":"P1","status":"open"}
```

Each line is a complete task record. Git tracks changes naturally.

## More Information

- GitHub: https://github.com/steveyegge/beads
- Medium articles by Steve Yegge: Search "Beads" on Medium
- Beads Best Practices: https://steve-yegge.medium.com/beads-best-practices-2db636b9760c

