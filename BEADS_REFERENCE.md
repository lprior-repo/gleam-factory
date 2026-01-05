# Complete Beads Reference & Workflows for Factory Gleam

## Quick Stats

```
Total Issues:  21 (21 open, 0 closed)
Ready Work:    7 tasks (no blockers)
Blocked:       14 tasks (waiting on dependencies)
Priorities:    P1=5, P2=11, P3=5
```

## Essential Commands at a Glance

### View & Search

```bash
bd ready              # Show tasks ready to work (no blockers) ⭐ START HERE
bd blocked            # Show tasks waiting on dependencies
bd list               # List all issues
bd list -p 1          # List only P1 issues
bd show <id>          # Show issue details and history
bd search "keyword"   # Search issues by text
bd stats              # Show statistics
```

### Workflow Commands

```bash
bd create "Title" -p 1 --description "Desc"    # Create new task
bd edit <id> --status in_progress              # Mark as working
bd edit <id> --status done                     # Complete task
bd close <id>                                  # Close issue
bd reopen <id>                                 # Reopen closed issue
```

### Dependency Management

```bash
bd dep add <child> <parent>      # Make parent block child
bd dep rm <child> <parent>       # Remove dependency
bd dep list                      # Show all dependencies
bd blocked                       # Show what's blocked
```

### Labels & Organization

```bash
bd label add <id> "test"         # Add label to issue
bd label rm <id> "test"          # Remove label
```

### Batch Operations

```bash
bd edit <id1> <id2> <id3> --status done       # Close multiple
bd update <id1> <id2> --priority 1            # Update multiple
```

## Task Organization

### Current Structure

**7 Ready Tasks** (no blockers - can start now):
- `factory-gleam-cwl` [P1] JSON parsing in persistence
- `factory-gleam-7eg` [P1] Real stage execution (Gleam/Go/Rust/Python)
- `factory-gleam-akp` [P3] README documentation
- `factory-gleam-e63` [P3] Performance benchmarks
- `factory-gleam-8bi` [P3] Dry-run mode
- `factory-gleam-euq` [P3] Stage filtering
- `factory-gleam-5ku` [P3] Retry logic

**14 Blocked Tasks** (waiting on dependencies):
- P1: 3 tasks (need test tasks to complete)
- P2: 11 tasks (need other tasks to complete)

## Workflow Patterns

### Pattern 1: Start a Task

```bash
# 1. See what's ready to work
bd ready

# 2. Pick highest priority
# For example: factory-gleam-cwl

# 3. Check details
bd show factory-gleam-cwl

# 4. Mark as in progress
bd edit factory-gleam-cwl --status in_progress

# 5. Work on implementation...

# 6. When done
bd edit factory-gleam-cwl --status done
```

### Pattern 2: Block/Unblock Tasks

Example: Task B cannot start until Task A is done

```bash
# Create the blocker
bd dep add <task-A> <task-B>

# Verify it's blocked
bd show <task-B>
# Output shows: "Blocked by: <task-A>"

# When Task A is done
bd edit <task-A> --status done

# The blocker is removed automatically when dependencies are met
bd ready
# Now <task-B> will appear in ready list
```

### Pattern 3: Complete Related Work

Example: All unit tests can be done together

```bash
# Check which tests exist
bd list -p 2 | grep "unit tests"

# Complete them together
bd edit factory-gleam-4rv factory-gleam-0t5 factory-gleam-mi1 \
         factory-gleam-kga factory-gleam-1ol factory-gleam-hjl \
         factory-gleam-3jm factory-gleam-pt4 \
         --status done

# See what's newly ready
bd ready
```

### Pattern 4: Create Subtasks

For complex tasks, break them into subtasks:

```bash
# Create subtasks
bd create "Implement json_to_record function" -p 1 \
  --description "Parse TaskRecord from JSON in persistence.gleam"

bd create "Implement record_to_json function" -p 1 \
  --description "Serialize TaskRecord to JSON in persistence.gleam"

# Make them blockers for the main task
bd dep add <subtask1> factory-gleam-j7c
bd dep add <subtask2> factory-gleam-j7c

# Now factory-gleam-j7c won't complete until both subtasks are done
bd show factory-gleam-j7c
```

## Database Format

Beads stores all data as JSONL in `.beads/beads.db`:

```json
{
  "id": "factory-gleam-cwl",
  "title": "Implement real JSON parsing in persistence.gleam using simplifile",
  "priority": "P1",
  "status": "open",
  "description": "Parse and serialize TaskRecord/StageRecord to/from JSON...",
  "created_at": "2026-01-04T20:30:00Z",
  "modified_at": "2026-01-04T20:35:00Z",
  "dependencies": [],
  "labels": []
}
```

Each line is a complete task. Git tracks changes naturally.

## Advanced Features

### Comments

```bash
bd comment <id>           # Add comment to issue
bd comments list <id>     # Show all comments
```

### Labels for Organization

```bash
bd label add factory-gleam-4rv "testing"
bd label add factory-gleam-0t5 "testing"
bd label list            # Show all labels
```

### Search & Filter

```bash
bd search "JSON"         # Find tasks mentioning JSON
bd list --label testing  # Show all "testing" labeled tasks
bd list -p "1,2"        # Show P1 and P2 only
```

### Health Checks

```bash
bd doctor               # Check installation health
bd validate             # Comprehensive database validation
bd duplicates           # Find duplicate issues
```

### Maintenance

```bash
bd cleanup              # Delete closed issues to save space
bd compact              # Compact old closed issues
```

## Pro Tips

1. **Always check `bd ready` first** - See what you can actually work on
2. **Use priorities wisely**: 
   - P1 = Must do (core functionality)
   - P2 = Important (testing & verification)
   - P3 = Nice to have (features & polish)
3. **Batch operations save time**: Update multiple related tasks at once
4. **Good descriptions preserve context**: Include "why" and "what needs to be done"
5. **Dependencies enforce workflow**: Can't skip steps accidentally
6. **Status matters**: Keep it updated for accurate "ready" list
7. **Search before creating**: Use `bd search` to avoid duplicates

## Command Cheat Sheet

| Command | Purpose | Example |
|---------|---------|---------|
| `bd ready` | Show ready work | `bd ready` |
| `bd blocked` | Show blocked tasks | `bd blocked` |
| `bd create` | Create new task | `bd create "Title" -p 1` |
| `bd show` | View task details | `bd show factory-gleam-cwl` |
| `bd edit` | Edit task fields | `bd edit factory-gleam-cwl --status done` |
| `bd list` | List all tasks | `bd list -p 1` |
| `bd search` | Search by text | `bd search "JSON"` |
| `bd dep add` | Create blocker | `bd dep add task1 task2` |
| `bd dep rm` | Remove blocker | `bd dep rm task1 task2` |
| `bd close` | Close task | `bd close factory-gleam-cwl` |
| `bd stats` | Show statistics | `bd stats` |
| `bd label add` | Add label | `bd label add task1 "testing"` |
| `bd comment` | Add comment | `bd comment factory-gleam-cwl` |

## Current Task Dependencies

```
factory-gleam-4rv    (unit test: domain)
factory-gleam-0t5    (unit test: cli)     blocks --> factory-gleam-6ug (CLI parsing at entry)
factory-gleam-mi1    (unit test: stages)  blocks --> factory-gleam-duh (subprocess execution)
factory-gleam-kga    (unit test: worktree)
factory-gleam-1ol    (unit test: persistence) blocks --> factory-gleam-j7c (JSON parsing)
factory-gleam-hjl    (unit test: tcr)
factory-gleam-3jm    (unit test: repo)
factory-gleam-pt4    (unit test: integration)
       |
       +---> factory-gleam-gs0 (E2E test)
              |
              +---> factory-gleam-3wr (build & verify)
                     |
                     +---> factory-gleam-dem (CLI testing)
                            |
                            +---> factory-gleam-akp (README docs)
```

P1 tasks ready to work (no blockers):
- `factory-gleam-cwl` (JSON parsing)
- `factory-gleam-7eg` (Stage execution)

When a test task completes, its blocking task becomes ready.

## Next Steps

1. **Start here**: `bd ready` to see available work
2. **Pick one**: Choose highest priority with no blockers
3. **Get details**: `bd show <id>` to understand scope
4. **Mark progress**: `bd edit <id> --status in_progress`
5. **Deliver**: Complete task and `bd edit <id> --status done`
6. **Repeat**: Run `bd ready` again to see what's newly available

Good luck! The dependency system will guide you through the work in the right order.
