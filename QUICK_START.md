# 🚀 Quick Start - Beads for Factory Gleam

## The One Command You Need

```bash
bd ready
```

This shows 7 tasks you can start right now.

## The Basic Workflow

```bash
# 1. See what's ready
bd ready

# 2. Pick one (e.g., factory-gleam-cwl)
bd show factory-gleam-cwl

# 3. Start it
bd edit factory-gleam-cwl --status in_progress

# 4. Work on it...

# 5. Mark done
bd edit factory-gleam-cwl --status done

# 6. Go back to step 1
bd ready
```

## All Available Commands

```bash
# Viewing
bd ready              # Ready work (most important!)
bd blocked            # What's blocked
bd list               # All tasks
bd list -p 1          # P1 only
bd show <id>          # Task details
bd search "text"      # Find tasks
bd stats              # Statistics

# Workflow
bd create "Title" -p 1           # Create task
bd edit <id> --status done       # Mark done
bd edit <id> --status in_progress  # Mark in progress
bd close <id>                    # Close task

# Dependencies
bd dep add <task1> <task2>   # task1 blocks task2
bd dep rm <task1> <task2>    # Remove blocker

# Other
bd info              # Database info
bd doctor            # Health check
```

## What You Have

✓ 21 tasks (5 P1, 11 P2, 5 P3)
✓ 7 ready to start right now
✓ 14 blocked (waiting on dependencies)
✓ All in git-backed database
✓ Dependency system prevents mistakes

## Priority Guide

**P1** = Must do (core implementation)
**P2** = Important (testing & verification)
**P3** = Nice to have (features & polish)

Start with P1 tasks: `factory-gleam-cwl` or `factory-gleam-7eg`

## Documentation

- **BEADS_SETUP_COMPLETE.md** - Full overview
- **BEADS_GUIDE.md** - Detailed guide
- **BEADS_REFERENCE.md** - Command reference

## Why Beads?

Beads by Steve Yegge is specifically built for AI agents:
- Dependencies prevent mistakes
- Git-backed = no setup
- Zero merge conflicts
- Designed for coding agents

That's it! Run `bd ready` to start.
