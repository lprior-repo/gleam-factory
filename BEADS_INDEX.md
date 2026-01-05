# Beads System - Complete Index

## Overview
Beads is Steve Yegge's dependency-aware task management system built for AI coding agents. All 21 work items for Factory Gleam are organized here.

## Reading Guide

### Start Here (Pick One)
1. **QUICK_START.md** - 5-minute overview with essential commands
2. **BEADS_SETUP_COMPLETE.md** - Full setup summary and context

### Going Deeper
3. **BEADS_GUIDE.md** - Comprehensive feature guide
4. **BEADS_REFERENCE.md** - Complete command reference with workflows

## One-Minute Summary

```bash
cd /home/lewis/src/factory-gleam
bd ready    # See 7 tasks ready to work on now
bd show factory-gleam-cwl  # Understand a task
bd edit factory-gleam-cwl --status in_progress  # Start working
# ... work on implementation ...
bd edit factory-gleam-cwl --status done  # Mark complete
bd ready    # See newly available work
```

## Task Organization

| Priority | Count | Type | Status |
|----------|-------|------|--------|
| P1       | 5     | Core Implementation | 2 ready, 3 blocked |
| P2       | 11    | Testing & Build | 0 ready, 11 blocked |
| P3       | 5     | Features & Polish | 5 ready, 0 blocked |
| **Total**| **21** | | **7 ready, 14 blocked** |

## Current State

- ✅ 21 tasks created and prioritized
- ✅ Dependency chain established (14 blocked tasks)
- ✅ 7 tasks ready to start immediately
- ✅ Database initialized in stealth mode
- ✅ All documentation written

## Key Files in This Directory

```
.beads/
├── beads.db          SQLite database with all tasks
└── config.toml       Beads configuration

QUICK_START.md        One-page essential guide
BEADS_SETUP_COMPLETE.md  Full setup summary
BEADS_GUIDE.md        Comprehensive feature guide
BEADS_REFERENCE.md    Command reference & patterns
BEADS_INDEX.md        This file
```

## Most Important Commands

```bash
bd ready              # Essential: see work ready to do
bd blocked            # See what's waiting on dependencies
bd show <id>          # View task details
bd edit <id> --status done   # Mark task complete
bd list -p 1          # List P1 priority tasks
bd stats              # Project statistics
```

## Next Steps

1. Run: `bd ready`
2. Pick highest priority: `factory-gleam-cwl` or `factory-gleam-7eg`
3. Read: `bd show <id>`
4. Work on implementation
5. Mark done: `bd edit <id> --status done`
6. Repeat: `bd ready`

## Learning Resources

**Local Documentation:**
- QUICK_START.md - Fastest way to get started
- BEADS_REFERENCE.md - Complete command guide

**Online:**
- GitHub: https://github.com/steveyegge/beads
- Medium: Search "Steve Yegge Beads"
- Best Practices: https://steve-yegge.medium.com/beads-best-practices-2db636b9760c

## FAQ

**Q: Which task should I work on?**
A: Run `bd ready` - shows 7 tasks with no blockers. Start with P1 priority.

**Q: How do dependencies work?**
A: When a task is done, any tasks it was blocking automatically become ready.

**Q: Can I create new tasks?**
A: Yes! `bd create "Title" -p 1 --description "Details"`

**Q: How is data stored?**
A: SQLite database in `.beads/beads.db`, backed by git, stealth mode (not committed).

**Q: Why not use GitHub issues?**
A: Beads is lightweight, dependency-aware, and designed for AI agents.

---

**You're ready to go!** Start with:
```bash
cd /home/lewis/src/factory-gleam && bd ready
```
