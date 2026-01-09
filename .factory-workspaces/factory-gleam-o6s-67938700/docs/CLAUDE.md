# Claude AI Agent Guide for Factory-Gleam

```
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║  ⚠️  CRITICAL: READ THIS FIRST  ⚠️                            ║
║                                                               ║
║  1. Use CODANNA for ALL code search (MANDATORY)              ║
║     - codanna mcp semantic_search_with_context               ║
║     - codanna mcp search_documents                           ║
║     - codanna retrieve symbol                                 ║
║                                                               ║
║  2. Use BV for triage and planning (NOT bd/grep)             ║
║     - bv --robot-triage                                      ║
║     - bv --robot-plan                                        ║
║                                                               ║
║  3. ALWAYS research with Codanna BEFORE writing code         ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

This document provides guidance for AI agents (like Claude) working on this codebase.

## Project Overview

Factory-Gleam is a Gleam implementation of the Factory-Synapse distributed build system using the Beads issue tracking methodology.

## Using bv as an AI Sidecar

bv is a graph-aware triage engine for Beads projects (.beads/beads.jsonl). Instead of parsing JSONL or hallucinating graph traversal, use robot flags for deterministic, dependency-aware outputs with precomputed metrics (PageRank, betweenness, critical path, cycles, HITS, eigenvector, k-core).

**Scope boundary:** bv handles *what to work on* (triage, priority, planning). For agent-to-agent coordination (messaging, work claiming, file reservations), use [MCP Agent Mail](https://github.com/Dicklesworthstone/mcp_agent_mail).

**⚠️ CRITICAL: Use ONLY `--robot-*` flags. Bare `bv` launches an interactive TUI that blocks your session.**

### The Workflow: Start With Triage

**`bv --robot-triage` is your single entry point.** It returns everything you need in one call:
- `quick_ref`: at-a-glance counts + top 3 picks
- `recommendations`: ranked actionable items with scores, reasons, unblock info
- `quick_wins`: low-effort high-impact items
- `blockers_to_clear`: items that unblock the most downstream work
- `project_health`: status/type/priority distributions, graph metrics
- `commands`: copy-paste shell commands for next steps

```bash
bv --robot-triage        # THE MEGA-COMMAND: start here
bv --robot-next          # Minimal: just the single top pick + claim command
```

### Other Commands

**Planning:**
| Command | Returns |
|---------|---------|
| `--robot-plan` | Parallel execution tracks with `unblocks` lists |
| `--robot-priority` | Priority misalignment detection with confidence |

**Graph Analysis:**
| Command | Returns |
|---------|---------|
| `--robot-insights` | Full metrics: PageRank, betweenness, HITS (hubs/authorities), eigenvector, critical path, cycles, k-core, articulation points, slack |
| `--robot-label-health` | Per-label health: `health_level` (healthy\|warning\|critical), `velocity_score`, `staleness`, `blocked_count` |
| `--robot-label-flow` | Cross-label dependency: `flow_matrix`, `dependencies`, `bottleneck_labels` |
| `--robot-label-attention [--attention-limit=N]` | Attention-ranked labels by: (pagerank × staleness × block_impact) / velocity |

**History & Change Tracking:**
| Command | Returns |
|---------|---------|
| `--robot-history` | Bead-to-commit correlations: `stats`, `histories` (per-bead events/commits/milestones), `commit_index` |
| `--robot-diff --diff-since <ref>` | Changes since ref: new/closed/modified issues, cycles introduced/resolved |

**Other Commands:**
| Command | Returns |
|---------|---------|
| `--robot-burndown <sprint>` | Sprint burndown, scope changes, at-risk items |
| `--robot-forecast <id\|all>` | ETA predictions with dependency-aware scheduling |
| `--robot-alerts` | Stale issues, blocking cascades, priority mismatches |
| `--robot-suggest` | Hygiene: duplicates, missing deps, label suggestions, cycle breaks |
| `--robot-graph [--graph-format=json\|dot\|mermaid]` | Dependency graph export |
| `--export-graph <file.html>` | Self-contained interactive HTML visualization |

### Scoping & Filtering

```bash
bv --robot-plan --label backend              # Scope to label's subgraph
bv --robot-insights --as-of HEAD~30          # Historical point-in-time
bv --recipe actionable --robot-plan          # Pre-filter: ready to work (no blockers)
bv --recipe high-impact --robot-triage       # Pre-filter: top PageRank scores
bv --robot-triage --robot-triage-by-track    # Group by parallel work streams
bv --robot-triage --robot-triage-by-label    # Group by domain
```

### Understanding Robot Output

**All robot JSON includes:**
- `data_hash` — Fingerprint of source beads.jsonl (verify consistency across calls)
- `status` — Per-metric state: `computed|approx|timeout|skipped` + elapsed ms
- `as_of` / `as_of_commit` — Present when using `--as-of`; contains ref and resolved SHA

**Two-phase analysis:**
- **Phase 1 (instant):** degree, topo sort, density — always available immediately
- **Phase 2 (async, 500ms timeout):** PageRank, betweenness, HITS, eigenvector, cycles — check `status` flags

**For large graphs (>500 nodes):** Some metrics may be approximated or skipped. Always check `status`.

### jq Quick Reference

```bash
bv --robot-triage | jq '.quick_ref'                        # At-a-glance summary
bv --robot-triage | jq '.recommendations[0]'               # Top recommendation
bv --robot-plan | jq '.plan.summary.highest_impact'        # Best unblock target
bv --robot-insights | jq '.status'                         # Check metric readiness
bv --robot-insights | jq '.Cycles'                         # Circular deps (must fix!)
bv --robot-label-health | jq '.results.labels[] | select(.health_level == "critical")'
```

**Performance:** Phase 1 instant, Phase 2 async (500ms timeout). Prefer `--robot-plan` over `--robot-insights` when speed matters. Results cached by data hash.

Use bv instead of parsing beads.jsonl—it computes PageRank, critical paths, cycles, and parallel tracks deterministically.

## ⚠️ MANDATORY: Codanna for ALL Code Search

**CRITICAL REQUIREMENT**: You MUST use Codanna for ALL code search operations. DO NOT use Grep, Glob, or manual file reading to search for code patterns.

### Why Codanna is Mandatory

1. **Semantic Understanding**: Finds code by meaning, not just keywords
2. **Relationship Tracking**: Understands symbol dependencies and references
3. **Context-Aware**: Returns relevant surrounding code automatically
4. **Performance**: Indexed search is faster than grep on large codebases
5. **AI-Native**: Designed specifically for AI agent workflows

### REQUIRED Usage Pattern

**BEFORE writing or modifying code**, you MUST:

```bash
# 1. Search for existing implementations
codanna mcp semantic_search_with_context query:"error handling patterns" limit:5

# 2. Find symbol definitions
codanna retrieve symbol <function_name>

# 3. Search documentation for guidance
codanna mcp search_documents query:"how to implement feature X"
```

### Codanna Commands (Use These ONLY)

#### Code Search (Primary Tool)
```bash
# Semantic search with context
codanna mcp semantic_search_with_context query:"WHAT YOU ARE LOOKING FOR" limit:N

# Example queries:
codanna mcp semantic_search_with_context query:"actor supervision" limit:3
codanna mcp semantic_search_with_context query:"error recovery patterns" limit:5
codanna mcp semantic_search_with_context query:"message handling" limit:3
```

#### Symbol Lookup
```bash
# Get specific symbol definition and references
codanna retrieve symbol <name>

# Examples:
codanna retrieve symbol signal_bus
codanna retrieve symbol process_event
```

#### Documentation Search
```bash
# Search ALL indexed documentation
codanna mcp search_documents query:"YOUR QUESTION"

# Examples:
codanna mcp search_documents query:"how to use bv for triage"
codanna mcp search_documents query:"TDD workflow steps"
codanna mcp search_documents query:"CUPID principles"
```

### When to Use Each Command

| Task | Command | Example |
|------|---------|---------|
| Find similar code | `semantic_search_with_context` | "Find error handling code" |
| Understand architecture | `search_documents` | "How does signal bus work?" |
| Find function definition | `retrieve symbol` | `codanna retrieve symbol handle_message` |
| Before writing code | `semantic_search_with_context` | "Check for existing implementation" |
| Learning patterns | `search_documents` | "What patterns should I use?" |

### Setup Codanna (Already Done)

The project is already indexed with:
- ✅ Source code: `/home/lewis/src/factory-gleam/src`
- ✅ Documentation: `./docs` (CLAUDE.md, ARCHITECTURE.md, etc.)
- ✅ Beads data: `./.beads`

To re-index after major changes:
```bash
codanna index src
codanna documents index
```

## Development Workflow

### TDD-TCR-Refactor Loop

Use `./tdd-tcr-refactor-loop` for automated test-driven development with:
- **Auditor**: Writes failing tests
- **Implementer**: Makes tests pass (TCR enforced - test && commit || revert)
- **Architect**: Refactors for quality every N cycles
- **Reviewer**: Final polish

```bash
# Single bead
./tdd-tcr-refactor-loop --bead <bead-id>

# Process all ready beads
./tdd-tcr-refactor-loop --all

# Custom requirements
./tdd-tcr-refactor-loop "Add email validation function"
```

### Recommended AI Workflow (FOLLOW THIS)

1. **Start with triage**: `bv --robot-triage` to understand what needs work
   ```bash
   bv --robot-triage | jq '.triage.quick_ref.top_picks[0]'
   ```

2. **Research with Codanna** (MANDATORY STEP):
   ```bash
   # Search for existing patterns
   codanna mcp semantic_search_with_context query:"FEATURE DESCRIPTION" limit:5

   # Search documentation for guidance
   codanna mcp search_documents query:"how to implement FEATURE"

   # Find related symbols
   codanna retrieve symbol <relevant_name>
   ```

3. **Pick a task**: Use top bv recommendation
   ```bash
   BEAD_ID=$(bv --robot-triage | jq -r '.triage.quick_ref.top_picks[0].id')
   ```

4. **Execute with TDD-TCR-Refactor**:
   ```bash
   ./tdd-tcr-refactor-loop --bead $BEAD_ID
   ```

5. **Verify impact**: Check graph after completion
   ```bash
   bv --robot-insights | jq '.status'
   ```

### Example Complete Workflow

```bash
# 1. What should I work on?
bv --robot-triage | jq '.triage.quick_ref.top_picks[0]'
# Output: { "id": "factory-gleam-z24", "title": "Add gleam_otp dependencies", ... }

# 2. Research existing code (REQUIRED)
codanna mcp semantic_search_with_context query:"gleam dependencies management" limit:3
codanna mcp search_documents query:"how to add Gleam dependencies"

# 3. Execute
./tdd-tcr-refactor-loop --bead factory-gleam-z24

# 4. Verify
bv --robot-insights
```

## Code Quality Standards

- **Functional Core / Imperative Shell**: Keep business logic pure
- **CUPID Properties**: Composable, Unix-philosophy, Predictable, Idiomatic, Domain-based
- **Line Limit**: No function over 30 lines
- **Testing**: All code must pass tests (TCR enforced)
- **Gleam Idioms**: Use pipelines (`|>`), pattern matching, `Result`/`Option` types

## References

- [Beads Viewer (bv)](https://github.com/Dicklesworthstone/beads_viewer)
- [Codanna](https://github.com/bartolli/codanna)
- [MCP Agent Mail](https://github.com/Dicklesworthstone/mcp_agent_mail)
