# Claude AI Agent Guide for Factory-Gleam

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

## Using Codanna for Semantic Code Search

Codanna provides semantic search and relationship tracking for the codebase. Use it to understand code structure and find relevant implementations.

### Basic Usage

```bash
# Semantic search for code
codanna mcp semantic_search_with_context query:"where do we handle errors" limit:3

# Search documentation
codanna mcp search_documents query:"authentication flow"
```

### Setup Codanna

If not already indexed:
```bash
codanna init
codanna index src --progress
codanna documents add-collection docs ./docs  # if you have docs
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

### Recommended AI Workflow

1. **Start with triage**: `bv --robot-triage` to understand what needs work
2. **Pick a task**: Use top recommendation or check `--robot-next`
3. **Understand context**: Use Codanna to search related code
4. **Execute**: Use `./tdd-tcr-refactor-loop --bead <id>` to implement
5. **Verify**: Check with `bv --robot-insights` for impact on graph

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
