# ✅ Setup Complete: bv + Codanna Integration

## What Was Done

### 1. Installed & Configured Tools

#### ✅ bv (Beads Viewer) - Graph-Aware Triage
- **Status**: Already installed at `/home/lewis/.opencode/bin/bv`
- **Purpose**: Graph analysis, dependency-aware triage, PageRank-based prioritization
- **Usage**: `bv --robot-triage` for full analysis, `bv --robot-next` for top pick

#### ✅ Codanna - Semantic Code Search  
- **Status**: Newly installed (`v0.9.0`)
- **Purpose**: Semantic code search, symbol lookup, documentation search
- **Indexed**:
  - ✅ Source code: `src/`
  - ✅ Documentation: `docs/` (CLAUDE.md, ARCHITECTURE.md, etc.)
  - ✅ Beads data: `.beads/`

### 2. Created Documentation

#### ✅ docs/CLAUDE.md - AI Agent Guide
**CRITICAL**: This file contains MANDATORY requirements for AI agents:
- **Codanna is REQUIRED** for all code search (NOT Grep/Glob)
- Complete bv command reference with examples
- Required workflow: Research with Codanna BEFORE coding
- Code quality standards (CUPID, Functional Core/Imperative Shell)

#### ✅ docs/INTEGRATION_SUMMARY.md
- Complete integration details
- Tool capabilities and command reference
- Usage examples for AI and humans
- Benefits and next steps

### 3. Updated Scripts

#### ✅ tdd-tcr-refactor-loop
- Integrated `bv --recipe actionable --robot-plan` for bead selection
- Added `show_bv_triage()` function for graph insights
- Enhanced dependency checking with optional tool warnings
- Updated help text with tool integration info

#### ✅ setup-tools.sh
- Automated installation of bv and Codanna
- Checks dependencies
- Initializes and indexes project
- Tests installations

#### ✅ test-tooling.sh
- Validates bv integration
- Validates Codanna code search
- Validates Codanna document search
- Provides example commands

## Verification

```bash
$ ./test-tooling.sh
Testing integrated tooling...

1. Testing bv --robot-triage...
   ✓ bv working
2. Testing Codanna code search...
   ✓ Codanna code search working
3. Testing Codanna document search...
   ✓ Codanna document search working
     Found: Found 5 document(s) matching 'bv triage':

All tests passed! ✓
```

## Quick Start

### For AI Agents (Claude)

**READ THIS FIRST**: `cat docs/CLAUDE.md`

Required workflow:
```bash
# 1. Triage - what to work on?
bv --robot-triage | jq '.triage.quick_ref.top_picks[0]'

# 2. Research - MANDATORY step
codanna mcp semantic_search_with_context query:"FEATURE" limit:5
codanna mcp search_documents query:"how to FEATURE"

# 3. Execute
./tdd-tcr-refactor-loop --bead <id>

# 4. Verify
bv --robot-insights
```

### For Humans

```bash
# See what needs work (graph-aware)
bv --robot-triage | jq '.triage.quick_ref'

# Process all ready beads automatically
./tdd-tcr-refactor-loop --all

# Search code semantically
codanna mcp semantic_search_with_context query:"error handling" limit:5

# Search documentation
codanna mcp search_documents query:"TDD workflow"

# Visualize dependency graph
bv --export-graph project-graph.html
```

## Key Commands

### bv (Graph Analysis)
```bash
bv --robot-triage              # Full triage with recommendations
bv --robot-next                # Just top pick
bv --robot-plan                # Parallel execution tracks
bv --robot-insights            # Full graph metrics
bv --robot-alerts              # Stale issues, cascading blocks
```

### Codanna (Semantic Search)
```bash
# Code search
codanna mcp semantic_search_with_context query:"WHAT" limit:N

# Symbol lookup
codanna retrieve symbol <name>

# Documentation search  
codanna mcp search_documents query:"QUESTION"
```

### TDD-TCR-Refactor Loop
```bash
./tdd-tcr-refactor-loop --bead <id>    # Single bead
./tdd-tcr-refactor-loop --all          # All ready beads
./tdd-tcr-refactor-loop "requirements"  # Custom requirements
```

## File Structure

```
factory-gleam/
├── docs/                           # ← Documentation (NEW)
│   ├── CLAUDE.md                   # ← AI agent guide (CRITICAL)
│   ├── ARCHITECTURE.md
│   ├── INTEGRATION_SUMMARY.md
│   └── .ai-learnings.md
├── .codanna/                       # ← Codanna config & indexes
│   ├── settings.toml
│   └── index/
│       ├── tantivy/               # Code index
│       ├── semantic/              # Embeddings
│       └── documents/             # Doc index
├── tdd-tcr-refactor-loop           # ← Updated with bv
├── setup-tools.sh                  # ← Tool installer
├── test-tooling.sh                 # ← Validation script
└── SETUP_COMPLETE.md               # ← This file
```

## What Changed

### BREAKING CHANGES
1. **Codanna is now MANDATORY** for all code search
2. AI agents **MUST NOT** use Grep/Glob for code search
3. AI agents **MUST** research with Codanna BEFORE writing code

### Integration Benefits
- ✅ Graph-aware prioritization (PageRank, betweenness, critical path)
- ✅ Semantic code understanding (not just keyword matching)
- ✅ Dependency-aware triage (respects blockers automatically)
- ✅ Fast indexed search (faster than grep on large codebases)
- ✅ Context-aware results (relevant surrounding code included)
- ✅ Relationship tracking (understands symbol dependencies)

## Next Steps

1. **For new AI sessions**: Start with `cat docs/CLAUDE.md`
2. **For development**: Run `./tdd-tcr-refactor-loop --all`
3. **For triage**: Run `bv --robot-triage | jq '.triage.quick_ref'`
4. **For research**: Use Codanna before writing any code

## Sources & References

- [bv (Beads Viewer)](https://github.com/Dicklesworthstone/beads_viewer) - Graph-aware triage engine
- [Codanna](https://github.com/bartolli/codanna) - Semantic code search tool
- [awesome-claude-code #108](https://github.com/hesreallyhim/awesome-claude-code/issues/108) - Integration guide
- [MCP Agent Mail](https://github.com/Dicklesworthstone/mcp_agent_mail) - For multi-agent coordination

## Troubleshooting

### Re-index Codanna
```bash
codanna index src
codanna documents index
```

### Test Integration
```bash
./test-tooling.sh
```

### Verify bv
```bash
bv --robot-triage | jq '.triage.quick_ref'
```

### Verify Codanna
```bash
codanna mcp search_documents query:"test"
```

---

**Status**: ✅ ALL SYSTEMS OPERATIONAL

Last updated: 2026-01-06
