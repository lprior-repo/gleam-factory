# TDD-TCR-Refactor Tooling Integration Summary

## Changes Made

### 1. Created CLAUDE.md
- Comprehensive AI agent guide
- Full bv robot command documentation
- Codanna semantic search usage
- Development workflow instructions
- Code quality standards (CUPID, Functional Core/Imperative Shell)

### 2. Updated tdd-tcr-refactor-loop Script

#### Integration with bv (Beads Viewer)
- **get_all_open_beads()**: Now uses `bv --recipe actionable --robot-plan` first
  - Graph-aware priority (PageRank, betweenness, critical path)
  - Respects blockers automatically
  - Falls back to `bd ready` then manual JSONL parsing
  
- **show_bv_triage()**: New function to display triage summary
  - Shows open/actionable/blocked counts
  - Displays top recommendation with score
  - Called automatically in `--all` mode

- **check_dependencies()**: Enhanced to check for optional tools
  - Warns if bv, bd, or codanna are missing
  - Provides installation URLs
  - Doesn't fail if optional tools absent

#### Documentation Updates
- Updated help text to mention bv/bd/codanna integration
- Added tooling detection information
- Enhanced configuration display

### 3. Installed and Configured Tools

#### bv (Beads Viewer)
- Already installed at: `/home/lewis/.opencode/bin/bv`
- Tested with `--robot-triage` command
- Returns graph metrics: PageRank, betweenness, critical path, cycles, etc.

#### Codanna (Semantic Code Search)
- Installed via cargo: `v0.9.0`
- Initialized in project: `.codanna/` directory created
- Source code indexing started
- Embedding model: AllMiniLML6V2

### 4. Created setup-tools.sh
- Automated installation script for all tools
- Checks for required dependencies
- Installs bv and codanna if missing
- Initializes and indexes project automatically
- Tests installations

### 5. Moved bead-runner Features
All valuable features from `bead-runner` were integrated into `tdd-tcr-refactor-loop`:
- Graph-aware bead selection (via bv)
- Better failure handling (comments on failed beads)
- Pause between beads in --all mode
- Enhanced statistics tracking
- bd CLI integration for updates

## Tool Capabilities Summary

### bv (Beads Viewer)
**Purpose**: Graph-aware triage and planning

**Key Commands** (always use `--robot-*` flags):
```bash
bv --robot-triage              # Complete triage with recommendations
bv --robot-next                # Just the top pick
bv --robot-plan                # Parallel execution tracks
bv --robot-insights            # Full graph metrics
bv --robot-label-health        # Per-label health scores
bv --robot-alerts              # Stale issues, cascading blocks
```

**Returns**: JSON with dependency graph analysis, PageRank scores, critical paths, cycle detection

**Note**: bv is read-only (analysis only), use `bd` for updates

### bd (Beads CLI)
**Purpose**: Bead CRUD operations

**Key Commands**:
```bash
bd show <id> --json           # Get bead details
bd ready --json               # Get ready beads (no blockers)
bd update <id> --status <s>   # Update status
bd close <id> --reason "..."  # Close with reason
bd comment <id> "..."         # Add comment
```

### Codanna
**Purpose**: Semantic code search and relationship tracking

**Key Commands**:
```bash
codanna init                                              # Initialize
codanna index src                                         # Index source
codanna mcp semantic_search_with_context query:"..." limit:N  # Search code
codanna retrieve symbol <name>                           # Get symbol info
codanna documents add-collection docs ./docs             # Add docs
codanna documents index                                   # Index docs
```

**Returns**: Code matches with context, symbol definitions, relationships

## Usage Examples

### For AI Agents (Claude)
```bash
# 1. Check what to work on
bv --robot-triage | jq '.triage.quick_ref'

# 2. Get the top recommendation
bv --robot-triage | jq '.triage.quick_ref.top_picks[0]'

# 3. Understand related code
codanna mcp semantic_search_with_context query:"signal handling" limit:5

# 4. Execute the work
./tdd-tcr-refactor-loop --bead factory-gleam-z24
```

### For Humans
```bash
# Process all ready beads with graph-aware priority
./tdd-tcr-refactor-loop --all

# See graph visualization in browser
bv --export-graph project-graph.html

# Check project health
bv --robot-label-health | jq '.results.labels[] | select(.health_level == "critical")'
```

## File Structure

```
factory-gleam/
├── CLAUDE.md                      # AI agent guide (NEW)
├── INTEGRATION_SUMMARY.md         # This file (NEW)
├── setup-tools.sh                 # Tool installer (NEW)
├── tdd-tcr-refactor-loop          # Updated with bv/bd/codanna
├── bead-runner                    # Can be deleted (features moved)
├── .codanna/                      # Codanna config & indexes (NEW)
│   ├── settings.toml
│   └── .codannaignore
└── .beads/                        # Beads data (existing)
    └── beads.jsonl
```

## Integration Benefits

1. **Graph-Aware Priority**: bv uses PageRank to identify foundational blockers
2. **Dependency-Aware**: Automatically respects blocking relationships
3. **Semantic Understanding**: Codanna finds relevant code by meaning, not just keywords
4. **Automated Workflow**: `--all` mode processes beads in optimal order
5. **No Manual Parsing**: Tools handle JSONL parsing and graph analysis
6. **Metric-Driven**: Decisions based on betweenness, critical path, etc.

## Next Steps

1. **Initialize if needed**: Run `./setup-tools.sh`
2. **Read the guide**: `cat CLAUDE.md`
3. **Start working**: `./tdd-tcr-refactor-loop --all`
4. **Monitor progress**: `bv --robot-triage | jq '.triage.quick_ref'`

## References

- [bv (Beads Viewer)](https://github.com/Dicklesworthstone/beads_viewer)
- [Codanna](https://github.com/bartolli/codanna)
- [MCP Agent Mail](https://github.com/Dicklesworthstone/mcp_agent_mail) (for multi-agent coordination)

## Sources

This integration follows best practices from:
- [Beads Viewer GitHub](https://github.com/Dicklesworthstone/beads_viewer)
- [Codanna GitHub](https://github.com/bartolli/codanna)
- [awesome-claude-code Issue #108](https://github.com/hesreallyhim/awesome-claude-code/issues/108)
