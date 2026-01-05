# Next Session: Reverse Prompt for Claude Code

**Purpose**: Start the next session with full context without re-reading all documents

**Status**: Ready to copy/paste into Claude Code prompt

---

## The Context (Condensed)

### What We Built
You're building **Factory v2** - a 10-stage AI-assisted engineering pipeline for solo founders.

**Key tech**:
- Language: Gleam (compiling, clean build)
- VCS: Jujutsu (jj) workspaces
- Task mgmt: Beads (task database)
- CI/CD: moon.dev (task orchestration)
- Code search: Codanna (semantic indexing)
- Specs: Intent specs (executable specifications)

### The Problem v2 Solves
Your original design doc identified **10 critical problems** in v1:
1. Auto-commit trap (recursive self-approval)
2. Confidence scoring fiction (0.92 = meaningless)
3. Learning loop backfires (becomes conservative)
4. Monitoring lag (5-60 min to detect issues)
5. Audit trail confusion (system auto-updates beads)
6. No boundary enforcement (hope prevents scope creep)
7. Coverage metrics meaningless (87% says nothing)
8. Error output noise (actual errors buried)
9. Language hardcoded for Go (fails for Gleam)
10. Rollback unclear (TCR reverts to where?)

### What v2 Fixes
- **Removes**: Auto-commits, confidence scores, hope-based enforcement
- **Adds**: Feature flags, approval gates, audit trail, error extraction, language detection, explicit TCR, metrics dashboard, coverage analysis, surgical learning
- **Keeps**: jj workspaces, 10-stage pipeline, TCR discipline, contracts, Gleam impl, integrations

### Documents Created Last Session
1. **V2_SUMMARY.md** - 500 lines, executive summary
2. **V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md** - 2,500 lines, detailed specs
3. **MOON_INTEGRATION_CONTRACT.md** - 1,500 lines, moon.dev integration
4. **V2_IMPLEMENTATION_ROADMAP.md** - 1,000 lines, 4-phase plan
5. **V2_DESIGN_INDEX.md** - Navigation guide

All in `/home/lewis/src/factory-gleam/`

### Implementation Plan
**Phase 1** (Week 1): Safety - remove auto-commit, add flags, boundaries, audit
**Phase 2** (Week 2): Core - error handling, language detection, TCR specs
**Phase 3** (Week 3): Polish - metrics, coverage, learning
**Phase 4** (Ongoing): Integration - Beads, Codanna, Intent, moon links

---

## Where We Left Off

**Status**: Design complete, ready for implementation

**Decision Point**: Did you review the v2 documents? Any changes needed?

**Approved to proceed with**: [YOUR ANSWER NEEDED]

---

## For This Session

### If Reviewing Design
- Read and critique the v2 design documents
- Propose changes or ask clarifying questions
- Approve or request modifications
- When ready: say "approved, start Phase 1"

### If Starting Implementation
- Pick Phase 1 task (from V2_IMPLEMENTATION_ROADMAP.md)
- I'll help implement that specific problem fix
- We'll code the solution and test it
- Move to next Phase 1 task

### If Debugging/Exploring
- Describe what you're investigating
- I'll search the codebase and help diagnose

---

## Key Files You Might Reference

```
/home/lewis/src/factory-gleam/
├── src/domain.gleam           # Core types (brilliant, keep it)
├── src/cli.gleam              # Command parsing (good)
├── src/factory.gleam          # Main entry point (needs work)
├── src/stages.gleam           # Pipeline stages (needs language detection)
├── src/process.gleam          # Shell execution (needs implementation)
├── src/persistence.gleam      # Task state (needs completion)
├── src/worktree.gleam         # jj workspaces (needs jj integration)
├── src/tcr.gleam              # Test && Commit || Revert (needs implementation)
├── src/integration.gleam      # Post-merge testing (skeleton)
├── src/repo.gleam             # Repo detection (skeleton)
│
├── V2_SUMMARY.md              # (Last session - your approval needed)
├── V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md
├── MOON_INTEGRATION_CONTRACT.md
├── V2_IMPLEMENTATION_ROADMAP.md
├── V2_DESIGN_INDEX.md
│
├── IMPLEMENTATION_PLAN.md     # Original v1 plan (reference)
├── gleam.toml                 # Project manifest
├── manifest.toml              # Dependencies
└── README.md                  # Project description
```

---

## Quick Command Reference

```bash
# Check current status
git status
git log --oneline | head -5

# See what's already implemented
gleam build                    # Should compile cleanly

# Run tests (if any)
gleam test

# Check code structure
ls -la src/

# View a specific file
cat src/domain.gleam | head -50

# Create a task workspace (once factory works)
factory new batch-upload --from bd-52.1
```

---

## The Honest Assessment (From Your Doc)

You wrote:
> "You cannot have genuine autonomous AI without maintaining oversight. The question isn't 'how do we eliminate your involvement' but 'how do we make your involvement faster and more effective.'"

**That's what v2 does.** It's not autonomy—it's leverage through encoding your judgment.

---

## What Claude Code Will Do This Session

Based on your answer:

### If "Review Design"
- Read through v2 docs you created
- Look for gaps, inconsistencies, missed cases
- Suggest refinements
- Help you make final design decisions

### If "Start Phase 1"
- Pick first problem to fix (auto-commit removal or feature flags)
- Write code in Gleam that implements it
- Test the implementation
- Create test cases
- Document what we built

### If "Debug Codebase"
- Search for relevant code patterns
- Explain how things work
- Identify what's blocking progress
- Propose fixes

---

## For Your Next Message

Tell Claude Code:

```
What I want to do this session:
[Choose one]
  A) Review and finalize v2 design
  B) Start implementing Phase 1 (Problem #1, #4, #5, or #6)
  C) Debug/explore [specific issue]
  D) [Something else]

Current blockers or concerns:
[What's on your mind]

Approval status on v2 design:
[Approved as-is / Need changes / Want to review first]
```

---

## Success Criteria for This Session

### If Reviewing
✅ All v2 design decisions understood
✅ All 10 problems adequately addressed
✅ No safety gaps remaining
✅ Ready to implement

### If Implementing Phase 1
✅ Auto-commit removal done
✅ Feature flags working
✅ Pre-commit hook enforcing boundaries
✅ Audit trail tracking who approved what
✅ Tests verify all 4 safety mechanisms

### If Debugging
✅ Root cause understood
✅ Solution identified
✅ Plan to proceed

---

## Things We Know Work

✅ Gleam compiles cleanly
✅ Domain model is solid (Language, Task, Stage types)
✅ CLI parsing is good (no interactive prompts)
✅ 10-stage pipeline architecture is right
✅ Contract-driven development is the way
✅ jj workspaces are brilliant for isolation
✅ TCR discipline is gold standard

## Things We Know Need Work

❌ process.gleam (shell execution not implemented)
❌ persistence.gleam (JSON parsing incomplete)
❌ worktree.gleam (needs actual jj calls)
❌ Error handling (needs extraction + summarization)
❌ Language detection (needs auto-config generation)
❌ Feature flags (not implemented yet)
❌ Audit trail (needs beads integration)

---

## The Big Picture

You're not trying to make AI autonomous. You're building **infrastructure that encodes your engineering judgment**.

Over 12 months:
- Month 1: You review everything (2 min/change)
- Month 3: 60% auto-ships, you review risky ones
- Month 6: 90% auto-ships, system learned your patterns
- Month 12: 95% ships autonomously, you manage by exception

**But you're more confident in month 12, not less**, because the system learned what actually breaks in YOUR codebase.

v2 makes that possible safely.

---

## One More Thing

The documents you created last session aren't finished. They're proposals.

This session is your chance to:
1. Review them critically
2. Propose changes
3. Refine them
4. Or start building from them

No decision is final until you're ready to code.

---

## Ready for Next Session?

When you open Claude Code again, just say:

```
I'm resuming Factory v2 work. We created comprehensive v2 design docs last session.

Today I want to: [A/B/C/D from above]

Key context: [Any new info since last session]
```

And I'll have full context of where we left off.

---

## Session Checklist Before You Start

Before you come back, you might want to:
- [ ] Skim V2_SUMMARY.md (5 min)
- [ ] Decide: Review design or start Phase 1?
- [ ] List any questions or concerns
- [ ] Check git status (new docs are untracked)
- [ ] Verify gleam build still works

But no pressure—I'll help with all of that in the next session.

