# Implementation Guide for Cheap/Free LLMs

## Zero-Ambiguity Contract System

Each bead now includes:

1. **EXACT SIGNATURE** - Copy-paste ready Gleam code with:
   - Explicit types: `let assert Ok(process.Success(_, _, 0)) =`
   - No placeholders - all values concrete
   - Step-by-step comments: `// SETUP:`, `// EXECUTE:`, `// ASSERT:`

2. **CUE Contract** - Machine-readable validation:
   - Input constraints: `temp_dir: string & =~"^/tmp/tcr-test-[0-9]+$"`
   - Output assertions: `jj_log_output: string & =~"factory: test-stage passed"`
   - Type safety: `outcome_commits_made: 1`

3. **Acceptance Criteria** - Concrete observable outcomes:
   - "jj log shows commit" (run command, check output)
   - "poison.txt deleted" (file exists = False)
   - "commits_made=1" (exact numeric equality)

## How Cheap LLM Should Implement

### Step 1: Read Exact Signature
```gleam
// FROM BEAD tcr-001
pub fn tcr_jj_commit_verification_test() {
  let ts = get_timestamp_ms()
  let tmp = "/tmp/tcr-test-" <> ts
  // ... rest of signature
}
```

**DO**: Copy entire function body
**DON'T**: Rewrite, refactor, or "improve"

### Step 2: Verify Against Contract
```cue
// FROM .beads/contracts/tcr-001.cue
assertions: {
  jj_log_output: string & =~"factory: test-stage passed"
  outcome_commits_made: 1
}
```

**DO**: Match exact regex patterns
**DON'T**: Guess string formats

### Step 3: Run Acceptance Check
```bash
gleam test --target erlang
# MUST see: jj log shows commit with "factory: test-stage passed"
# MUST see: commits_made == 1
```

**DO**: Verify every acceptance criterion
**DON'T**: Assume passing compile = correct

## Type Signatures Eliminate Ambiguity

### BAD (Original):
```
"Create temp jj repo, run tcr.run_with_tcr, verify commit"
```
→ Cheap LLM guesses: What's temp? What's verify? What's commit format?

### GOOD (With Contract):
```gleam
let assert Ok(process.Success(_, _, 0)) = 
  process.run_command("jj", ["git", "init", tmp], "")
```
→ Cheap LLM copies: Exact command, exact args, exact exit code check

## Property Test Contracts

### tcr-003 (Property Test):
```gleam
pub fn prop_tcr_leaves_clean_state__test() {
  use pass <- qcheck.given(qcheck.bool())  // INPUT: randomized bool
  
  let _ = tcr.run_with_tcr(stage, tmp, execute_fn)
  
  // PROPERTY: ∀ pass ∈ Bool, jj_status(after_tcr) = "No changes"
  assert string.contains(status, "No changes")
}
```

**Contract**: `Bool → clean_state`
**Cheap LLM Task**: Copy qcheck.bool() generator, assert string match

## No Placeholders Policy

### BAD:
```gleam
let tmp = "/tmp/test-dir"  // TODO: make unique
```

### GOOD:
```gleam
let ts = get_timestamp_ms()
let tmp = "/tmp/tcr-test-" <> ts  // Unique via timestamp
```

## Regex Constraints Prevent Drift

### CUE Contract:
```cue
temp_dir: string & =~"^/tmp/tcr-test-[0-9]+$"
```

**Cheap LLM**: Must generate `/tmp/tcr-test-1234567890`
**Cannot**: Use `/tmp/test`, `/tmp/foo`, or any non-matching path

## Function Signature Truth Table

| Bead | Input Types | Output Type | Side Effects |
|------|-------------|-------------|--------------|
| tcr-001 | `(Stage, String, fn() -> Result(Nil, String))` | `Result(TCROutcome, String)` | Creates jj repo, writes file, cleans up |
| tcr-002 | `(Stage, String, fn() -> Result(Nil, String))` | `Result(TCROutcome, String)` | Creates jj repo, writes poison, deletes poison |
| prop-008 | `Int` (ASCII code) | `Result(Slug, String)` | None (pure) |
| prop-010 | `(Int, Int)` (retries, attempt) | `Bool` | None (pure) |

## Cheap LLM Implementation Checklist

- [ ] Copy exact function signature from bead spec
- [ ] Verify all `let assert` patterns match types
- [ ] Check regex patterns in CUE contract
- [ ] Run `gleam format` (no changes = correct style)
- [ ] Run `gleam test` (all green = correct behavior)
- [ ] Verify acceptance criteria: shell out, check files, parse output

## What Makes This Work

1. **No Natural Language**: "Create a test" → ambiguous
2. **Only Code**: `let assert Ok(process.Success(_, _, 0))` → unambiguous
3. **CUE Validation**: Machine checks regex, types, structure
4. **Acceptance = Shell Commands**: Observable, verifiable, concrete

Cheap LLM copies code → compiles → tests pass → beads complete.
No creativity required. No ambiguity possible.
