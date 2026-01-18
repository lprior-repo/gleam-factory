# Quick Fix Reference - 41 Test Failures

## TL;DR

4 independent issues causing 41 test failures. Fix in order:

1. **otp_actor FFI mismatch** → 29 tests (CaseClause errors)
2. **Float conversion bug** → 7 tests (Badarith errors)
3. **Signal bus not broadcasting** → 1 test (timeout)
4. **Approval gate logic** → 1 test (assertion)

## Issue 1: otp_actor FFI (29 Tests - CaseClause(Ok))

**Problem:** Mixed actor module imports cause type confusion

Files importing `otp_actor as actor` (FFI wrapper):
- src/golden_master.gleam (line 5)
- src/factory_dispatcher.gleam
- src/merge_queue.gleam

Files importing `gleam/otp/actor` (stdlib):
- src/heartbeat.gleam
- src/resource_governor.gleam
- src/workspace_manager.gleam
- src/factory_loop.gleam

**Fix:** Rename `src/gleam/otp/actor.gleam` → `src/custom_actor.gleam`

```bash
# Step 1: Rename file
mv src/gleam/otp/actor.gleam src/custom_actor.gleam

# Step 2: Update imports in 3 files
# src/golden_master.gleam line 5:
import custom_actor as actor

# src/factory_dispatcher.gleam:
import custom_actor as actor

# src/merge_queue.gleam:
import custom_actor as actor

# Step 3: Delete FFI wrapper (no longer needed)
rm src/otp_actor.gleam

# Step 4: Test
gleam test
```

Expected: 29 CaseClause errors gone

---

## Issue 2: Float Conversion (7 Tests - Badarith)

**Problem:** Wrong FFI binding for int_to_float

File: `test/performance_test.gleam`

**Current Code (BROKEN):**
```gleam
@external(erlang, "erlang", "float")
fn int_to_float(i: Int) -> Float

// Later used in:
let throughput = case duration_ms {
  0 -> 0.0
  d -> 1000.0 /. int_to_float(d)  // ← ERROR: Badarith
}
```

**Fix (Option A - Preferred):**
```gleam
import gleam/int

// Remove the broken external binding

fn benchmark_stage(stage: domain.Stage) -> BenchmarkResult {
  // ... setup code ...

  let throughput = case duration_ms {
    0 -> 0.0
    d -> 1000.0 /. int.to_float(d)  // ← Use stdlib function
  }

  // ... rest of function
}
```

**Fix (Option B - If int.to_float not available):**
```gleam
// Replace lines 37-38 with proper conversion
@external(erlang, "erlang", "integer_to_float")
fn int_to_float(i: Int) -> Float

// OR use multiplication trick:
let throughput = case duration_ms {
  0 -> 0.0
  d -> 1000.0 /. (int.to_float(d) +. 0.0)  // Force float
}
```

Expected: 7 Badarith errors gone

---

## Issue 3: Heartbeat Signal (1 Test - Timeout)

**File:** `test/heartbeat_test.gleam` line 70

**Problem:** No TestPassing signal broadcast on Red→Green transition

**Test Expected Flow:**
1. Heartbeat status: Red (test_cmd fails)
2. Tick 1: Red status confirmed
3. Create new heartbeat with passing test_cmd
4. Tick 2: Status Red→Green transition
5. Signal broadcast: TestPassing ← **NOT HAPPENING**

**Fix Location:** `src/heartbeat.gleam`

Find the status update logic and add signal broadcast:

```gleam
// In status update handler, after changing status to Green:
case old_status, new_status {
  Red, Green -> signal_bus.broadcast(bus, signal_bus.TestPassing)
  Green, Red -> signal_bus.broadcast(bus, signal_bus.TestFailure)
  _, _ -> Nil
}
```

Expected: Signal timeout test passes

---

## Issue 4: Approval Gate (1 Test - Assertion)

**File:** `test/integration_test.gleam`

**Problem:** Phase transitions stuck at Reviewing→Pushing

**Error:** Expected "Pushing" but got "Reviewing"

**Related Code:**
- `src/factory_loop.gleam` - phase state machine
- Commit c54ab1e - approval gate implementation

**Fix:** Review factory_loop.gleam for:

1. Check approval gate logic allows Reviewing→Pushing
2. Verify gate opens without external trigger
3. Ensure no blocking condition on transition
4. Look for recent changes that might block transition

```gleam
// In factory_loop phase handler:
Reviewing -> {
  // Gate should open automatically or with test signal
  // Allow transition to Pushing
  // Current code might have blocking condition
}
```

Add logging to debug:
```gleam
logging.log(
  logging.Debug,
  "Phase transition: " <> old_phase <> " → " <> new_phase,
  dict.new()
)
```

Expected: Phase transitions work correctly

---

## Testing Strategy

### Test Each Fix Independently

```bash
# After Fix 1:
gleam test -- factory_supervisor_test 2>&1 | tail -3

# After Fix 2:
gleam test -- performance_test 2>&1 | tail -3

# After Fix 3:
gleam test -- heartbeat_test 2>&1 | tail -3

# After Fix 4:
gleam test -- integration_test 2>&1 | tail -3

# Final:
gleam test 2>&1 | tail -10
```

### Expected Progress

| Phase | Pass | Fail | Status |
|-------|------|------|--------|
| Start | 0 | 41 | All failing |
| After Fix 1 | 29 | 12 | CaseClause gone |
| After Fix 2 | 36 | 5 | Badarith gone |
| After Fix 3 | 37 | 4 | Timeout gone |
| After Fix 4 | 38 | 3 | Assertion gone |

The 3 remaining failures (graceful_shutdown_test) need further investigation.

---

## Files to Modify

- [ ] `src/gleam/otp/actor.gleam` → rename to `src/custom_actor.gleam`
- [ ] `src/golden_master.gleam` - update import
- [ ] `src/factory_dispatcher.gleam` - update import
- [ ] `src/merge_queue.gleam` - update import
- [ ] Delete `src/otp_actor.gleam`
- [ ] `test/performance_test.gleam` - fix float conversion
- [ ] `src/heartbeat.gleam` - add signal broadcast
- [ ] `src/factory_loop.gleam` - verify approval gate

---

## Estimated Time

- Fix 1: 30-45 min
- Fix 2: 15-20 min
- Fix 3: 20-30 min
- Fix 4: 20-30 min
- Test & validate: 15-20 min

**Total: 2-2.5 hours**

---

## Commands for Implementation

```bash
# Fix 1: Rename and update imports
cd /home/lewis/src/factory-gleam
mv src/gleam/otp/actor.gleam src/custom_actor.gleam
# Then manually update 3 imports

# Verify Fix 1
gleam test

# Fix 2: Edit test/performance_test.gleam
# - Remove lines 37-38
# - Update line 54 to use int.to_float(d)

# Verify Fix 2
gleam test -- performance_test

# Fix 3: Edit src/heartbeat.gleam
# Add signal_bus.broadcast calls

# Verify Fix 3
gleam test -- heartbeat_test

# Fix 4: Review src/factory_loop.gleam
# Debug and fix approval gate

# Verify Fix 4
gleam test -- integration_test

# Final verification
gleam test
```

---
