# Property-Based Test Gaps (Adversarial)

## EAR-008: Slug Validation Edge Cases
**Event**: Validate slug with boundary chars (ASCII 44-46, 91-96)
**Action**: domain.validate_slug(char)
**Result**: Only 45 (hyphen) and 95 (underscore) pass

**Inversion**: What if comma (44), period (46), backtick (96) slip through? Current property tests check uppercase/unicode but skip ASCII boundaries. Result: Invalid slugs in persistence, filesystem corruption, beads parser breaks.

**Bead**: test/qcheck/property_slug_boundaries_test.gleam
```gleam
pub fn prop_slug_rejects_ascii_boundaries__test()
  // Test ASCII 44-47 (,-./ where only - is valid)
  // Test ASCII 91-96 ([\]^_` where only _ is valid)
  use code <- qcheck.given(qcheck.one_of([
    qcheck.bounded_int(44, 47),
    qcheck.bounded_int(91, 96),
  ]))
  let char = codepoint_to_string(code)
  assert case code {
    45 | 95 -> is_ok(validate_slug(char))
    _ -> is_error(validate_slug(char))
  }
```

---

## EAR-009: Pipeline Order Stability
**Event**: Load standard_pipeline() 1000 times
**Action**: Extract stage names list
**Result**: Order identical every time (implement → accept)

**Inversion**: What if list ordering nondeterministic? What if dict randomization affects pipeline? Current code uses plain list but no property test verifies order. Result: filter_stages breaks, stages run out of order, TCR commits chaos.

**Bead**: test/qcheck/property_pipeline_order_test.gleam
```gleam
pub fn prop_pipeline_order_stable__test()
  use _iteration <- qcheck.given(qcheck.bounded_int(1, 100))
  let p1 = domain.standard_pipeline()
  let p2 = domain.standard_pipeline()
  let names1 = list.map(p1, fn(s) { s.name })
  let names2 = list.map(p2, fn(s) { s.name })
  assert names1 == names2
  assert list.first(names1) == Ok("implement")
  assert list.last(names1) == Ok("accept")
```

---

## EAR-010: Stage Retry Exhaustion
**Event**: Stage fails, retry count decrements
**Action**: Call tcr.should_retry(stage, attempt)
**Result**: Returns False when attempt >= stage.retries

**Inversion**: What if off-by-one? What if retry count wraps negative? Current tests check simple cases, no property test with randomized retry counts. Result: Infinite retry loop, factory hangs, CI times out.

**Bead**: test/qcheck/property_tcr_retry_test.gleam
```gleam
pub fn prop_retry_exhausts_correctly__test()
  use retries <- qcheck.given(qcheck.bounded_int(1, 10))
  use attempt <- qcheck.given(qcheck.bounded_int(1, 20))
  let stage = domain.Stage("test", "gate", retries, True)
  let should = tcr.should_retry(stage, attempt)
  assert should == (attempt < retries)
```

---

## EAR-011: Language Detection Precedence
**Event**: Multiple manifest files exist (gleam.toml + Cargo.toml)
**Action**: detect_language_from_files(True, False, True, False)
**Result**: Always returns Gleam (priority order enforced)

**Inversion**: What if pattern match order changes? What if Rust priority flipped? Current property test checks Gleam wins but doesn't verify full precedence chain. Result: Wrong language detected, wrong stages run, pipeline chaos.

**Bead**: test/qcheck/property_language_precedence_test.gleam
```gleam
pub fn prop_language_precedence_gleam_go_rust_python__test()
  // Gleam > Go > Rust > Python
  assert detect(True, True, True, True) == Ok(Gleam)
  assert detect(False, True, True, True) == Ok(Go)
  assert detect(False, False, True, True) == Ok(Rust)
  assert detect(False, False, False, True) == Ok(Python)
```

---

## EAR-012: TaskStatus Transition Validity
**Event**: Update task status from A to B
**Action**: Validate state machine allows A → B
**Result**: Only valid transitions succeed

**Inversion**: What if Created → Integrated (skip InProgress)? What if Completed → InProgress (backwards)? Current code has no transition guard. Result: Invalid state machines, persistence corruption, factory logic broken.

**Bead**: test/qcheck/property_status_transitions_test.gleam
```gleam
pub fn prop_status_transitions_monotonic__test()
  // Created → InProgress → PassedPipeline → Integrated
  // FailedPipeline is terminal
  use from <- qcheck.given(gen_status())
  use to <- qcheck.given(gen_status())
  let valid = is_valid_transition(from, to)
  // Assert: no backwards transitions
  // Assert: no skipping InProgress
  assert valid
```
