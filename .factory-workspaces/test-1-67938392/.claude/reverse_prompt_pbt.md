# Reverse Prompt: Property-Based Testing Deep Analysis

## Mission
Leverage existing code samples to systematically add property-based tests that expose bugs and force system evolution. Each test must be a real contract that breaks when assumptions change.

## Starting Point
Reference implementation: `test/qcheck/property_domain_test.gleam`

Real property tests that work:
- `prop_slug_rejects_uppercase__test`: Uses `qcheck.bounded_int(65, 90)` to generate 1000 A-Z chars. Exposed blacklist bug.
- `prop_slug_rejects_unicode__test`: Uses `qcheck.bounded_int(256, 500)` to generate 1000 non-ASCII chars. Exposed unicode handling bug.
- `prop_pipeline_size__test`: Verifies invariant that never changes - 10 stages always.
- `prop_filter_stages_contiguous__test`: Tests composition property - slice length must match formula.

## Required Workflow Before Adding Tests

1. **Code Pattern Analysis** - Read implementation fully:
   - `src/domain.gleam` - All functions and types
   - `test/factory_test.gleam` - Existing unit tests (these reveal spec assumptions)
   - `test/qcheck/property_domain_test.gleam` - Study test structure

2. **Deep Issue Search** - For EACH function/module you want to test:
   - Use `Grep` to find ALL usages of the function across codebase
   - Check error handling paths - what can fail?
   - Look for string comparisons - are they case-sensitive when they shouldn't be?
   - Find boundary conditions - lengths, ranges, counts
   - Search for magic numbers - these break properties
   - Look for type mismatches or coercions

3. **Invariant Extraction** - Ask these questions:
   - What MUST be true after this operation?
   - What CANNOT change no matter what input?
   - What properties would break if logic inverted?
   - What happens at boundaries (empty, max, min)?
   - What remains constant across all valid inputs?

4. **Generate Test Cases** - For each invariant found:
   - Use qcheck generators that create distributions (NOT single values)
   - `bounded_int(low, high)` - creates 1000 values across range
   - `bool()` - creates mix of True/False
   - `string_from(codepoint_generator)` - creates varied strings
   - Combine with `tuple2`, `tuple3` for multi-param functions
   - Name tests after the property: `prop_X_must_Y__test`

5. **Run Tests & Fix Bugs** - Tests are contracts:
   - If test fails, fix CODE not test
   - Shrinking output shows minimal counterexample
   - Error message reveals actual bug in implementation
   - Update unit tests if spec was wrong

## Example Pattern from Existing Code

```gleam
// PATTERN: Generate distribution of invalid inputs
pub fn prop_function_rejects_invalid_input__test() {
  use invalid_value <- qcheck.given(qcheck.bounded_int(INVALID_MIN, INVALID_MAX))
  // Convert to domain type
  let input = convert_value(invalid_value)
  // Test the invariant - function MUST reject this
  assert case domain.function(input) {
    Ok(_) -> False  // Should never succeed
    Error(_) -> True
  }
}

// PATTERN: Generate valid inputs, verify property holds
pub fn prop_function_preserves_invariant__test() {
  use valid_value <- qcheck.given(qcheck.bounded_int(VALID_MIN, VALID_MAX))
  let input = convert_value(valid_value)
  assert case domain.function(input) {
    Ok(result) -> verify_invariant(result)  // Property check
    Error(_) -> False
  }
}

// PATTERN: Generate parameters, verify composition
pub fn prop_function_composition_property__test() {
  use param1 <- qcheck.given(qcheck.bounded_int(MIN1, MAX1))
  use param2 <- qcheck.given(qcheck.bounded_int(MIN2, MAX2))
  let result1 = domain.function(param1, param2)
  let result2 = domain.function_alt(param1)
  assert verify_composition(result1, result2)  // Composition invariant
}
```

## Functions to Target Next (Priority Order)

1. **Repo Module** (`src/repo.gleam`)
   - Deep search: How are paths validated?
   - Invariants: What makes a valid repo?
   - Tests: Generate invalid paths, verify rejection

2. **File Operations**
   - Deep search: File reading/writing functions
   - Invariants: File state before/after
   - Tests: Generate filenames with special chars

3. **Error Module** (`src/errors.gleam`)
   - Deep search: All error types and matching
   - Invariants: Error messages contain required info
   - Tests: Generate error conditions, verify messages

4. **Main Pipeline** (`src/main.gleam` or orchestrator)
   - Deep search: Stage transitions
   - Invariants: No invalid state transitions
   - Tests: Generate stage sequences, verify validity

## Commands to Execute Before Each Test

```bash
# 1. Search for existing tests of function
grep -r "function_name" test/ src/

# 2. Read full implementation
cat src/module.gleam | less

# 3. Check what errors are possible
grep "Error(" src/module.gleam

# 4. Find boundary conditions
grep "if\|case\|<\|>\|==" src/module.gleam

# 5. Run existing tests
gleam test
```

## Test Quality Checklist

Each new property test must satisfy ALL:

- [ ] Uses qcheck generator that creates distributions (not constants)
- [ ] Generator creates 1000+ different values per test run
- [ ] Test name matches pattern: `prop_THING_must_PROPERTY__test`
- [ ] Test FAILS on unfixed bug (verify by temporarily breaking code)
- [ ] Test uses exhaustive pattern matching, not conditionals
- [ ] Shrinking would produce minimal counterexample if it fails
- [ ] Comment explains the invariant being tested
- [ ] All 144+ tests pass after implementation
- [ ] No magic numbers - use spec constants
- [ ] No tests that just verify determinism (useless)

## Anti-Patterns to Avoid

```gleam
// BAD - determinism test (garbage)
pub fn prop_function_deterministic__test() {
  use _n <- qcheck.given(qcheck.small_non_negative_int())
  let r1 = domain.function()
  let r2 = domain.function()
  assert r1 == r2  // Pure functions always do this, useless test
}

// BAD - testing implementation detail
pub fn prop_function_uses_algorithm__test() {
  use n <- qcheck.given(qcheck.bounded_int(1, 100))
  // DON'T test HOW it works, test WHAT it produces
  assert internal_algorithm_was_called  // Can't verify this
}

// BAD - nested use statements (causes timeouts)
pub fn prop_function_test() {
  use a <- qcheck.given(gen_a())
  use b <- qcheck.given(gen_b())
  use c <- qcheck.given(gen_c())
  // Use tuple3 instead
}

// GOOD - composition with tuple
pub fn prop_function_test() {
  use vals <- qcheck.given(qcheck.tuple3(gen_a(), gen_b(), gen_c()))
  let #(a, b, c) = vals
  // Now test invariant
}
```

## How to Proceed

1. Pick a module from target list
2. **Grep Search**: Find all functions, all error cases, all boundaries
3. **Read Full Code**: Understand what MUST be true
4. **Generate Tests**: Write 3-5 property tests that expose real issues
5. **Run gleam test**: Let tests break the code
6. **Fix Domain Code**: Make tests pass by fixing bugs
7. **Commit**: Property tests + fixes together
8. **Repeat**: Next module

## Expected Outcomes

After systematic property testing:
- Bugs in edge cases exposed and fixed
- Invalid states become impossible to represent
- Adding new features breaks relevant tests (good - they catch regressions)
- Code becomes specification - tests ARE the contract
- 200+ passing property tests covering real invariants
