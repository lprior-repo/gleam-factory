# Gleam Best Practices Research

Researched: 2026-01-18
Domain: Gleam language patterns
Confidence: HIGH

## Summary

Gleam enforces type-safe, functional patterns through exhaustive pattern matching, Result-based error handling, and use-syntax for callback unwrapping. Property-based testing via gleam_qcheck provides shrinking and random test generation. Core anti-patterns: stringly-typed code, imperative style, non-exhaustive matches, ignoring Result chains.

Primary recommendation: Use Result/try chains, exhaustive patterns, opaque types, small composable functions with pipes.

## Error Handling Patterns

### Result Over Option for Failures

Result is idiomatic for operations that may fail. Option is for absent values only.

Pattern:
```gleam
// GOOD - use Result for operations
pub fn parse_config(path: String) -> Result(Config, String) {
  use content <- result.try(read_file(path))
  use parsed <- result.try(json.decode(content))
  Ok(Config(..))
}

// BAD - Option loses error context
pub fn parse_config(path: String) -> Option(Config)
```

### Use Syntax for Result Chains

Replaces nested callback hell with flat code.

```gleam
// GOOD - use syntax
pub fn process() -> Result(Output, Error) {
  use config <- result.try(load_config())
  use data <- result.try(fetch_data(config))
  use validated <- result.try(validate(data))
  Ok(transform(validated))
}

// BAD - nested map/try
pub fn process() -> Result(Output, Error) {
  result.try(load_config(), fn(config) {
    result.try(fetch_data(config), fn(data) {
      result.map(validate(data), transform)
    })
  })
}
```

### Avoid Unwrapping Without Fallback

Use result.unwrap only with meaningful defaults. Prefer pattern matching for distinct error cases.

```gleam
// GOOD - explicit handling
case parse_port(input) {
  Ok(port) -> start_server(port)
  Error(_) -> start_server(default_port)
}

// BAD - silent fallback hides issues
let port = result.unwrap(parse_port(input), default_port)
```

## Pattern Matching

### Exhaustiveness is Mandatory

Compiler enforces all cases handled. Use _ for catch-all only after specific patterns.

```gleam
// GOOD - exhaustive with specific cases first
case status {
  Running -> "active"
  Paused -> "suspended"
  Complete -> "done"
  Failed -> "error"
}

// BAD - compiler error if case missing
case status {
  Running -> "active"
  _ -> "other"  // hides Paused, Complete, Failed
}
```

### Pattern Order Matters

Top-to-bottom evaluation. Specific patterns before wildcards.

```gleam
// GOOD
case list {
  [] -> "empty"
  [x] -> "single: " <> x
  [x, y] -> "pair: " <> x <> ", " <> y
  _ -> "many"
}

// BAD - wildcard catches all
case list {
  _ -> "many"
  [] -> "empty"  // never reached
}
```

### Alternative Patterns with |

Combine cases with same logic.

```gleam
case response.status {
  200 | 201 | 204 -> Ok(Nil)
  404 | 410 -> Error(NotFound)
  _ -> Error(UnknownStatus)
}
```

### Guards Limited to Simple Conditions

No function calls, case expressions, or blocks in guards.

```gleam
// GOOD
case value {
  n if n > 0 -> "positive"
  n if n < 0 -> "negative"
  _ -> "zero"
}

// BAD - function call in guard fails to compile
case value {
  n if is_valid(n) -> "ok"  // compiler error
  _ -> "invalid"
}
```

## Type Safety Patterns

### Opaque Types for Validation

Enforce invariants at construction. types.gleam demonstrates with GitHash (40 hex chars).

```gleam
// types.gleam pattern
pub opaque type GitHash {
  GitHash(hash: String)
}

pub fn git_hash_parse(input: String) -> Result(GitHash, String) {
  case string.length(trimmed), is_valid_hex(trimmed) {
    40, True -> Ok(GitHash(trimmed))
    40, False -> Error("must be lowercase hex")
    len, _ -> Error("expected 40 chars, got " <> int.to_string(len))
  }
}
```

### Single Unwrap Function per Opaque Type

```gleam
pub fn git_hash_to_string(hash: GitHash) -> String {
  hash.hash
}
```

### Type Annotations on Public Functions

Required for clarity and intentional design.

```gleam
// GOOD
pub fn transform(input: String, rules: List(Rule)) -> Result(Output, Error)

// BAD - inferred but unclear
pub fn transform(input, rules)
```

## Function Design

### Accumulators Private, Wrapper Public

Hide implementation details. utils.gleam shows run_pipeline wrapping list.try_map.

```gleam
// Public wrapper
pub fn run_pipeline(
  name: String,
  commands: List(#(String, String, List(String), String)),
) -> Result(List(String), String) {
  io.println("▶ " <> name)
  commands |> list.try_map(run_single_command) |> handle_result
}

// Internal recursion (if needed)
fn run_pipeline_acc(cmds, acc) -> Result(List(String), String) {
  // implementation
}
```

### Pipes for Linear Transformations

Use |> for data flow. Avoid deep nesting.

```gleam
// GOOD
input
|> string.trim
|> string.split(",")
|> list.filter(non_empty)
|> list.map(parse_field)

// BAD
list.map(list.filter(string.split(string.trim(input), ","), non_empty), parse_field)
```

### Small Functions (under 30 lines)

CLAUDE.md enforces fn<30lines. factory.gleam run_loop_cycle is 48 lines - refactor candidate.

### Avoid Magic Numbers

Use named constants or config.

```gleam
// types.gleam GOOD example
const timeout_ms = 5000

// BAD
process.receive(reply_subj, 5000)
```

## Property-Based Testing

### gleam_qcheck Integration with gleeunit

Use qcheck.given with gleeunit assertions.

```gleam
import qcheck
import gleeunit/should

pub fn addition_commutativity__test() {
  use a <- qcheck.given(qcheck.small_non_negative_int())
  use b <- qcheck.given(qcheck.small_non_negative_int())
  should.equal(a + b, b + a)
}
```

### Generators for Custom Types

```gleam
fn point_generator() {
  use x, y <- qcheck.map2(qcheck.uniform_int(), qcheck.uniform_int())
  Point(x, y)
}

pub fn point_serialization__test() {
  use point <- qcheck.given(point_generator())
  let json = encode_point(point)
  let decoded = decode_point(json)
  should.equal(Ok(point), decoded)
}
```

### Applicative Style for Better Shrinking

Independent fields use apply over sequential use.

```gleam
qcheck.return(fn(x, y) { Point(x, y) })
  |> qcheck.apply(qcheck.bounded_int(-100, 100))
  |> qcheck.apply(qcheck.bounded_int(-100, 100))
```

### Shrinking Provides Minimal Failing Case

Output shows orig (original), shrnk (simplified), steps (iterations).

## Anti-Patterns to Avoid

### Stringly-Typed Code

CLAUDE.md explicitly bans. Use custom types over strings.

```gleam
// BAD
pub type Status = String  // could be "running", "paused", or "runnign" (typo)

// GOOD
pub type Status {
  Running
  Paused
  Complete
}
```

### Non-Exhaustive Matching

Compiler prevents, but using _ too early is anti-pattern.

```gleam
// BAD - hides cases
case msg {
  Request(_) -> handle_request()
  _ -> Nil  // Release silently ignored
}

// GOOD - explicit
case msg {
  Request(reply) -> handle_request(reply)
  Release(ticket, reply) -> handle_release(ticket, reply)
}
```

### Imperative Style

Use functional composition.

```gleam
// BAD - imperative
let mut result = []
for item in items {
  result.append(transform(item))
}
result

// GOOD - functional
items |> list.map(transform)
```

### Multiple Function Heads

Not supported in Gleam. Use case in body.

```gleam
// BAD - doesn't compile
pub fn process([]) { [] }
pub fn process([x, ..xs]) { [transform(x), ..process(xs)] }

// GOOD
pub fn process(list) {
  case list {
    [] -> []
    [x, ..xs] -> [transform(x), ..process(xs)]
  }
}
```

### Ignoring Result in Chains

Every Result must be handled or propagated.

```gleam
// BAD - ignores failure
let _ = validate(input)
proceed()

// GOOD - propagates
use validated <- result.try(validate(input))
proceed(validated)
```

### External Functions Overused

Minimize FFI. Use sparingly.

```gleam
// factory-gleam uses @external only for erlang:phash2
@external(erlang, "erlang", "phash2")
fn hash_pid(pid: process.Pid) -> Int
```

## Current Project Analysis

### Patterns Found in Codebase

types.gleam demonstrates opaque types (GitHash, ProcessId, WorkspaceId, GpuGovernor) with validation at construction.

utils.gleam uses pipes and Result chains (run_pipeline, run_with_status).

factory.gleam shows nested case for state inspection but run_loop_cycle exceeds 30-line guideline (48 lines).

### Refactor Candidates

1. factory.gleam run_loop_cycle - extract state handling
2. types.gleam handle_request/handle_release - factor out state update logic
3. Any pattern: fn>30lines triggers CLAUDE.md ban

### Conformance to CLAUDE.md

CUPID principles applied: Compose (pipes), Unix (single-purpose fns), Pure (Result chains), Idiom (pattern match), Domain (opaque types).

Missing: fn<30lines enforcement in factory.gleam, types.gleam loops.

## Sources

### Primary (HIGH confidence)
- [Gleam Result Documentation](https://hexdocs.pm/gleam_stdlib/gleam/result.html)
- [Gleam Language Tour - Everything](https://tour.gleam.run/everything/)
- [Gleam Exhaustive Pattern Matching](https://gleam.run/news/v0.33-exhaustive-gleam/)
- [gleam_qcheck GitHub](https://github.com/mooreryan/gleam_qcheck)

### Secondary (MEDIUM confidence)
- [Error Handling in Gleam (Benjamin Peinhardt)](https://www.benjaminpeinhardt.com/error-handling-in-gleam/)
- [Gleam on Exercism - Results](https://exercism.org/tracks/gleam/concepts/results)
- [Gleam on Exercism - Case Expressions](https://exercism.org/tracks/gleam/concepts/case-expressions)

### Tertiary (LOW confidence)
- WebSearch: Gleam best practices 2026 (no dedicated guide found, extracted from docs)
- WebSearch: Gleam anti-patterns (no comprehensive list, inferred from docs/discussions)

## Metadata

Confidence breakdown:
- Error handling patterns: HIGH - official docs + Context7
- Pattern matching: HIGH - exhaustiveness algorithm paper implemented
- Opaque types: HIGH - idiomatic pattern, used throughout stdlib
- Property testing: HIGH - official gleam_qcheck docs
- Anti-patterns: MEDIUM - inferred from docs, not explicitly listed

Research date: 2026-01-18
Valid until: 2026-02-18 (30 days - stable language features)
