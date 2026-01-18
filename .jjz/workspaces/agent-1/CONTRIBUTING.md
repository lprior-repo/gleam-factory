# Contributing to Factory

## Prerequisites

- Gleam >= 0.44.0
- Erlang/OTP >= 26
- jj (Jujutsu VCS) - latest stable

Optional for target languages:
- Go + gofmt + gosec
- Rust + cargo + clippy + tarpaulin + audit
- Python + pytest + coverage + black + mypy + bandit

## Setup

```bash
git clone https://github.com/your-org/gleam-factory.git
cd gleam-factory
gleam build
gleam test
```

## Running Tests

```bash
# All tests
gleam test

# Single test file
gleam test -- --filter=domain_test
```

## Code Style

Follow CUPID principles:

- **Composable**: Small surface area, minimal dependencies, use pipes
- **Unix**: Each function does one thing
- **Pure**: Same input = same output
- **Idiomatic**: Use `|>`, pattern matching, Result/Option types
- **Domain**: Types reflect the problem domain

Specifics:
- Types: PascalCase
- Functions: snake_case
- Max 30 lines per function
- No magic numbers
- Pattern match over conditionals
- Exhaustive Result/Option handling
- No unused code

```gleam
// Good: pipe-based, pattern matching
pub fn validate(input: String) -> Result(Slug, Error) {
  input
  |> string.trim
  |> check_length
  |> check_format
}

// Bad: imperative, nested conditionals
pub fn validate(input: String) -> Result(Slug, Error) {
  let trimmed = string.trim(input)
  if string.length(trimmed) > 50 { ... }
  else if ... { ... }
}
```

## Submitting PRs

1. Create branch from main
2. Write tests first (TDD)
3. Ensure `gleam test` passes
4. Run `gleam format`
5. Open PR with clear description

## Bead Workflow

This project uses Beads for issue tracking (.beads/ directory).

```bash
# View issues
bd list

# Pick next work item
bv --robot-next

# See full triage
bv --robot-triage

# Get parallel work plan
bv --robot-plan
```

When starting a bead:
1. Note bead ID (e.g., ISS-0042)
2. Create branch: `git checkout -b feature/ISS-0042-description`
3. Reference bead in commits
4. Update bead status when done: `bd update ISS-0042 --status done`
