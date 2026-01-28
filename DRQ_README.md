# DRQ: Dynamic Red Queen Test Evolution System

## What is DRQ?

DRQ (Dynamic Red Queen) is an evolutionary test system inspired by the Red Queen Hypothesis from evolutionary biology: *"it takes all the running you can do, to keep in the same place."*

In DRQ, tests and code engage in a co-evolutionary arms race within an arena:

- **Tests evolve** to find new edge cases and weaknesses
- **Code evolves** to pass increasingly challenging tests
- **Champions emerge** - implementations that survive the test arena
- **The Red Queen runs** - continuous pressure prevents stagnation

### Core Philosophy

1. **Test-Driven Evolution**: Tests don't just verify; they actively drive improvement
2. **Survival of the Fittest**: Only code that passes all arena tests survives
3. **Continuous Pressure**: New tests constantly challenge existing champions
4. **Structured Competition**: Arena-based tournaments with clear victory conditions

### Integration with TDD15

DRQ extends the TDD15 workflow (see `/home/lewis/src/gleam-factory/src/tdd15/`):

- **TDD15 Phases 0-3**: Research, planning, and verification setup
- **DRQ Arena**: Phases 4-6 (Red-Green-Refactor) become an evolutionary battleground
- **Phase 7+**: Martin Fowler refactoring and verification on evolved codebases

## How DRQ Works

### Arena Structure

```
                    DRQ ARENA
        +-------------------------+
        |                         |
        |   Test Bank (Mutator)   |
        |                         |
        +-----------+-------------+
                    |
                    | injects tests
                    v
        +-------------------------+
        |                         |
        |    Challenger Zone      |
        |                         |
        |  +----------+  +------+ |
        |  |Impl A    |  |Impl B| |
        |  |(champion)|  |(new) | |
        |  +----------+  +------+ |
        +-------------------------+
                    |
                    | survivors
                    v
        +-------------------------+
        |                         |
        |   Champion Pool         |
        |                         |
        +-------------------------+
```

### Core Components

**Test Bank**
- Stores all generated tests with metadata
- Categories: boundary, edge, property, invariant, security
- Each test tracks: kill_count, survivors, generation_added

**Arena**
- Isolated execution environment
- Runs challenger implementations against test bank
- Tracks performance metrics
- Determines champions by survival rate

**Champions**
- Implementations that pass 100% of tests
- Stored with lineage (parent, mutation, generation)
- Can be challenged by new implementations

**Rounds**
- One complete evolutionary cycle
- Consists of: mutate tests -> run arena -> select champions -> repeat

### Round Lifecycle

```
Round N:
  1. MUTATE: Test bank generates N new tests
  2. CHALLENGE: All implementations run against full test suite
  3. SELECT: Implementations with 100% pass rate become champions
  4. RECORD: Save lineage, metrics, and test metadata
  5. PRUNE: Remove tests that kill nothing (redundancy elimination)
```

### Victory Conditions

- **Absolute Champion**: Passes all tests for 3 consecutive rounds
- **Relative Champion**: Highest pass rate when no one reaches 100%
- **Consensus Champion**: Selected by multiple independent arena runs

## Quick Start Guide

### Installation

```bash
# Clone gleam-factory
cd gleam-factory
gleam build

# Initialize DRQ in your project
drq init --language gleam
```

### Basic Workflow

```bash
# Initialize DRQ arena
drq init

# Create first test and implementation
drq run --phase red
# Prompts: Describe behavior to test

# Run arena round
drq run --round

# Show current champions
drq show --champions

# View test bank statistics
drq show --test-bank
```

### Configuration

Create `drq.toml` in your project root:

```toml
[arena]
language = "gleam"
test_dir = "test/"
src_dir = "src/"
max_rounds = 100
champion_stability_rounds = 3

[tests]
target_count = 50
categories = ["boundary", "edge", "property", "invariant"]

[mutation]
rate = 0.1
strategies = ["input", "control", "path"]

[stopping]
conditions = ["champion_stable", "max_rounds", "test_saturation"]
saturation_threshold = 0.95  # 95% of tests pass for 3 rounds
```

## Configuration Options

### Arena Settings

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `language` | string | auto-detected | gleam, rust, go, python |
| `test_dir` | path | test/ | Test file location |
| `src_dir` | path | src/ | Source file location |
| `max_rounds` | int | 100 | Maximum arena rounds |
| `champion_stability_rounds` | int | 3 | Rounds to hold championship |
| `parallel_workers` | int | 4 | Concurrent test runners |

### Test Generation

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `target_count` | int | 50 | Target test bank size |
| `categories` | list | all | Test categories to generate |
| `property_tests_per_type` | int | 10 | Property test density |
| `boundary_tests_per_fn` | int | 5 | Boundary test density |
| `edge_case_depth` | int | 3 | Edge case exploration depth |

### Mutation Settings

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rate` | float | 0.1 | Mutation probability |
| `strategies` | list | all | input, control, path, opcode |
| `max_mutations_per_round` | int | 5 | Cap on new test mutations |

### Stopping Conditions

| Condition | Description |
|-----------|-------------|
| `champion_stable` | Same champion wins N rounds |
| `max_rounds` | Hit configured maximum |
| `test_saturation` | No new tests kill any impl |
| `time_limit` | Exceeds time budget |
| `coverage_plateau` | Coverage unchanged for N rounds |

## Architecture Overview

### Module Structure

```
drq/
├── arena.gleam          # Arena orchestration
├── test_bank.gleam      # Test storage and retrieval
├── champion.gleam       # Champion selection
├── mutator.gleam        # Test generation
├── execution.gleam      # Isolated test running
├── metrics.gleam        # Performance tracking
└── lineage.gleam        # Evolution history
```

### Data Flow

```
User Input
    |
    v
[Requirements] --> [Phase 0: Triage]
                          |
                          v
                    [Phase 1: Research]
                          |
                          v
                    [Phase 2: Plan]
                          |
                          v
                    [DRQ Arena Init]
                          |
          +---------------+---------------+
          |               |               |
          v               v               v
    [Test Bank]    [Challengers]    [Metrics]
          |               |               |
          +-------+-------+               |
                  |                       |
                  v                       v
            [Execution] <-----------[History]
                  |
                  v
            [Selection]
                  |
        +-----------+-----------+
        |                       |
        v                       v
   [Champions]            [Next Round]
        |
        v
[Phase 7+: MF Refactoring]
```

### Type System

```gleam
// Core arena types
pub type Test {
  Test(
    id: String,
    category: TestCategory,
    code: String,
    kill_count: Int,
    generation: Int,
  )
}

pub type Champion {
  Champion(
    implementation: Implementation,
    lineage: Lineage,
    survival_rounds: Int,
    pass_rate: Float,
  )
}

pub type ArenaResult {
  ArenaResult(
    round: Int,
    survivors: List(Champion),
    new_tests: List(Test),
    metrics: ArenaMetrics,
  )
}

pub type Lineage {
  Lineage(
    parent: Option(String),
    mutations: List(String),
    generation: Int,
  )
}
```

## Integration with TDD15

### Phase Mapping

| TDD15 Phase | DRQ Role | Output |
|-------------|----------|--------|
| 0: Triage | Complexity assessment | Route selection |
| 1: Research | Domain analysis | Test categories |
| 2: Plan | Arena configuration | drq.toml |
| 3: Verify | Contract validation | Initial test bank |
| 4: Red | Initial test creation | Test set v0 |
| 5: Green | First implementation | Challenger v0 |
| 6: Refactor | Internal improvements | Champion candidate |
| 7: MF #1 | Deep refactoring | Stable champion |
| 8+: Verify | Final arena validation | Certified champion |

### State Integration

DRQ uses the TDD15 state system (see `/home/lewis/src/gleam-factory/src/tdd15/state.gleam`):

```gleam
// DRQ extends Progress with arena-specific state
pub type ArenaProgress {
  ArenaProgress(
    bead_id: String,
    tdd_progress: Progress,  // From tdd15/state
    arena_round: Int,
    test_bank_size: Int,
    champion_id: Option(String),
  )
}
```

### Cache Structure

```
.tdd15-cache/<bead_id>/
├── progress.json          # TDD15 progress
├── bead.json              # Original requirements
├── arena.json             # DRQ arena state
├── test_bank.json         # All generated tests
├── champions.json         # Champion lineage
└── rounds/
    ├── round_001.json
    ├── round_002.json
    └── ...
```

## Troubleshooting Guide

### Arena Won't Start

**Problem**: `drq run --round` fails immediately

**Solutions**:
1. Check `drq.toml` syntax: `drq validate --config`
2. Ensure test directory exists: `mkdir -p test/`
3. Verify language detection: `drq show --language`
4. Check dependencies: `gleam deps download`

### No Tests Generated

**Problem**: Test bank remains empty

**Solutions**:
1. Verify source files have types: `drq analyze --types`
2. Check test categories are enabled in config
3. Increase `target_count` in `drq.toml`
4. Run `drq diagnose --test-generation` for details

### All Implementations Die

**Problem**: 0% survival rate every round

**Solutions**:
1. Tests may be over-constrained - review `test_bank.json`
2. Check for contradictory invariants
3. Run `drq analyze --conflicts` to find conflicting tests
4. Temporarily disable strict categories: `categories = ["boundary"]`

### Champion Oscillation

**Problem**: Different champion each round (no stability)

**Solutions**:
1. Tests may be too random - check `mutation.rate`
2. Increase `champion_stability_rounds` threshold
3. Review test quality: `drq show --weak-tests`
4. Consider domain splitting if requirements are broad

### Performance Issues

**Problem**: Rounds take too long

**Solutions**:
1. Reduce `target_count` in config
2. Increase `parallel_workers`
3. Enable test caching: `cache_tests = true`
4. Use incremental mode: `drq run --incremental`

### Common Exit Codes

| Code | Meaning | Action |
|------|---------|--------|
| 0 | Success (champion found) | Report and continue |
| 1 | Configuration error | Fix drq.toml |
| 2 | No tests generated | Check source analysis |
| 3 | All implementations failed | Review test constraints |
| 4 | Round limit reached | Increase or declare winner |
| 5 | Timeout | Optimize or parallelize |

## Examples for Gleam Projects

### Example 1: Simple Function Evolution

```gleam
// src/calculator.gleam
pub fn add(a: Int, b: Int) -> Int {
  a + b
}
```

```bash
# Initialize DRQ for calculator
drq init --function calculator.add

# Run arena
drq run --round --config drq-calculator.toml

# Output after 5 rounds:
# Round 5: 12 tests, 1 champion
#   Champion: add_v3 (pass_rate: 100%)
#   Lineage: add_v1 -> add_v2 (fix overflow) -> add_v3
#   Tests killed: 2 (overflow cases)
```

### Example 2: Data Structure Validation

```gleam
// src/stack.gleam
pub type Stack(t) {
  Stack(elements: List(t), max_size: Int)
}

pub fn push(stack: Stack(t), item: t) -> Result(Stack(t), String) {
  // Implementation...
}
```

```bash
# DRQ discovers edge cases
drq run --round --categories boundary,property,invariant

# Generated tests include:
# - push to full stack
# - push with size overflow
# - property: size(push(s, x)) == size(s) + 1
# - invariant: stack never exceeds max_size
```

### Example 3: State Machine Evolution

```gleam
// src/counter.gleam
pub type Counter {
  Counter(value: Int, max: Int)
}

pub fn increment(c: Counter) -> Counter {
  // Must handle saturation at max
}
```

```bash
# DRQ validates state transitions
drq run --round --test-state-machine

# Automatically tests:
# - All state transitions
# - Boundary conditions (0, max, max+1)
# - Invariants (value always in [0, max])
# - Property preservation (idempotency, commutativity)
```

### Example 4: API Contract Testing

```gleam
// src/user_service.gleam
pub fn create_user(email: String, age: Int) -> Result(User, List(Error)) {
  // Complex validation
}
```

```bash
# DRQ validates contract
drq run --round --contract-mode --strict

# Tests generated for:
# - Invalid email formats
# - Age boundaries (<0, >150)
# - Error reporting completeness
# - Success case purity
```

## Advanced Usage

### Custom Test Generators

```gleam
// test/generators/custom.gleam
import drq/generator

pub fn date_generator() -> generator.Generator(Date) {
  generator.int_range(0, 9999)
  |> generator.map(fn(y) { Date.new(y, 1, 1) })
}

// Register in drq.toml
[custom_generators]
date = "test/generators/custom.date_generator"
```

### Arena Persistence

```bash
# Save arena state
drq save --round 10 --output arena_state.json

# Resume from saved state
drq resume --input arena_state.json
```

### Multi-Language Projects

```toml
# drq.toml for polyglot project
[[arenas]]
name = "core"
language = "rust"
path = "core/"

[[arenas]]
name = "bindings"
language = "gleam"
path = "bindings/gleam/"
depends_on = ["core"]

[[arenas]]
name = "cli"
language = "go"
path = "cmd/cli/"
depends_on = ["core", "bindings"]
```

### CI/CD Integration

```yaml
# .github/workflows/drq.yml
name: DRQ Arena
on: [push, pull_request]

steps:
  - uses: actions/checkout@v4
  - uses: gleam-lang/setup-gleam@v1
  - name: Run DRQ Arena
    run: |
      drq init
      drq run --round --max-rounds 10
      drq export --format junit --output drq-results.xml
```

## References

- **TDD15**: `/home/lewis/src/gleam-factory/src/tdd15/`
- **Factory Pipeline**: `/home/lewis/src/gleam-factory/README.md`
- **Contract Validation**: `/home/lewis/src/gleam-factory/.factory/cue/contract.cue`

## License

Apache-2.0 (same as gleam-factory)
