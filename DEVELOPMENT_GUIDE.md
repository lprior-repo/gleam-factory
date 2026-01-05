# Factory Gleam - Development Guide

**How to extend, maintain, and develop Factory**

**Last Updated**: January 5, 2026

---

## Getting Started with Development

### Prerequisites

```bash
# Install Gleam
# See https://gleam.run for your OS

# Verify installation
gleam version

# Install Jujutsu (for workspace testing)
cargo install jj-cli

# Verify it works
jj --version
```

### Project Structure

```
factory-gleam/
├── src/
│   ├── factory.gleam        # Main orchestrator
│   ├── domain.gleam         # Type definitions
│   ├── cli.gleam            # CLI parsing
│   ├── process.gleam        # Command execution
│   ├── repo.gleam           # Repo operations
│   ├── worktree.gleam       # Workspace management
│   ├── stages.gleam         # Pipeline stages
│   ├── tcr.gleam            # TCR discipline
│   ├── persistence.gleam    # State storage
│   ├── integration.gleam    # External tools
│   └── main.gleam           # Entry stub
│
├── test/
│   ├── factory_test.gleam
│   ├── domain_test.gleam
│   ├── cli_test.gleam
│   ├── stages_test.gleam
│   └── ...
│
├── .beads/
│   ├── beads.db             # Task database
│   └── config.toml
│
├── .factory/
│   ├── contract.yaml        # Global contract
│   └── bd-*/               # Task workspaces
│
├── gleam.toml               # Project manifest
├── moon.yml                 # Moon task definitions
├── ARCHITECTURE.md
├── MODULE_REFERENCE.md
├── PIPELINE_STAGES.md
├── DOMAIN_MODEL.md
└── DEVELOPMENT_GUIDE.md     # This file
```

### Build and Test

```bash
# Build the project
gleam build

# Run all tests
gleam test

# Run specific test file
gleam test --filter factory_test

# Run specific test function
gleam test --filter "module=factory_test name=test_new_task"

# Check formatting
gleam format --check src/

# Format all code
gleam format src/ test/
```

---

## Module Development Patterns

### Adding a New Function to an Existing Module

Example: Add language autodetection to `repo.gleam`

**1. Define the function signature** in the module:

```gleam
// In repo.gleam
pub fn detect_language_from_files(
  file_paths: List(String),
) -> Result(Language, String) {
  // Implementation
}
```

**2. Implement the logic**:

```gleam
pub fn detect_language_from_files(
  file_paths: List(String),
) -> Result(Language, String) {
  // Check for language indicators
  case file_paths {
    // Look for go.mod
    [] | _ if list.any(file_paths, fn(f) { string.contains(f, "go.mod") }) ->
      Ok(Go)

    // Look for gleam.toml
    _ if list.any(file_paths, fn(f) { string.contains(f, "gleam.toml") }) ->
      Ok(Gleam)

    // Look for Cargo.toml
    _ if list.any(file_paths, fn(f) { string.contains(f, "Cargo.toml") }) ->
      Ok(Rust)

    // Look for pyproject.toml
    _ if list.any(file_paths, fn(f) { string.contains(f, "pyproject.toml") }) ->
      Ok(Python)

    _ -> Error("Could not detect language from files")
  }
}
```

**3. Write tests**:

```gleam
// In test/repo_test.gleam
import gleeunit
import gleeunit/should
import repo

pub fn test_detect_gleam_language() {
  let files = ["gleam.toml", "src/main.gleam", "test/main_test.gleam"]
  repo.detect_language_from_files(files)
  |> should.equal(Ok(domain.Gleam))
}

pub fn test_detect_go_language() {
  let files = ["go.mod", "main.go"]
  repo.detect_language_from_files(files)
  |> should.equal(Ok(domain.Go))
}

pub fn test_ambiguous_language() {
  let files = ["gleam.toml", "go.mod"]  // Both present
  repo.detect_language_from_files(files)
  |> should.be_error()
}
```

**4. Run tests**:

```bash
gleam test --filter repo_test
```

### Adding a New Pipeline Stage

Example: Add a new "security" stage

**1. Define the stage** in the contract:

```yaml
# .factory/contract.yaml
pipeline:
  - name: implement
    # ... existing
  - name: security
    command: "gleam compile --target erlang --deny-warnings"
    timeout_seconds: 60
    required_success: true
  - name: deploy
    # ... rest of pipeline
```

**2. Implement stage handler** in `stages.gleam`:

```gleam
pub fn run_stage(
  task: Task,
  workspace: Workspace,
  stage: Stage,
) -> Result(StageResult, String) {
  case stage.name {
    "implement" -> run_implement(...)
    "unit-test" -> run_unit_test(...)
    "security" -> run_security(workspace)  // New!
    "deploy" -> run_deploy(...)
    _ -> Error("Unknown stage: " <> stage.name)
  }
}

fn run_security(workspace: Workspace) -> Result(StageResult, String) {
  let cmd = "gleam compile --target erlang --deny-warnings"
  let options = ExecutionOptions(
    cwd: workspace.path,
    timeout_seconds: 60,
    capture_stdout: True,
    capture_stderr: True,
    env_vars: dict.new(),
  )

  case process.run_command("sh", ["-c", cmd], options) {
    Ok(Success(output)) -> {
      Ok(StageResult(
        stage_name: "security",
        status: "success",
        stdout: output,
        stderr: "",
        duration_ms: 5000,
        metrics: dict.from_list([#("warnings", "0")]),
      ))
    }
    Ok(Failure(code, error)) -> {
      Ok(StageResult(
        stage_name: "security",
        status: "failure",
        stdout: "",
        stderr: error,
        duration_ms: 5000,
        metrics: dict.from_list([#("exit_code", int.to_string(code))]),
      ))
    }
    Error(e) -> Error(e)
  }
}
```

**3. Test it**:

```gleam
pub fn test_security_stage_passes() {
  let workspace = create_test_workspace()
  stages.run_stage(test_task, workspace, security_stage())
  |> should.be_ok()
}
```

### Adding Support for a New Language

Example: Add Kotlin support

**1. Add to Language type** in `domain.gleam`:

```gleam
pub type Language {
  Go
  Gleam
  Rust
  Python
  Kotlin  // New!
}
```

**2. Add detection logic** in `repo.gleam`:

```gleam
pub fn detect_language_from_files(file_paths: List(String)) -> Result(Language, String) {
  case file_paths {
    // ... existing cases
    _ if list.any(file_paths, fn(f) { string.contains(f, "build.gradle.kts") }) ->
      Ok(Kotlin)
    _ -> Error("Could not detect language")
  }
}
```

**3. Add configuration** in `stages.gleam`:

```gleam
fn build_language_config(language: Language) -> Dict(String, String) {
  case language {
    Go -> dict.from_list([#("build", "go build ./...")])
    Gleam -> dict.from_list([#("build", "gleam build --target javascript")])
    Rust -> dict.from_list([#("build", "cargo build")])
    Python -> dict.from_list([#("build", "python -m py_compile src/")])
    Kotlin -> dict.from_list([
      #("build", "gradle build"),
      #("test", "gradle test"),
      #("lint", "ktlint"),
    ])
  }
}
```

**4. Test language detection**:

```gleam
pub fn test_detect_kotlin() {
  repo.detect_language_from_files(["build.gradle.kts", "src/Main.kt"])
  |> should.equal(Ok(Kotlin))
}
```

---

## Testing Patterns

### Unit Tests

Test individual functions in isolation:

```gleam
import gleeunit
import gleeunit/should

pub fn test_validate_slug_success() {
  domain.validate_slug("bd-52.1")
  |> should.be_ok()
}

pub fn test_validate_slug_failure() {
  domain.validate_slug("bd 52")  // Space not allowed
  |> should.be_error()
}

pub fn test_validate_slug_empty() {
  domain.validate_slug("")
  |> should.be_error()
}
```

### Integration Tests

Test multiple modules working together:

```gleam
pub fn test_full_pipeline_success() {
  let task = create_test_task()
  let workspace = worktree.create_workspace(task) |> result.unwrap(Nil)

  stages.run_all_stages(task, workspace)
  |> should.be_ok()
  |> list.length()
  |> should.equal(10)  // All 10 stages ran
}
```

### Property-Based Tests

Test with generated inputs:

```gleam
import gleam_community.gen

pub fn test_slug_validation_idempotent() {
  // slug_to_string(validate_slug(s)) == s for all valid s
  gen.string(gen.ascii_letters, 1, 50)
  |> gen.filter(fn(s) { string.contains(s, "-") || string.contains(s, ".") })
  |> gen.sample()
  |> list.each(fn(slug_str) {
    domain.validate_slug(slug_str)
    |> result.unwrap(Nil)
    |> domain.slug_to_string()
    |> should.equal(slug_str)
  })
}
```

---

## Debugging Tips

### Print Debugging

```gleam
import gleam/io

pub fn run_stage(task: Task) -> Result(StageResult, String) {
  io.debug(task)  // Print task structure
  // ... rest of function
}
```

### Read Saved Artifacts

After running a stage:

```bash
# See what was generated
cat .factory/bd-52.1/stage-implement.json

# See full output for debugging
cat .factory/bd-52.1/stage-implement-full.log

# See audit trail
cat .factory/bd-52.1/audit.yaml
```

### Run Single Stage for Debugging

```bash
# Run just the implement stage
factory stage bd-52.1 implement

# Check result
cat .factory/bd-52.1/stage-implement.json
```

### Test with Real Workspace

```bash
# Create a test workspace
factory new test-task

# Now cd into it and work
cd .factory/test-task/workspace

# Run commands to test
gleam build
gleam test

# View results
cd ../../../
cat .factory/test-task/stage-implement.json
```

---

## Common Development Tasks

### Fixing a Bug

1. **Reproduce with test**:
```gleam
pub fn test_bug_scenario() {
  // Write test that demonstrates the bug
  stages.run_stage(broken_task, workspace, stage)
  |> should.be_ok()  // This will fail
}
```

2. **Find the bug**:
```bash
gleam test --filter test_bug_scenario
# Run with debug output
```

3. **Fix the code**:
Edit the module to fix the issue

4. **Verify fix**:
```bash
gleam test --filter test_bug_scenario
# Should now pass
```

5. **Run all tests**:
```bash
gleam test
```

### Adding a New Configuration Option

1. **Update Domain**:
```gleam
// In domain.gleam
pub type Contract {
  Contract(
    // ... existing fields
    new_config_option: String,  // Add here
  )
}
```

2. **Update Parsing**:
```gleam
// In persistence.gleam
fn parse_contract_yaml(yaml: String) -> Result(Contract, String) {
  // Parse new_config_option from YAML
}
```

3. **Update Usage**:
```gleam
// In stages.gleam
fn run_stage(...) -> Result(...) {
  // Use task.contract.new_config_option
}
```

4. **Test it**:
```gleam
pub fn test_new_config_option() {
  let contract = Contract(..., new_config_option: "value")
  // Test behavior with new option
}
```

### Performance Optimization

1. **Identify bottleneck**:
```bash
# Time the stages
time factory stage bd-52.1 implement
time factory stage bd-52.1 unit-test
```

2. **Optimize**:
- Cache results where appropriate
- Parallelize independent operations
- Avoid repeated work

3. **Measure improvement**:
```bash
time factory stage bd-52.1 implement
# Compare before/after
```

---

## Code Style Guide

### Naming Conventions

```gleam
// Functions: snake_case
pub fn run_pipeline(task: Task) -> Result(Nil, String)

// Types: PascalCase
pub type TaskStatus { Created Ready InProgress }

// Constants: UPPER_SNAKE_CASE
const max_retries = 5

// Module-level functions: lowercase_with_underscores
fn helper_function() -> String
```

### Comments

```gleam
// Only document the "why", not the "what"

// Bad:
// This function validates the slug
fn validate_slug(s: String) -> Result(Slug, String)

// Good:
// Slugs identify tasks in the Beads system, so they must be valid identifiers
fn validate_slug(s: String) -> Result(Slug, String)
```

### Error Messages

```gleam
// Be specific and actionable
Error("Invalid slug: must contain only alphanumeric, hyphen, dot")

// Not generic
Error("Invalid input")
```

### Module Structure

```gleam
// 1. Imports at top
import gleam/list
import gleam/result

// 2. Type definitions
pub type MyType { ... }

// 3. Public functions
pub fn public_function() -> Result(String, String) { ... }

// 4. Private helper functions
fn private_helper() -> String { ... }
```

---

## Deployment

### Building for Release

```bash
# Build optimized version
gleam build

# Create distributable
mkdir -p build/bin
cp build/dev/javascript/factory/bin/factory build/bin/
```

### Testing Before Release

```bash
# Run all tests
gleam test

# Check formatting
gleam format --check src/ test/

# Manual smoke test
./build/dev/javascript/factory/bin/factory new test-task
./build/dev/javascript/factory/bin/factory stage test-task implement
```

### Version Bumping

```bash
# Update version in gleam.toml
[package]
name = "factory"
version = "0.2.0"  # Was 0.1.0

# Commit
git add gleam.toml
git commit -m "Bump version to 0.2.0"

# Tag
git tag v0.2.0
git push --tags
```

---

## Contributing

### Making a Change

1. **Create feature branch**:
```bash
git checkout -b feature/new-feature
```

2. **Write tests first**:
```gleam
pub fn test_new_feature() {
  // Test the desired behavior
}
```

3. **Implement feature**:
```gleam
pub fn new_feature() -> Result(String, String) {
  // Implementation
}
```

4. **Run tests**:
```bash
gleam test
gleam format --check src/
```

5. **Create pull request**:
```bash
git push origin feature/new-feature
# Create PR on GitHub
```

### Code Review Checklist

When reviewing changes:

- [ ] Tests pass: `gleam test`
- [ ] Code is formatted: `gleam format --check src/`
- [ ] Types are checked: `gleam build`
- [ ] No debug prints left: `io.debug` removed
- [ ] Comments explain "why" not "what"
- [ ] Error messages are helpful
- [ ] No new panics or unwraps

---

## Useful Resources

- **Gleam Docs**: https://gleam.run/
- **Gleam Community**: https://tour.gleam.run/
- **Jujutsu Docs**: https://jj-vcs.github.io/jj/
- **Moon Docs**: https://moonrepo.dev/
- **Factory Design**: See V2_DESIGN_INDEX.md

---

## Summary

To develop Factory:

1. **Understand the architecture** - Read ARCHITECTURE.md
2. **Know the domain model** - Read DOMAIN_MODEL.md
3. **Write tests first** - Then implement
4. **Run `gleam test` often** - Catch issues early
5. **Follow style guide** - Keep code consistent
6. **Document the why** - Not the what
7. **Ask questions** - It's a complex system

Welcome to Factory development!
