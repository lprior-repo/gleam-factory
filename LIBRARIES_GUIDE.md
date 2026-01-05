# Factory Gleam - Libraries Integration Guide

**How to use the Gleam ecosystem to build Factory**

**Last Updated**: January 5, 2026

---

## Overview

Factory uses the best-in-class Gleam libraries for CLI, shell operations, and user interaction. This document explains what each library does and how to use it.

### Libraries Added

```bash
gleam add argv clip glint glitzer rad shellout spinner stdin
```

---

## Library Reference

### 1. **argv** - Command Line Arguments

**Purpose**: Access command line arguments passed to the program

**Key Functions**:
```gleam
import argv

pub fn load() -> Arguments {
  Arguments(program: String, arguments: List(String))
}
```

**Usage in Factory**:
```gleam
import argv

pub fn main() {
  case argv.load().arguments {
    [] -> show_help()
    ["new", slug] -> handle_new(slug)
    ["stage", slug, stage_name] -> handle_stage(slug, stage_name)
    [unknown, ..] -> show_error("Unknown command: " <> unknown)
  }
}
```

**Benefits**:
- ✓ Cross-platform argument access
- ✓ Works on all Gleam targets (Erlang, JavaScript, Node, Deno, Bun)
- ✓ Simple, pure function

---

### 2. **glint** - CLI Framework

**Purpose**: Powerful command-line argument parsing and routing

**Key Concepts**:
- Commands: Each action (new, stage, approve)
- Flags: Options (--dry-run, --force, --interactive)
- Arguments: Required values (slug, stage-name)

**Usage in Factory**:
```gleam
import glint
import glint/flag

let cmd = glint.new()
  |> glint.add(["new"], handle_new_command)
  |> glint.add(["stage"], handle_stage_command)
  |> glint.run(argv.load().arguments)
```

**Benefits**:
- ✓ Structured argument parsing
- ✓ Automatic help generation
- ✓ Flag validation
- ✓ Type-safe commands

**See Also**: `cli_enhanced.gleam` for full examples

---

### 3. **clip** - Option Parser

**Purpose**: Alternative to glint for simpler option parsing

**Usage**:
```gleam
import clip

pub fn parse_task() -> Result(TaskOptions, ParseError) {
  use slug <- clip.required_positional("slug")
  use contract <- clip.optional_named("contract")
  use interactive <- clip.flag("interactive")

  Ok(TaskOptions(slug, contract, interactive))
}
```

**When to Use**:
- Simpler CLIs with fewer flags
- Quick scripts that don't need full framework
- Lightweight alternative to glint

**Factory Usage**: Can be used for simpler command parsing where glint is overkill

---

### 4. **shellout** - Cross-Platform Shell Execution

**Purpose**: Execute shell commands safely and get results

**Key Functions**:
```gleam
import shellout

// Run a command
shellout.command(
  "gleam",           // Command
  ["build"],         // Arguments
  "/home/user/repo"  // Working directory
) -> Result(String, ShelloutError)
```

**Usage in Factory**:
```gleam
import shellout

pub fn run_stage_command(stage: String, path: String) -> Result(String, String) {
  case stage {
    "implement" ->
      shellout.command("gleam", ["build", "--target", "javascript"], path)

    "unit-test" ->
      shellout.command("gleam", ["test"], path)

    "coverage" ->
      shellout.command("gleam", ["test", "--cover"], path)

    _ -> Error("Unknown stage: " <> stage)
  }
  |> result.map_error(fn(err) {
    "Shell command failed: " <> string.inspect(err)
  })
}
```

**Benefits**:
- ✓ Cross-platform (Windows, Mac, Linux)
- ✓ Safe argument passing (no injection)
- ✓ Direct result capture
- ✓ Clean error handling

**Critical for Factory**: This is how we actually run tests, build code, etc.

---

### 5. **spinner** - Progress Indicators

**Purpose**: Show animated loading spinners while operations run

**Usage**:
```gleam
import spinner

pub fn run_with_progress(description: String, operation: fn() -> Result(a, e)) -> Result(a, e) {
  // Show spinner while operation runs
  let spin_ref = spinner.start(description)
  let result = operation()
  spinner.stop(spin_ref)
  result
}
```

**Factory Examples**:
```gleam
// While running a stage
let spin = spinner.start("Running unit tests...")
let result = run_unit_tests(workspace)
spinner.stop(spin)

// While waiting for approval
let spin = spinner.start("Waiting for human approval...")
let approved = wait_for_approval(task)
spinner.stop(spin)

// While deploying
let spin = spinner.start("Deploying to 1% of users...")
let rollout = enable_feature_flag(task, 1)
spinner.stop(spin)
```

**Benefits**:
- ✓ Visual feedback to user
- ✓ Shows something is happening
- ✓ Better UX than silent operations
- ✓ Animated (not boring static text)

---

### 6. **stdin** - Interactive Input

**Purpose**: Read user input from standard input

**Usage**:
```gleam
import stdin

pub fn get_user_confirmation(message: String) -> Result(Bool, Nil) {
  io.print(message <> " (y/n): ")
  case stdin.read_line() {
    Ok("y") | Ok("yes") | Ok("Y") -> Ok(True)
    Ok("n") | Ok("no") | Ok("N") -> Ok(False)
    Ok(input) -> get_user_confirmation("Please enter 'y' or 'n'")
    Error(_) -> Error(Nil)
  }
}
```

**Factory Examples**:
```gleam
// Confirm deployment
pub fn confirm_deployment(task: Task) -> Result(Bool, String) {
  io.println("About to deploy " <> task.slug)
  io.println("Coverage: 89%")
  io.println("Lines changed: 47")

  use confirmed <- result.try(get_user_confirmation("Approve?"))
  Ok(confirmed)
}

// Get rollout strategy
pub fn choose_rollout_strategy() -> Result(RolloutStrategy, String) {
  io.println("Choose rollout strategy:")
  io.println("1. Gradual (1% → 10% → 100%)")
  io.println("2. Staged (10% → 50% → 100%)")
  io.println("3. Immediate (100%)")

  case stdin.read_line() {
    Ok("1") -> Ok(Gradual)
    Ok("2") -> Ok(Staged)
    Ok("3") -> Ok(Immediate)
    _ -> Error("Invalid choice")
  }
}
```

**Benefits**:
- ✓ Synchronous input reading
- ✓ Works on all targets
- ✓ Simple API
- ✓ Perfect for CLI confirmations

---

### 7. **glitzer** - Console Effects & Formatting

**Purpose**: Color output, formatting, and visual effects

**Usage**:
```gleam
import glitzer

pub fn print_success(message: String) {
  glitzer.bold(message)  // Bold text
  glitzer.green(message)  // Colored text
  glitzer.bright_green(message)  // Bright colors
}

pub fn print_error(message: String) {
  glitzer.red("ERROR: " <> message)
}
```

**Factory Examples**:
```gleam
// Stage results
pub fn print_stage_result(stage: String, status: String) {
  case status {
    "success" ->
      io.println(glitzer.green("✓") <> " " <> stage)
    "failure" ->
      io.println(glitzer.red("✗") <> " " <> stage)
    "skipped" ->
      io.println(glitzer.yellow("⊘") <> " " <> stage)
  }
}

// Approval prompt
pub fn print_approval_prompt(task: Task) {
  io.println(glitzer.bold("Ready for deployment"))
  io.println("")
  io.println("Task: " <> glitzer.cyan(task.slug))
  io.println("Coverage: " <> glitzer.green("89%"))
  io.println("Files: " <> glitzer.yellow("2"))
  io.println("")
  io.println(glitzer.bright_cyan("Approve? (y/n)"))
}

// Error context
pub fn print_error_with_context(error: String) {
  io.println(glitzer.bright_red("✗ Error: ") <> error)
}
```

**Benefits**:
- ✓ Makes CLI output readable and professional
- ✓ Color helps distinguish errors from success
- ✓ Bold/bright helps highlight important info
- ✓ Improves user experience significantly

---

### 8. **rad** - Task Runner Integration

**Purpose**: Run tasks defined in rad configuration

**Gleam Integration**:
```gleam
// rad.yml can define:
build:
  command: gleam build

test:
  command: gleam test

fmt:
  command: gleam format src/
```

**Factory Usage**: Can integrate with moon.dev and other task runners

---

## Putting It All Together

### Example: Complete `new` Command

```gleam
import argv
import glint
import glint/flag
import shellout
import spinner
import stdin
import glitzer
import io

pub fn handle_new_command(input) -> Result(Command, String) {
  use slug <- glint.string_flag("slug", flag.Flag() |> flag.required())(input)
  use contract <- glint.optional_string_flag(
    "contract",
    flag.Flag() |> flag.description("Path to contract.yaml")
  )(input)
  use interactive <- glint.bool_flag(
    "interactive",
    flag.Flag() |> flag.default(False)
  )(input)

  Ok(New(slug, contract, interactive))
}

pub fn execute_new(slug: String, contract: Option(String), interactive: Bool) -> Result(Nil, String) {
  // Show header
  io.println("")
  io.println(glitzer.bold(glitzer.cyan("Creating new task...")))
  io.println("")

  // Validate slug
  use valid_slug <- result.try(validate_slug(slug))
  io.println(glitzer.green("✓") <> " Slug validated: " <> slug)

  // Load contract
  use loaded_contract <- result.try(
    case contract {
      Some(path) -> {
        io.println("Loading contract from: " <> path)
        load_contract_file(path)
      }
      None -> {
        io.println("Using default contract")
        Ok(default_contract())
      }
    }
  )

  // Confirm if interactive
  use confirmed <- result.try(
    case interactive {
      True -> {
        io.println("")
        io.println("Task details:")
        io.println("  Slug: " <> slug)
        io.println("  Contract: default")

        case stdin.read_line() {
          Ok("y") | Ok("yes") -> Ok(True)
          Ok("n") | Ok("no") -> Error("Cancelled by user")
          _ -> Error("Invalid input")
        }
      }
      False -> Ok(True)
    }
  )

  // Create workspace with spinner
  let spin = spinner.start("Creating jj workspace...")
  use workspace <- result.try(create_jj_workspace(slug))
  spinner.stop(spin)
  io.println(glitzer.green("✓") <> " Workspace created")

  // Save task
  let spin = spinner.start("Saving task definition...")
  use _ <- result.try(save_task(slug, loaded_contract))
  spinner.stop(spin)
  io.println(glitzer.green("✓") <> " Task saved")

  io.println("")
  io.println(glitzer.bright_green("✓ Task created successfully!"))
  io.println("")
  io.println("Next steps:")
  io.println("  1. cd " <> workspace.path)
  io.println("  2. Implement your changes")
  io.println("  3. factory stage " <> slug <> " implement")
  io.println("")

  Ok(Nil)
}
```

### Example: Complete `approve` Command

```gleam
pub fn handle_approval(slug: String, strategy: Option(String), force: Bool) -> Result(Nil, String) {
  // Load task and check all stages passed
  use task <- result.try(load_task(slug))
  use _ <- result.try(verify_all_stages_passed(task))

  // Load metrics
  use metrics <- result.try(load_metrics(slug))

  // Display approval prompt with colors
  io.println("")
  io.println(glitzer.bold(glitzer.bright_cyan("=== Deployment Approval ===")))
  io.println("")

  print_metrics(metrics)

  io.println("")
  io.println("Rollout Strategy: Gradual")
  io.println("  Phase 1: 1% of users (5 minutes)")
  io.println("  Phase 2: 10% of users (5 minutes)")
  io.println("  Phase 3: 100% of users (full rollout)")
  io.println("")

  // Wait for confirmation (unless --force)
  use confirmed <- result.try(
    case force {
      True -> {
        io.println(glitzer.yellow("⚠ Forced approval (--force used)"))
        Ok(True)
      }
      False -> {
        io.print(glitzer.bright_cyan("Approve deployment? (y/n): "))
        case stdin.read_line() {
          Ok("y") | Ok("yes") -> Ok(True)
          Ok("n") | Ok("no") -> Error("Deployment rejected")
          _ -> Error("Invalid input")
        }
      }
    }
  )

  // Deploy with spinner
  let spin = spinner.start("Enabling feature flag to 1%...")
  use _ <- result.try(enable_feature_flag(task.slug, 1))
  spinner.stop(spin)

  io.println(glitzer.green("✓") <> " Feature flag enabled")
  io.println(glitzer.green("✓") <> " Monitoring started")
  io.println(glitzer.green("✓") <> " Deployment in progress")
  io.println("")

  Ok(Nil)
}
```

---

## Best Practices

### 1. Always Use Spinners for Long Operations
```gleam
// Good
let spin = spinner.start("Running tests...")
let result = run_tests()
spinner.stop(spin)

// Bad - user doesn't know what's happening
let result = run_tests()
```

### 2. Validate Early, Confirm Late
```gleam
// Good - validate input immediately
use slug <- result.try(validate_slug(slug))

// Then ask for confirmation
use confirmed <- result.try(get_confirmation())

// Bad - ask before validating
use confirmed <- result.try(get_confirmation())
use slug <- result.try(validate_slug(slug))
```

### 3. Use Color for Status, Not Decoration
```gleam
// Good - color conveys meaning
io.println(glitzer.green("✓ Tests passed"))
io.println(glitzer.red("✗ Coverage failed"))

// Bad - color is just flashy
io.println(glitzer.bright_magenta("Running tests..."))
```

### 4. Handle stdin Errors Gracefully
```gleam
// Good
case stdin.read_line() {
  Ok(input) -> process(input)
  Error(_) -> handle_eof_or_error()
}

// Bad
case stdin.read_line() {
  Ok(input) -> process(input)
  Error(_) -> panic("stdin failed")  // No panics!
}
```

### 5. Structure Commands with glint
```gleam
// Good - clean, extensible
glint.new()
  |> glint.add(["new"], handle_new)
  |> glint.add(["stage"], handle_stage)
  |> glint.add(["approve"], handle_approve)

// Bad - big match statement
case argv.load().arguments {
  ["new", ..] -> ...
  ["stage", ..] -> ...
  // Gets unwieldy with many commands
}
```

---

## Troubleshooting

### "Command not found" with shellout

**Problem**: `shellout.command("gleam", ...)` fails with "command not found"

**Solution**: Make sure the command is in PATH, or use absolute path:
```gleam
shellout.command("/usr/bin/gleam", ["build"], path)
```

### stdin doesn't work in tests

**Problem**: Tests hang when reading from stdin

**Solution**: Don't use stdin in testable functions. Read in main, pass to functions:
```gleam
// Not testable
fn handle_with_input() -> String {
  stdin.read_line()  // Blocks in tests
}

// Testable
fn handle_input(input: String) -> String {
  process(input)
}

pub fn main() {
  case stdin.read_line() {
    Ok(input) -> handle_input(input)
    Error(_) -> Nil
  }
}
```

### Spinner reference from wrong context

**Problem**: `spinner.stop()` called from different function than `spinner.start()`

**Solution**: Return spinner reference or pass through:
```gleam
// Good
fn run_operation() -> Result(String, Error) {
  let spin = spinner.start("Working...")
  let result = operation()
  spinner.stop(spin)
  result
}

// Not recommended
let GLOBAL_SPIN = spinner.start("Working...")  // Global state
```

---

## Summary

The Gleam ecosystem provides excellent libraries for building Factory:

| Library | Purpose | Critical |
|---------|---------|----------|
| **argv** | Get CLI arguments | ✓ Core |
| **glint** | Parse commands & flags | ✓ Core |
| **clip** | Simple option parsing | Alternative |
| **shellout** | Run shell commands | ✓ Critical |
| **spinner** | Progress indicators | ✓ UX |
| **stdin** | User input | ✓ Approval |
| **glitzer** | Colored output | ✓ UX |
| **rad** | Task running | Integration |

Use them together to create a professional, user-friendly CLI that's a joy to use!
