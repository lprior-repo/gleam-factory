# Troubleshooting Guide

Common issues and solutions for Gleam Factory.

## Setup Issues

### "escript not found" error
**Problem**: `error: Program not found. The program 'escript' was not found. Is it installed?`

**Solution**: Install Erlang/OTP. Factory requires Erlang to run compiled Gleam code.
```bash
# macOS
brew install erlang

# Linux (Ubuntu/Debian)
sudo apt-get install erlang-runtime

# Arch
pacman -S erlang
```

### "jj not found" error
**Problem**: Commands fail with "jj: command not found"

**Solution**: Install Jujutsu (jj) version control system
```bash
# macOS
brew install jujutsu

# Linux
cargo install jujutsu

# From source
https://github.com/martinvonz/jj
```

## Test Issues

### "Heartbeat test timeout"
**Problem**: `heartbeat_test.gleam` tests timeout waiting for signal

**Solution**:
- Check system load - tests may need longer timeouts under high load
- Increase `process.sleep()` durations in test if running on slow machine
- Run tests individually: `gleam test -- --exact test_name`

### "Signal bus subscription not receiving signals"
**Problem**: Tests subscribe to signal but `process.receive()` times out

**Solution**:
- Ensure signal is published to the bus AFTER subscription
- Check signal type matches subscription (case-sensitive)
- Increase timeout: `process.receive(subscriber, 5000)` instead of 500ms
- Verify bus is started before subscribing

### "Golden master tests fail with path issues"
**Problem**: `golden_master_test.gleam` fails with permission or directory errors

**Solution**:
- Check `/tmp` exists and is writable
- Ensure test cleanup removes files: `simplifile.delete_directory_all()`
- Use absolute paths in tests, not relative paths
- Run tests with proper permissions: `gleam test` (not `sudo`)

## Runtime Issues

### "Supervisor fails to start"
**Problem**: `factory_supervisor.start_link()` returns error

**Solution**:
- Check hardware requirements: `min_free_ram_mb` available
- Ensure all child services can start (signal_bus, heartbeat, etc.)
- Check logs via `factory_supervisor.log_system_ready()` output
- Verify golden_master path exists and is readable

### "Merge queue patch not absorbing"
**Problem**: `merge_queue.is_absorbing()` returns False when expecting True

**Solution**:
- Ensure patch proposal is sent before checking absorbing state
- Add sleep between `propose_patch()` and `is_absorbing()`: `process.sleep(100)`
- Check merge queue was started successfully with signal bus
- Verify patch hash is not empty string

### "Heartbeat test status not changing"
**Problem**: `heartbeat.get_status()` always returns Green even with failing tests

**Solution**:
- Check test command is actually failing (try running manually)
- Ensure test_cmd returns non-zero exit code on failure
- Add tick after status change: `heartbeat.tick(hb); process.sleep(100)`
- Verify signal bus is connected when starting heartbeat

## Logging Issues

### "Logging output not appearing"
**Problem**: `logging.log()` calls don't produce output

**Solution**:
- Logging uses `io.println()` which outputs to stdout
- Redirect stderr to stdout: `gleam test 2>&1`
- Check for output buffering issues in your shell
- Verify logging.log() is imported: `import logging`

### "Timestamp in logs is wrong"
**Problem**: Log timestamps don't match wall clock time

**Solution**:
- Erlang/OTP uses UTC internally
- Check system timezone matches expected
- Ensure Erlang has correct system time
- Log timestamps use ISO 8601 format: `2026-01-09T10:00:00Z`

## Feature Flags Issues

### "Feature flag validation fails"
**Problem**: `feature_flags.validate_config()` returns InvalidConfig

**Solution**:
- Check percentage is 0-100: `initial_percentage: 50` ✓
- Check error threshold is 0.0-1.0: `error_threshold: 0.05` ✓
- Ensure both are correct type: Int and Float
- Review config before passing: print values if unsure

### "Rollback not triggered on error spike"
**Problem**: Feature flag doesn't rollback even with high error rate

**Solution**:
- Verify error threshold is realistic: 0.05 = 5% errors
- Check health metrics are updating: `request_count > 0`
- Ensure error_rate calculation: `error_count / request_count`
- Call `advance_rollout()` with current metrics after each stage

## Persistence Issues

### "Task records not saving"
**Problem**: `persistence.save_task_record()` returns error

**Solution**:
- Ensure `.factory` directory exists and is writable
- Check file permissions: should be user-readable/writable
- Verify JSON serialization works: call `persistence.record_to_json()` separately
- Check disk space available

### "Roundtrip deserialization fails"
**Problem**: `persistence.record_to_task()` returns Error

**Solution**:
- Verify JSON is valid: use online JSON validator
- Check all required fields present in JSON
- Ensure language is one of: "go", "gleam", "rust", "python"
- Check status is one of: "created", "in_progress", "passed", "failed", "integrated"
- Validate slug format: 1-50 chars, [a-z0-9_-]

## Integration Test Issues

### "CLI integration tests timeout"
**Problem**: `cli_integration_test.gleam` times out waiting for jj commands

**Solution**:
- Ensure jj is installed and in PATH
- Check test setup: `setup_jj_repo()` completes successfully
- Increase timeout in `process.run_command()` if needed
- Verify /tmp has space and permissions for test repos

### "Pipeline integration test hangs"
**Problem**: `pipeline_integration_test.gleam` never completes

**Solution**:
- Add `process.sleep()` between state changes
- Check for deadlocks in actor message handling
- Verify signal_bus is not full of messages
- Try test in isolation: create smaller test file

## Performance Issues

### "Tests run slowly"
**Problem**: Test suite takes longer than expected

**Solution**:
- Reduce `test_interval_ms` in heartbeat config
- Decrease `process.sleep()` durations if safe
- Run tests in parallel (if gleam test supports it)
- Profile with `time gleam test`

### "High memory usage during tests"
**Problem**: System runs out of memory during test run

**Solution**:
- Reduce max_workspaces in supervisor config
- Cleanup temporary files in teardown
- Reduce list sizes in test data
- Check for memory leaks in long-running tests

## Debugging Strategies

### Enable detailed logging
```gleam
// Add to suspect functions
logging.log(logging.Debug, "Function entry: " <> debug_info, dict.new())
logging.log(logging.Info, "State: " <> state_str, dict.new())
logging.log(logging.Error, "Error occurred: " <> error_msg, dict.new())
```

### Use assert statements
```gleam
let assert Ok(value) = potentially_failing_operation()
// Fails with clear message if operation returns Error
```

### Inspect types
```gleam
let value = process.receive(subscriber, timeout)
io.println(string.inspect(value))  // Print Gleam term representation
```

### Check supervisor health
```gleam
// Verify all actors started
let bus = factory_supervisor.get_signal_bus(started)
let hb = factory_supervisor.get_heartbeat(started)
let queue = factory_supervisor.get_merge_queue(started)
// If any of these fail, supervisor startup had issues
```

## Getting Help

1. **Check tests first**: Look for similar test in `test/*.gleam`
2. **Read module docs**: Check pub fn comments in `src/*.gleam`
3. **Trace error messages**: Use logging to see execution flow
4. **Simplify reproducer**: Create minimal failing example
5. **Check Gleam docs**: https://gleam.run/documentation/
6. **Check Erlang/OTP docs**: Underlying runtime behavior

## Known Limitations

- Signal timeouts may need tuning for slow systems
- Feature flags require manual metrics updates
- No automatic retry beyond TCR stage retries
- Persistence uses JSON, not distributed database
- Logging outputs to stdout only, not file logging

## Contributing Fixes

If you find an issue not listed here:
1. Create minimal reproducer
2. Check logs and error messages
3. Write test case that fails
4. Fix implementation
5. Document in this guide
