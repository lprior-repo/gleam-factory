Test Failure Analysis: 45 Failures

Generated: 2026-01-18
Total: 886 passed, 45 failures (4.8% failure rate)

FAILURE CATEGORIZATION

Pattern 1: Assertion Failures "False should equal True" (28 failures)
=========================================================================

Test File: factory_supervisor_test.gleam (15 failures)
- supervisor_starts_test
- signal_bus_accessible_test
- heartbeat_accessible_test
- multiple_supervisors_test
- heartbeat_tick_received_test
- signal_bus_publish_received_test
- supervisor_children_responsive_test
- accessor_functions_correct_test
- supervisor_config_flexibility_test
- start_link_all_actors_success_test
- supervisor_restarts_failed_child_test
- supervisor_returns_all_actor_subjects_test
- graceful_degradation_on_child_failure_test
- log_system_ready_outputs_message_test
- system_ready_after_all_services_started_test

Root Cause Analysis:
All tests call factory_supervisor.start_link(config) and expect Ok(_)
Tests then access fields like .signal_bus_subject, .heartbeat_subject
"False should equal True" error suggests:
  - Case statement not matching Ok pattern
  - OR supervisor.start_link() returning Error when it should return Ok
  - OR the returned Started type is not properly constructed

The logs show "Signal bus started" successfully, which suggests:
  - signal_bus.start_link() succeeds
  - heartbeat.start_link() succeeds
  - BUT factory_supervisor.start_link() itself is failing

Test structure:
  case factory_supervisor.start_link(config) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
  |> should.equal(Nil)

If start_link returns Error, should.fail() gets called, which returns False
Then should.equal(Nil) compares False to Nil

Likely issue: One of the child initializations in start_link is failing
- hardware_verification.verify() failing?
- One of the actor.start() calls returning Error?
- Function at end not returning Ok(Started)?

Test File: graceful_shutdown_test.gleam (3 failures)
- graceful_shutdown_test
- shutdown_idempotent_test
- shutdown_timeout_test

Root Cause: Same pattern - shutdown_system() returning Error
All tests expect Ok but get Error

Test File: pipeline_integration_test.gleam (10 failures)
- pipeline_supervisor_startup_test
- pipeline_signal_broadcast_test
- pipeline_heartbeat_polling_test
- pipeline_merge_queue_patch_absorption_test
- pipeline_test_failure_signal_test
- pipeline_golden_master_preparation_test
- pipeline_multiple_subscriptions_test
- pipeline_error_handling_resilience_test
- pipeline_complete_workflow_test
- pipeline_shutdown_test

Root Cause: Cascading from supervisor failures
All depend on factory_supervisor.start_link(config) returning Ok
Since supervisor tests fail, these are blocked


DEPENDENCY: pipeline_integration_test blocked by factory_supervisor_test


Pattern 2: Heartbeat Status Initialization (2 failures)
=========================================================================

Test File: heartbeat_test.gleam (2 failures)
- get_status_returns_current_state_test: Red should equal Green
- progress_buffer_bounded_test: Red should equal Green

Root Cause: Initial status initialization wrong
- heartbeat.start_link() initializes last_status: Red (line 63)
- Tests that call heartbeat.tick() and immediately check status get Red
- But tests expect Green on "true" command execution

Specific conflict:
From test (line 26-27): status |> should.equal(heartbeat.Green)
From src/heartbeat.gleam (line 63): last_status: Red

The comment on recent commit says:
"FIX(heartbeat): Initialize status as Red instead of Green"

But tests still expect Green initial state. This is intentional change
vs test expectation mismatch.


Pattern 3: Heartbeat Signal Broadcasting (5 failures)
=========================================================================

Test File: heartbeat_test.gleam (5 failures)
- transition_from_green_to_red_broadcasts_test_failure_test
- no_signal_broadcast_when_status_unchanged_green_test
- no_signal_broadcast_when_status_unchanged_red_test
- multiple_transitions_test
- factory_loop_phase_transitions_test (integration test from integration_test.gleam)

Root Cause: Signal broadcast not triggered on state transitions

Example test (line 47-67):
  let subscriber = process.new_subject()
  signal_bus.subscribe(bus, signal_bus.TestFailure, subscriber)
  heartbeat.start_link with test_cmd: "false"
  heartbeat.tick()
  process.receive(subscriber, 500) → expects Ok(signal_bus.TestFailure)
  → gets Error/timeout instead

Logs show transitions happening:
  "INFO: Heartbeat started with test_cmd: false"
  "INFO: Heartbeat started with test_cmd: true"

But custom error message: "Expected TestFailure signal on Green->Red transition"

This indicates:
1. Status transition happens (Green->Red detection works)
2. signal_bus.broadcast() calls exist in update_status (lines 130, 134)
3. BUT subscriber.receive() times out instead of getting signal

Likely issues:
- signal_bus.subscribe() not properly registering subscriber
- signal_bus.broadcast() not actually sending to subscribers
- OR signal is broadcast before subscriber connects


Pattern 4: Arithmetic Error "Badarith" (7 failures)
=========================================================================

Test File: performance_test.gleam (7 failures)
- benchmark_implement_stage_test
- benchmark_unit_test_stage_test
- benchmark_lint_stage_test
- benchmark_static_stage_test
- benchmark_integration_stage_test
- benchmark_throughput_calculation_test
- benchmark_memory_measurement_test

Root Cause: Division by zero or arithmetic on invalid type
- Badarith is Erlang arithmetic error (not caught in try/catch)
- All benchmarks call benchmark_stage() which calculates throughput
- Code (lines 52-55):
    let throughput = case duration_ms {
      0 -> 0.0
      d -> 1000.0 /. int_to_float(d)
    }

Problem: monotonic_time_ms() at lines 42, 47
  native = monotonic_time_native()
  native / divisor
  where divisor = 1_000_000

If Erlang's monotonic_time() returns value smaller than divisor:
  native / 1_000_000 → rounds to 0 (integer division)
  Then: 1000.0 /. 0.0 → Badarith (can't divide float by zero)

OR: the int_to_float() conversion is failing with Badarith


Pattern 5: Timeout Error in stages_test.gleam (1 failure)
=========================================================================

Error trace:
- Test: ExecuteGleamUnitTestTest from stages_test.gleam line 50
- Stacktrace shows: Process.RunCommand → Stages.ExecuteStage
- Process execution with os.erl:723 (GetData)
- Times out during gleam unit test execution

Root Cause: Subprocess timeout waiting for output
- Timeout in process.gleam:174 run_command
- Stages.gleam:230 gleam_unit_test anonymous function
- Test framework trying to run nested test and timing out
- Likely issue: test harness doesn't capture/close stdout properly
- Cascading: 1 test timeout causes 2 follow-up module load errors


Pattern 6: Module Load Errors from stages_test (2 failures)
=========================================================================

Cascading failures from timeout above:
1. "module 'stages_test'" undefined - test module won't load after timeout
2. Blame error - process group cleanup issue

Root Cause: Fallout from timeout
- When subprocess times out, test runner can't clean up properly
- Module definitions lost in process
- Need to fix underlying timeout first


SUMMARY BY ROOT CAUSE

1. Supervisor initialization incomplete (15 failures)
   - factory_supervisor.start_link() returning Error
   - One child actor start failing
   - Blocks all downstream integration tests

2. Shutdown not implemented (3 failures)
   - shutdown_system() exists but returns Error
   - Independent issue

3. Pipeline integration broken (10 failures)
   - Cascading from supervisor failures
   - Once supervisor fixed, these should resolve

4. Heartbeat initialization conflict (2 failures)
   - Code initialized as Red, tests expect Green
   - Minor test expectation vs code mismatch

5. Signal broadcast subscription issue (5 failures)
   - signal_bus.subscribe() or broadcast() not working
   - Subscriber doesn't receive signals
   - Depends on supervisor working first

6. Performance benchmark timing math (7 failures)
   - monotonic_time_ms() returning 0 or very small value
   - Division by zero in throughput calculation
   - Independent of other failures

7. Subprocess timeout cascade (3 failures)
   - 1 timeout in stages_test → 2 cascading module errors
   - Process cleanup not working properly


PRIORITY FIXING ORDER

TIER 1 (Blocking 25+ tests): MUST FIX FIRST
---

1. Fix factory_supervisor.start_link()
   Status: CRITICAL - blocks all integration tests
   Failures: 15 direct + 10 pipeline = 25 tests

   Find which child actor is failing in start_link:
   - hardware_verification.verify()?
   - signal_bus.start_link()?
   - heartbeat.start_link()?
   - resource_governor.start_link()?
   - workspace_manager.start_link()?
   - golden_master.start_link()?
   - Other actors?

   Action: Add detailed error logging to identify which step fails

2. Fix signal_bus subscription/broadcast
   Status: HIGH - blocks 5 signal tests
   After supervisor works, verify signal mechanism:
   - Does signal_bus.subscribe() register subscriber?
   - Does broadcast() actually send to registered subscribers?
   - Are signals being sent before subscriber connects?

   Action: Add logging to signal_bus to track publish/subscribe


TIER 2 (Once tier 1 done): 2-5 failures each
---

3. Fix heartbeat initialization logic
   Status: MEDIUM - causes 2 failures
   Decision needed:
   - Option A: Change test expectations to expect Red
   - Option B: Change heartbeat initialization to Green

   Depends on: What should the initial state be?

4. Fix performance benchmark timing
   Status: MEDIUM - causes 7 failures
   Fix: Ensure duration_ms never reaches throughput calculation as 0

   Options:
   - Add minimum elapsed time constant
   - Use monotonic_time with larger divisor
   - Check divisor before float conversion
   - Mock timing in tests


TIER 3 (Independent, can fix anytime)
---

5. Fix graceful shutdown
   Status: LOW - causes 3 failures
   Find: Why shutdown_system() returns Error
   Action: Add error logging

6. Fix stages test subprocess timeout
   Status: LOW - causes 3 failures (1 timeout + 2 cascading)
   Find: Why stdout not closing properly
   Action: Add timeout handling or fix process cleanup


CURRENT STATE INDICATORS

From recent commits:
- FIX(heartbeat): Initialize status as Red instead of Green
  → Intentional change but conflicts with test expectations

- FIX(factory_loop): Implement approval gate for Reviewing→Pushing
  → approval_gate blocking transition in test: expects Pushing, gets Reviewing

- FIX(domain): is_ready_for_integration should support Go and Rust
  → Suggests multi-language support work in progress

Logs showing transition errors:
- "ERROR: Unexpected phase/event in transition {event=PushSuccess, phase=reviewing}"
- "ERROR: Unexpected phase/event in transition {event=TestPassed, phase=rebasing}"

These indicate phase_handlers incomplete - missing event handlers for some states


QUICK REFERENCE TABLE

File                              Failures  Type              Root Cause
factory_supervisor_test.gleam     15        Ok/Error          start_link() failing
pipeline_integration_test.gleam   10        Ok/Error          Cascading supervisor
graceful_shutdown_test.gleam      3         Ok/Error          shutdown not working
heartbeat_test.gleam              9         Mixed             Status + broadcast
performance_test.gleam            7         Badarith          Division by zero
stages_test.gleam                 1         Timeout           Subprocess cleanup
integration_test.gleam            1         Assertion         Phase transition blocked
(cascading module errors)          2         Module load       Timeout fallout

Total: 45 failures
Blocking failures (must fix first): 1 core issue (supervisor start_link)
Cascading failures (resolve after): 23 tests
Independent failures: 22 tests

SUGGESTED DEBUGGING STEPS

1. In factory_supervisor.gleam start_link():
   Add logging AFTER each step:
   - After hardware_verification.verify()
   - After each signal_bus, heartbeat, resource_governor, etc start_link()
   - Before returning Ok(Started(...))

   Run: gleam test to see which step logs last before failure

2. In signal_bus.gleam:
   Add logging to subscribe() and broadcast():
   - Log when subscriber registered
   - Log when broadcast called
   - Log when signal sent to each subscriber

   Re-run heartbeat signal tests

3. In heartbeat.gleam:
   Check if update_status() ever calls broadcast():
   - Is condition "new_status != state.last_status" ever true?
   - Is broadcast line actually reached?

4. In performance_test.gleam:
   Check monotonic_time_ms():
   - What values does native return?
   - What values does divisor produce?
   - Add guard against duration_ms = 0 before throughput calc
